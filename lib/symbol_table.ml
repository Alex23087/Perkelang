open Ast

let type_symbol_table :
    (perkident, perktype * string option * perkident list) Hashtbl.t =
  Hashtbl.create 10

let lookup_type (id : perkident) : perktype option =
  if Hashtbl.mem type_symbol_table id then
    let t, _code, _deps = Hashtbl.find type_symbol_table id in
    Some t
  else None

let rec resolve_type (typ : perktype) : perktype =
  let attrs, typ', quals = typ in
  ( attrs,
    (match typ' with
    | Basetype t -> (
        match lookup_type t with None -> typ' | Some (_, t, _) -> t)
    | Pointertype t -> Pointertype (resolve_type t)
    | Funtype (params, ret) ->
        Funtype (List.map resolve_type params, resolve_type ret)
    | Arraytype (t, n) -> Arraytype (resolve_type t, n)
    (* | Structtype t -> (
        match lookup_type t with None -> typ' | Some (_, t, _) -> t) *)
    | Structtype _t -> typ'
    | ArcheType (name, decls) ->
        let decls' = List.map (fun (t, id) -> (resolve_type t, id)) decls in
        ArcheType (name, decls')
    | ArchetypeSum ts -> ArchetypeSum (List.map resolve_type ts)
    | Modeltype (name, archetypes, decls, constr_params) ->
        let decls' = List.map (fun (t, id) -> (resolve_type t, id)) decls in
        Modeltype (name, archetypes, decls', constr_params)
    | Optiontype t -> Optiontype (resolve_type t)
    | Tupletype t -> Tupletype (List.map resolve_type t)
    | Vararg -> Vararg
    | Infer -> Infer),
    quals )

let rec type_descriptor_of_perktype (t : perktype) : string =
  let _, t, _ = t in
  match t with
  | Basetype s -> s
  | Structtype s -> s
  | Funtype (args, ret) ->
      let args_str =
        String.concat "__" (List.map type_descriptor_of_perktype args)
      in
      Printf.sprintf "l_%s_to_%s_r" args_str (type_descriptor_of_perktype ret)
  | Pointertype t -> Printf.sprintf "%s_ptr" (type_descriptor_of_perktype t)
  | Arraytype (t, _) -> Printf.sprintf "%s_arr" (type_descriptor_of_perktype t)
  | Vararg ->
      "vararg"
      (* This is probably problematic, cannot define function pointers with ... . Nvm, apparently you can 😕*)
  | ArcheType (name, _decls) -> name
  | ArchetypeSum archs ->
      "Sum" ^ String.concat "Plus" (List.map type_descriptor_of_perktype archs)
  | Modeltype (name, _archs, _decls, _constr_params) -> name
  | Optiontype t -> Printf.sprintf "%s_opt" (type_descriptor_of_perktype t)
  | Infer -> failwith "Impossible: type has not been inferred"
  | Tupletype ts ->
      Printf.sprintf "tup_%s_le"
        (String.concat "__" (List.map type_descriptor_of_perktype ts))

let print_type_symbol_table () =
  Printf.printf "Type Symbol Table:\n";
  Hashtbl.iter
    (fun id (typ, _code, deps) ->
      Printf.printf "Identifier: %s, Type: %s, Dependencies: [%s]\n" id
        (type_descriptor_of_perktype typ)
        (String.concat ", " deps))
    type_symbol_table

let rec bind_type (id : perkident) (t : perktype) =
  if not (Hashtbl.mem type_symbol_table id) then
    Hashtbl.add type_symbol_table id (t, None, dependencies_of_type t)

and dependencies_of_type (typ : perktype) : perkident list =
  let rec dependencies_of_type_aux typ =
    let typ = resolve_type typ in
    let _, typ', _ = typ in
    match typ' with
    | Basetype _ -> []
    | Pointertype t -> dependencies_of_type_aux t
    | Funtype (params, ret) ->
        type_descriptor_of_perktype typ
        :: (List.flatten (List.map dependencies_of_type_aux params)
           @ dependencies_of_type_aux ret)
    | Arraytype (t, _) -> dependencies_of_type_aux t
    | Structtype _t -> []
    | ArcheType (name, decls) ->
        name
        :: List.flatten
             (List.map (fun (t, _) -> dependencies_of_type_aux t) decls)
    | ArchetypeSum ts ->
        List.flatten (List.map (fun t -> dependencies_of_type_aux t) ts)
    | Modeltype (name, archetypes, decls, constr_params) ->
        let decls =
          List.map
            (fun (typ, id) ->
              match typ with
              | a, Funtype (params, ret), d ->
                  ( ( a,
                      Funtype
                        ( ([], Pointertype ([], Basetype name, []), []) :: params,
                          ret ),
                      d ),
                    id )
              | _ -> (typ, id))
            decls
        in
        (name :: archetypes)
        @ List.flatten
            (List.map (fun (t, _) -> dependencies_of_type_aux t) decls)
        @ List.flatten
            (List.map (fun t -> dependencies_of_type_aux t) constr_params)
    | Optiontype t -> dependencies_of_type_aux t
    | Tupletype ts ->
        type_descriptor_of_perktype typ
        :: List.flatten (List.map dependencies_of_type_aux ts)
    | Vararg -> []
    | Infer -> []
  in
  dependencies_of_type_aux typ
  |> List.sort_uniq String.compare
  |> List.filter (fun s -> s <> type_descriptor_of_perktype typ)

let rec bind_type_if_needed (typ : perktype) =
  let typ' = resolve_type typ in
  match typ' with
  | _, Basetype _t, _ -> ()
  | _, Pointertype t, _ -> bind_type_if_needed t
  | _, Funtype (params, ret), _ ->
      bind_type (type_descriptor_of_perktype typ') typ';
      List.iter bind_type_if_needed params;
      bind_type_if_needed ret
  | _, Arraytype (t, _), _ -> bind_type_if_needed t
  | _, Structtype _, _ -> ()
  | _, ArcheType (name, decls), _ ->
      bind_type name typ';
      List.iter (fun (typ, _id) -> bind_type_if_needed typ) decls
  | _, ArchetypeSum _ts, _ ->
      bind_type (type_descriptor_of_perktype typ') typ';
      List.iter bind_type_if_needed _ts
  | _, Modeltype (name, _archetypes, decls, constr_params), _ ->
      bind_type name typ';
      List.iter (fun (typ, _id) -> bind_type_if_needed typ) decls;
      List.iter (fun typ -> bind_type_if_needed typ) constr_params
  | _, Optiontype t, _ -> bind_type_if_needed t
  | _, Tupletype ts, _ ->
      bind_type (type_descriptor_of_perktype typ') typ';
      List.iter bind_type_if_needed ts
  | _, Vararg, _ -> ()
  | _, Infer, _ -> ()
