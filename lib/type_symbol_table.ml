open Ast
open Errors
open Utils
open Lambda_env_AI_unifier

let type_symbol_table : (perkident, perktype * string option) Hashtbl.t =
  Hashtbl.create 10

let lookup_type (id : perkident) : perktype option =
  if Hashtbl.mem type_symbol_table id then
    let t, _code = Hashtbl.find type_symbol_table id in
    Some t
  else None

let resolve_count, resolve_hit, resolve_miss = (ref 0, ref 0, ref 0)

let resolve_type (typ : perktype) : perktype =
  let type_resolve_table : (perktype, perktype) Hashtbl.t = Hashtbl.create 10 in
  let rec resolve_type_aux ?(unfold : int = 0) (typ : perktype)
      (lst : perktype list) : perktype * perktype list =
    resolve_count := !resolve_count + 1;
    say_here (Printf.sprintf "resolve_type: %s" (show_perktype typ));
    if unfold <= 0 then (typ, typ :: lst)
    else
      match Hashtbl.find_opt type_resolve_table typ with
      | Some t ->
          resolve_hit := !resolve_hit + 1;
          (t, t :: lst)
      | None ->
          resolve_miss := !resolve_miss + 1;
          let resolved_type, visited =
            if List.mem typ lst then (typ, lst)
            else
              let a, typ', q = typ in
              match typ' with
              | Basetype t ->
                  ( ( a,
                      (match lookup_type t with
                      | None -> typ'
                      | Some (_, t, _) -> t),
                      q ),
                    typ :: lst )
              | Pointertype t ->
                  let lst = typ :: lst in
                  let res_t, res_l =
                    resolve_type_aux ~unfold:(unfold - 1) t lst
                  in
                  ((a, Pointertype res_t, q), res_l)
              | Funtype (params, ret) ->
                  let lst = typ :: lst in
                  let params_t, params_l =
                    List.fold_right
                      (fun param (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        (res_t :: acc, res_l))
                      params ([], lst)
                  in
                  let ret_t, ret_l =
                    resolve_type_aux ~unfold:(unfold - 1) ret params_l
                  in
                  ((a, Funtype (params_t, ret_t), q), ret_l)
              | Lambdatype (params, ret, _free_vars) ->
                  let lst = typ :: lst in
                  let params_t, params_l =
                    List.fold_right
                      (fun param (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        (res_t :: acc, res_l))
                      params ([], lst)
                  in
                  let ret_t, ret_l =
                    resolve_type_aux ~unfold:(unfold - 1) ret params_l
                  in
                  (* TODO: Check if free vars need to be resolved *)
                  ((a, Lambdatype (params_t, ret_t, _free_vars), q), ret_l)
              | Arraytype (t, n) ->
                  let lst = typ :: lst in
                  let ret_t, ret_l =
                    resolve_type_aux ~unfold:(unfold - 1) t lst
                  in
                  ((a, Arraytype (ret_t, n), q), ret_l)
              | Structtype _t -> (typ, lst)
              | ArcheType (name, decls) ->
                  let lst = typ :: lst in
                  let decls_t, decls_l =
                    List.fold_right
                      (fun (param, ide) (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        ((res_t, ide) :: acc, res_l))
                      decls ([], lst)
                  in
                  ((a, ArcheType (name, decls_t), q), decls_l)
              | ArchetypeSum ts ->
                  let lst = typ :: lst in
                  let ts_t, ts_l =
                    List.fold_right
                      (fun param (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        (res_t :: acc, res_l))
                      ts ([], lst)
                  in
                  ((a, ArchetypeSum ts_t, q), ts_l)
              | Modeltype (name, archetypes, decls, constr_params) ->
                  let lst = typ :: lst in
                  let decls_t, decls_l =
                    List.fold_right
                      (fun (param, ide) (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        ((res_t, ide) :: acc, res_l))
                      decls ([], lst)
                  in
                  ( (a, Modeltype (name, archetypes, decls_t, constr_params), q),
                    decls_l )
              | Optiontype t ->
                  let lst = typ :: lst in
                  let ret_t, ret_l =
                    resolve_type_aux ~unfold:(unfold - 1) t lst
                  in
                  ((a, Optiontype ret_t, q), ret_l)
              | Tupletype ts ->
                  let lst = typ :: lst in
                  let ts_t, ts_l =
                    List.fold_right
                      (fun param (acc, lst) ->
                        let res_t, res_l =
                          resolve_type_aux ~unfold:(unfold - 1) param lst
                        in
                        (res_t :: acc, res_l))
                      ts ([], lst)
                  in
                  ((a, Tupletype ts_t, q), ts_l)
              | Vararg -> ((a, Vararg, q), lst)
              | Infer -> ((a, Infer, q), lst)
          in
          Hashtbl.add type_resolve_table typ resolved_type;
          (resolved_type, visited)
    (* | Structtype t -> (
          match lookup_type t with None -> typ' | Some (_, t, _) -> t) *)
  in
  fst (resolve_type_aux ~unfold:2 typ [])

(* Returns a C type for the input perktype. The types returned are the ones generated by the various synthesised typedefs *)
let rec type_descriptor_of_perktype ?(voidize : bool = false) (t : perktype) :
    string =
  let _, t', _ = resolve_type t in
  match t' with
  | Basetype s -> s
  | Structtype s ->
      s (* TODO: This is wrong, fix once structs are properly implemented *)
  | Funtype (args, ret) ->
      let args_str =
        String.concat "__"
          (List.map (type_descriptor_of_perktype ~voidize) args)
      in
      Printf.sprintf "l_%s_to_%s_r" args_str
        ((type_descriptor_of_perktype ~voidize) ret)
  | Lambdatype (_args, _ret, _free_vars) ->
      let lambda_type_desc =
        (type_descriptor_of_perktype ~voidize)
          (func_of_lambda_void ([], t', []))
      in
      (* let environment_type_desc = type_descriptor_of_environment free_vars in *)
      let environment_type_desc = get_type_descriptor_for_partition t in
      let capture_type_desc = lambda_type_desc ^ "_" ^ environment_type_desc in
      capture_type_desc
  | Pointertype t ->
      Printf.sprintf "%s_ptr" ((type_descriptor_of_perktype ~voidize) t)
  | Arraytype (t, None) ->
      Printf.sprintf "%s_arr" ((type_descriptor_of_perktype ~voidize) t)
  | Arraytype (t, Some n) ->
      Printf.sprintf "%s_%d_arr" ((type_descriptor_of_perktype ~voidize) t) n
  | Vararg ->
      "vararg"
      (* This is probably problematic, cannot define function pointers with ... . Nvm, apparently you can 😕*)
  | ArcheType (name, _decls) -> name
  | ArchetypeSum _archs -> archetype_sum_name t
  | Modeltype (name, _archs, _decls, _constr_params) ->
      if voidize then type_descriptor_of_perktype void_pointer else name
  | Optiontype t ->
      Printf.sprintf "%s_opt" ((type_descriptor_of_perktype ~voidize) t)
  | Infer ->
      raise
        (Not_inferred
           "Impossible: type has not been inferred in \
            type_descriptor_of_perktype")
  | Tupletype ts ->
      Printf.sprintf "tup_%s_le"
        (String.concat "__"
           (List.map (type_descriptor_of_perktype ~voidize) ts))

(* and type_descriptor_of_environment (_free_vars : perkvardesc list) : string = *)
(* "env_" *)
(* ^ String.concat "_"
      (List.map (fun (typ, _id) -> type_descriptor_of_perktype typ) free_vars) *)

(* Prints the symbol table 🤯 *)
let print_type_symbol_table () =
  Printf.printf "Type Symbol Table:\n";
  Hashtbl.iter
    (fun id (typ, _code) ->
      Printf.printf "%s: %s,\n\n" id (type_descriptor_of_perktype typ))
    type_symbol_table

(* Binds a type in the symble table. NOT Throws an exception if the name has already been defined *)
let bind_type (t : perktype) =
  say_here (Printf.sprintf "bind_type: %s" (show_perktype t));
  let id = type_descriptor_of_perktype t in
  (* if not (Hashtbl.mem type_symbol_table id) then *)
  Hashtbl.add type_symbol_table id (t, None)
(* else
    match t with
    | _, Basetype _, _
    | _, Pointertype _, _
    | _, Funtype _, _
    | _, Arraytype _, _
    | _, Structtype _, _
    | _, ArchetypeSum _, _
    | _, Optiontype _, _
    | _, Tupletype _, _
    | _, Vararg, _
    | _, Infer, _ ->
        ()
    | _, ArcheType _, _ | _, Modeltype _, _ ->
        raise
          (Double_declaration (Printf.sprintf "Type %s already declared" id)) *)

(* Replaces a type in the symbol table. Throws an exception if the name is not bound. Used by typecheck_deferred_function to replace temporary function types *)
let rebind_type (id : perkident) (t : perktype) =
  if Hashtbl.mem type_symbol_table id then
    Hashtbl.replace type_symbol_table id (t, None)
  else raise (Undeclared ("Type not found in symbol table: " ^ id))

let used_counter, unused_counter, called_counter = (ref 0, ref 0, ref 0)

(* Returns the dependencies of a type. This is used to sort them when generating the typedefs for the program.*)
let dependencies_of_type (typ : perktype) : perkident list =
  called_counter := !called_counter + 1;
  let type_dep_table : (perktype, perkident list) Hashtbl.t =
    Hashtbl.create 10
  in

  (* Auxiliary functions that takes a list as input to avoid circular dependencies *)
  (* The voidize parameters controls whether models have to be erased to void*. This is needed to avoid circular dependencies *)
  let rec dependencies_of_type_aux ?(voidize : bool = false) (typ : perktype)
      (lst : perktype list) : perkident list * perktype list =
    let typ = resolve_type typ in
    say_here (Printf.sprintf "dependencies_of_type: %s\n\n" (show_perktype typ));
    match Hashtbl.find_opt type_dep_table typ with
    | Some ls ->
        used_counter := !used_counter + 1;
        (ls, typ :: lst)
    | None ->
        unused_counter := !unused_counter + 1;
        (* print_endline "unUsed deps hashtable"; *)
        let deps, visited =
          if List.mem typ lst then
            ( (match typ with
              | _, Basetype _, _ -> []
              | _ -> [ type_descriptor_of_perktype typ ]),
              lst )
          else
            let _, typ', _ = typ in
            match typ' with
            | Basetype _ -> ([], typ :: lst)
            | Pointertype t -> dependencies_of_type_aux ~voidize t (typ :: lst)
            | Funtype (params, ret) ->
                let lst = typ :: lst in
                let params_t, params_l =
                  List.fold_right
                    (fun param (acc, lst) ->
                      let res_t, res_l =
                        dependencies_of_type_aux ~voidize:true param lst
                      in
                      (res_t @ acc, res_l))
                    params ([], lst)
                in
                let ret_t, ret_l =
                  dependencies_of_type_aux ~voidize:true ret (ret :: params_l)
                in
                ((type_descriptor_of_perktype typ :: params_t) @ ret_t, ret_l)
            | Lambdatype (_params, _ret, free_vars) ->
                let lst = typ :: lst in
                (* let params_t, params_l =
                  List.fold_right
                    (fun param (acc, lst) ->
                      let res_t, res_l =
                        dependencies_of_type_aux ~voidize:true param lst
                      in
                      (res_t @ acc, res_l))
                    params ([], lst)
                in *)
                (* let ret_t, ret_l =
                  dependencies_of_type_aux ~voidize:true ret (ret :: lst)
                in *)
                let underlying_deps, underlying_l =
                  dependencies_of_type_aux ~voidize:true
                    (func_of_lambda_void typ) lst
                in
                let free_deps, free_lst =
                  List.fold_right
                    (fun (param, _ide) (acc, lst) ->
                      let res_t, res_l =
                        dependencies_of_type_aux ~voidize:true param lst
                      in
                      (res_t @ acc, res_l))
                    free_vars ([], underlying_l)
                in
                ( (type_descriptor_of_perktype typ :: underlying_deps)
                  @ free_deps,
                  free_lst )
            | Arraytype (t, _) ->
                dependencies_of_type_aux ~voidize t (typ :: lst)
            | Structtype _t -> ([], lst)
            | ArcheType (name, decls) ->
                let lst = typ :: lst in
                let decls_t, decls_l =
                  List.fold_right
                    (fun (param, _ide) (acc, lst) ->
                      let res_t, res_l =
                        dependencies_of_type_aux ~voidize param lst
                      in
                      (res_t @ acc, res_l))
                    decls ([], lst)
                in
                (name :: decls_t, decls_l)
            | ArchetypeSum ts ->
                let lst = typ :: lst in
                List.fold_left
                  (fun (acc, lst) param ->
                    let res_t, res_l =
                      dependencies_of_type_aux ~voidize param lst
                    in
                    (res_t @ acc, res_l))
                  ([ type_descriptor_of_perktype typ ], lst)
                  ts
            | Modeltype (name, archetypes, decls, constr_params) ->
                let lst = typ :: lst in
                if voidize then ([], lst)
                else
                  let decls_t, lst =
                    List.fold_right
                      (fun (param, _ide) (acc, lst) ->
                        let res_t, res_l =
                          dependencies_of_type_aux ~voidize param lst
                        in
                        (res_t @ acc, res_l))
                      decls ([], lst)
                  in
                  let constructor_params_t, constr_params_l =
                    List.fold_left
                      (fun (acc, lst) param ->
                        let res_t, res_l =
                          dependencies_of_type_aux ~voidize param lst
                        in
                        (res_t @ acc, res_l))
                      ([], lst) constr_params
                  in
                  ( (name :: archetypes) @ decls_t @ constructor_params_t,
                    constr_params_l )
            | Optiontype t ->
                let deps, visited =
                  dependencies_of_type_aux ~voidize:true t (typ :: lst)
                in
                (type_descriptor_of_perktype typ :: deps, visited)
            | Tupletype ts ->
                let lst = typ :: lst in
                List.fold_left
                  (fun (acc, lst) param ->
                    let res_t, res_l =
                      dependencies_of_type_aux ~voidize param lst
                    in
                    (res_t @ acc, res_l))
                  ([ type_descriptor_of_perktype typ ], lst)
                  ts
            | Vararg -> ([], lst)
            | Infer -> ([], lst)
        in
        Hashtbl.add type_dep_table typ deps;
        (* Printf.printf "Type Dependency Table:\n";
        Hashtbl.iter
          (fun typ deps ->
            Printf.printf "%s: [%s]\n" (show_perktype typ)
              (String.concat ", " deps))
          type_dep_table; *)
        (deps, visited)
  in
  fst (dependencies_of_type_aux typ [])
  (* Remove duplicates *)
  |> List.sort_uniq String.compare
  (* Remove the type itself from the dependencies *)
  |> List.filter (fun s -> s <> type_descriptor_of_perktype typ)

(* Binds a type in the type symbol table, only if it is a non-native type (e.g., tuples, options, models, etc.) *)
let rec bind_type_if_needed (typ : perktype) =
  match typ with
  | [], Infer, [] -> ()
  | _ -> (
      match
        try lookup_type (type_descriptor_of_perktype typ)
        with Unbound_lambda_partition _ -> Some void_pointer
      with
      | Some _ -> ()
      | None -> (
          say_here
            (Printf.sprintf "bind_type_if_needed: %s" (show_perktype typ));
          let typ' = resolve_type typ in
          match typ' with
          | _, Basetype _t, _ -> ()
          | _, Pointertype t, _ ->
              bind_type typ;
              bind_type_if_needed t
          | _, Funtype (_params, _ret), _ ->
              bind_type typ';
              List.iter bind_type_if_needed _params;
              bind_type_if_needed _ret
          | _, Lambdatype (_params, _ret, _free_variables), _ ->
              (* TODO: LAMBDA pass env to function *)
              bind_type typ';
              (* Bind the type of the underlying function *)
              bind_type_if_needed (func_of_lambda_void typ);
              (* Bind the parameters and return type of the lambda *)
              List.iter bind_type_if_needed _params;
              bind_type_if_needed _ret
          | _, Arraytype (t, _), _ ->
              bind_type typ';
              bind_type_if_needed t
          | _, Structtype _, _ -> ()
          | _, ArcheType (_name, _decls), _ ->
              bind_type typ';
              List.iter (fun (typ, _id) -> bind_type_if_needed typ) _decls
          | _, ArchetypeSum _ts, _ ->
              bind_type typ';
              List.iter bind_type_if_needed _ts
          | _, Modeltype (_name, _archetypes, _decls, _constr_params), _ ->
              bind_type typ'
              (* ; List.iter (fun (typ, _id) -> bind_type_if_needed typ) decls;
                 List.iter (fun typ -> bind_type_if_needed typ) constr_params *)
          | _, Optiontype t, _ ->
              bind_type typ';
              bind_type_if_needed t
          | _, Tupletype ts, _ ->
              bind_type typ';
              List.iter bind_type_if_needed ts
          | _, Vararg, _ -> ()
          | _, Infer, _ -> ()))

(* Manually add code to the binding. Used by codegen functions for Models and Archetypes, where the struct code is generated during regular codegen, instead of at the end like for simple synthesized types like tuples and functions *)
let add_code_to_type_binding (_typ : perktype) (code : string) : unit =
  bind_type_if_needed _typ;
  let key = type_descriptor_of_perktype _typ in
  let _t, _code = Hashtbl.find type_symbol_table key in
  Hashtbl.replace type_symbol_table key (_t, Some code)
