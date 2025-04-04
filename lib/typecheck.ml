open Ast
open Errors

let raise_type_error (node : 'a annotated) (msg : string) =
  let line, col = (( @@ ) node).start_pos in
  raise (Type_error (line, col, msg))

let symbol_table : (perkident, perktype) Hashtbl.t list ref = ref []
let push_symbol_table () = symbol_table := Hashtbl.create 10 :: !symbol_table
let pop_symbol_table () = symbol_table := List.tl !symbol_table

let lookup_symbol_table (id : perkident) : perktype option =
  let rec lookup_in_tables tables =
    match tables with
    | [] -> None
    | h :: t ->
        if Hashtbl.mem h id then Some (Hashtbl.find h id)
        else lookup_in_tables t
  in
  lookup_in_tables !symbol_table

let lookup_var (id : perkident) : perktype option =
  lookup_symbol_table ("I_" ^ id)

let lookup_type (id : perkident) : perktype option =
  lookup_symbol_table ("T_" ^ id)

let print_symbol_table () =
  Printf.printf "Symbol Table:\n";
  let print_table table =
    Hashtbl.iter
      (fun id typ ->
        Printf.printf "Identifier: %s, Type: %s\n" id (Codegen.codegen_type typ))
      table
  in
  List.iter print_table !symbol_table

let bind_ident (id : perkident) (t : perktype) =
  match !symbol_table with
  | [] -> failwith "No symbol table available"
  | h :: _ ->
      if Hashtbl.mem h id then
        raise (Double_declaration ("Identifier already defined: " ^ id))
      else Hashtbl.add h id t
(* ;print_symbol_table () *)

let bind_var (id : perkident) (t : perktype) = bind_ident ("I_" ^ id) t
let bind_type (id : perkident) (t : perktype) = bind_ident ("T_" ^ id) t

let rec typecheck_program (ast : command_a) : command_a =
  push_symbol_table ();
  let res = typecheck_command ast in
  print_symbol_table ();
  res

and typecheck_command ?(retype : perktype option = None) (cmd : command_a) :
    command_a =
  match ( $ ) cmd with
  | Import _ -> cmd
  | InlineC _ -> cmd
  | Block c ->
      push_symbol_table ();
      let c_res = typecheck_command ~retype c in
      pop_symbol_table ();
      annot_copy cmd (Block c_res)
  | Def ((typ, id), expr) ->
      let typ' = resolve_type typ in
      let expr_res, expr_type = typecheck_expr expr in
      let typ'' =
        try match_types typ' expr_type
        with Type_match_error msg -> raise_type_error cmd msg
      in
      (* Check if the type is a user-defined type *)
      bind_var id typ'';
      annot_copy cmd (Def ((typ'', id), expr_res))
  | Fundef (ret_type, id, params, body) ->
      push_symbol_table ();
      List.iter
        (fun (typ, id) ->
          try bind_var id typ
          with Double_declaration msg -> raise_type_error cmd msg)
        params;
      let body_res = typecheck_command ~retype:(Some ret_type) body in
      pop_symbol_table ();
      bind_var id
        ([], Funtype (List.map (fun (typ, _) -> typ) params, ret_type), []);
      annot_copy cmd (Fundef (ret_type, id, params, body_res))
  | Extern (id, typ) ->
      (match lookup_var id with
      | Some _ ->
          raise_type_error cmd
            (Printf.sprintf "Identifier %s is already defined" id)
      | None -> ());
      bind_var id typ;
      annotate_dummy Skip
      (* Externs are only useful for type checking. No need to keep it for codegen step *)
  | Assign (lhs, rhs) ->
      let lhs_res, lhs_type = typecheck_expr lhs in
      let rhs_res, rhs_type = typecheck_expr rhs in
      let _ =
        try match_types lhs_type rhs_type
        with Type_match_error msg -> raise_type_error cmd msg
      in
      annot_copy cmd (Assign (lhs_res, rhs_res))
  | Seq (c1, c2) ->
      let c1_res = typecheck_command ~retype c1 in
      let c2_res = typecheck_command ~retype c2 in
      annot_copy cmd (Seq (c1_res, c2_res))
  | IfThenElse _ -> cmd
  | Whiledo _ -> cmd
  | Dowhile _ -> cmd
  | For _ -> cmd
  | Expr e -> annot_copy cmd (Expr (fst (typecheck_expr e)))
  | Switch _ -> cmd
  | Skip -> cmd
  | Archetype (name, decls) ->
      (try bind_type name ([], ArcheType (name, decls), [])
       with Double_declaration msg -> raise_type_error cmd msg);
      cmd
  | Model (ident, archetypes, fields) ->
      (* Check that the model is not already defined *)
      (let m = lookup_type ident in
       match m with
       | Some _ ->
           raise_type_error cmd
             (Printf.sprintf "Model %s is already defined" ident)
       | None -> ());
      (* Get implemented archetypes *)
      let archetypes_t =
        List.map
          (fun a ->
            match lookup_type a with
            | Some t -> (
                match t with
                | _, ArcheType _, _ -> t
                | _ ->
                    raise_type_error cmd
                      (Printf.sprintf
                         "Model %s is trying to implement non-archetype %s"
                         ident a))
            | None ->
                raise_type_error cmd
                  (Printf.sprintf
                     "Model %s implements non-existent Archetype %s" ident a))
          archetypes
      in
      (* Get all the fields that the model is required to implement *)
      let required_fields =
        List.flatten
          (List.map
             (fun (_, t, _) ->
               match t with
               | ArcheType (ident, params) ->
                   List.map (fun p -> (p, ident)) params
               | _ ->
                   failwith
                     "Impossible, archetypes were checked just a few lines \
                      before")
             archetypes_t)
      in
      (* Check that all the required fields are defined *)
      List.iter
        (fun ((typ, id), arch) ->
          match
            List.exists
              (fun ((t, i), e) ->
                if id = i then
                  let _ =
                    try
                      match_types t typ
                      (* TODO: Check very carefully: Should it be t typ or typ t? *)
                    with Type_match_error msg -> raise_type_error e msg
                  in
                  true
                else false)
              fields
          with
          | true -> ()
          | false ->
              raise_type_error cmd
                (Printf.sprintf
                   "Model %s is missing required field %s of type %s, declared \
                    in archetype %s"
                   ident id arch (show_perktype typ)))
        required_fields;
      (* Get constructor, if exists *)
      push_symbol_table ();
      bind_var "self"
        ([], Modeltype (ident, archetypes, List.map fst fields, []), []);
      let constr =
        List.find_opt (fun ((_typ, id), _expr) -> id = "constructor") fields
      in
      let constr_params =
        match constr with
        | Some (((_, Funtype (params, ret), _), _), _) ->
            (* Check that constructor returns void *)
            let _ =
              try match_types ([], Basetype "void", []) ret
              with Type_match_error msg -> raise_type_error cmd msg
            in
            params
        | Some (((_, Infer, _), _), expr) -> (
            let _expr_res, (_, expr_type, _) = typecheck_expr expr in
            match expr_type with
            | Funtype (params, ret) ->
                let _ =
                  try match_types ([], Basetype "void", []) ret
                  with Type_match_error msg -> raise_type_error cmd msg
                in
                params
            | _ -> raise_type_error expr "constructor should be a function")
        | Some (_, def) ->
            raise_type_error def "constructor should be a function"
            (* This error should go on the type, not on the definition. But for now, types are not annotated *)
        | None -> []
      in
      pop_symbol_table ();
      (* Check that all the fields defined in the model are well-typed *)
      push_symbol_table ();
      bind_var "self"
        ( [],
          Modeltype (ident, archetypes, List.map fst fields, constr_params),
          [] );
      let fields_res =
        List.map
          (fun ((typ, id), expr) ->
            let expr_res, expr_type = typecheck_expr expr in
            let typ' =
              try match_types typ expr_type
              with Type_match_error msg -> raise_type_error expr msg
            in
            bind_var id typ';
            ((typ', id), expr_res))
          fields
      in
      pop_symbol_table ();
      (* Add model to the symbol table *)
      bind_type ident
        ( [],
          Modeltype (ident, archetypes, List.map fst fields_res, constr_params),
          [] );
      annot_copy cmd (Model (ident, archetypes, fields_res))
  | Banish _ -> cmd
  | Return e ->
      let e_res, e_type = typecheck_expr e in
      (match retype with
      | Some t ->
          ignore
            (try match_types t e_type
             with Type_match_error msg -> raise_type_error cmd msg)
      | None -> ());
      annot_copy cmd (Return e_res)

and typecheck_expr (expr : expr_a) : expr_a * perktype =
  match ( $ ) expr with
  | Int _ -> (expr, ([], Basetype "int", []))
  | Float _ -> (expr, ([], Basetype "float", []))
  | Char _ -> (expr, ([], Basetype "char", []))
  | String _ -> (expr, ([], Pointertype ([], Basetype "char", []), []))
  | Pointer e ->
      let expr_res, t = typecheck_expr e in
      (annot_copy expr (Pointer expr_res), ([], Pointertype t, []))
  | Var id -> (
      match lookup_var id with
      | Some t -> (expr, t)
      | None -> raise_type_error expr ("Unknown identifier: " ^ id))
  | Apply (func, params) ->
      let fun_expr, fun_type = typecheck_expr func in
      (* Check that the function is a function 👍 *)
      let fun_param_types, fun_ret_type =
        match fun_type with
        | _, Funtype (param_types, ret_type), _ -> (param_types, ret_type)
        | _ ->
            let line, col = (( @@ ) func).start_pos in
            raise (Type_error (line, col, "Function type expected"))
      in
      let param_rets = List.map typecheck_expr params in
      let param_types = match_type_list fun_param_types param_rets in
      ( annot_copy expr (Apply (fun_expr, List.map fst param_rets)),
        ([], Funtype (param_types, fun_ret_type), []) )
  | Binop (op, lhs, rhs) ->
      let lhs_res, lhs_type = typecheck_expr lhs in
      let rhs_res, rhs_type = typecheck_expr rhs in
      let res_type =
        try match_types lhs_type rhs_type
        with Type_match_error msg -> raise_type_error expr msg
      in
      (annot_copy expr (Binop (op, lhs_res, rhs_res)), res_type)
  | PreUnop _ -> failwith "preu"
  | Lambda (retype, params, body) ->
      push_symbol_table ();
      List.iter
        (fun (typ, id) ->
          try bind_var id typ
          with Double_declaration msg -> raise_type_error expr msg)
        params;
      let body_res = typecheck_command ~retype:(Some retype) body in
      pop_symbol_table ();
      ( annot_copy expr (Lambda (retype, params, body_res)),
        ([], Funtype (List.map (fun (typ, _) -> typ) params, retype), []) )
  | PostUnop _ -> failwith "postu"
  | Parenthesised _ -> failwith "par"
  | Subscript _ -> failwith "sub"
  | Summon (typeid, params) -> (
      let typ = lookup_type typeid in
      match typ with
      | Some (attrs, Modeltype (_name, archetypes, fields, constr_params), specs)
        ->
          let param_rets = List.map typecheck_expr params in
          let _ = match_type_list constr_params param_rets in
          ( annot_copy expr (Summon (typeid, List.map fst param_rets)),
            (attrs, Modeltype (typeid, archetypes, fields, constr_params), specs)
          )
      | Some _ ->
          raise_type_error expr
            (Printf.sprintf "Can only summon model types. %s is not a model."
               typeid)
      | None -> raise_type_error expr (Printf.sprintf "Unknown type: %s" typeid)
      )
  | Access (expr, ide) ->
      let expr_res, expr_type = typecheck_expr expr in
      let res_type =
        match expr_type with
        | _, Modeltype (name, _archetypes, fields, _constr_params), _ -> (
            let field = List.find_opt (fun (_, id) -> id = ide) fields in
            match field with
            | Some (typ, _) -> typ
            | None ->
                raise_type_error expr
                  (Printf.sprintf "Field %s not found in model %s" ide name))
        | _ ->
            raise_type_error expr
              (Printf.sprintf "Cannot access field %s of non-model type" ide)
      in
      (annot_copy expr (Access (expr_res, ide)), res_type)

(* Add more type checking logic as needed: pepperepeppe     peppè! culo*)

and match_types (expected : perktype) (actual : perktype) : perktype =
  let (_, expected', _), (_, actual', _) = (expected, actual) in
  match (expected', actual') with
  | Basetype t1, Basetype t2 when t1 = t2 -> actual
  | Pointertype t1, Pointertype t2 when t1 = t2 -> actual
  | Funtype (params1, ret1), Funtype (params2, ret2)
    when List.length params1 = List.length params2 ->
      let param_types = List.map2 match_types params1 params2 in
      let ret_type = match_types ret1 ret2 in
      ([], Funtype (param_types, ret_type), [])
  | Arraytype (t1, n1), Arraytype (t2, n2) when n1 = n2 ->
      let t = match_types t1 t2 in
      ([], Arraytype (t, n1), [])
  | Structtype t1, Structtype t2 when t1 = t2 -> actual
  | ArcheType (name1, decls1), ArcheType (name2, decls2)
    when name1 = name2 && List.length decls1 = List.length decls2 ->
      let decls_types =
        List.map2
          (fun (t1, id1) (t2, id2) ->
            if id1 = id2 then (match_types t1 t2, id1)
            else
              raise
                (Type_match_error
                   (Printf.sprintf
                      "Archetype %s has different field names: %s and %s" name1
                      id1 id2)))
          decls1 decls2
      in
      ([], ArcheType (name1, decls_types), [])
  | ( Modeltype (name1, archetypes1, decls1, constr_params1),
      Modeltype (name2, archetypes2, decls2, constr_params2) )
    when name1 = name2
         && List.length archetypes1 = List.length archetypes2
         && List.length decls1 = List.length decls2
         && List.length constr_params1 = List.length constr_params2 ->
      let decls_types =
        List.map2
          (fun (t1, id1) (t2, id2) ->
            if id1 = id2 then (match_types t1 t2, id1)
            else
              raise
                (Type_match_error
                   (Printf.sprintf
                      "Model %s has different field names: %s and %s" name1 id1
                      id2)))
          decls1 decls2
      in
      let constr_types = List.map2 match_types constr_params1 constr_params2 in
      ([], Modeltype (name1, archetypes1, decls_types, constr_types), [])
  | Vararg, Vararg -> actual
  | Infer, _ -> actual
  | _ ->
      raise
        (Type_match_error
           (Printf.sprintf "Type mismatch: expected %s, got %s instead"
              (* (Codegen.codegen_type ~expand:true expected)
              (Codegen.codegen_type ~expand:true actual))) *)
              (show_perktype expected)
              (show_perktype actual)))

and match_type_list (expected : perktype list)
    (actual : (expr_a * perktype) list) : perktype list =
  let rec match_type_list_aux expected' actual' =
    match (expected', actual') with
    | ([], Vararg, []) :: _ :: _, _ ->
        raise_type_error
          (fst (List.hd actual'))
          "Function has a vararg with other parameters after it"
    | [], [] -> []
    | [ ([], Vararg, []) ], [] -> []
    | [], _ | _, [] ->
        raise_type_error
          (fst (List.hd actual'))
          (Printf.sprintf "Expected %d parameters, but got %d"
             (List.length expected) (List.length actual))
    | [ ([], Vararg, []) ], a :: at ->
        snd a :: match_type_list_aux [ ([], Vararg, []) ] at
    | e :: et, a :: at ->
        let typ =
          try match_types e (snd a)
          with Type_match_error msg -> raise_type_error (fst a) msg
        in
        typ :: match_type_list_aux et at
  in
  match_type_list_aux expected actual

and resolve_type (typ : perktype) : perktype =
  let attrs, typ', quals = typ in
  ( attrs,
    (match typ' with
    | Basetype t -> (
        match lookup_type t with None -> typ' | Some (_, t, _) -> t)
    | Pointertype t -> Pointertype (resolve_type t)
    | Funtype (params, ret) ->
        Funtype (List.map resolve_type params, resolve_type ret)
    | Arraytype (t, n) -> Arraytype (resolve_type t, n)
    | Structtype t -> (
        match lookup_type t with None -> typ' | Some (_, t, _) -> t)
    | ArcheType (name, decls) ->
        let decls' = List.map (fun (t, id) -> (resolve_type t, id)) decls in
        ArcheType (name, decls')
    | Modeltype (name, archetypes, decls, constr_params) ->
        let decls' = List.map (fun (t, id) -> (resolve_type t, id)) decls in
        Modeltype (name, archetypes, decls', constr_params)
    | Vararg -> Vararg
    | Infer -> Infer),
    quals )
