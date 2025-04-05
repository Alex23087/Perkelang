open Ast
open Errors
open Symbol_table

let raise_type_error (node : 'a annotated) (msg : string) =
  raise (Type_error ((( @@ ) node).start_pos, (( @@ ) node).end_pos, msg))

let var_symbol_table : (perkident, perktype) Hashtbl.t list ref = ref []

let push_symbol_table () =
  var_symbol_table := Hashtbl.create 10 :: !var_symbol_table

let pop_symbol_table () = var_symbol_table := List.tl !var_symbol_table

let lookup_var (id : perkident) : perktype option =
  let rec lookup_in_tables tables =
    match tables with
    | [] -> None
    | h :: t ->
        if Hashtbl.mem h id then Some (Hashtbl.find h id)
        else lookup_in_tables t
  in
  lookup_in_tables !var_symbol_table

let print_symbol_table () =
  Printf.printf "Symbol Table:\n";
  let print_table table =
    Hashtbl.iter
      (fun id typ ->
        Printf.printf "Identifier: %s, Type: %s\n" id (Codegen.codegen_type typ))
      table
  in
  List.iter print_table !var_symbol_table

let bind_var (id : perkident) (t : perktype) =
  match !var_symbol_table with
  | [] -> failwith "No symbol table available"
  | h :: _ ->
      if Hashtbl.mem h id then
        raise (Double_declaration ("Identifier already defined: " ^ id))
      else Hashtbl.add h id t
(* ;print_symbol_table () *)

let rec typecheck_program (ast : topleveldef_a list) : topleveldef_a list =
  push_symbol_table ();
  let res = List.map typecheck_topleveldef ast in
  (* Will it do it in the right order?? *)
  (* print_symbol_table ();
  print_type_symbol_table (); *)
  res

and typecheck_topleveldef (tldf : topleveldef_a) : topleveldef_a =
  match ( $ ) tldf with
  | Import _ -> tldf
  | InlineC _ -> tldf
  | Def ((typ, id), expr) ->
      let typ' = resolve_type typ in
      let expr_res, expr_type = typecheck_expr expr in
      let typ'' =
        try match_types typ' expr_type
        with Type_match_error msg -> raise_type_error tldf msg
      in
      (* Check if the type is a user-defined type *)
      bind_var id typ'';
      annot_copy tldf (Def ((typ'', id), expr_res))
  | Fundef (ret_type, id, params, body) ->
      push_symbol_table ();
      List.iter
        (fun (typ, id) ->
          try bind_var id typ
          with Double_declaration msg -> raise_type_error tldf msg)
        params;
      let body_res = typecheck_command ~retype:(Some ret_type) body in
      pop_symbol_table ();
      let funtype =
        ([], Funtype (List.map (fun (typ, _) -> typ) params, ret_type), [])
      in
      bind_var id funtype;
      bind_type (type_descriptor_of_perktype funtype) funtype;
      annot_copy tldf (Fundef (ret_type, id, params, body_res))
  | Extern (id, typ) ->
      (match lookup_var id with
      | Some _ ->
          raise_type_error tldf
            (Printf.sprintf "Identifier %s is already defined" id)
      | None -> ());
      bind_var id typ;
      bind_type_if_needed typ;
      tldf
      (* annotate_dummy Skip *)
      (* Externs are only useful for type checking. No need to keep it for codegen step *)
  | Archetype (name, decls) ->
      (try bind_type name ([], ArcheType (name, decls), [])
       with Double_declaration msg -> raise_type_error tldf msg);
      tldf
  | Model (ident, archetypes, fields) ->
      (* Check that the model is not already defined *)
      (let m = lookup_type ident in
       match m with
       | Some _ ->
           raise_type_error tldf
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
                    raise_type_error tldf
                      (Printf.sprintf
                         "Model %s is trying to implement non-archetype %s"
                         ident a))
            | None ->
                raise_type_error tldf
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
              raise_type_error tldf
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
              with Type_match_error msg -> raise_type_error tldf msg
            in
            params
        | Some (((_, Infer, _), _), expr) -> (
            let _expr_res, (_, expr_type, _) = typecheck_expr expr in
            match expr_type with
            | Funtype (params, ret) ->
                let _ =
                  try match_types ([], Basetype "void", []) ret
                  with Type_match_error msg -> raise_type_error tldf msg
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
      bind_type_if_needed
        ( [],
          Modeltype (ident, archetypes, List.map fst fields_res, constr_params),
          [] );
      annot_copy tldf (Model (ident, archetypes, fields_res))

and typecheck_command ?(retype : perktype option = None) (cmd : command_a) :
    command_a =
  match ( $ ) cmd with
  | InlineCCmd _ -> cmd
  | Block c ->
      push_symbol_table ();
      let c_res = typecheck_command ~retype c in
      pop_symbol_table ();
      annot_copy cmd (Block c_res)
  | DefCmd ((typ, id), expr) ->
      let typ' = resolve_type typ in
      let expr_res, expr_type = typecheck_expr expr in
      let typ'' =
        try match_types typ' expr_type
        with Type_match_error msg -> raise_type_error cmd msg
      in
      bind_type_if_needed typ'';
      bind_var id typ'';
      annot_copy cmd (DefCmd ((typ'', id), expr_res))
  | Assign (lhs, rhs, _) ->
      let lhs_res, lhs_type = typecheck_expr lhs in
      let rhs_res, rhs_type = typecheck_expr rhs in
      let _ =
        try match_types lhs_type rhs_type
        with Type_match_error msg -> raise_type_error cmd msg
      in
      let acctype =
        match ( $ ) lhs_res with Access (_, _, t) -> t | _ -> None
      in
      annot_copy cmd (Assign (lhs_res, rhs_res, acctype))
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
        | _ -> raise_type_error func "Function type expected"
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
      let lamtype =
        ([], Funtype (List.map (fun (typ, _) -> typ) params, retype), [])
      in
      pop_symbol_table ();
      bind_type_if_needed lamtype;
      (annot_copy expr (Lambda (retype, params, body_res)), lamtype)
  | PostUnop _ -> failwith "postu"
  | Parenthesised e -> typecheck_expr e
  | Subscript (container, accessor) -> (
      let container_res, container_type = typecheck_expr container in
      let accessor_res, accessor_type = typecheck_expr accessor in
      (match accessor_type with
      | _, Basetype "int", _ -> ()
      | _ ->
          raise_type_error expr
            (Printf.sprintf "Subscript operator requires int, got %s"
               (show_perktype accessor_type)));
      match container_type with
      | _, Arraytype (t, _n), _ ->
          (annot_copy expr (Subscript (container_res, accessor_res)), t)
      | _, Tupletype ts, _ -> (
          match ( $ ) accessor_res with
          | Int i ->
              if i < 0 || i >= List.length ts then
                raise_type_error expr
                  (Printf.sprintf "Subscript out of bounds: %d" i);
              ( annot_copy expr (TupleSubscript (container_res, i)),
                List.nth ts i )
          | _ ->
              raise_type_error expr
                "Subscript operator requires constant integer")
      | _ ->
          raise_type_error expr
            (Printf.sprintf "Subscript operator requires array or tuple, got %s"
               (show_perktype container_type)))
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
  | Access (expr, ide, _) ->
      let expr_res, expr_type = typecheck_expr expr in
      let res_type, access_type =
        match resolve_type expr_type with
        | _, Modeltype (name, _archetypes, fields, _constr_params), _ -> (
            let field = List.find_opt (fun (_, id) -> id = ide) fields in
            match field with
            | Some (typ, _) -> (typ, None)
            | None ->
                raise_type_error expr
                  (Printf.sprintf "Field %s not found in model %s" ide name))
        | _, ArchetypeSum archetypes, _ -> (
            let archs_with_idents =
              List.map
                (fun a ->
                  match resolve_type a with
                  | _, ArcheType (_aid, decls), [] ->
                      List.map (fun (t, id) -> (a, t, id)) decls
                  | _ ->
                      failwith
                        "Impossible: Model is implementing a non-archetype")
                archetypes
              |> List.flatten
            in
            match
              List.find_opt (fun (_arch, _t, id) -> id = ide) archs_with_idents
            with
            | Some (arch, t, _id) -> (t, Some arch)
            | None ->
                raise_type_error expr
                  (Printf.sprintf
                     "Field %s not found in archetypes implemented by variable"
                     ide))
        | _, ArcheType (_name, decls), _ -> (
            match List.find_opt (fun (_t, id) -> id = ide) decls with
            | Some (t, _id) -> (t, Some (resolve_type expr_type))
            | None ->
                raise_type_error expr
                  (Printf.sprintf
                     "Field %s not found in archetypes implemented by variable"
                     ide))
        | _ ->
            raise_type_error expr
              (Printf.sprintf "Cannot access field %s of non-model type %s" ide
                 (show_perktype expr_type))
      in
      (annot_copy expr (Access (expr_res, ide, access_type)), res_type)
  | Tuple (exprs, _) ->
      let exprs_res = List.map typecheck_expr exprs in
      let types = List.map snd exprs_res in
      let tupletype = ([], Tupletype types, []) in
      bind_type_if_needed tupletype;
      ( annot_copy expr (Tuple (List.map fst exprs_res, Some tupletype)),
        ([], Tupletype types, []) )
  | TupleSubscript _ ->
      failwith
        "Should not happen: this variant is only generated by the typechecker"
  | As (id, archs) -> (
      let typ = lookup_var id in
      match typ with
      | Some t -> (
          match t with
          | _, Modeltype (_name, archetypes, _fields, _constr_params), _ ->
              let archs_idents =
                List.map
                  (fun a ->
                    match resolve_type a with
                    | _, ArcheType (name, _decls), _ -> name
                    | _ ->
                        failwith
                          (Printf.sprintf
                             "Impossible: archetype expected. Got %s"
                             (show_perktype a)))
                  archs
              in
              List.iter
                (fun arch ->
                  match List.find_opt (fun id -> id = arch) archetypes with
                  | Some _ -> ()
                  | None ->
                      raise_type_error expr
                        (Printf.sprintf
                           "Archetype %s not found in model %s. Type is %s" arch
                           id (show_perktype t)))
                archs_idents;
              (annot_copy expr (As (id, archs)), ([], ArchetypeSum archs, []))
          | _ ->
              raise_type_error expr
                (Printf.sprintf "Cannot ~> non-model variable %s" id))
      | None ->
          raise_type_error expr
            (Printf.sprintf "Identifier %s is not defined" id))

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
  | Optiontype t, Optiontype s -> ([], Optiontype (match_types t s), [])
  (* | Optiontype t, _ -> ([], Optiontype (match_types t actual), []) *)
  | Tupletype t1, Tupletype t2 ->
      ([], Tupletype (List.map2 match_types t1 t2), [])
  | ArchetypeSum t1, ArchetypeSum t2 ->
      ( [],
        ArchetypeSum
          (match_type_list t1
             (List.map (fun t -> (annotate_dummy (Int (-1)), t)) t2)),
        [] )
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
