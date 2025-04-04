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

let print_symbol_table () =
  Printf.printf "Symbol Table:\n";
  let print_table table =
    Hashtbl.iter
      (fun id typ ->
        Printf.printf "Identifier: %s, Type: %s\n" id (show_perktype typ))
      table
  in
  List.iter print_table !symbol_table

let bind_ident (id : perkident) (t : perktype) =
  (match !symbol_table with
  | [] -> failwith "No symbol table available"
  | h :: _ ->
      if Hashtbl.mem h id then
        raise (Double_declaration ("Identifier already defined: " ^ id))
      else Hashtbl.add h id t);
  print_symbol_table ()

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
      let expr_res, expr_type = typecheck_expr expr in
      let _ =
        try match_types typ expr_type
        with Type_match_error msg -> raise_type_error cmd msg
      in
      bind_ident id typ;
      annot_copy cmd (Def ((typ, id), expr_res))
  | Fundef (ret_type, id, params, body) ->
      push_symbol_table ();
      List.iter
        (fun (typ, id) ->
          try bind_ident id typ
          with Double_declaration msg -> raise_type_error cmd msg)
        params;
      let body_res = typecheck_command ~retype:(Some ret_type) body in
      pop_symbol_table ();
      bind_ident id
        ([], Funtype (List.map (fun (typ, _) -> typ) params, ret_type), []);
      annot_copy cmd (Fundef (ret_type, id, params, body_res))
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
      (try bind_ident name ([], ArcheType (name, decls), [])
       with Double_declaration msg -> raise_type_error cmd msg);
      cmd
  | Model (ident, archetypes, fields) ->
      (* Check that the model is not already defined *)
      (let m = lookup_symbol_table ident in
       match m with
       | Some _ ->
           raise_type_error cmd
             (Printf.sprintf "Model %s is already defined" ident)
       | None -> ());
      (* Get implemented archetypes *)
      let archetypes_t =
        List.map
          (fun a ->
            match lookup_symbol_table a with
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
                    try match_types typ t
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
      let constr =
        List.find_opt (fun ((_typ, id), _expr) -> id = "constructor") fields
      in
      let constr_params =
        match constr with
        | Some (((_, Funtype (_params, _ret), _), _), _) ->
            (* Check that constructor returns void *)
            let _ =
              try match_types ([], Basetype "void", []) _ret
              with Type_match_error msg -> raise_type_error cmd msg
            in
            _params
        | Some _ -> failwith "Impossible, constructor should be a function"
        | None -> []
      in
      (* Check that all the fields defined in the model are well-typed *)
      push_symbol_table ();
      bind_ident "self"
        ( [],
          Modeltype (ident, archetypes, List.map fst fields, constr_params),
          [] );
      let fields_res =
        List.map
          (fun ((typ, id), expr) ->
            let expr_res, expr_type = typecheck_expr expr in
            let _ =
              try match_types typ expr_type
              with Type_match_error msg -> raise_type_error expr msg
            in
            bind_ident id typ;
            ((typ, id), expr_res))
          fields
      in
      pop_symbol_table ();
      (* Add model to the symbol table *)
      bind_ident ident
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
  | String _ -> (expr, ([], Basetype "char*", []))
  | Pointer e ->
      let expr_res, t = typecheck_expr e in
      (annot_copy expr (Pointer expr_res), ([], Pointertype t, []))
  | Var id -> (
      match lookup_symbol_table id with
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
      if List.length fun_param_types <> List.length param_rets then
        raise_type_error func
          (Printf.sprintf "Function %s expects %d parameters, but got %d"
             (show_expr_a fun_expr)
             (List.length fun_param_types)
             (List.length param_rets));
      let param_types =
        List.map2
          (fun t1 (e, t2) ->
            try match_types t1 t2
            with Type_match_error m -> raise_type_error e m)
          fun_param_types param_rets
      in
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
          try bind_ident id typ
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
      let typ = lookup_symbol_table typeid in
      match typ with
      | Some (attrs, Modeltype (_name, archetypes, fields, constr_params), specs)
        ->
          let param_rets = List.map typecheck_expr params in
          if List.length constr_params <> List.length param_rets then
            raise_type_error expr
              (Printf.sprintf "Constructor %s expects %d parameters, but got %d"
                 typeid
                 (List.length constr_params)
                 (List.length param_rets));
          let _ =
            List.map2
              (fun t1 (e, t2) ->
                try match_types t1 t2
                with Type_match_error m -> raise_type_error e m)
              constr_params param_rets
          in
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
  | _ ->
      raise
        (Type_match_error
           (Printf.sprintf "Type mismatch: expected %s, \n\ngot %s instead"
              (* (Codegen.codegen_type ~expand:true expected)
              (Codegen.codegen_type ~expand:true actual))) *)
              (show_perktype expected)
              (show_perktype actual)))
