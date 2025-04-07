open Ast
open Errors
open Type_symbol_table
open Var_symbol_table

let rec typecheck_program (ast : topleveldef_a list) : topleveldef_a list =
  push_symbol_table ();
  let res = List.map typecheck_topleveldef ast in
  let res = List.map typecheck_deferred_function res in
  (* Will it do it in the right order?? *)
  (* print_symbol_table (); *)
  (* print_type_symbol_table (); *)
  res

and typecheck_deferred_function (tldf : topleveldef_a) : topleveldef_a =
  match ( $ ) tldf with
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
      rebind_var id funtype;
      rebind_type (type_descriptor_of_perktype funtype) funtype;
      annot_copy tldf (Fundef (ret_type, id, params, body_res))
  | _ -> tldf

and typecheck_topleveldef (tldf : topleveldef_a) : topleveldef_a =
  match ( $ ) tldf with
  | Import _ -> tldf
  | InlineC _ -> tldf
  | Def (((typ, id), expr), _) ->
      let typ' = resolve_type typ in
      let expr_res, expr_type = typecheck_expr expr in
      let expr_type =
        match (typ', expr_type) with
        | _, (_, Infer, _) -> typ'
        | (_, Infer, _), _ -> expr_type
        | _ -> expr_type
      in
      let typ'' =
        try match_types ~coalesce:true typ' expr_type
        with Type_match_error msg -> raise_type_error tldf msg
      in
      let typ''_nocoal =
        try match_types ~coalesce:false typ' expr_type
        with Type_match_error _ -> ([], Infer, [])
      in
      let deftype =
        if equal_perktype typ'' typ''_nocoal then None else Some typ''
      in
      bind_type_if_needed typ';
      bind_type_if_needed typ'';
      bind_var id typ'';
      annot_copy tldf (Def (((typ'', id), expr_res), deftype))
  | Fundef (ret_type, id, params, body) ->
      let funtype =
        ([], Funtype (List.map (fun (typ, _) -> typ) params, ret_type), [])
      in
      bind_var id funtype;
      bind_type_if_needed funtype;
      annot_copy tldf (Fundef (ret_type, id, params, body))
      (* |> ignore; typecheck_deferred_function tldf *)
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
  | Archetype (name, decls) -> (
      match lookup_type name with
      | Some _ ->
          raise_type_error tldf
            (Printf.sprintf "Archetype %s is already defined" name)
      | None ->
          bind_type_if_needed ([], ArcheType (name, decls), []);
          List.iter
            (fun (typ, _id) ->
              typ |> add_parameter_to_func void_pointer |> bind_type_if_needed)
            decls;
          tldf)
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
      (* !!!!!WARNING!!!!! THIS CANNOT BE DONE LIKE THAT. MUST BE CHECKED AS FOR PROGRAM FOR HOISTED FUNCTIONS !!!!!WARNING!!!!! *)
      (* TODO: For some reason, you can write things like member[0] instead of self.member[0]. Investigate *)
      let temp_model_type =
        ( [],
          Modeltype (ident, archetypes, List.map fst fields, constr_params),
          [] )
      in
      bind_type_if_needed temp_model_type;
      bind_var "self" temp_model_type;
      let fields_res =
        List.map
          (fun ((typ, id), expr) ->
            let expr_res, expr_type = typecheck_expr expr in
            let expr_res, expr_type = fill_nothing expr_res expr_type typ in
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
      let modeltype =
        ( [],
          Modeltype (ident, archetypes, List.map fst fields_res, constr_params),
          [] )
      in
      rebind_type (type_descriptor_of_perktype modeltype) modeltype;
      List.iter
        (fun ((typ, _id), _expr) ->
          typ |> add_parameter_to_func modeltype |> bind_type_if_needed)
        fields_res;
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
  | DefCmd (((typ, id), expr), _) ->
      let typ' = resolve_type typ in
      let expr_res, expr_type = typecheck_expr expr in
      let expr_res, expr_type = fill_nothing expr_res expr_type typ' in
      let expr_type =
        match (typ', expr_type) with
        | _, (_, Infer, _) -> typ'
        | (_, Infer, _), _ -> expr_type
        | _ -> expr_type
      in
      let typ'' =
        try match_types ~coalesce:true typ' expr_type
        with Type_match_error msg -> raise_type_error cmd msg
      in
      let typ''_nocoal =
        try match_types ~coalesce:false typ' expr_type
        with Type_match_error _ -> ([], Infer, [])
      in
      let deftype =
        if equal_perktype typ'' typ''_nocoal then None else Some typ''
      in
      bind_type_if_needed typ';
      bind_type_if_needed typ'';
      bind_var id typ'';
      (* Printf.printf "DefCmd: %s, deftype: %s\n" id
         (match deftype with
         | Some deftype -> show_perktype deftype
         | None -> "None"); *)
      annot_copy cmd (DefCmd (((typ'', id), expr_res), deftype))
  | Assign (lhs, rhs, _, _) ->
      let lhs_res, lhs_type = typecheck_expr lhs in
      let rhs_res, rhs_type = typecheck_expr rhs in
      let rhs_res, rhs_type = fill_nothing rhs_res rhs_type lhs_type in
      let exprval =
        try match_types ~coalesce:true lhs_type rhs_type
        with Type_match_error msg -> raise_type_error cmd msg
      in
      let exprval_nocoal =
        try match_types ~coalesce:false lhs_type rhs_type
        with Type_match_error _ -> ([], Infer, [])
      in
      let acctype =
        match ( $ ) lhs_res with Access (_, _, t) -> t | _ -> None
      in
      let rasstype =
        if equal_perktype exprval exprval_nocoal then None else Some exprval
      in
      (* Printf.printf "Assign: %s = %s, acctype: %s, rasstype: %s\n"
         (show_perktype lhs_type) (show_perktype rhs_type)
         (match acctype with Some t -> show_perktype t | None -> "None")
         (match rasstype with Some t -> show_perktype t | None -> "None"); *)
      annot_copy cmd (Assign (lhs_res, rhs_res, acctype, rasstype))
  | Seq (c1, c2) ->
      let c1_res = typecheck_command ~retype c1 in
      let c2_res = typecheck_command ~retype c2 in
      annot_copy cmd (Seq (c1_res, c2_res))
  | IfThenElse (guard, then_branch, else_branch) ->
      let guard_res, guard_type = typecheck_expr guard in
      (match guard_type with
      | _, Basetype "int", _ -> () (* TODO: Decide what TODO with booleans *)
      | _ ->
          raise_type_error cmd
            (Printf.sprintf
               "If guard must be a boolean (or an integer, we are still a bit \
                confused ok?!?), got %s"
               (show_perktype guard_type)));
      push_symbol_table ();
      let then_branch_res = typecheck_command ~retype then_branch in
      pop_symbol_table ();
      push_symbol_table ();
      let else_branch_res = typecheck_command ~retype else_branch in
      pop_symbol_table ();
      annot_copy cmd (IfThenElse (guard_res, then_branch_res, else_branch_res))
  | Whiledo (guard, body) ->
      let guard_res, guard_type = typecheck_expr guard in
      (match guard_type with
      | _, Basetype "int", _ -> () (* TODO: Decide what TODO with booleans *)
      | _ ->
          raise_type_error cmd
            (Printf.sprintf
               "If guard must be a boolean (or an integer, we are still a bit \
                confused ok?!?), got %s"
               (show_perktype guard_type)));
      push_symbol_table ();
      let body_res = typecheck_command ~retype body in
      pop_symbol_table ();
      annot_copy cmd (Whiledo (guard_res, body_res))
  | Dowhile (guard, body) ->
      let guard_res, guard_type = typecheck_expr guard in
      (match guard_type with
      | _, Basetype "int", _ -> () (* TODO: Decide what TODO with booleans *)
      | _ ->
          raise_type_error cmd
            (Printf.sprintf
               "While guard must be a boolean (or an integer, we are still a \
                bit confused ok?!?), got %s"
               (show_perktype guard_type)));
      push_symbol_table ();
      let body_res = typecheck_command ~retype body in
      pop_symbol_table ();
      annot_copy cmd (Dowhile (guard_res, body_res))
  | For (initcmd, guard, incrcmd, body) ->
      let initcmd_res = typecheck_command ~retype initcmd in
      let guard_res, guard_type = typecheck_expr guard in
      (match guard_type with
      | _, Basetype "int", _ -> () (* TODO: Decide what TODO with booleans *)
      | _ ->
          raise_type_error cmd
            (Printf.sprintf
               "For guard must be a boolean (or an integer, we are still a bit \
                confused ok?!?), got %s"
               (show_perktype guard_type)));
      let incrcmd_res = typecheck_command ~retype incrcmd in
      push_symbol_table ();
      let body_res = typecheck_command ~retype body in
      pop_symbol_table ();
      annot_copy cmd (For (initcmd_res, guard_res, incrcmd_res, body_res))
  | Expr e -> annot_copy cmd (Expr (fst (typecheck_expr e)))
  | Switch _ -> cmd
  | Skip -> cmd
  | Banish id ->
      (* TODO: Let banish unbind the future *)
      (match Option.map resolve_type (lookup_var id) with
      | None -> raise_syntax_error cmd ("Identifier " ^ id ^ " not found")
      | Some (_, Modeltype _, _) -> ()
      | Some _ ->
          raise_syntax_error cmd
            (Printf.sprintf "Variable %s is not a model" id));
      cmd
  | Return e ->
      let e_res, e_type = typecheck_expr e in
      (match retype with
      | Some t ->
          ignore
            (try match_types t e_type
             with Type_match_error msg -> raise_type_error cmd msg)
      | None -> ());
      annot_copy cmd (Return e_res)

and typecheck_expr ?(expected_return : perktype option = None) (expr : expr_a) :
    expr_a * perktype =
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
  | Apply (func, params, _) ->
      let fun_expr, fun_type = typecheck_expr func in
      (* Check that the function is a function 👍 *)
      let fun_param_types, fun_ret_type, apptype =
        match fun_type with
        | _, Funtype (param_types, ret_type), _ -> (param_types, ret_type, None)
        | _, Lambdatype (param_types, ret_type, _), _ ->
            (param_types, ret_type, Some fun_type)
        | _ -> raise_type_error func "Function type expected"
      in
      let param_rets =
        (* TODO: Fix varargs for this *)
        try
          List.map2
            (fun par exp -> typecheck_expr ~expected_return:(Some exp) par)
            params fun_param_types
        with Invalid_argument _ ->
          raise_type_error expr
            (Printf.sprintf
               "Wrong number of parameters passed to function: expected %d, \
                got %d"
               (List.length fun_param_types)
               (List.length params))
      in
      let _param_types =
        try match_type_list fun_param_types param_rets
        with Type_match_error msg -> raise_type_error expr msg
      in
      let param_rets =
        List.map2
          (fun (e, t1) t2 -> fill_nothing e t1 t2)
          param_rets _param_types
      in
      ( annot_copy expr (Apply (fun_expr, List.map fst param_rets, apptype)),
        fun_ret_type )
  | Binop (op, lhs, rhs) ->
      let lhs_res, lhs_type = typecheck_expr lhs in
      let rhs_res, rhs_type = typecheck_expr rhs in
      let res_type =
        try match_types lhs_type rhs_type
        with Type_match_error msg -> raise_type_error expr msg
      in
      let lhs_res, _lhs_type = fill_nothing lhs_res lhs_type res_type in
      let rhs_res, _rhs_type = fill_nothing rhs_res rhs_type res_type in
      (annot_copy expr (Binop (op, lhs_res, rhs_res)), res_type)
  | PreUnop (op, e) ->
      let expr_res, expr_type = typecheck_expr e in
      let res_type =
        match (op, resolve_type expr_type) with
        | Dereference, (_, Pointertype t, _) -> t
        | Reference, t -> ([], Pointertype t, [])
        | Dereference, _ ->
            raise_type_error expr
              (Printf.sprintf "Cannot dereference non-pointer type %s"
                 (show_perktype expr_type))
        | _, t -> t
      in
      (annot_copy expr (PreUnop (op, expr_res)), res_type)
  | Lambda (retype, params, body, _) ->
      push_symbol_table ();
      List.iter
        (fun (typ, id) ->
          try bind_var id typ
          with Double_declaration msg -> raise_type_error expr msg)
        params;
      let body_res = typecheck_command ~retype:(Some retype) body in
      let free_vars = fst (free_variables_expr expr) in
      let free_vars =
        List.map
          (fun v ->
            match lookup_var v with
            | Some vt -> (vt, v)
            | None ->
                raise_type_error expr (Printf.sprintf "Unbound variable %s" v))
          free_vars
      in
      let lamtype =
        ( [],
          Lambdatype (List.map (fun (typ, _) -> typ) params, retype, free_vars),
          [] )
      in
      pop_symbol_table ();
      bind_type_if_needed lamtype;
      autocast
        (annot_copy expr (Lambda (retype, params, body_res, free_vars)))
        lamtype expected_return
  | PostUnop (op, e) ->
      let expr_res, expr_type = typecheck_expr e in
      let op, res_type =
        match (op, resolve_type expr_type) with
        | OptionGet _, (_, Optiontype t, _) -> (OptionGet (Some t), t)
        | OptionIsSome, (_, Optiontype _t, _) ->
            (op, ([], Basetype "int", []))
            (* TODO: Decide what TODO with bools*)
        | OptionGet _, _ | OptionIsSome, _ ->
            raise_type_error expr
              (Printf.sprintf "Option operator requires option type, got %s"
                 (show_perktype expr_type))
        | _, t -> (op, t)
      in
      (annot_copy expr (PostUnop (op, expr_res)), res_type)
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
          say_here
            (Printf.sprintf "Summon: %s\n" (show_perktype (Option.get typ))
            ^ Printf.sprintf "constr_params: %s\n"
                (String.concat ", " (List.map show_perktype constr_params))
            ^ Printf.sprintf "params: %s\n"
                (String.concat ", "
                   (List.map show_perktype (List.map snd param_rets))));
          flush stdout;
          let param_rets =
            if List.length param_rets <> List.length constr_params then
              raise_type_error expr
                (Printf.sprintf
                   "Wrong number of parameters passed to constructor: expected \
                    %d, got %d%s"
                   (List.length constr_params)
                   (List.length param_rets)
                   (if List.exists (fun (_typ, id) -> id = "constructor") fields
                    then ""
                    else ". Constructor is not defined"))
            else
              List.map2
                (fun (a, b) c -> fill_nothing a b c)
                param_rets constr_params
          in
          let _ =
            try match_type_list constr_params param_rets
            with Type_match_error msg -> raise_type_error expr msg
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
      let exprs_res = List.map (fun (a, b) -> fill_nothing a b b) exprs_res in
      (* TODO: This won't work on typles *)
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
  | Something (e, _) ->
      let e_res, e_type = typecheck_expr e in
      (annot_copy expr (Something (e_res, e_type)), ([], Optiontype e_type, []))
  | Nothing _ -> (expr, ([], Infer, []))
  | Array exprs -> (
      match exprs with
      | [] -> (expr, ([], Infer, []))
      | x :: xs ->
          let xexpr, xtyp = typecheck_expr x in
          let constant_list = List.init (List.length xs) (fun _ -> xtyp) in
          let exprs_res = List.map typecheck_expr xs in
          let exprs_e = List.map fst exprs_res in
          (* let exprs_t = List.map snd exprs_res in *)
          let _ =
            try match_type_list constant_list exprs_res
            with Type_match_error msg -> raise_type_error expr msg
          in
          let arraytype =
            ([], Arraytype (xtyp, Some (List.length xs + 1)), [])
          in
          bind_type_if_needed arraytype;
          (annot_copy expr (Array (xexpr :: exprs_e)), arraytype))
  | Cast (t, e) -> (annot_copy expr (Cast (t, fst (typecheck_expr e))), t)

and fill_nothing (expr : expr_a) (exprtyp : perktype) (typ : perktype) :
    expr_a * perktype =
  match (( $ ) expr, typ) with
  | Nothing _, ([], Optiontype _, []) -> (annot_copy expr (Nothing typ), typ)
  | Nothing _, _ ->
      raise_type_error expr "Nothing can only be used with Optiontype"
  | _ -> (expr, exprtyp)

(* Add more type checking logic as needed: pepperepeppe     peppè! culo*)

and match_types ?(coalesce : bool = false) (expected : perktype)
    (actual : perktype) : perktype =
  let expected = resolve_type expected in
  let actual = resolve_type actual in
  let rec match_types_aux expected actual =
    (* This catches the case where one type has not been bound yet and thus results as a basetype *)
    let equal =
      try
        type_descriptor_of_perktype expected
        = type_descriptor_of_perktype actual
      with Not_inferred _ -> false
    in
    if equal then actual
    else
      let (_, expected', _), (_, actual', _) = (expected, actual) in
      match (expected', actual') with
      | Basetype t1, Basetype t2 when t1 = t2 -> actual
      | Pointertype t1, Pointertype t2 when t1 = t2 -> actual
      | Funtype (params1, ret1), Funtype (params2, ret2)
        when List.length params1 = List.length params2 ->
          let param_types = List.map2 match_types_aux params1 params2 in
          let ret_type = match_types_aux ret1 ret2 in
          ([], Funtype (param_types, ret_type), [])
      | Arraytype (t1, n1), Arraytype (t2, n2) when n1 = n2 ->
          let t = match_types_aux t1 t2 in
          ([], Arraytype (t, n1), [])
      | Structtype t1, Structtype t2 when t1 = t2 -> actual
      | ArcheType (name1, decls1), ArcheType (name2, decls2)
        when name1 = name2 && List.length decls1 = List.length decls2 ->
          let decls_types =
            List.map2
              (fun (t1, id1) (t2, id2) ->
                if id1 = id2 then (match_types_aux t1 t2, id1)
                else
                  raise
                    (Type_match_error
                       (Printf.sprintf
                          "Archetype %s has different field names: %s and %s"
                          name1 id1 id2)))
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
                if id1 = id2 then (match_types_aux t1 t2, id1)
                else
                  raise
                    (Type_match_error
                       (Printf.sprintf
                          "Model %s has different field names: %s and %s" name1
                          id1 id2)))
              decls1 decls2
          in
          let constr_types =
            List.map2 match_types_aux constr_params1 constr_params2
          in
          ([], Modeltype (name1, archetypes1, decls_types, constr_types), [])
      | Vararg, Vararg -> actual
      | Infer, _ | _, Infer -> actual
      | Optiontype t, Optiontype s -> ([], Optiontype (match_types_aux t s), [])
      | Tupletype t1, Tupletype t2 ->
          ([], Tupletype (List.map2 match_types_aux t1 t2), [])
      | ArchetypeSum t1, ArchetypeSum t2 ->
          ( [],
            ArchetypeSum
              (match_type_list t1
                 (List.map (fun t -> (annotate_dummy (Int (-1)), t)) t2)),
            [] )
      | _ ->
          raise
            (Type_match_error
               (Printf.sprintf "Type mismatch: expected %s,\ngot %s instead"
                  (* (Codegen.codegen_type ~expand:true expected)
                     (Codegen.codegen_type ~expand:true actual))) *)
                  (show_perktype expected)
                  (show_perktype actual)))
  in
  match (coalesce, expected, actual) with
  | _, (_, Optiontype t, _), (_, Optiontype s, _) ->
      ([], Optiontype (match_types_aux t s), [])
  | true, (_, Optiontype t, _), _ ->
      ([], Optiontype (match_types_aux t actual), [])
  | _, _, _ -> match_types_aux expected actual

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
    | [], _ ->
        raise_type_error
          (fst (List.hd actual'))
          (Printf.sprintf "Expected %d parameters, but got %d"
             (List.length expected) (List.length actual))
    | _, [] ->
        raise
          (Type_match_error
             (Printf.sprintf "Expected %d parameters, but got %d"
                (List.length expected) (List.length actual)))
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

and autocast (expr : expr_a) (typ : perktype) (expected : perktype option) :
    expr_a * perktype =
  match expected with
  | None -> (expr, typ)
  | Some t -> (annot_copy expr (Cast (t, expr)), t)

(* returns pair of lists: FIRST LIST IS FREE VARS, SECOND LIST IS BOUND *)
and free_variables_command (cmd : command_a) : perkident list * perkident list =
  let free_variables_command_aux (cmd : command_a) :
      perkident list * perkident list =
    match ( $ ) cmd with
    | InlineCCmd _ -> ([], [])
    | Block c ->
        let free, _ = free_variables_command c in
        (free, [])
    | Assign (e1, e2, _, _) ->
        let free_e1, bound_e1 = free_variables_expr e1 in
        let free_e2, bound_e2 = free_variables_expr e2 in
        (free_e1 @ free_e2, bound_e1 @ bound_e2)
    | Seq (c1, c2) ->
        let free_c1, bound_c1 = free_variables_command c1 in
        let free_c2, bound_c2 = free_variables_command c2 in
        (list_minus (free_c1 @ free_c2) bound_c1, bound_c1 @ bound_c2)
    | IfThenElse (e1, c1, c2) ->
        let free_e1, _ = free_variables_expr e1 in
        let free_c1, _ = free_variables_command c1 in
        let free_c2, _ = free_variables_command c2 in
        (free_e1 @ free_c1 @ free_c2, [])
    | Whiledo (e1, c1) | Dowhile (e1, c1) ->
        let free_e1, _ = free_variables_expr e1 in
        let free_c1, _ = free_variables_command c1 in
        (free_e1 @ free_c1, [])
    | For (c1, e1, c2, c3) ->
        (* ES THES FOCKEN REIGHT!?!?!?!? *)
        (* for(a; b; {int a; int b}) *)
        let free_e1, _ = free_variables_expr e1 in
        let free_c1, bound_c1 = free_variables_command c1 in
        let free_c2, bound_c2 = free_variables_command c2 in
        let free_c3, _ = free_variables_command c3 in
        ( free_c1 @ free_e1
          @ list_minus free_c2 bound_c1
          @ list_minus free_c3 (bound_c1 @ bound_c2),
          bound_c1 @ bound_c2 )
    | Expr e1 -> free_variables_expr e1
    | Switch _ -> failwith "yomumsaho"
    | Skip -> ([], [])
    | Banish id -> ([ id ], []) (* TODO: needda somme morre thinkin' *)
    | Return e1 -> free_variables_expr e1
    | DefCmd (((_, id), def), _) ->
        let free, _ = free_variables_expr def in
        (free, [ id ])
  in
  let out_free, out_bound = free_variables_command_aux cmd in
  ( List.sort_uniq String.compare out_free,
    List.sort_uniq String.compare out_bound )

and free_variables_expr (e : expr_a) : perkident list * perkident list =
  let free_variables_expr (e : expr_a) : perkident list * perkident list =
    ( (match ( $ ) e with
      | Nothing _ | Int _ | Float _ | Char _ | String _ -> []
      | Something (e1, _) -> free_variables_expr e1 |> fst
      | Pointer e1 -> free_variables_expr e1 |> fst
      | Var id -> [ id ]
      | Apply (e1, el, _) ->
          fst (free_variables_expr e1)
          @ List.flatten (List.map (fun x -> free_variables_expr x |> fst) el)
      | Binop (_, e1, e2) ->
          fst (free_variables_expr e1) @ fst (free_variables_expr e2)
      | PreUnop (_, e1) | PostUnop (_, e1) -> fst (free_variables_expr e1)
      | Lambda (_, params, body, _) ->
          list_minus (fst (free_variables_command body)) (List.map snd params)
      | Parenthesised e1 -> fst (free_variables_expr e1)
      | Subscript (e1, e2) ->
          fst (free_variables_expr e1) @ fst (free_variables_expr e2)
      | TupleSubscript (e1, _) -> fst (free_variables_expr e1)
      | Summon (_, el) ->
          List.flatten (List.map (fun x -> fst (free_variables_expr x)) el)
      | Access (e1, id, _) -> id :: fst (free_variables_expr e1)
      | Tuple (el, _) | Array el ->
          List.flatten (List.map (fun x -> fst (free_variables_expr x)) el)
      | As (id, _) -> [ id ]
      | Cast (_, e1) -> fst (free_variables_expr e1)),
      [] )
  in
  let out_free, out_bound = free_variables_expr e in
  ( list_minus
      (List.sort_uniq String.compare out_free)
      (get_all_global_identifiers ()),
    List.sort_uniq String.compare out_bound )

and list_minus (l1 : 'a list) (l2 : 'a list) : 'a list =
  List.filter (fun x -> not (List.mem x l2)) l1
