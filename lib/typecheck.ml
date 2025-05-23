open Ast
open Errors
open Utils
open Type_symbol_table
open Var_symbol_table
open Free_variables

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
      if id = "self" then raise_type_error tldf "Identifier self is reserved"
      else
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
      if id = "self" then raise_type_error tldf "Identifier self is reserved"
      else
        let funtype =
          ([], Funtype (List.map (fun (typ, _) -> typ) params, ret_type), [])
        in
        bind_var id funtype;
        bind_type_if_needed funtype;
        annot_copy tldf (Fundef (ret_type, id, params, body))
        (* |> ignore; typecheck_deferred_function tldf *)
  | Extern (id, typ) ->
      (if id = "self" then raise_type_error tldf "Identifier self is reserved"
       else
         match lookup_var id with
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
      if name = "self" then raise_type_error tldf "Identifier self is reserved"
      else
        match lookup_type name with
        | Some _ ->
            raise_type_error tldf
              (Printf.sprintf "Archetype %s is already defined" name)
        | None ->
            List.iter bind_type_if_needed
              (List.map
                 (fun d ->
                   d |> decl_of_declorfun
                   |> (fun (typ, id) ->
                   (add_parameter_to_func_only void_pointer typ, id))
                   |> fst)
                 decls);
            bind_type_if_needed
              ([], ArcheType (name, List.map decl_of_declorfun decls), []);
            tldf)
  | Model (ident, archetypes, fields) ->
      (if ident = "self" then
         raise_type_error tldf "Identifier self is reserved"
       else
         (* Check that the model is not already defined *)
         let m = lookup_type ident in
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
              (fun def ->
                let t, i = decl_of_deforfun def in
                if id = i then
                  let _ =
                    try
                      match_types t typ
                      (* TODO: Check very carefully: Should it be t typ or typ t? *)
                    with Type_match_error msg -> raise_type_error def msg
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

      (* Gather member functions *)
      let member_funcs = get_member_functions fields in

      (* Get constructor, if exists *)
      push_symbol_table ();
      bind_var "self"
        ( [],
          Modeltype
            ( ident,
              archetypes,
              List.map (fun d -> d |> decl_of_deforfun) fields,
              [],
              member_funcs ),
          [] );
      let constr =
        List.find_opt
          (fun def ->
            match ( $ ) def with
            | DefFun (_, id, _, _) -> id = "constructor"
            | DefVar ((_, id), expr) ->
                if id = "constructor" then
                  raise_type_error expr "Constructor must be a function"
                else false)
          fields
      in
      let constr_params =
        match Option.map ( $ ) constr with
        | Some (DefFun (ret, _, params, _)) ->
            (* Check that constructor returns void *)
            let _ =
              try match_types ([], Basetype "void", []) ret
              with Type_match_error msg -> raise_type_error tldf msg
            in
            List.map fst params
        | Some _ ->
            raise_type_error (Option.get constr)
              "Constructor must be a function 2"
        | None -> []
      in
      pop_symbol_table ();

      (* Check that all the fields defined in the model are well-typed *)
      push_symbol_table ();
      (* !!!!!WARNING!!!!! THIS CANNOT BE DONE LIKE THAT. MUST BE CHECKED AS FOR PROGRAM FOR HOISTED FUNCTIONS !!!!!WARNING!!!!! *)
      (* TODO: For some reason, you can write things like member[0] instead of self.member[0]. Investigate *)
      let temp_model_type =
        ( [],
          Modeltype
            ( ident,
              archetypes,
              List.map decl_of_deforfun fields,
              constr_params,
              member_funcs ),
          [] )
      in
      bind_type_if_needed temp_model_type;
      bind_var "self" temp_model_type;
      let fields_res =
        List.map
          (fun def ->
            match ( $ ) def with
            | DefFun (ret, id, params, body) ->
                push_symbol_table ();
                List.iter
                  (fun (typ, id) ->
                    try bind_var id typ
                    with Double_declaration msg -> raise_type_error def msg)
                  params;
                let body_res = typecheck_command ~retype:(Some ret) body in
                pop_symbol_table ();
                annot_copy def (DefFun (ret, id, params, body_res))
            | DefVar ((typ, id), expr) ->
                let expr_res, expr_type = typecheck_expr expr in
                let expr_res, expr_type = fill_nothing expr_res expr_type typ in
                let typ' =
                  try match_types typ expr_type
                  with Type_match_error msg -> raise_type_error expr msg
                in
                bind_var id typ';
                annot_copy def (DefVar ((typ', id), expr_res)))
          fields
      in
      pop_symbol_table ();
      (* Add model to the symbol table *)
      let modeltype =
        ( [],
          Modeltype
            ( ident,
              archetypes,
              List.map decl_of_deforfun fields_res,
              constr_params,
              member_funcs ),
          [] )
      in
      rebind_type (type_descriptor_of_perktype modeltype) modeltype;
      List.iter
        (fun def ->
          match ( $ ) def with
          | DefFun (typ, _, params, _) ->
              (* Generate type binding for functions. Need to add virtual self to the parameters *)
              ([], Funtype (List.map fst params, typ), [])
              |> add_parameter_to_func modeltype
              |> bind_type_if_needed
          | DefVar ((typ, _), _) -> bind_type_if_needed typ)
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
      if id = "self" then raise_type_error cmd "Identifier self is reserved"
      else
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
        match ( $ ) lhs_res with Access (_, _, t, _) -> t | _ -> None
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
      | _, Basetype "bool", _ -> () (* TODO: Decide what TODO with booleans *)
      | _, Basetype "int", _ -> ()
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
      | _, Basetype "bool", _ -> () (* TODO: Decide what TODO with booleans *)
      | _, Basetype "int", _ -> ()
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
      | _, Basetype "bool", _ -> () (* TODO: Decide what TODO with booleans *)
      | _, Basetype "int", _ -> ()
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
      | _, Basetype "bool", _ -> () (* TODO: Decide what TODO with booleans *)
      | _, Basetype "int", _ -> ()
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
  | Return None -> (
      match retype with
      | Some (_, Basetype "void", _) | None -> cmd
      | Some t ->
          raise_type_error cmd
            (Printf.sprintf
               "This return is supposed to return a value of type %s, but it's \
                empty"
               (show_perktype t)))
  | Return (Some e) ->
      let e_res, e_type = typecheck_expr e in
      (match retype with
      | Some t ->
          ignore
            (try match_types t e_type
             with Type_match_error msg -> raise_type_error cmd msg)
      | None ->
          raise_type_error cmd "This return is not supposed to return any value");
      annot_copy cmd (Return (Some e_res))

and typecheck_expr ?(expected_return : perktype option = None) (expr : expr_a) :
    expr_a * perktype =
  match ( $ ) expr with
  | Bool _ -> (expr, ([], Basetype "bool", []))
  | Int _ -> (expr, ([], Basetype "int", []))
  | Float _ -> (expr, ([], Basetype "float", []))
  | Char _ -> (expr, ([], Basetype "char", []))
  | String _ -> (expr, ([], Pointertype ([], Basetype "char", []), []))
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
        try typecheck_expr_list params fun_param_types
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
      (match op with
      | Add | Sub | Mul | Div ->
          let lhs_res, lhs_type = typecheck_expr lhs in
          let rhs_res, rhs_type = typecheck_expr rhs in
          let lhs_res, lhs_type, rhs_res, rhs_type =
            match (lhs_res, lhs_type, rhs_res, rhs_type) with
            (* Two int or two float expressions, no cast to be done *)
            | _, (_al, Basetype "int", _ql), _, (_ar, Basetype "int", _qr)
            | _, (_al, Basetype "float", _ql), _, (_ar, Basetype "float", _qr)
            | _, (_al, Basetype "int64_t", _ql), _, (_ar, Basetype "int64_t", _qr)
              ->
                (lhs_res, lhs_type, rhs_res, rhs_type)
            (* int op float, cast first one to float *)
            | _, (_al, Basetype "int", _ql), _, (_ar, Basetype "float", _qr) ->
                ( annot_copy lhs_res (Cast ((lhs_type, rhs_type), lhs_res)),
                  rhs_type,
                  rhs_res,
                  rhs_type )
            (* float op int, cast second one to float *)
            | _, (_al, Basetype "float", _ql), _, (_ar, Basetype "int", _qr) ->
                ( lhs_res,
                  lhs_type,
                  annot_copy rhs_res (Cast ((rhs_type, lhs_type), rhs_res)),
                  lhs_type )
            
            (* int64 op int, cast second one to int64 *)
            | _, (_al, Basetype "int64_t", _ql), _, (_ar, Basetype "int", _qr) ->
                    ( lhs_res,
                      lhs_type,
                      annot_copy rhs_res (Cast ((rhs_type, lhs_type), rhs_res)),
                      lhs_type )

            (* int op int64, cast first one to int64 *)
            | _, (_al, Basetype "int", _ql), _, (_ar, Basetype "int64_t", _qr) ->
                ( annot_copy lhs_res (Cast ((lhs_type, rhs_type), lhs_res)),
                  rhs_type,
                  rhs_res,
                  rhs_type )
                  
            | _, (_a, Basetype "float", _q), _, _
            | _, (_a, Basetype "int", _q), _, _ ->
                raise_type_error rhs "Numerical type expected"
            | _, _, _, (_a, Basetype "float", _q)
            | _, _, _, (_a, Basetype "int", _q) ->
                raise_type_error lhs "Numerical type expected"
            | _ -> raise_type_error expr "Numerical type expected"
          in
          let res_type =
            try match_types lhs_type rhs_type
            with Type_match_error msg -> raise_type_error expr msg
          in
          let lhs_res, _lhs_type = fill_nothing lhs_res lhs_type res_type in
          let rhs_res, _rhs_type = fill_nothing rhs_res rhs_type res_type in
          (annot_copy expr (Binop (op, lhs_res, rhs_res)), res_type)
          |> ignore (*TODO: Remove this ignore*)
      | Eq | Lt | Leq | Gt | Geq | Neq | Land | Lor -> ());
      (* TODO: Continue here *)
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
      (* if a lambda has no free variables, it is made into a function *)
      let lamtype =
        match free_vars with
        | [] ->
            if static_compilation then
              ([], Funtype (List.map (fun (typ, _) -> typ) params, retype), [])
            else
              ( [],
                Lambdatype
                  (List.map (fun (typ, _) -> typ) params, retype, free_vars),
                [] )
        | _ ->
            ( [],
              Lambdatype
                (List.map (fun (typ, _) -> typ) params, retype, free_vars),
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
            (op, ([], Basetype "bool", []))
            (* TODO: Decide what TODO with bools*)
        | OptionGet _, _ | OptionIsSome, _ ->
            raise_type_error expr
              (Printf.sprintf "Option operator requires option type, got %s"
                 (show_perktype expr_type))
        | _, t -> (op, t)
      in
      (annot_copy expr (PostUnop (op, expr_res)), res_type)
  | Parenthesised e ->
      let e1, typ = typecheck_expr ~expected_return e in
      (annot_copy expr (Parenthesised e1), typ)
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
      | Some
          ( attrs,
            Modeltype (_name, archetypes, fields, constr_params, member_funcs),
            specs ) ->
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
            ( attrs,
              Modeltype (typeid, archetypes, fields, constr_params, member_funcs),
              specs ) )
      | Some _ ->
          raise_type_error expr
            (Printf.sprintf "Can only summon model types. %s is not a model."
               typeid)
      | None -> raise_type_error expr (Printf.sprintf "Unknown type: %s" typeid)
      )
  | Access (expr, ide, _, _) ->
      let expr_res, expr_type = typecheck_expr expr in
      let res_type, access_type, rightype =
        match resolve_type expr_type with
        | ( _,
            Modeltype (name, _archetypes, fields, _constr_params, _member_funcs),
            _ ) -> (
            let field = List.find_opt (fun (_, id) -> id = ide) fields in
            match field with
            | Some (typ, _) -> (typ, Some expr_type, Some typ)
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
            | Some (arch, t, _id) -> (t, Some arch, Some t)
            | None ->
                raise_type_error expr
                  (Printf.sprintf
                     "Field %s not found in archetypes implemented by variable"
                     ide))
        (* | _, ArcheType (_name, decls), _ -> (
            match List.find_opt (fun (_t, id) -> id = ide) decls with
            | Some (t, _id) -> (t, Some (resolve_type expr_type))
            | None ->
                raise_type_error expr
                  (Printf.sprintf
                     "Field %s not found in archetypes implemented by variable"
                     ide)) *)
        | _ ->
            raise_type_error expr
              (Printf.sprintf "Cannot access field %s of non-model type %s" ide
                 (show_perktype expr_type))
      in
      (annot_copy expr (Access (expr_res, ide, access_type, rightype)), res_type)
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
          | _, Modeltype (_name, archetypes, _fields, _constr_params, _), _ ->
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
  | Nothing typ -> (expr, (typ)) (* this used to be infer by default, but added "of" syntax *)
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
  | Cast (t, e) ->
      bind_type_if_needed (snd t);
      (annot_copy expr (Cast (t, fst (typecheck_expr e))), snd t)
  | IfThenElseExpr (guard, then_e, else_e) ->
      let guard_res, guard_type =
        typecheck_expr ~expected_return:(Some bool_type) guard
      in
      (match guard_type with
      | _, Basetype "bool", _ -> () (* TODO: Decide what TODO with booleans *)
      | _, Basetype "int", _ -> ()
      | _ ->
          raise_type_error expr
            (Printf.sprintf
               "If guard must be a boolean (or an integer, we are still a bit \
                confused ok?!?), got %s"
               (show_perktype guard_type)));
      let then_e_res, then_e_type = typecheck_expr then_e in
      let else_e_res, else_e_type = typecheck_expr else_e in
      let res_type =
        try match_types then_e_type else_e_type
        with Type_match_error msg -> raise_type_error else_e msg
      in
      let then_e_res, _then_e_type =
        fill_nothing then_e_res then_e_type res_type
      in
      let else_e_res, _else_e_type =
        fill_nothing else_e_res else_e_type res_type
      in
      ( annot_copy expr (IfThenElseExpr (guard_res, then_e_res, else_e_res)),
        res_type )

and typecheck_expr_list (exprs : expr_a list) (types : perktype list) :
    (expr_a * perktype) list =
  let typecheck_expr_list_aux (exps : expr_a list) (typs : perktype list) =
    match (exps, typs) with
    | [], [] -> []
    | [], [ (_, Vararg, _) ] -> []
    | [], _ | _, [] ->
        raise
          (Invalid_argument
             (Printf.sprintf "Wrong number of parameters: expected %d got %d"
                (List.length exprs) (List.length types)))
    | x :: xs, [ (_, Vararg, _) ] ->
        let x_res, x_type = typecheck_expr x in
        (x_res, x_type) :: typecheck_expr_list xs typs
    | x :: xs, t :: ts ->
        let x_res, x_type = typecheck_expr ~expected_return:(Some t) x in
        (x_res, x_type) :: typecheck_expr_list xs ts
  in
  typecheck_expr_list_aux exprs types

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
      | ( Modeltype (name1, archetypes1, decls1, constr_params1, member_funcs1),
          Modeltype (name2, archetypes2, decls2, constr_params2, member_funcs2)
        )
        when name1 = name2
             && List.length archetypes1 = List.length archetypes2
             && List.length decls1 = List.length decls2
             && List.length constr_params1 = List.length constr_params2
             && List.equal String.equal member_funcs1 member_funcs2 ->
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
          ( [],
            Modeltype
              (name1, archetypes1, decls_types, constr_types, member_funcs1),
            [] )
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
      | Lambdatype (params1, ret1, _), Funtype (params2, ret2) ->
          let param_types = List.map2 match_types_aux params1 params2 in
          let ret_type = match_types_aux ret1 ret2 in
          ([], Lambdatype (param_types, ret_type, []), [])
      | Lambdatype (params1, ret1, free1), Lambdatype (params2, ret2, _) ->
          let param_types = List.map2 match_types_aux params1 params2 in
          let ret_type = match_types_aux ret1 ret2 in
          ([], Lambdatype (param_types, ret_type, free1), [])
      | Pointertype (_, Basetype "void", _), Modeltype _ -> actual
      | _ ->
          raise
            (Type_match_error
               (Printf.sprintf "Type mismatch: expected %s,\ngot %s instead"
                  (Codegen.codegen_type ~expand:true expected)
                  (Codegen.codegen_type ~expand:true actual)))
    (* (show_perktype expected)
                  (show_perktype actual))) *)
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
  | Some t -> (annot_copy expr (Cast ((typ, t), expr)), t)
