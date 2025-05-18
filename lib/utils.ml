open Ast
open Errors

let static_compilation : bool = false

let rec say_here (_msg : string) : unit =
  (* Printf.printf "%s\n" _msg;
     flush stdout *)
  ()

(* Utility function to add a parameter (i.e., self) to a type, iff it is a functional *)
and add_parameter_to_func (param_type : perktype) (func_type : perktype) :
    perktype =
  match func_type with
  | a, Lambdatype (params, ret, free_vars), d ->
      let new_params = param_type :: params in
      (a, Lambdatype (new_params, ret, free_vars), d)
  | a, Funtype (params, ret), d ->
      let new_params = param_type :: params in
      (a, Funtype (new_params, ret), d)
  | _ -> func_type

(* Utility function to add a parameter (i.e., self) to a type, iff it is a function *)
and add_parameter_to_func_only (param_type : perktype) (func_type : perktype) :
    perktype =
  match func_type with
  | a, Funtype (params, ret), d ->
      let new_params = param_type :: params in
      (a, Funtype (new_params, ret), d)
  | _ -> func_type

(* Utility function to add a parameter (i.e., self) to a type, iff it is a function *)
and add_parameter_to_func_2 (param_type : perktype) (func_type : perktype) :
    perktype =
  match func_type with
  | a, Lambdatype (params, ret, free_vars), d ->
      let new_params = List.hd params :: param_type :: List.tl params in
      (a, Lambdatype (new_params, ret, free_vars), d)
  | _ -> func_type

and void_type : perktype = ([], Basetype "void", [])
and int_type : perktype = ([], Basetype "int", [])
and float_type : perktype = ([], Basetype "float", [])
and char_type : perktype = ([], Basetype "char", [])
and bool_type : perktype = ([], Basetype "bool", [])
and void_pointer : perktype = ([], Pointertype ([], Basetype "void", []), [])
and self_type (name : perkident) : perktype = ([], Basetype name, [])

and func_of_lambda_void (t : perktype) : perktype =
  match t with
  | a, Lambdatype (args, ret, _), q ->
      ( a,
        Funtype (([], Pointertype ([], Basetype "void", []), []) :: args, ret),
        q )
  | _ -> failwith "func_of_lambda_void: not a lambda type"

and functype_of_lambdatype (t : perktype) : perktype =
  match t with
  | a, Lambdatype (args, ret, _), q -> (a, Funtype (args, ret), q)
  | _ -> failwith "functype_of_lambdatype: not a lambda type"

and lambdatype_of_func (typ : perktype) : perktype =
  match typ with
  | a, Funtype (params, ret), q ->
      (a, Lambdatype (List.map lambdatype_of_func params, ret, []), q)
  | _ -> typ

and lambdatype_of_func_with_self (typ : perktype) (selftype : perktype) :
    perktype =
  match typ with
  | a, Funtype (params, ret), q ->
      ( a,
        Lambdatype (selftype :: List.map lambdatype_of_func params, ret, []),
        q )
  | _ -> typ

and lambda_expr_of_func_expr_with_self (expr : expr_a) (fromtype : perktype)
    (selftype : perktype) : expr_a =
  match ( $ ) expr with
  | Var _ ->
      annot_copy expr
        (Cast ((fromtype, lambdatype_of_func_with_self fromtype selftype), expr))
  | _ -> expr

and lambda_expr_of_func_expr (expr : expr_a) (fromtype : perktype) : expr_a =
  match ( $ ) expr with
  | Var _ ->
      annot_copy expr (Cast ((fromtype, lambdatype_of_func fromtype), expr))
  | _ -> expr

(* and lambda_def_of_func_def_with_self (def : perkdef) (selftype : perktype) :
     perkdef =
   match def with
   | (typ, id), expr ->
       let new_typ = lambdatype_of_func typ in
       let new_expr = lambda_expr_of_func_expr_with_self expr typ selftype in
       ((new_typ, id), new_expr) *)

and lambda_def_of_func_def (def : perkdef) : perkdef =
  let (typ, id), expr = def in
  match ( $ ) expr with
  | Lambda _ ->
      let new_typ = lambdatype_of_func typ in
      ((new_typ, id), expr)
  | _ -> def

and lambda_def_of_func_def_ (def : perkdef) : perkdef =
  match def with
  | (typ, id), expr ->
      let new_typ = lambdatype_of_func typ in
      let new_expr = lambda_expr_of_func_expr expr typ in
      ((new_typ, id), new_expr)

and discard_type_aq (typ : perktype) : perktype_partial =
  let _a, t, _q = typ in
  t

and lambda_of_func (func : perkfundef) : expr_t =
  let typ, _id, args, body = func in
  Lambda (typ, args, body, [])

and decl_of_deforfun (def : deforfun_a) : perkdecl =
  match ( $ ) def with
  (* If this def is a function, make its type a function type *)
  | DefFun (typ, id, params, _) ->
      let new_typ = ([], Funtype (List.map fst params, typ), []) in
      (new_typ, id)
  (* If this def is a lambda, make its type a lambda type *)
  | DefVar ((typ, id), _) ->
      (* let new_typ =
        match typ with
        | a, Funtype (params, ret), d -> (a, Lambdatype (params, ret, []), d)
        | _ -> typ
      in *)
      (typ, id)

and decl_of_declorfun (def : declorfun_a) : perkdecl =
  match ( $ ) def with
  (* If this def is a function, make its type a function type *)
  | DeclFun (typ, id) ->
      let new_typ =
        match typ with
        | a, Lambdatype (params, ret, free_vars), d ->
            if free_vars <> [] then
              raise_type_error def "function contains free vars"
            else (a, Funtype (params, ret), d)
        | _ -> typ
      in
      (new_typ, id)
  (* If this def is a lambda, make its type a lambda type *)
  | DeclVar (typ, id) ->
      (* let new_typ =
        match typ with
        | a, Funtype (params, ret), d -> (a, Lambdatype (params, ret, []), d)
        | _ -> typ
      in *)
      (typ, id)

and funtype_of_perkfundef (def : perkfundef) : perktype =
  let typ, _id, args, _body = def in
  ([], Funtype (List.map fst args, typ), [])

and get_member_functions (defs : deforfun_a list) : perkident list =
  List.filter
    (fun def ->
      match ( $ ) def with
      | DefFun (_, _, _, _) -> true
      | DefVar ((_, _), _) -> false)
    defs
  |> List.map (fun f ->
         match ( $ ) f with
         | DefFun (_, id, _, _) -> id
         | _ -> failwith "impossible: vars have been already filtered away")
