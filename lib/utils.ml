open Ast

let rec say_here (_msg : string) : unit =
  (* Printf.printf "%s\n" _msg;
     flush stdout *)
  ()

(* Utility function to add a parameter (i.e., self) to a type, iff it is a function *)
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
and add_parameter_to_func_2 (param_type : perktype) (func_type : perktype) :
    perktype =
  match func_type with
  | a, Lambdatype (params, ret, free_vars), d ->
      let new_params = List.hd params :: param_type :: List.tl params in
      (a, Lambdatype (new_params, ret, free_vars), d)
  | _ -> func_type

and void_type : perktype = ([], Basetype "void", [])
and void_pointer : perktype = ([], Pointertype ([], Basetype "void", []), [])
and self_type (name : perkident) : perktype = ([], Basetype name, [])

and func_of_lambda_void (t : perktype) : perktype =
  match t with
  | a, Lambdatype (args, ret, _), q ->
      ( a,
        Funtype (([], Pointertype ([], Basetype "void", []), []) :: args, ret),
        q )
  | _ -> failwith "func_of_lambda_void: not a lambda type"

and func_of_lambda (t : perktype) : perktype =
  match t with
  | a, Lambdatype (args, ret, _), q -> (a, Funtype (args, ret), q)
  | _ -> failwith "func_of_lambda: not a lambda type"

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

and archetype_sum_name (typ : perktype) : string =
  match typ with
  | _a, ArchetypeSum archs, _q ->
      "Sum"
      ^ String.concat "Plus"
          (List.map
             (fun a ->
               match discard_type_aq a with
               | Basetype name | ArcheType (name, _) -> name
               | _ ->
                   failwith
                     "Impossible: archetype sum is summing non-archetype types")
             archs)
  | _ -> failwith "archetype_sum_name: not an archetype sum type"

and unresolve_type (typ : perktype) : perktype =
  let a, t, q = typ in
  match t with
  | Basetype _ | Vararg | Infer -> typ
  | Pointertype t ->
      let new_t = unresolve_type t in
      (a, Pointertype new_t, q)
  | Optiontype t ->
      let new_t = unresolve_type t in
      (a, Optiontype new_t, q)
  | Tupletype ts ->
      let new_ts = List.map unresolve_type ts in
      (a, Tupletype new_ts, q)
  | Funtype (params, ret) ->
      let new_params = List.map unresolve_type params in
      let new_ret = unresolve_type ret in
      (a, Funtype (new_params, new_ret), q)
  | Lambdatype (params, ret, free_vars) ->
      let new_params = List.map unresolve_type params in
      let new_ret = unresolve_type ret in
      let new_free_vars =
        List.map (fun (t, i) -> (unresolve_type t, i)) free_vars
      in
      (a, Lambdatype (new_params, new_ret, new_free_vars), q)
  | Arraytype (typ, size) ->
      let new_typ = unresolve_type typ in
      (a, Arraytype (new_typ, size), q)
  | Structtype _name -> (* TODO: Struct *) typ
  | Modeltype (name, _archs, _fields, _constr_params) -> (a, Basetype name, q)
  | ArcheType (name, _fields) -> (a, Basetype name, q)
  | ArchetypeSum _ -> (a, Basetype (archetype_sum_name typ), q)
