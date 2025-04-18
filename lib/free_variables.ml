open Ast
open Var_symbol_table

(* returns pair of lists: FIRST LIST IS FREE VARS, SECOND LIST IS BOUND *)
let rec free_variables_command (cmd : command_a) :
    perkident list * perkident list =
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
    | Return None -> ([], [])
    | Return (Some e1) -> free_variables_expr e1
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
      | Access (e1, _id, _, _) -> fst (free_variables_expr e1)
      | Tuple (el, _) | Array el ->
          List.flatten (List.map (fun x -> fst (free_variables_expr x)) el)
      | As (id, _) -> [ id ]
      | Cast (_, e1) -> fst (free_variables_expr e1)
      | IfThenElseExpr (e1, e2, e3) ->
          fst (free_variables_expr e1)
          @ fst (free_variables_expr e2)
          @ fst (free_variables_expr e3)),
      [] )
  in
  let out_free, out_bound = free_variables_expr e in
  ( list_minus
      (List.sort_uniq String.compare out_free)
      (get_all_global_identifiers ()),
    List.sort_uniq String.compare out_bound )

and list_minus (l1 : 'a list) (l2 : 'a list) : 'a list =
  List.filter (fun x -> not (List.mem x l2)) l1
