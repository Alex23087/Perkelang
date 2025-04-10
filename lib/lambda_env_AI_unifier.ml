open Ast
open Utils

let __lambda_type_symbol_table :
    (perkident, perktype * string option * perkident list) Hashtbl.t ref =
  ref (Hashtbl.create 0)

let lambda_env_partition_table :
    (perktype_partial, int * perkvardesc list) Hashtbl.t =
  Hashtbl.create 10

let lambda_env_counter = ref 0

let lambda_env_bind (t : perktype) (l : perkvardesc list) =
  let id = !lambda_env_counter in
  Hashtbl.add lambda_env_partition_table (discard_type_aq t) (id, l);
  incr lambda_env_counter;
  Printf.printf "Binding type %s to ID %d\n" (show_perktype t) id

let lambda_env_lookup (t : perktype) =
  try Some (Hashtbl.find lambda_env_partition_table (discard_type_aq t))
  with Not_found -> None

let lambda_env_unify (t1 : perktype) (t2 : perktype) =
  match (discard_type_aq t1, discard_type_aq t2) with
  | Lambdatype _, Lambdatype _ -> (
      let id1 = lambda_env_lookup t1 in
      let id2 = lambda_env_lookup t2 in
      match (id1, id2) with
      | Some (i1, _l1), Some (i2, _l2) when i1 = i2 -> ()
      | Some (i1, _l1), Some (i2, _l2) ->
          Printf.printf "Unifying lambda types with IDs %d and %d\n" i1 i2;
          Hashtbl.iter
            (fun key (value, fv) ->
              if value = i1 then
                Hashtbl.replace lambda_env_partition_table key (i2, fv))
            lambda_env_partition_table;
          Hashtbl.iter
            (fun key value ->
              Hashtbl.remove !__lambda_type_symbol_table key;
              Hashtbl.add
                !__lambda_type_symbol_table
                (Str.global_replace
                   (Str.regexp (Printf.sprintf "_env_%d" i1))
                   (Printf.sprintf "_env_%d" i2)
                   key)
                value)
            !__lambda_type_symbol_table
      | Some (i1, _l1), None ->
          Hashtbl.add lambda_env_partition_table (discard_type_aq t2) (i1, [])
      | None, Some (i2, _l2) ->
          Hashtbl.add lambda_env_partition_table (discard_type_aq t1) (i2, [])
      | _ ->
          failwith
            (Printf.sprintf "Unification failed: types are not bound: %s %s"
               (show_perktype t1) (show_perktype t2)))
  | _ ->
      (* (Printf.printf "unifier called with types %s %s\n")
        (show_perktype t1) (show_perktype t2) *)
      ()
(* Ignore when called with other types *)

let print_envs () =
  Printf.printf "Environment Partition Table:\n";
  Hashtbl.iter
    (fun key (value, free_vars) ->
      Printf.printf "Type: %s, Partition: %d, Free vars: %s.\n"
        (show_perktype_partial key)
        value
        (String.concat ", " (List.map show_perkvardesc free_vars)))
    lambda_env_partition_table;
  Printf.printf "Counter: %d\n" !lambda_env_counter

let rec lambda_env_unify_lists (t1 : perktype list) (t2 : perktype list) =
  match (t1, t2) with
  | [], [] -> ()
  | [ (_, Vararg, _) ], _ | _, [ (_, Vararg, _) ] -> ()
  | x :: xs, y :: ys ->
      lambda_env_unify x y;
      lambda_env_unify_lists xs ys
  | _ -> ()

let lambda_env_get_partitions () =
  let partitions =
    Hashtbl.fold (fun _ (i, _) acc -> i :: acc) lambda_env_partition_table []
  in
  List.sort_uniq compare partitions

let lambda_env_get_free_vars_by_partition (num : int) =
  Hashtbl.fold
    (fun _ (i, fv) acc -> if i = num then fv @ acc else acc)
    lambda_env_partition_table []

let lambda_env_get_free_vars_lists () =
  let partitions = lambda_env_get_partitions () in
  List.map (fun p -> lambda_env_get_free_vars_by_partition p) partitions

let lambda_env_print_partitions () =
  let partitions = lambda_env_get_partitions () in
  List.iter
    (fun p ->
      Printf.printf "Partition %d: %s\n" p
        (String.concat ", "
           (List.map show_perkvardesc (lambda_env_get_free_vars_by_partition p))))
    partitions

(* let __completed = ref false
let lambda_env_typecheck_complete () = __completed := true *)

let get_type_descriptor_for_partition (lambda : perktype) : string =
  match lambda_env_lookup lambda with
  | Some (id, _free_vars) -> Printf.sprintf "_env_%d" id
  | None -> "env"
(* failwith
        (Printf.sprintf "No partition found for the given type: %s"
           (show_perktype lambda)) *)
