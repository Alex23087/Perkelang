open Ast
open Errors
open Utils

let lambda_env_partition_table :
    (* Maybe do expr + loc instead of perktype, finer analysis 👍 *)
    (perktype_partial, int * perkvardesc list) Hashtbl.t =
  Hashtbl.create 10

let partition_table : (int, int) Hashtbl.t = Hashtbl.create 10
let lambda_env_counter = ref 0

let lambda_env_lookup (t : perktype) =
  try
    Some
      (Hashtbl.find lambda_env_partition_table
         (t |> unresolve_type |> discard_type_aq))
  with Not_found -> None

let lambda_env_bind (t : perktype) (l : perkvardesc list) =
  match discard_type_aq t with
  | Lambdatype _ -> (
      match lambda_env_lookup t with
      | Some (_id, _l) -> ()
      | None ->
          let t = unresolve_type t in
          let id = !lambda_env_counter in
          Hashtbl.add lambda_env_partition_table (discard_type_aq t) (id, l);
          Hashtbl.add partition_table id id;
          incr lambda_env_counter
          (* Printf.printf "Binding type %s to ID %d\n" (show_perktype t) id *))
  | _ -> ()

let rec lambda_env_bind_rec (t : perktype) (l : perkvardesc list) =
  match discard_type_aq t with
  | Lambdatype (params, ret, _) ->
      lambda_env_bind t l;
      lambda_env_bind ret [];
      List.iter (fun param -> lambda_env_bind_rec param []) params
  | _ -> ()

let lambda_env_unify (t1 : perktype) (t2 : perktype) =
  match (discard_type_aq t1, discard_type_aq t2) with
  | Lambdatype _, Lambdatype _ -> (
      let id1 = lambda_env_lookup t1 in
      let id2 = lambda_env_lookup t2 in
      match (id1, id2) with
      | Some (i1, _l1), Some (i2, _l2) when i1 = i2 ->
          (* Printf.printf "Same ID for %s, %s\n" (show_perktype t1)
            (show_perktype t2) *)
          ()
      | Some (i1, _l1), Some (i2, _l2) ->
          (* Printf.printf "Unifying lambda types with IDs %d and %d\n" i1 i2; *)
          Hashtbl.replace partition_table i1 i2
      | Some (i1, _l1), None ->
          (* Printf.printf "Unifying lambda types with IDs %d: %s\n" i1
            (show_perktype t1); *)
          Hashtbl.add lambda_env_partition_table
            (t2 |> unresolve_type |> discard_type_aq)
            (i1, [])
      | None, Some (i2, _l2) ->
          (* Printf.printf "Unifying lambda types with IDs %d: %s\n" i2
            (show_perktype t2); *)
          Hashtbl.add lambda_env_partition_table
            (t1 |> unresolve_type |> discard_type_aq)
            (i2, [])
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
  Printf.printf "Counter: %d\n" !lambda_env_counter;
  Printf.printf "Partition Table:\n";
  Hashtbl.iter
    (fun key value -> Printf.printf "ID: %d, Partition: %d\n" key value)
    partition_table

let rec lambda_env_unify_lists (t1 : perktype list) (t2 : perktype list) =
  match (t1, t2) with
  | [], [] -> ()
  | [ (_, Vararg, _) ], _ | _, [ (_, Vararg, _) ] -> ()
  | x :: xs, y :: ys ->
      lambda_env_unify x y;
      lambda_env_unify_lists xs ys
  | _ -> ()

let lambda_env_get_partitions () =
  Hashtbl.fold (fun _ i acc -> i :: acc) partition_table []
  |> List.sort_uniq compare

let lambda_env_get_free_vars_by_partition (num : int) =
  Hashtbl.fold
    (fun _ (i, fv) acc ->
      if Hashtbl.find partition_table i = num then fv @ acc else acc)
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
  let lambda = unresolve_type lambda in
  match lambda_env_lookup lambda with
  | Some (id, _free_vars) -> Printf.sprintf "_env_%d" id
  | None ->
      raise
        (Unbound_lambda_partition
           (Printf.sprintf "No partition found for the given type: %s"
              (show_perktype lambda)))

let lambda_env_get_free_vars_by_lambda (lambda : perktype) =
  let lambda = unresolve_type lambda in
  match lambda_env_lookup lambda with
  | Some (id, _) ->
      lambda_env_get_free_vars_by_partition (Hashtbl.find partition_table id)
  | None ->
      raise
        (Unbound_lambda_partition
           (Printf.sprintf "No partition found for the given type: %s"
              (show_perktype lambda)))

let lambda_env_get_partition_bind (lambda : perktype) : int * int =
  let lambda = unresolve_type lambda in
  match lambda_env_lookup lambda with
  | Some (id, _free_vars) ->
      let partition = Hashtbl.find partition_table id in
      (id, partition)
  | None ->
      raise
        (Unbound_lambda_partition
           (Printf.sprintf "No partition found for the given type: %s"
              (show_perktype lambda)))
