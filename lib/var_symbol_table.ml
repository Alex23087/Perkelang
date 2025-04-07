open Ast
open Errors

let var_symbol_table : (perkident, perktype) Hashtbl.t list ref = ref []

let push_symbol_table_local symbol_table =
  symbol_table := Hashtbl.create 10 :: !symbol_table

let push_symbol_table () = push_symbol_table_local var_symbol_table
let pop_symbol_table_local symbol_table = symbol_table := List.tl !symbol_table
let pop_symbol_table () = pop_symbol_table_local var_symbol_table

let lookup_var_local symbol_table (id : perkident) : perktype option =
  let rec lookup_in_tables tables =
    match tables with
    | [] -> None
    | h :: t ->
        if Hashtbl.mem h id then Some (Hashtbl.find h id)
        else lookup_in_tables t
  in
  lookup_in_tables !symbol_table

let lookup_var = lookup_var_local var_symbol_table

let print_symbol_table_local symbol_table =
  Printf.printf "Symbol Table:\n";
  let print_table table =
    Hashtbl.iter
      (fun id typ ->
        Printf.printf "Identifier: %s, Type: %s\n" id (Codegen.codegen_type typ))
      table
  in
  List.iter print_table !symbol_table

let print_symbol_table () = print_symbol_table_local var_symbol_table

let bind_var_local symbol_table (id : perkident) (t : perktype) =
  match !symbol_table with
  | [] -> failwith "No symbol table available"
  | h :: _ ->
      if Hashtbl.mem h id then
        raise (Double_declaration ("Identifier already defined: " ^ id))
      else Hashtbl.add h id t
(* ;print_symbol_table () *)

let bind_var = bind_var_local var_symbol_table

let rebind_var_local symbol_table (id : perkident) (t : perktype) =
  match !symbol_table with
  | [] -> failwith "No symbol table available"
  | h :: _ ->
      if Hashtbl.mem h id then Hashtbl.replace h id t
      else raise (Undeclared ("Identifier wasn't defined: " ^ id))

let rebind_var = rebind_var_local var_symbol_table
