open Ast

let fresh_var_counter = ref 0
let fresh_var (): string =
  let v = !fresh_var_counter in
  fresh_var_counter := v + 1;
  Printf.sprintf "__perkelang_lambda_%d" v

let rec type_descriptor_of_perktype (t: perktype) : string =
  match t with
  | Basetype s -> s
  | Funtype (args, ret) ->
      let args_str = String.concat "__" (List.map type_descriptor_of_perktype args) in
      Printf.sprintf "%s_to_%s" args_str (type_descriptor_of_perktype ret)
  | Pointertype t -> Printf.sprintf "%s_ptr" (type_descriptor_of_perktype t)
  | Arraytype (t, _) -> Printf.sprintf "%s_arr" (type_descriptor_of_perktype t)
  | Classtype s -> s

let function_type_hashmap: (perktype, (string * string)) Hashtbl.t = Hashtbl.create 10
let lambdas_hashmap: (expr, (string * string)) Hashtbl.t = Hashtbl.create 10
let symbol_table: (perkident, perktype) Hashtbl.t = Hashtbl.create 10

let rec get_function_type (t: perktype) : (string * string) =
  try Hashtbl.find function_type_hashmap t
  with Not_found ->
    match t with
    | Funtype (args, ret) -> (
      let type_str = type_descriptor_of_perktype t in
      let typedef_str = Printf.sprintf "typedef %s"
        ((codegen_type ret) ^ " (*" ^ type_str ^ ")(" ^
        (String.concat ", " (List.map (fun t -> (codegen_type t)) args)) ^ ")") in
      Hashtbl.add function_type_hashmap t (type_str, typedef_str);
      type_str, typedef_str
    )
    | _ -> failwith "get_function_type: not a function type"

and get_lambda (e: expr) : (string * string) =
  try Hashtbl.find lambdas_hashmap e
  with Not_found ->
    let id = fresh_var () in
    let compiled = (match e with
    | Lambda (retype, args, body) -> (
      let type_str = codegen_type retype in
      let args_str = String.concat ", " (List.map (fun (t, id) -> Printf.sprintf "%s %s" (codegen_type t) id) args) in
      let body_str = codegen_command body 1 in
      let funtype  = Funtype (List.map (fun (t, _) -> t) args, retype) in
      put_symbol id funtype;
      Printf.sprintf "%s %s(%s) {\n%s\n}" type_str id args_str body_str
    )
    | _ -> failwith "get_lambda: not a lambda expression")
    in
    Hashtbl.add lambdas_hashmap e (id, compiled);
    id, compiled

and put_symbol (ident: perkident) (typ: perktype): unit =
  try let _ = Hashtbl.find symbol_table ident in ()
  with Not_found ->
    Hashtbl.add symbol_table ident typ

and codegen_program (cmd: command) : string =
  let body = codegen_command cmd 0 in
  (* Write typedefs *)
  Hashtbl.fold (fun _ v acc ->
    Printf.sprintf "%s%s;\n" acc (snd v)
  ) function_type_hashmap ""
  ^ "\n" ^
  (* Write hoisted function signatures *)
  Hashtbl.fold (fun id typ acc ->
    Printf.sprintf "%s%s;\n" acc (codegen_fundecl id typ)
  ) symbol_table ""
  ^ "\n" ^
  (* Write lambdas *)
  Hashtbl.fold (fun _ v acc ->
    Printf.sprintf "%sstatic %s;\n" acc (snd v)
  ) lambdas_hashmap ""
  ^ "\n" ^ body ^ ";"

and codegen_command (cmd: command) (indentation: int) : string =
  let indent_string = String.make (4 * indentation) ' ' in
  match cmd with
  | InlineC s -> s
  | Block c -> indent_string ^ "{\n" ^ codegen_command c (indentation + 1) ^ "\n" ^ indent_string ^ "}"
  | Def (t, e) -> indent_string ^ codegen_def t e
  | Fundef (Fun(t, id, args, body)) -> indent_string ^ codegen_fundef t id args body
  | Assign (s, e) -> indent_string ^ Printf.sprintf "%s = %s;" s (codegen_expr e)
  | Seq (c1, c2) -> Printf.sprintf "%s\n%s" (codegen_command c1 indentation) (codegen_command c2 indentation)
  | IfThenElse (e, c1, c2) ->
    indent_string ^ Printf.sprintf ("if (%s) {\n%s\n%s} else {\n%s\n%s}") (codegen_expr e) (codegen_command c1 (indentation + 1)) indent_string (codegen_command c2 (indentation + 1)) indent_string
  | Whiledo (e, c) ->
    indent_string ^ Printf.sprintf "while (%s) {\n%s\n%s}" (codegen_expr e) (codegen_command c (indentation + 1)) indent_string
  | Dowhile (e, c) ->
    indent_string ^ Printf.sprintf "do {\n%s\n%s} while (%s);" (codegen_command c (indentation + 1)) indent_string (codegen_expr e)
  | For (e1, e2, c1, c2) ->
    indent_string ^ Printf.sprintf "for (%s; %s; %s) {\n%s\n%s}" (codegen_expr e1) (codegen_expr e2) (codegen_command c1 0) (codegen_command c2 (indentation + 1)) indent_string
  | Expr e -> indent_string ^ Printf.sprintf "%s;" (codegen_expr e)
  | Skip -> "" (*TODO: Should this be ; ?*)
  | Switch (e, cases) ->
      let cases_str = String.concat "\n" (List.map (fun (e, c) -> indent_string ^ "    " ^ Printf.sprintf "case %s: {\n%s\n}" (codegen_expr e) (codegen_command c (indentation + 1))) cases) in
      indent_string ^ Printf.sprintf "switch (%s) {\n%s\n%s}" (codegen_expr e) cases_str indent_string
  | Return e -> indent_string ^ Printf.sprintf "return %s;" (codegen_expr e)

and codegen_def (t: perkvardesc) (e: expr) : string =
  let (t, id) = t in
  let type_str = codegen_type t in
  let expr_str = codegen_expr e in
  Printf.sprintf "%s %s = %s;" type_str id expr_str

and codegen_fundef (t: perktype) (id: perkident) (args: perkvardesc list) (body: command) : string =
  let type_str = codegen_type t in
  let args_str = String.concat ", " (List.map (fun (t, id) -> Printf.sprintf "%s %s" (codegen_type t) id) args) in
  let body_str = codegen_command body 1 in
  let funtype  = Funtype (List.map (fun (t, _) -> t) args, t) in
  put_symbol id funtype;
  Printf.sprintf "%s %s(%s) {\n%s\n}" type_str id args_str body_str

and codegen_type (t: perktype) : string =
  match t with
  | Basetype s -> s
  | Funtype _ ->
      t |> get_function_type |> fst
  | Pointertype t -> Printf.sprintf "%s*" (codegen_type t)
  | Arraytype (t, Some n) -> Printf.sprintf "%s[%d]" (codegen_type t) n
  | Arraytype (t, None) -> Printf.sprintf "%s[]" (codegen_type t)
  | Classtype s -> s

and codegen_expr (e: expr) : string =
  match e with
  | Int i -> string_of_int i
  | Pointer e -> (
    let code = codegen_expr e in
    Printf.sprintf "*%s" code
  )
  | Var id -> id
  | Apply (e, args) -> 
      (let args_str = String.concat ", " (List.map codegen_expr args) in
      Printf.sprintf "%s(%s)" (codegen_expr e) args_str)
  | Binop (op, e1, e2) ->
      Printf.sprintf "%s %s %s" (codegen_expr e1) (codegen_binop op) (codegen_expr e2)
  | Unop (op, e) -> (
    Printf.sprintf "%s %s" (codegen_unop op) (codegen_expr e)
  )
  | Lambda _ -> (
      e |> get_lambda |> fst
  )

and codegen_binop (op: binop) : string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Eq -> "=="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="

and codegen_unop (op: unop) : string =
  match op with
  | Neg -> "-"
  | Not -> "!"

and codegen_fundecl (id: perkident) (typ: perktype): string =
  match typ with
  | Funtype (args, ret) -> (codegen_type ret) ^ " " ^ id ^ " (" ^ (String.concat ", " (List.map codegen_type args)) ^ ")"
  | _ -> failwith "codegen_fundecl: called with a non function type"