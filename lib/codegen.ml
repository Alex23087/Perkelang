open Ast
open Errors

exception TypeError of string

let fst_3 (x, _, _) = x
let snd_3 (_, x, _) = x
let thrd_3 (_, _, x) = x
let swizzle (x, y) = (y, x)
let hashtbl_forall f h = Hashtbl.fold (fun k v acc -> f k v && acc) h true
let hashtbl_exists f h = Hashtbl.fold (fun k v acc -> f k v || acc) h false
let fresh_var_counter = ref 0

let fresh_var (s : string) : string =
  let v = !fresh_var_counter in
  fresh_var_counter := v + 1;
  Printf.sprintf "__perkelang_%s_%d" s v

let rec type_descriptor_of_perktype (t : perktype) : string =
  let _, t, _ = t in
  match t with
  | Basetype s -> s
  | Structtype s -> "struct_" ^ s
  | Funtype (args, ret) ->
      let args_str =
        String.concat "__" (List.map type_descriptor_of_perktype args)
      in
      Printf.sprintf "l_%s_to_%s_r" args_str (type_descriptor_of_perktype ret)
  | Pointertype t -> Printf.sprintf "%s_ptr" (type_descriptor_of_perktype t)
  | Arraytype (t, _) -> Printf.sprintf "%s_arr" (type_descriptor_of_perktype t)
  | Vararg ->
      "vararg"
      (* This is probably problematic, cannot define function pointers with ... . Nvm, apparently you can 😕*)
  | ArcheType (name, _decls) -> name
  | Modeltype (name, _archs, _decls, _constr_params) -> name
  | Optiontype t -> Printf.sprintf "%s_opt" (type_descriptor_of_perktype t)
  | Infer -> failwith "Impossible: type has not been inferred"
  | Tupletype ts ->
      Printf.sprintf "begintup_%s_endtup"
        (String.concat "__" (List.map type_descriptor_of_perktype ts))

let function_type_hashmap : (perktype, string * string * string) Hashtbl.t =
  Hashtbl.create 10

let lambdas_hashmap : (expr_a, string * string) Hashtbl.t = Hashtbl.create 10
let symbol_table : (perkident, perktype) Hashtbl.t = Hashtbl.create 10
let import_list : string list ref = ref []
let tuple_hashmap : (perktype, string) Hashtbl.t = Hashtbl.create 10

let archetype_hashtable : (string, (string, perktype) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 10

let struct_def_list : string list ref = ref []

let add_struct_def (code : string) : unit =
  struct_def_list := code :: !struct_def_list

let add_archetype (name : string) : (string, perktype) Hashtbl.t =
  let new_archetype = Hashtbl.create 10 in
  Hashtbl.add archetype_hashtable name new_archetype;
  new_archetype

let get_archetype (name : string) : (string, perktype) Hashtbl.t =
  try Hashtbl.find archetype_hashtable name
  with Not_found ->
    raise (TypeError (Printf.sprintf "Archetype %s not found" name))

let add_binding_to_archetype (name : string) (id : perkident) (typ : perktype) :
    unit =
  let archetype = get_archetype name in
  if not (Hashtbl.mem archetype id) then Hashtbl.add archetype id typ

let add_import (lib : string) : unit =
  if not (List.mem lib !import_list) then import_list := lib :: !import_list

let rec get_tuple (t : perktype) : string =
  try Hashtbl.find tuple_hashmap t
  with Not_found -> (
    match t with
    | _, Tupletype ts, _ ->
        let type_str = type_descriptor_of_perktype t in
        let compiled =
          Printf.sprintf "typedef struct {%s} %s;"
            (if List.length ts = 0 then ""
             else
               String.concat "; "
                 (List.mapi
                    (fun i t -> Printf.sprintf "%s _%d" (codegen_type t) i)
                    ts)
               ^ ";")
            type_str
        in
        Hashtbl.add tuple_hashmap t compiled;
        type_str
    | _ ->
        failwith
          (Printf.sprintf "get_tuple: not a tuple type, got %s"
             (type_descriptor_of_perktype t)))

and get_function_type (t : perktype) : string * string * string =
  try Hashtbl.find function_type_hashmap t
  with Not_found -> (
    let _, t', _ = t in
    (*TODO: Implement type attribs and qualifiers*)
    match t' with
    | Funtype (args, ret) ->
        let type_str = type_descriptor_of_perktype t in
        let typedef_str =
          Printf.sprintf "typedef %s"
            (codegen_type ret ^ " (*" ^ type_str ^ ")("
            ^ String.concat ", "
                (List.map (fun t -> codegen_type t ~expand:true) args)
            ^ ")")
        in
        let expanded_str =
          Printf.sprintf "%s"
            (codegen_type ret ^ " (*)("
            ^ String.concat ", "
                (List.map (fun t -> codegen_type t ~expand:false) args)
            ^ ")")
        in
        Hashtbl.add function_type_hashmap t (type_str, typedef_str, expanded_str);
        (type_str, typedef_str, expanded_str)
    | _ -> failwith "get_function_type: not a function type")

and get_lambda (e : expr_a) : string * string =
  try Hashtbl.find lambdas_hashmap e
  with Not_found ->
    let id = fresh_var "lambda" in
    let compiled =
      match ( $ ) e with
      | Lambda (retype, args, body) ->
          let type_str = codegen_type retype in
          let args_str =
            String.concat ", "
              (List.map
                 (fun (t, id) -> Printf.sprintf "%s %s" (codegen_type t) id)
                 args)
          in
          let body_str = codegen_command body 1 in
          let funtype =
            ([ Static ], Funtype (List.map (fun (t, _) -> t) args, retype), [])
          in
          put_symbol id funtype;
          Printf.sprintf "static %s %s(%s) {\n%s\n}" type_str id args_str
            body_str
      | _ -> failwith "get_lambda: not a lambda expression"
    in
    Hashtbl.add lambdas_hashmap e (id, compiled);
    (id, compiled)

and put_symbol (ident : perkident) (typ : perktype) : unit =
  try
    let _ = Hashtbl.find symbol_table ident in
    ()
  with Not_found -> Hashtbl.add symbol_table ident typ

and codegen_program (cmd : command_a) : string =
  let body = codegen_command cmd 0 in
  (* Write includes *)
  String.concat "\n"
    (List.map (fun lib -> Printf.sprintf "#include %s" lib) !import_list)
  ^ "\n\n"
  (* Write typedefs *)
  ^ Hashtbl.fold
      (fun _ v acc -> Printf.sprintf "%s%s;\n" acc (snd_3 v))
      function_type_hashmap ""
  ^ "\n"
  (* Write typedefs for tuples *)
  ^ Hashtbl.fold
      (fun _ v acc -> Printf.sprintf "%s%s;\n" acc v)
      tuple_hashmap ""
  (* Write struct definitions *)
  ^ String.concat "\n" (List.rev !struct_def_list)
  ^ "\n\n"
  (* Write hoisted function signatures *)
  ^ Hashtbl.fold
      (fun id typ acc -> Printf.sprintf "%s%s;\n" acc (codegen_fundecl id typ))
      symbol_table ""
  ^ "\n" ^ body ^ "\n"
  ^
  (* Write lambdas *)
  Hashtbl.fold
    (fun _ v acc -> Printf.sprintf "%s\n%s\n" acc (snd v))
    lambdas_hashmap ""

and codegen_command (cmd : command_a) (indentation : int) : string =
  let indent_string = String.make (4 * indentation) ' ' in
  match ( $ ) cmd with
  | Archetype (i, l) ->
      let _ = add_archetype i in
      List.iter
        (fun t ->
          let typ, id = t in
          add_binding_to_archetype i id typ)
        l;
      add_struct_def
        (Printf.sprintf "%sstruct %s {\n%s\n};\n" indent_string i
           (if List.length l = 0 then ""
            else
              (indent_string ^ "    "
              ^ String.concat
                  (";\n" ^ indent_string ^ "    ")
                  (List.map
                     (fun ((a, typ, d), id) ->
                       let typ =
                         match typ with
                         | Funtype (params, ret) ->
                             ( a,
                               Funtype
                                 ( ( [],
                                     Pointertype ([], Basetype "void", []),
                                     [] )
                                   :: params,
                                   ret ),
                               d )
                         | _ -> ([], Pointertype (a, typ, d), [])
                       in
                       codegen_decl (typ, id))
                     l))
              ^ ";"));
      ""
  | Model (name, il, defs) ->
      let mems = List.map (fun ((typ, id), _) -> (typ, id)) defs in
      let archetypes = List.map (fun n -> (n, get_archetype n)) il in
      let archetype_decls =
        List.map
          (fun (n, h) -> Hashtbl.fold (fun k v acc -> (v, k, n) :: acc) h [])
          archetypes
      in
      let archetype_decls = List.flatten archetype_decls in
      let missing_decl_name = ref ("", "") in
      let (*theorem: *) has_all_the_right_things =
        List.for_all
          (fun (typ, id, arch) ->
            if List.mem (typ, id) mems then true
            else (
              missing_decl_name := (arch, id);
              false))
          archetype_decls
      in
      if not has_all_the_right_things then
        let line, col = (( @@ ) cmd).start_pos in
        raise
          (Type_error
             ( line,
               col,
               Printf.sprintf
                 "Model %s does not properly implement Archetype %s: missing \
                  declaration for %s"
                 name (fst !missing_decl_name) (snd !missing_decl_name) ))
      else
        let constructor =
          List.find_opt (fun ((_, id), _) -> id = "constructor") defs
        in
        let defs =
          List.map
            (fun ((typ, id), expr) ->
              let selftype = ([], Pointertype ([], Structtype name, []), []) in
              match (typ, ( $ ) expr) with
              | ( (attrs, Funtype (params, ret), specs),
                  Lambda (lret, lparams, lexpr) ) ->
                  ( ((attrs, Funtype (selftype :: params, ret), specs), id),
                    annotate_dummy
                      (Lambda (lret, (selftype, "self") :: lparams, lexpr)) )
              | _ -> ((typ, id), expr))
            defs
        in
        let mems = List.map (fun ((typ, id), _) -> (typ, id)) defs in
        add_struct_def
          (Printf.sprintf "%sstruct %s {\n%s%s\n};\n%stypedef struct %s* %s;\n"
             indent_string name
             (if List.length mems = 0 then ""
              else
                (indent_string ^ "    "
                ^ String.concat
                    (";\n" ^ indent_string ^ "    ")
                    (List.map codegen_decl mems))
                ^ ";")
             (if List.length il = 0 then ""
              else
                "\n\n"
                ^ String.concat "\n"
                    (List.map
                       (fun s ->
                         Printf.sprintf "%sstruct %s %s;"
                           (indent_string ^ "    ") s s)
                       il))
             indent_string name name);
        let params_str_with_types, params_str =
          match constructor with
          | None -> ("", "")
          | Some ((typ, _), _) -> (
              match typ with
              | _, Funtype (params, _), _ ->
                  ( String.concat ", "
                      (List.mapi
                         (fun (i : int) (t : perktype) ->
                           Printf.sprintf "%s arg_%d" (codegen_type t) i)
                         params),
                    String.concat ", "
                      (List.mapi (fun i _ -> Printf.sprintf "arg_%d" i) params)
                  )
              | _ -> raise (TypeError "Constructor is not a function type"))
        in
        let params_str = if params_str = "" then "" else ", " ^ params_str in
        (* TODO: Add error locations *)
        Printf.sprintf
          "%s%s %s_init(%s) {\n\
          \    %s%s obj = malloc(sizeof(struct %s));\n\
          \    %s%s self = obj;\n\
           %s\n\
           %s    %sreturn obj;\n\
           }\n"
          indent_string name name params_str_with_types indent_string name name
          indent_string name
          (if List.length mems = 0 then ""
           else
             indent_string ^ "    "
             ^ String.concat
                 (";\n" ^ indent_string ^ "    ")
                 (List.map
                    (fun ((typ, id), expr) ->
                      Printf.sprintf "obj->%s = (%s) %s" id (codegen_type typ)
                        (codegen_expr expr))
                    defs)
             ^ ";")
          (match constructor with
          | None -> ""
          | Some _ ->
              Printf.sprintf "%s    obj->constructor(obj%s);\n" indent_string
                params_str)
          indent_string
  | InlineC s -> s
  | Block c ->
      indent_string ^ "{\n"
      ^ codegen_command c (indentation + 1)
      ^ "\n" ^ indent_string ^ "}"
  | Def (t, e) -> indent_string ^ codegen_def t e
  | Fundef (t, id, args, body) -> indent_string ^ codegen_fundef t id args body
  | Extern _ ->
      ""
      (* Externs are only useful for type checking. No need to keep it for codegen step *)
  | Assign (l, r) ->
      indent_string
      ^ Printf.sprintf "%s = %s;" (codegen_expr l) (codegen_expr r)
  | Seq (c1, c2) ->
      let c1_code = codegen_command c1 indentation in
      let c2_code = codegen_command c2 indentation in
      if String.length c1_code = 0 then Printf.sprintf "%s" c2_code
      else if String.length c2_code = 0 then Printf.sprintf "%s" c1_code
      else Printf.sprintf "%s\n%s" c1_code c2_code
  | IfThenElse (e, c1, c2) ->
      indent_string
      ^ Printf.sprintf "if (%s) {\n%s\n%s} else {\n%s\n%s}" (codegen_expr e)
          (codegen_command c1 (indentation + 1))
          indent_string
          (codegen_command c2 (indentation + 1))
          indent_string
  | Whiledo (e, c) ->
      indent_string
      ^ Printf.sprintf "while (%s) {\n%s\n%s}" (codegen_expr e)
          (codegen_command c (indentation + 1))
          indent_string
  | Dowhile (e, c) ->
      indent_string
      ^ Printf.sprintf "do {\n%s\n%s} while (%s);"
          (codegen_command c (indentation + 1))
          indent_string (codegen_expr e)
  | For (c1, e2, c3, body) ->
      let c1_code = codegen_command c1 0 in
      let c1_code =
        if String.ends_with c1_code ~suffix:";" then
          String.sub c1_code 0 (String.length c1_code - 1)
        else c1_code
      in
      let c3_code = codegen_command c3 0 in
      let c3_code =
        if String.ends_with c3_code ~suffix:";" then
          String.sub c3_code 0 (String.length c3_code - 1)
        else c3_code
      in
      indent_string
      ^ Printf.sprintf "for (%s; %s; %s) {\n%s\n%s}" c1_code (codegen_expr e2)
          c3_code
          (codegen_command body (indentation + 1))
          indent_string
  | Expr e -> indent_string ^ Printf.sprintf "%s;" (codegen_expr e)
  | Skip -> "" (*TODO: Should this be ; ?*)
  | Switch (e, cases) ->
      let cases_str =
        String.concat "\n"
          (List.map
             (fun (e, c) ->
               indent_string ^ "    "
               ^ Printf.sprintf "case %s: {\n%s\n}" (codegen_expr e)
                   (codegen_command c (indentation + 1)))
             cases)
      in
      indent_string
      ^ Printf.sprintf "switch (%s) {\n%s\n%s}" (codegen_expr e) cases_str
          indent_string
  | Import lib ->
      add_import lib;
      ""
  | Banish name ->
      (* TODO: Automatically banish children *)
      Printf.sprintf "%sfree(%s);\n%s%s = NULL;" indent_string name
        indent_string name
  | Return e -> indent_string ^ Printf.sprintf "return %s;" (codegen_expr e)

and codegen_def (t : perkvardesc) (e : expr_a) : string =
  let decl_str = codegen_decl t in
  let expr_str = codegen_expr e in
  Printf.sprintf "%s = %s;" decl_str expr_str

and codegen_decl (t : perkvardesc) : string =
  let t, id = t in
  let type_str = codegen_type t in
  Printf.sprintf "%s %s" type_str id

and codegen_fundef (t : perktype) (id : perkident) (args : perkvardesc list)
    (body : command_a) : string =
  let type_str = codegen_type t in
  let args_str =
    String.concat ", "
      (List.map
         (fun (t, id) -> Printf.sprintf "%s %s" (codegen_type t) id)
         args)
  in
  let body_str = codegen_command body 1 in
  let funtype = ([], Funtype (List.map (fun (t, _) -> t) args, t), []) in
  put_symbol id funtype;
  Printf.sprintf "%s %s(%s) {\n%s\n}" type_str id args_str body_str

and codegen_type ?(expand : bool = false) (t : perktype) : string =
  let attrs, t', quals = t in
  let attrs_str = String.concat " " (List.map codegen_attr attrs) in
  let quals_str = String.concat " " (List.map codegen_qual quals) in
  let type_str =
    match t' with
    | Basetype s -> s
    | Structtype s -> "struct " ^ s
    | Funtype _ -> t |> get_function_type |> if expand then thrd_3 else fst_3
    | Pointertype ([], Structtype _, _) when expand -> "void*"
    | Pointertype t -> Printf.sprintf "%s*" (codegen_type t ~expand)
    | Arraytype (t, Some n) ->
        Printf.sprintf "%s[%d]" (codegen_type t ~expand) n
    | Arraytype (t, None) -> Printf.sprintf "%s[]" (codegen_type t ~expand)
    | Vararg -> "..."
    | Modeltype (name, _archs, _decls, _constr_params) -> name
    | ArcheType (name, _decls) -> name
    | Infer -> failwith "Impossible: type has not been inferred"
    | Optiontype t ->
        Printf.sprintf "struct {int is_some; %s contents}" (codegen_type t)
    | Tupletype _ts ->
        let _ = get_tuple t in
        type_descriptor_of_perktype t
  in
  if attrs_str = "" && quals_str = "" then type_str
  else if attrs_str = "" then Printf.sprintf "%s %s" quals_str type_str
  else if quals_str = "" then Printf.sprintf "%s %s" attrs_str type_str
  else Printf.sprintf "%s %s %s" attrs_str quals_str type_str

and codegen_attr (attr : perktype_attribute) : string =
  match attr with Public -> "" | Private -> "static" | Static -> "static"

and codegen_qual (qual : perktype_qualifier) : string =
  match qual with
  | Const -> "const"
  | Volatile -> "volatile"
  | Restrict -> "restrict"

and codegen_expr (e : expr_a) : string =
  let e = ( $ ) e in
  match e with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Char c -> Printf.sprintf "'%c'" c
  | String s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | Pointer e ->
      let code = codegen_expr e in
      Printf.sprintf "*%s" code
  | Var id -> id
  | Apply (e, args) -> (
      let expr_str = codegen_expr e in
      let args_str = String.concat ", " (List.map codegen_expr args) in
      match ( $ ) e with
      | Access (e1, _) ->
          let e1_str = codegen_expr e1 in
          let args_str = if List.length args = 0 then "" else ", " ^ args_str in
          Printf.sprintf "%s(%s%s)" expr_str e1_str args_str
      | _ -> Printf.sprintf "%s(%s)" expr_str args_str)
  | Access (e1, ide) -> Printf.sprintf "%s->%s" (codegen_expr e1) ide
  | Binop (op, e1, e2) ->
      Printf.sprintf "%s %s %s" (codegen_expr e1) (codegen_binop op)
        (codegen_expr e2)
  | PreUnop (op, e) ->
      Printf.sprintf "%s%s" (codegen_preunop op) (codegen_expr e)
  | PostUnop (op, e) ->
      Printf.sprintf "%s%s" (codegen_expr e) (codegen_postunop op)
  | Parenthesised e -> Printf.sprintf "(%s)" (codegen_expr e)
  | Lambda _ -> e |> annotate_dummy |> get_lambda |> fst
  | Subscript (e1, e2) ->
      Printf.sprintf "%s[%s]" (codegen_expr e1) (codegen_expr e2)
  | Summon (typident, args) ->
      let args_str = String.concat ", " (List.map codegen_expr args) in
      (* Printf.sprintf "%sstruct %s %s;\n%s%s_init(&%s);\n%s%s.constructor((void*)&%s, %s);" indent_string typident name indent_string typident name indent_string name name args_str *)
      Printf.sprintf "%s_init(%s)" typident args_str
  | Tuple (es, typ) ->
      Printf.sprintf "(%s){%s}"
        (typ |> Option.get |> codegen_type)
        (String.concat ", "
           (List.map (fun e -> Printf.sprintf "%s" (codegen_expr e)) es))
  | TupleSubscript (e, i) -> Printf.sprintf "%s._%d" (codegen_expr e) i
(* | Nothing t -> Printf.sprintf "{0, 0}" TODO: Continue here *)

(* struct {int is_empty; int value;} palle = {0,1}; *)

and codegen_binop (op : binop) : string =
  match op with
  | Add -> "+"
  | Mul -> "*"
  | Div -> "/"
  | Sub -> "-"
  | Eq -> "=="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="

and codegen_preunop (op : preunop) : string =
  match op with
  | Neg -> "-"
  | Not -> "!"
  | Dereference -> "*"
  | Reference -> "&"
  | PreIncrement -> "++"
  | PreDecrement -> "--"

and codegen_postunop (op : postunop) : string =
  match op with PostIncrement -> "++" | PostDecrement -> "--"

and codegen_fundecl (id : perkident) (typ : perktype) : string =
  let attrs, t, _ = typ in
  let attrs_str = String.concat " " (List.map codegen_attr attrs) in
  let type_str =
    match t with
    | Funtype (args, ret) ->
        codegen_type ret ^ " " ^ id ^ " ("
        ^ String.concat ", " (List.map codegen_type args)
        ^ ")"
    | _ -> failwith "codegen_fundecl: called with a non function type"
  in
  if attrs_str = "" then type_str else Printf.sprintf "%s %s" attrs_str type_str
