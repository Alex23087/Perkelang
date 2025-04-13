open Ast
open Errors
open Utils
open Type_symbol_table

exception TypeError of string

let fst_4 (x, _, _, _) = x
let snd_4 (_, x, _, _) = x
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

(* Lambda expression, identifier, generated code, capture list, type_descriptor*)
let lambdas_hashmap : (expr_a, string * string * string list * string) Hashtbl.t
    =
  Hashtbl.create 10

let fundecl_symbol_table : (perkident, perktype) Hashtbl.t = Hashtbl.create 10
let import_list : string list ref = ref []

let archetype_hashtable : (string, (string, perktype) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 10

let add_archetype (name : string) : (string, perktype) Hashtbl.t =
  let new_archetype = Hashtbl.create 10 in
  Hashtbl.add archetype_hashtable name new_archetype;
  new_archetype

(* TODO: Remove this. This info can be retrieved from type_symbol_table *)
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

let lambda_environments : (string * string) list ref = ref []
let lambda_capture_dummies : (string * string) list ref = ref []

let rec codegen_lambda (e : expr_a) : string =
  try fst_4 (Hashtbl.find lambdas_hashmap e)
  with Not_found ->
    let id = fresh_var "lambda" in
    let compiled, free_variables, type_descriptor =
      match ( $ ) e with
      | Lambda (retype, args, body, free_variables) ->
          (* TODO: LAMBDA *)
          (* try *)
          let type_str = codegen_type retype in
          let args_str =
            if List.length args = 0 then ""
            else
              ", "
              ^ String.concat ", "
                  (List.map
                     (fun (t, id) -> Printf.sprintf "%s %s" (codegen_type t) id)
                     args)
          in
          let env_bind_str =
            if List.length free_variables = 0 then "\n"
            else
              "\n"
              ^ (let envtype = type_descriptor_of_environment free_variables in
                 Printf.sprintf "    %s* _env = (%s*) __env;\n" envtype envtype)
              ^ String.concat ";\n"
                  (List.mapi
                     (fun i (t, id) ->
                       Printf.sprintf "    %s %s = (%s)_env->_%d"
                         (codegen_type t) id (codegen_type t) i)
                     free_variables)
              ^ ";\n"
          in
          let body_str = codegen_command body 1 in
          let lambdatype =
            ( [ Static ],
              Lambdatype
                (List.map (fun (t, _) -> t) args, retype, free_variables),
              [] )
          in
          bind_function_type id (add_parameter_to_func void_pointer lambdatype);
          let type_descriptor =
            type_descriptor_of_perktype (func_of_lambda_void lambdatype)
          in
          (* codegen_lambda_capture e lambdatype; *)
          ( Printf.sprintf "static %s %s(void* __env%s) {%s%s\n}" type_str id
              args_str env_bind_str body_str,
            List.map snd free_variables,
            type_descriptor )
          (* with Not_inferred s -> raise_type_error e s *)
      | _ -> failwith "get_lambda: not a lambda expression"
    in
    Hashtbl.add lambdas_hashmap e (id, compiled, free_variables, type_descriptor);
    Printf.sprintf "{{%s}, (%s) %s}"
      (String.concat ", " (List.map (fun s -> "(void*)" ^ s) free_variables))
      type_descriptor id

and bind_function_type (ident : perkident) (typ : perktype) : unit =
  try
    let _ = Hashtbl.find fundecl_symbol_table ident in
    ()
  with Not_found -> Hashtbl.add fundecl_symbol_table ident typ

and codegen_program (tldfs : topleveldef_a list) : string =
  let s =
    say_here "codegen_program";
    filter_var_table ();
    (* List.iter
      (fun (id, typ) ->
        Printf.printf "%s: %s\n" id (type_descriptor_of_perktype typ))
      !all_vars; *)
    let body =
      String.concat "\n"
        (List.map
           (fun tldf ->
             let code = codegen_topleveldef tldf in
             if String.length code = 0 then "" else Printf.sprintf "%s\n" code)
           tldfs)
      |> String.trim
    in
    (* Write includes *)
    "#include <malloc.h>\n"
    ^ String.concat "\n"
        (List.map (fun lib -> Printf.sprintf "#include %s" lib) !import_list)
    ^ "\n\n"
    (* Write macros *)
    ^ "#define CAST_LAMBDA(name, from_type, to_type, func_type) \
       ((__perkelang_capture_dummy_##from_type = name, (to_type) \
       {__perkelang_capture_dummy_##from_type.env, \
       (func_type)__perkelang_capture_dummy_##from_type.func}))\n"
    ^ "#define CALL_LAMBDA0(l, t) (__perkelang_capture_dummy_##t = l, \
       __perkelang_capture_dummy_##t.func(&(__perkelang_capture_dummy_##t.env)))\n"
    ^ "#define CALL_LAMBDA(l, t, ...) (__perkelang_capture_dummy_##t = l, \
       __perkelang_capture_dummy_##t.func(&(__perkelang_capture_dummy_##t.env), \
       __VA_ARGS__))\n" ^ "\n\n"
    (* Write typedefs for functions*)
    (* ^ Hashtbl.fold
         (fun _ v acc -> Printf.sprintf "%s%s;\n" acc (snd_3 v))
         function_type_hashmap ""
     ^ "\n" *)
    (* Write typedefs for tuples *)
    (* ^ Hashtbl.fold
      (fun _ v acc -> Printf.sprintf "%s%s;\n" acc v)
      tuple_hashmap "" *)
    (* Write struct definitions *)
    (* ^ String.concat "\n" (List.rev !struct_def_list)
     ^ "\n\n" *)
    ^ generate_types ()
    ^ "\n"
    (* Write environment typedefs *)
    (* ^ (if List.length !lambda_environments = 0 then ""
        else
          "\n"
          ^ String.concat ";\n"
              (List.map (fun (_name, typ) -> typ) !lambda_environments)
          ^ ";\n\n")
     (* Write lambda capture dummies *)
     ^ (if List.length !lambda_capture_dummies = 0 then ""
        else
          "\n"
          ^ String.concat ";\n"
              (List.map (fun (_name, typ) -> typ) !lambda_capture_dummies)
          ^ ";\n\n") *)
    (* Write hoisted function signatures *)
    ^ Hashtbl.fold
        (fun id typ acc ->
          Printf.sprintf "%s%s;\n" acc (codegen_fundecl id typ))
        fundecl_symbol_table ""
    (* Write program code *)
    ^ "\n"
    ^ body ^ "\n"
    (* Write lambdas *)
    ^ Hashtbl.fold
        (fun _ v acc -> Printf.sprintf "%s\n%s\n" acc (snd_4 v))
        lambdas_hashmap ""
  in
  (* Printf.printf "dependenciesoftype called: %d\nused: %d\nunused: %d\n"
    !called_counter !used_counter !unused_counter;
  Printf.printf "resolve_type called: %d\nused: %d\nunused: %d\n" !resolve_count
    !resolve_hit !resolve_miss; *)
  s

and codegen_topleveldef (tldf : topleveldef_a) : string =
  (* try *)
  say_here (Printf.sprintf "codegen_topleveldef: %s" (show_topleveldef_a tldf));
  let indent_string = "" in
  (*TODO: Remove*)
  match ( $ ) tldf with
  | Archetype (i, l) ->
      let _ = add_archetype i in
      List.iter
        (fun t ->
          let typ, id = t in
          add_binding_to_archetype i id typ)
        l;
      add_code_to_type_binding
        ([], ArcheType (i, l), [])
        (Printf.sprintf "\n%sstruct %s {\n%s\n};\ntypedef struct %s %s;"
           indent_string i
           (if List.length l = 0 then ""
            else
              (indent_string ^ "    "
              ^ String.concat
                  (";\n" ^ indent_string ^ "    ")
                  (List.map
                     (fun ((a, typ, d), id) ->
                       let typ =
                         match typ with
                         (* | Funtype (_, _) ->
                             add_parameter_to_func void_pointer (a, typ, d) *)
                         | Lambdatype (_params, _ret, _free_variables) ->
                             add_parameter_to_func void_pointer (a, typ, d)
                         | _ -> ([], Pointertype (a, typ, d), [])
                       in
                       codegen_decl (typ, id))
                     l))
              ^ ";")
           i i);
      ""
  | Model (name, il, defs) ->
      (* let mems = List.map (fun ((typ, id), _) -> (typ, id)) defs in *)
      let archetypes = List.map (fun n -> (n, get_archetype n)) il in
      let archetype_decls =
        List.map
          (fun (n, h) -> Hashtbl.fold (fun k v acc -> (v, k, n) :: acc) h [])
          archetypes
      in
      let archetype_decls = List.flatten archetype_decls in
      (* let missing_decl_name = ref ("", "") in
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
           raise
             (Type_error
                ( (( @@ ) tldf).start_pos,
                  (( @@ ) tldf).end_pos,
                  Printf.sprintf
                    "Model %s does not properly implement Archetype %s: missing \
                     declaration for %s"
                    name (fst !missing_decl_name) (snd !missing_decl_name) ))
         else *)
      let defs =
        List.map
          (fun ((typ, id), expr) ->
            let selftype = self_type name in
            match (typ, ( $ ) expr) with
            | ( (attrs, Lambdatype (params, ret, free_vars), specs),
                Lambda (lret, lparams, lexpr, _free_vars) ) ->
                let (typ, id), expr =
                  ( ( ( attrs,
                        Lambdatype (selftype :: params, ret, free_vars),
                        specs ),
                      id ),
                    annot_copy expr
                      (Lambda
                         (lret, (selftype, "serf") :: lparams, lexpr, free_vars))
                  )
                in
                ((typ, id), expr)
            | _ -> ((typ, id), expr))
          defs
      in
      let constructor =
        List.find_opt (fun ((_, id), _) -> id = "constructor") defs
      in
      let mems = List.map (fun ((typ, id), _) -> (typ, id)) defs in
      let params_str_with_types, params_str, params_typ =
        match constructor with
        | None -> ("", "", [])
        | Some ((typ, _), _) -> (
            match typ with
            | _, Funtype (params, _), _ ->
                ( String.concat ", "
                    (List.mapi
                       (fun (i : int) (t : perktype) ->
                         Printf.sprintf "%s arg_%d" (codegen_type t) i)
                       params),
                  String.concat ", "
                    (List.mapi (fun i _ -> Printf.sprintf "arg_%d" i) params),
                  params )
            (* TODO: LAMBDA pass free vars to init *)
            | _, Lambdatype (params, _, _), _ ->
                let params =
                  try List.tl params
                  with Failure _ ->
                    raise_type_error tldf
                      "constructor has 0 arguments. If you see this error, \
                       please open an issue at \
                       https://github.com/Alex23087/Perkelang"
                in
                ( String.concat ", "
                    (List.mapi
                       (fun (i : int) (t : perktype) ->
                         Printf.sprintf "%s arg_%d" (codegen_type t) i)
                       params),
                  String.concat ", "
                    (List.mapi (fun i _ -> Printf.sprintf "arg_%d" i) params),
                  params )
            | _ -> raise (TypeError "Constructor is not a function type"))
      in
      add_code_to_type_binding
        ([], Modeltype (name, il, List.map fst defs, params_typ), [])
        (Printf.sprintf "\n%sstruct %s {\n%s%s\n};\n%stypedef struct %s* %s;\n"
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
                       Printf.sprintf "%sstruct %s %s;" (indent_string ^ "    ")
                         s s)
                     il))
           indent_string name name);
      let params_str = if params_str = "" then "" else ", " ^ params_str in
      (* TODO: Add error locations *)
      Printf.sprintf
        "%s%s %s_init(%s) {\n\
        \    %s%s self = malloc(sizeof(struct %s));\n\
         %s\n\
         %s\n\
         %s    %sreturn self;\n\
         }"
        indent_string name name params_str_with_types indent_string name name
        (if List.length mems = 0 then ""
         else
           indent_string ^ "    "
           ^ String.concat
               (";\n" ^ indent_string ^ "    ")
               (List.map
                  (fun ((typ, id), expr) ->
                    Printf.sprintf "self->%s = (%s) %s" id (codegen_type typ)
                      (codegen_expr expr))
                  defs)
           ^ ";")
        (if List.length archetype_decls == 0 then ""
         else
           String.concat ";\n"
             (List.map
                (fun (a, id, t) ->
                  match t with
                  | _a, Lambdatype (_params, _retype, _free_vars), _q ->
                      let arch_funtype =
                        codegen_type
                          (add_parameter_to_func void_pointer t
                          |> add_parameter_to_func void_pointer
                          |> func_of_lambda)
                      in
                      let arch_lamtype =
                        codegen_type (add_parameter_to_func void_pointer t)
                      in
                      let lamtype =
                        codegen_type (add_parameter_to_func (self_type name) t)
                      in
                      (* Printf.sprintf
                        "%s    self->%s.%s = (__perkelang_capture_dummy_%s = \
                         self->%s, (%s){__perkelang_capture_dummy_%s.env, \
                         (%s)__perkelang_capture_dummy_%s.func})"
                        indent_string a id lamtype id arch_lamtype lamtype
                      arch_funtype lamtype *)
                      (* TEMPORARILY EXPAND CAST *)
                      Printf.sprintf
                        "%s    self->%s.%s = CAST_LAMBDA(self->%s,\n\
                         %s        %s,\n\
                         %s        %s,\n\
                         %s        %s)"
                        indent_string a id id indent_string lamtype
                        indent_string arch_lamtype indent_string arch_funtype
                  | _ ->
                      Printf.sprintf "%s    self->%s.%s = (%sself->%s"
                        indent_string a id
                        (codegen_type t ^ "*) &")
                        id)
                (List.flatten
                   (List.map
                      (let f (i, h) =
                         Hashtbl.fold (fun k v acc -> (i, k, v) :: acc) h []
                       in
                       f)
                      archetypes)))
           ^ ";")
        (match constructor with
        | None -> ""
        | Some ((constr_type, _constr_id), _constr_expr) -> (
            match discard_type_aq constr_type with
            | Lambdatype _ ->
                (* "(__perkelang_capture_dummy_%s = %s, \
                   __perkelang_capture_dummy_%s.func(&(__perkelang_capture_dummy_%s.env)%s))" *)
                let constr_type_desc =
                  type_descriptor_of_perktype constr_type
                in
                Printf.sprintf
                  "%s    (__perkelang_capture_dummy_%s = self->constructor, \
                   __perkelang_capture_dummy_%s.func(self, \
                   &(__perkelang_capture_dummy_%s.env)%s));\n"
                  indent_string constr_type_desc constr_type_desc
                  constr_type_desc params_str
                |> ignore;
                Printf.sprintf
                  "%s    CALL_LAMBDA(self->constructor, %s, self%s);\n"
                  indent_string constr_type_desc params_str
            | Funtype _ ->
                Printf.sprintf "%s    self->constructor(%s);\n" indent_string
                  params_str
            | _ ->
                failwith
                  "Impossible: constructor is not a functional type, in \
                   codegen call. If you see this error, please open an issue \
                   at https://github.com/Alex23087/Perkelang/issues"))
        indent_string
  | InlineC s -> s
  | Fundef (t, id, args, body) -> indent_string ^ codegen_fundef t id args body
  | Extern _ -> ""
  (* Externs are only useful for type checking. No need to keep it for codegen step *)
  | Def ((t, e), deftype) -> indent_string ^ codegen_def t e deftype
  | Import lib ->
      add_import lib;
      ""
(* with Not_inferred s -> raise_type_error tldf s *)

and codegen_command (cmd : command_a) (indentation : int) : string =
  let indent_string = String.make (4 * indentation) ' ' in
  match ( $ ) cmd with
  | InlineCCmd s -> s
  | Block c ->
      indent_string ^ "{\n"
      ^ codegen_command c (indentation + 1)
      ^ "\n" ^ indent_string ^ "}"
  | DefCmd ((t, e), deftype) -> indent_string ^ codegen_def t e deftype
  | Assign (l, r, lass_type, _rass_type) ->
      (* Printf.printf "Assignment type: %s\n"
         (match lass_type with Some t -> show_perktype t | None -> "None"); *)
      indent_string
      ^ (Printf.sprintf "%s = %s;"
           (match lass_type with
           | Some (_, ArchetypeSum _, _) | Some (_, ArcheType _, _) ->
               Printf.sprintf "%s" (codegen_expr l)
           | _ -> Printf.sprintf "%s" (codegen_expr l)))
          (match _rass_type with
          | Some (_, Optiontype _t, _) ->
              Printf.sprintf "((%s){1, %s})"
                (codegen_type (Option.get _rass_type))
                (codegen_expr r)
          | _ -> codegen_expr r)
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
  | Banish name ->
      (* TODO: Automatically banish children *)
      Printf.sprintf "%sfree(%s);\n%s%s = NULL;" indent_string name
        indent_string name
  | Return None -> indent_string ^ Printf.sprintf "return;"
  | Return (Some e) ->
      indent_string ^ Printf.sprintf "return %s;" (codegen_expr e)

and codegen_def (t : perkvardesc) (e : expr_a) (deftype : perktype option) :
    string =
  let decl_str = codegen_decl t in
  let expr_str = codegen_expr e in
  match deftype with
  | Some (_, Optiontype _, _) ->
      Printf.sprintf "%s = {1, %s};" decl_str expr_str
  | _ -> Printf.sprintf "%s = %s;" decl_str expr_str

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
         (fun (t, id) ->
           Printf.sprintf "%s %s" (codegen_type (resolve_type t)) id)
         args)
  in
  let body_str = codegen_command body 1 in
  let funtype = ([], Funtype (List.map (fun (t, _) -> t) args, t), []) in
  bind_function_type id funtype;
  Printf.sprintf "%s %s(%s) {\n%s\n}" type_str id args_str body_str

(* transforms a perktype into a C type *)
(* TODO figure out details...... *)
and codegen_type ?(expand : bool = false) (t : perktype) : string =
  (* Printf.printf "codegen_type: %s\n" (show_perktype t); flush stdout; *)
  let attrs, t', quals = t in
  let attrs_str = String.concat " " (List.map codegen_attr attrs) in
  let quals_str = String.concat " " (List.map codegen_qual quals) in
  let type_str =
    match t' with
    | Basetype s -> s
    | Structtype s -> "struct " ^ s
    | Funtype _ -> type_descriptor_of_perktype t
    | Lambdatype _ -> type_descriptor_of_perktype t
    | Pointertype ([], Structtype _, _) when expand -> "void*"
    | Pointertype t -> Printf.sprintf "%s*" (codegen_type t ~expand)
    | Arraytype (_at, _n) -> type_descriptor_of_perktype t
    | Vararg -> "..."
    | Modeltype (name, _archs, _decls, _constr_params) ->
        if expand then "void*" else name
    | ArcheType (name, _decls) -> name
    | ArchetypeSum _archs -> type_descriptor_of_perktype t
    | Infer ->
        raise
          (Not_inferred "Impossible: type has not been inferred in codegen_type")
    | Optiontype _t -> type_descriptor_of_perktype t
    | Tupletype _ts -> type_descriptor_of_perktype t
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
  let e' = ( $ ) e in
  match e' with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Char c -> Printf.sprintf "'%c'" c
  | String s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | Pointer e ->
      let code = codegen_expr e in
      Printf.sprintf "*%s" code
  | Var id -> id
  | Apply (e, args, _app_type) -> (
      (* TODO: LAMBDA handle application type *)
      (* Printf.printf "Applying lambda with type %s\n"
         (match _app_type with
         | None -> "None"
         | Some t -> type_descriptor_of_perktype t); *)
      let expr_str = codegen_expr e in
      let args_str = String.concat ", " (List.map codegen_expr args) in
      match _app_type with
      | None -> (
          match ( $ ) e with
          | Access (e1, _, acctype, _) -> (
              let e1_str = codegen_expr e1 in
              let args_str =
                if List.length args = 0 then "" else ", " ^ args_str
              in
              match acctype with
              | Some (_, Modeltype _, _) ->
                  Printf.sprintf "%s(%s%s)" expr_str e1_str args_str
              | Some _t ->
                  Printf.sprintf "%s(%s.self%s)" expr_str e1_str
                    args_str (* this is for archetype sums *)
              | None -> failwith "Impossible: no acctype for access"
              (* this is for models *))
          | _ -> Printf.sprintf "%s(%s)" expr_str args_str)
      | Some lamtype -> (
          match ( $ ) e with
          | Access (e1, _, acctype, _) -> (
              let e1_str = codegen_expr e1 in
              let args_str =
                if List.length args = 0 then "" else ", " ^ args_str
              in
              match Option.map resolve_type acctype with
              (* this is for models *)
              | Some (_, Modeltype (_n, _, _, _), _) ->
                  let lamtype = add_parameter_to_func (self_type _n) lamtype in
                  let lamtype_desc = type_descriptor_of_perktype lamtype in
                  (* Printf.sprintf
                    "(__perkelang_capture_dummy_%s = %s, \
                     __perkelang_capture_dummy_%s.func(&(__perkelang_capture_dummy_%s.env), \
                     %s%s))"
                    lamtype_desc expr_str lamtype_desc lamtype_desc e1_str
                    args_str *)
                  let args_str = e1_str ^ args_str in
                  Printf.sprintf "CALL_LAMBDA(%s, %s, %s)" expr_str lamtype_desc
                    args_str
              | Some (_, ArcheType (_name, _), _) ->
                  let lamtype = add_parameter_to_func void_pointer lamtype in
                  let lamtype_desc = type_descriptor_of_perktype lamtype in
                  (* Printf.sprintf
                    "(__perkelang_capture_dummy_%s = %s, \
                     __perkelang_capture_dummy_%s.func(&(__perkelang_capture_dummy_%s.env), \
                     %s.self%s))"
                    lamtype_desc expr_str lamtype_desc lamtype_desc e1_str
                    args_str  *)
                  let args_str = e1_str ^ ".self" ^ args_str in
                  Printf.sprintf "CALL_LAMBDA(%s, %s, %s)" expr_str lamtype_desc
                    args_str
                  (* this is for archetype sums *)
              | Some t ->
                  failwith
                    (Printf.sprintf
                       "Impossible: no acctype for access: got type %s"
                       (show_perktype t))
              | None -> failwith "Impossible: no acctype for access: got None")
          | _ ->
              let args_str =
                if List.length args = 0 then "" else ", " ^ args_str
              in
              (* Printf.sprintf
                "(__perkelang_capture_dummy_%s = %s, \
                 __perkelang_capture_dummy_%s.func(&(__perkelang_capture_dummy_%s.env)%s))"
                (type_descriptor_of_perktype lamtype)
                expr_str
                (type_descriptor_of_perktype lamtype)
                (type_descriptor_of_perktype lamtype)
                args_str *)
              if List.length args = 0 then
                Printf.sprintf "CALL_LAMBDA0(%s, %s)" expr_str
                  (type_descriptor_of_perktype lamtype)
              else
                Printf.sprintf "CALL_LAMBDA(%s, %s%s)" expr_str
                  (type_descriptor_of_perktype lamtype)
                  args_str))
  | Access (e1, ide, acctype, rightype) -> (
      match Option.map resolve_type acctype with
      | Some (_, Modeltype _, _) ->
          Printf.sprintf "%s->%s" (codegen_expr e1) ide
      | Some t -> (
          match Option.map discard_type_aq rightype with
          | Some (Lambdatype _) | Some (Funtype _) ->
              Printf.sprintf "%s.%s.%s" (codegen_expr e1)
                (type_descriptor_of_perktype t)
                ide
          | Some _ | None ->
              Printf.sprintf "(*(%s.%s.%s))" (codegen_expr e1)
                (type_descriptor_of_perktype t)
                ide)
      | None -> failwith "Impossible: no acctype for access")
  | Binop (op, e1, e2) ->
      Printf.sprintf "%s %s %s" (codegen_expr e1) (codegen_binop op)
        (codegen_expr e2)
  | PreUnop (op, e) ->
      Printf.sprintf "%s%s" (codegen_preunop op) (codegen_expr e)
  | PostUnop (op, e) -> (
      match op with
      | OptionGet (Some t) ->
          Printf.sprintf "((%s)%s%s)"
            (type_descriptor_of_perktype t)
            (codegen_expr e) (codegen_postunop op)
      | OptionGet None -> raise_type_error e "Option get type was not inferred"
      | _ -> Printf.sprintf "%s%s" (codegen_expr e) (codegen_postunop op))
  | Parenthesised e -> Printf.sprintf "(%s)" (codegen_expr e)
  | Lambda _ -> codegen_lambda e
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
  | As (id, typlist) ->
      (* let _ = get_tuple  *)
      let sum_type_descr = codegen_type ([], ArchetypeSum typlist, []) in
      Printf.sprintf "((%s) {%s%s})" sum_type_descr
        (if List.length typlist = 0 then ""
         else
           String.concat ", "
             (List.map
                (fun t ->
                  Printf.sprintf "%s->%s" id (codegen_type t ~expand:true))
                typlist)
           ^ ", ")
        id
  | Nothing t -> (
      match t with
      | _, Infer, _ ->
          failwith "Impossible: type for nothing has not been inferred"
      | t -> Printf.sprintf "((%s) {0, 0})" (type_descriptor_of_perktype t))
  | Something (e, t) -> (
      match t with
      | _, Infer, _ ->
          failwith "Impossible: type for something has not been inferred"
      | t ->
          Printf.sprintf "((%s) {1, %s})"
            (type_descriptor_of_perktype ([], Optiontype t, []))
            (codegen_expr e))
  | Array es ->
      Printf.sprintf "{%s}"
        (String.concat ", "
           (List.map (fun e -> Printf.sprintf "%s" (codegen_expr e)) es))
  | Cast ((from_t, to_t), e) -> (
      match (from_t, to_t) with
      | (_, Funtype _, _), (_, Lambdatype _, _) ->
          Printf.sprintf "((%s) {{}, %s})"
            (type_descriptor_of_perktype to_t)
            (codegen_expr e)
      | _ ->
          Printf.sprintf "((%s)%s)"
            (type_descriptor_of_perktype to_t)
            (codegen_expr e))

(* struct {int is_empty; int value;} palle = {0,1}; *)

(* generates code for binary operators *)
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
  | Land -> "&&"
  | Lor -> "||"
  | Neq -> "!="

(* generates code for prefix unary operators *)
and codegen_preunop (op : preunop) : string =
  match op with
  | Neg -> "-"
  | Not -> "!"
  | Dereference -> "*"
  | Reference -> "&"
  | PreIncrement -> "++"
  | PreDecrement -> "--"

(* generates code for postfix unary operators *)
and codegen_postunop (op : postunop) : string =
  match op with
  | PostIncrement -> "++"
  | PostDecrement -> "--"
  | OptionIsSome -> ".is_some"
  | OptionGet _ -> ".contents"

(* generates code for function declarations *)
and codegen_fundecl (id : perkident) (typ : perktype) : string =
  let attrs, t, _ = typ in
  let attrs_str = String.concat " " (List.map codegen_attr attrs) in
  let type_str =
    match t with
    | Funtype (args, ret) | Lambdatype (args, ret, _) ->
        codegen_type ret ^ " " ^ id ^ " ("
        ^ String.concat ", " (List.map codegen_type args)
        ^ ")"
    | _ -> failwith "codegen_fundecl: called with a non function type"
  in
  if attrs_str = "" then type_str else Printf.sprintf "%s %s" attrs_str type_str

and generate_types () =
  let out = ref "" in
  let ft_list =
    ref
      (Hashtbl.fold
         (fun k (typ, code) acc ->
           (k, (typ, code, dependencies_of_type typ)) :: acc)
         type_symbol_table [])
  in
  let sort_based_on_deps_count (_, (_, _, d_a)) (_, (_, _, d_b)) =
    compare (List.length d_a) (List.length d_b)
  in
  ft_list := List.sort sort_based_on_deps_count !ft_list;
  (* List.iter
    (fun (id, (_, _, deps)) ->
      Printf.printf "Type: %s, Dependencies: [%s]\n" id
        (String.concat ", " deps))
    !ft_list; *)
  while List.length !ft_list > 0 do
    (* say_here "generate_types"; *)
    let _id, (_typ, _code, _deps) = List.hd !ft_list in
    (* if List.length _deps > 0 then *)
    (* Printf.printf "%s\n"
       (Printf.sprintf "Warning: Type %s depends on %s, not yet generated\n"
          (List.hd (List.map fst !ft_list))
          (List.hd
             (List.map (fun (_, (_, _, d)) -> String.concat ", " d) !ft_list))); *)
    let _code = Some (codegen_type_definition _typ) in
    ft_list := List.tl !ft_list;
    (* Remove dzpendency from other elements *)
    ft_list :=
      List.sort sort_based_on_deps_count
        (List.map
           (fun (_i, (_typ, _code, _deps)) ->
             (_i, (_typ, _code, List.filter (fun id -> id <> _id) _deps)))
           !ft_list);
    match _code with
    | Some c -> out := Printf.sprintf "%s%s\n" !out c
    | None -> ()
  done;
  !out

and codegen_type_definition (t : perktype) : string =
  let key = type_descriptor_of_perktype t in
  let _t, _code = Hashtbl.find type_symbol_table key in
  match _code with
  (* Some types (e.g., Models, Archetypes) will have code already generated *)
  | Some c -> c
  | None -> (
      say_here (Printf.sprintf "codegen_type_definition: %s\n" key);
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
          Hashtbl.replace type_symbol_table key (_t, Some compiled);
          compiled
      | _, Optiontype u, _ ->
          let u = resolve_type u in
          let compiled =
            Printf.sprintf "typedef struct %s {int is_some; %s contents;} %s;"
              key
              (match u with
              | _, Modeltype _, _ -> "void*"
              | _ -> type_descriptor_of_perktype u)
              key
          in
          Hashtbl.replace type_symbol_table key (_t, Some compiled);
          compiled
      | _, Funtype (args, ret), _ ->
          let type_str = type_descriptor_of_perktype t in
          let typedef_str =
            Printf.sprintf "typedef %s"
              (codegen_type ret ~expand:true
              ^ " (*" ^ type_str ^ ")("
              ^ String.concat ", "
                  (List.map (fun t -> codegen_type t ~expand:true) args)
              ^ ");")
          in
          Hashtbl.replace type_symbol_table key (t, Some typedef_str);
          (* Hashtbl.add function_type_hashmap t (type_str, typedef_str, expanded_str); *)
          typedef_str
      | _, Pointertype t, _ ->
          Printf.sprintf "typedef %s* %s;" (codegen_type t ~expand:true) key
      | _, ArchetypeSum archs, _ ->
          let compiled =
            Printf.sprintf "struct %s {%svoid* self;};\ntypedef struct %s %s;"
              (type_descriptor_of_perktype t)
              (if List.length archs = 0 then ""
               else
                 String.concat "; "
                   (List.map
                      (fun t ->
                        Printf.sprintf "%s %s"
                          (type_descriptor_of_perktype t)
                          (type_descriptor_of_perktype t))
                      archs)
                 ^ "; ")
              (type_descriptor_of_perktype t)
              (type_descriptor_of_perktype t)
          in
          Hashtbl.replace type_symbol_table key (_t, Some compiled);
          compiled
      | _, Arraytype (at, n), _ ->
          let compiled =
            match n with
            | Some n ->
                Printf.sprintf "typedef %s %s[%d];"
                  (type_descriptor_of_perktype at)
                  key n
            | None ->
                Printf.sprintf "typedef %s %s[];"
                  (type_descriptor_of_perktype at)
                  key
          in
          Hashtbl.replace type_symbol_table key (_t, Some compiled);
          compiled
      | _, Lambdatype _, _ -> codegen_lambda_capture t
      | _ ->
          raise_type_error
            (annotate_dummy ([], Int (-1), []))
            (Printf.sprintf
               "Unexpected type generation request: got %s. If you see this \
                error, please file an issue at https://github.com/"
               (type_descriptor_of_perktype t)))

and codegen_lambda_environment (_free_vars : perkvardesc list) : string * string
    =
  let free_vars = List.map swizzle !all_vars in
  let environment_type_desc = type_descriptor_of_environment free_vars in
  let tmp_typedef =
    List.find_opt (fun (t, _) -> t = environment_type_desc) !lambda_environments
  in
  match tmp_typedef with
  | Some typedef -> (fst typedef, "")
  | None ->
      let environment_typedef =
        Printf.sprintf "typedef struct %s {%s;} %s;" environment_type_desc
          (String.concat "; "
             (List.mapi
                (fun i (_typ, _id) -> Printf.sprintf "void* _%d" i)
                  (* Printf.sprintf "%s _%d" (type_descriptor_of_perktype typ) i) *)
                free_vars))
          environment_type_desc
      in
      lambda_environments :=
        (environment_type_desc, environment_typedef) :: !lambda_environments;
      (environment_type_desc, environment_typedef)

and codegen_lambda_capture (lamtype : perktype) : string =
  match lamtype with
  | [], Lambdatype (_retype, _args, free_vars), [] -> (
      let lambda_type_desc =
        type_descriptor_of_perktype (func_of_lambda_void lamtype)
      in
      let environment_type_desc, environment_typedef =
        codegen_lambda_environment free_vars
      in
      let capture_type_desc = lambda_type_desc ^ "_" ^ environment_type_desc in
      let tmp_typedef =
        List.find_opt
          (fun (t, _) -> t = capture_type_desc)
          !lambda_capture_dummies
      in
      match tmp_typedef with
      | Some str -> snd str
      | None ->
          let typedef =
            Printf.sprintf
              "typedef struct %s {\n    struct %s env;\n    %s func;\n} %s"
              capture_type_desc environment_type_desc lambda_type_desc
              capture_type_desc
          in
          let dummy =
            Printf.sprintf "struct %s __perkelang_capture_dummy_%s"
              capture_type_desc capture_type_desc
          in
          let alltogether =
            Printf.sprintf "%s\n%s;\n%s;" environment_typedef typedef dummy
          in
          (* Printf.printf "%s\n\n\n\n\n\n" alltogether; *)
          lambda_capture_dummies :=
            (capture_type_desc, alltogether) :: !lambda_capture_dummies;
          alltogether)
  | _ ->
      failwith
        "Impossible: codegen_lambda_capture: not a lambda expression. If you \
         see this error please file an issue at \
         https://github.com/Alex23087/Perkelang/issues"
