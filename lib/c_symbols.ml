open Frontc

let perktype_of_csymbol (s : Cabs.definition) = match s with
  | FUNDEF ((_ctyp, _storage, _name), _) -> ()
  | _ -> failwith ""

let get_symbols s = 
  let parsed = parse  [FROM_FILE s] in
  match parsed with
    | PARSING_OK deflist -> deflist
    | PARSING_ERROR -> raise (Errors.CParsingError (Printf.sprintf "Could not parse C file %s" s))

(* TODO! Make this more general*)
let get_lib_symbols s = 
  let remove_first_last s =
    let len = String.length s in
    if len <= 2 then
      ""
    else
      String.sub s 1 (len - 2) in
  let s = Printf.sprintf "/usr/include/%s" (remove_first_last s) in
  get_symbols s

let rec string_of_ctype (ct : Cabs.base_type) = match ct with 
| NO_TYPE -> "/*no type*/"
| VOID -> "void"
| BOOL -> "_Bool"
| CHAR _ -> "<sign>char"
| INT (_, _) -> "<sign><size>int"
| BITFIELD (_, _) -> "<sign>bitfield(<expr>)"
| FLOAT false -> "float"
| FLOAT true  -> "long float"
| DOUBLE false -> "double"
| DOUBLE true  -> "long double"
| COMPLEX_FLOAT       -> "float _Complex"
| COMPLEX_DOUBLE      -> "double _Complex"
| COMPLEX_LONG_DOUBLE -> "long double _Complex"
| PTR t ->
  string_of_ctype t ^ " *"
| RESTRICT_PTR t ->
  string_of_ctype t ^ " * restrict"
| ARRAY (t, _) ->
  string_of_ctype t ^ "[<expr>]"
| STRUCT (name, _) ->
    "struct " ^ name ^ " { <fields> }"
| UNION (name, _) ->
    "union " ^ name ^ " { <fields> }"
| PROTO _ ->
    "<proto>"
| OLD_PROTO _ ->
    "<old_proto>"
| NAMED_TYPE name ->
    name
| ENUM (name, _) ->
    "enum " ^ name ^ " { <items> }"
| CONST t ->
    "const " ^ string_of_ctype t
| VOLATILE t ->
    "volatile " ^ string_of_ctype t
| GNU_TYPE (_, t) ->
  string_of_ctype t ^ " <gnu_attrs>"
| BUILTIN_TYPE s ->
    s
| TYPE_LINE (file, line, t) ->
  string_of_ctype t ^ " /* " ^ file ^ ":" ^ string_of_int line ^ " */"

  let string_of_name_group ((bt, _stor, names) : Cabs.name_group) : string =
    let names_str =
      names
      |> List.map (fun (ident, ctype, _attrs, _expr) ->
           ident
           ^ ": " ^ string_of_ctype ctype
           ^ " <gnu_attrs> = <expr>")
      |> String.concat "; "
    in
    "("
    ^ string_of_ctype bt
    ^ " <storage>"
    ^ " [" ^ names_str ^ "]"
    ^ ")"
  
  let string_of_definition : Cabs.definition -> string = function
    | FUNDEF (_fn, _body) ->
        "FUNDEF(<single_name>, <body>)"
    | OLDFUNDEF (_fn, params, _body) ->
        let params_str =
          params
          |> List.map string_of_name_group
          |> String.concat "; "
        in
        "OLDFUNDEF(<single_name>, [" ^ params_str ^ "], <body>)"
    | DECDEF ng ->
        "DECDEF(" ^ string_of_name_group ng ^ ")"
    | TYPEDEF (ng, _attrs) ->
        "TYPEDEF(" ^ string_of_name_group ng ^ ", <gnu_attrs>)"
    | ONLYTYPEDEF ng ->
        "ONLYTYPEDEF(" ^ string_of_name_group ng ^ ")"


let string_of_symbols sl = String.concat ",\n" (List.map string_of_definition sl)