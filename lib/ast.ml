type 'a annotated = {
  loc : Location.location; [@opaque]
  node : 'a;
}
[@@deriving show, eq]

let ( @@ ) (annotated_node : 'a annotated) : Location.location =
  annotated_node.loc

let ( $ ) (annotated_node : 'a annotated) : 'a = annotated_node.node

let annotate (node : 'a) (loc : Location.location) : 'a annotated =
  { loc; node }

let annotate_2 (loc : Location.location) (node : 'a) = { loc; node }

let annotate_code (node : 'a) (start_pos, end_pos) : 'a annotated =
  let loc = Location.to_code_position (start_pos, end_pos) in
  { loc; node }

let annotate_2_code (start_pos, end_pos) (node : 'a) : 'a annotated =
  let loc = Location.to_code_position (start_pos, end_pos) in
  { loc; node }

let annotate_dummy (node : 'a) : 'a annotated =
  { loc = Location.dummy_pos; node }

let annot_copy (annotated_node : 'a annotated) (node : 'a) : 'a annotated =
  { loc = annotated_node.loc; node }

type perkident = string [@@deriving show, eq]

type perktype_attribute =
  | Public
  | Private
  | Static
[@@deriving show, eq]

type perktype_qualifier =
  | Const
  | Volatile
  | Restrict
[@@deriving show, eq]

(* type of the perk -- giangpt *)
type perktype_partial =
  | Basetype of string
  | Funtype of perktype list * perktype
  | Pointertype of perktype
  | Arraytype of perktype * int option
  | Structtype of string
  | ArcheType of perkident * perkdecl list
  | Modeltype of perkident * perkident list * perkdecl list * perktype list
  | Optiontype of perktype
  | Tupletype of perktype list
  | ArchetypeSum of perktype list
  | Vararg
  | Infer
[@@deriving show, eq]

and perktype =
  perktype_attribute list * perktype_partial * perktype_qualifier list
[@@deriving show, eq]

(* and perktype_annotated = perktype annotated [@@deriving show, eq] *)
and perkvardesc = perktype * perkident [@@deriving show, eq]
and perkdecl = perkvardesc [@@deriving show, eq]

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
  | Leq
  | Gt
  | Geq
  | Neq
  | Land
  | Lor
(*  ... boolean and bitwise ops and all that  *)
[@@deriving show, eq]

type preunop =
  | Neg
  | Not
  | Dereference
  | Reference
  | PreIncrement
  | PreDecrement
[@@deriving show, eq]

type postunop =
  | PostIncrement
  | PostDecrement
[@@deriving show, eq]

type perkdef = perkdecl * expr_a [@@deriving show, eq]

(* name, attributes, methods *)
(* and perklass = Class of perkident * (perkdef list) * (perkfun list) [@@deriving show, eq] *)
and expr_t =
  | Nothing of perktype
  | Something of expr_a * perktype
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Pointer of expr_a
  | Var of perkident
  | Apply of expr_a * expr_a list
  | Binop of binop * expr_a * expr_a
  | PreUnop of preunop * expr_a
  | Lambda of perktype * perkvardesc list * command_a
  | PostUnop of postunop * expr_a
  | Parenthesised of expr_a
  | Subscript of expr_a * expr_a
  | TupleSubscript of expr_a * int
  | Summon of perkident * expr_a list
  | Access of expr_a * perkident * perktype option
  | Tuple of expr_a list * perktype option
  | As of perkident * perktype list
  | Array of expr_a list
[@@deriving show, eq]

(* Syntax of the language *)
and command_t =
  | InlineCCmd of string
  | DefCmd of perkdef * perktype option
  | Block of command_a
  | Assign of (expr_a * expr_a * perktype option * perktype option)
  | Seq of command_a * command_a
  | IfThenElse of expr_a * command_a * command_a
  | Whiledo of expr_a * command_a
  | Dowhile of expr_a * command_a
  | For of command_a * expr_a * command_a * command_a
  | Expr of expr_a
  | Switch of expr_a * (expr_a * command_a) list
  | Skip
  | Banish of perkident
  | Return of expr_a
[@@deriving show, eq]

and topleveldef_t =
  | InlineC of string
  | Import of string
  | Extern of perkident * perktype
  | Def of perkdef * perktype option
  | Fundef of
      perktype
      * perkident
      * perkvardesc list
      * command_a (* return, name, args, body *)
  | Archetype of perkident * perkdecl list
  | Model of perkident * perkident list * perkdef list

and expr_a = expr_t annotated [@@deriving show, eq]
and command_a = command_t annotated [@@deriving show, eq]
and topleveldef_a = topleveldef_t annotated [@@deriving show, eq]

let say_here (_msg : string) : unit =
  (* Printf.printf "%s\n" msg;
  flush stdout *)
  ()
