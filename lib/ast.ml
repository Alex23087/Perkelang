type 'a annotated = {
  loc : Location.location; [@opaque]
  node : 'a;
}
[@@deriving show]

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

type perkident = string [@@deriving show]

type perktype_attribute =
  | Public
  | Private
  | Static
  | Extern
[@@deriving show]

type perktype_qualifier =
  | Const
  | Volatile
  | Restrict
[@@deriving show]

(* type of the perk -- giangpt *)
type perktype_partial =
  | Basetype of string
  | Funtype of perktype list * perktype
  | Pointertype of perktype
  | Arraytype of perktype * int option
  | Classtype of string
  | Structtype of string
[@@deriving show]

and perktype =
  perktype_attribute list * perktype_partial * perktype_qualifier list
[@@deriving show]

(* and perktype_annotated = perktype annotated [@@deriving show] *)

type perkvardesc = perktype * perkident [@@deriving show]
type perkdecl = perkvardesc [@@deriving show]

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
  | Dot
(*  ... boolean and bitwise ops and all that  *)
[@@deriving show]

type preunop =
  | Neg
  | Not
  | Dereference
  | Reference
  | PreIncrement
  | PreDecrement
[@@deriving show]

type postunop =
  | PostIncrement
  | PostDecrement
[@@deriving show]

type perkdef = perkdecl * expr_a [@@deriving show]

(* name, attributes, methods *)
(* and perklass = Class of perkident * (perkdef list) * (perkfun list) [@@deriving show] *)
and expr_t =
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Pointer of expr_a
  | Var of perkident
  (* classname, identifier *)
  (* | Ob of string * int *)
  | Apply of expr_a * expr_a list
  | Binop of binop * expr_a * expr_a
  | PreUnop of preunop * expr_a
  | Lambda of perktype * perkvardesc list * command_a
  | PostUnop of postunop * expr_a
  | Parenthesised of expr_a
  | Subscript of expr_a * expr_a
[@@deriving show]

(* Syntax of the language *)
and command_t =
  | Import of string
  | InlineC of string
  | Block of command_a
  | Def of perkdef
  | Fundef of
      perktype
      * perkident
      * perkvardesc list
      * command_a (* return, name, args, body *)
  (* | Classdecl of perklass *)
  | Assign of (expr_a * expr_a)
  | Seq of command_a * command_a
  | IfThenElse of expr_a * command_a * command_a
  | Whiledo of expr_a * command_a
  | Dowhile of expr_a * command_a
  | For of command_a * expr_a * command_a * command_a
  | Expr of expr_a
  | Switch of expr_a * (expr_a * command_a) list
  | Skip
  | Archetype of perkident * perkdecl list
  | Model of perkident * perkident list * perkdef list
  | Summon of perkident * perkident * expr_a list
  | Return of expr_a
[@@deriving show]

and expr_a = expr_t annotated [@@deriving show]
and command_a = command_t annotated [@@deriving show]
