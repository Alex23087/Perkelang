type perkident = string
[@@deriving show]

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
type perktype = Basetype of string | Funtype of (perktype_complete list) * perktype_complete | Pointertype of perktype_complete | Arraytype of perktype_complete * (int option) | Classtype of string | Structtype of string
[@@deriving show]

and perktype_complete = perktype_attribute list * perktype * perktype_qualifier list
[@@deriving show]

type perkvardesc = perktype_complete * perkident
[@@deriving show]

type perkdecl = perkvardesc
[@@deriving show]

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

type preunop = Neg | Not | Dereference | Reference | PreIncrement | PreDecrement [@@deriving show]

type postunop = PostIncrement | PostDecrement [@@deriving show]

type perkdef = perkdecl * expr [@@deriving show]
  
(* return, name, args, body *)
and perkfun = Fun of perktype_complete * perkident * (perkvardesc list) * command [@@deriving show]

(* name, attributes, methods *)
(* and perklass = Class of perkident * (perkdef list) * (perkfun list) [@@deriving show] *)

and expr = 
  | Int of int
  | Char of char
  | String of string
  | Pointer of expr
  | Var of perkident
    (* classname, identifier *)
  (* | Ob of string * int *)
  | Apply of expr * (expr list)
  | Binop of binop * expr * expr
  | PreUnop of preunop * expr
  | Lambda of perktype_complete * (perkvardesc list) * command
  | PostUnop of postunop * expr
  | Parenthesised of expr
  | Subscript of expr * expr
  [@@deriving show]
  
(* Syntax of the language *)
and command =
  | Import of string
  | InlineC of string
  | Block of command
  | Def of perkdef
  | Fundef of perkfun
  (* | Classdecl of perklass *)
  | Assign of (expr * expr)
  | Seq of command * command
  | IfThenElse of expr * command * command
  | Whiledo of expr * command
  | Dowhile of expr * command
  | For of command * expr * command * command
  | Expr of expr
  | Switch of expr * (expr * command) list
  | Skip
  | Archetype of perkident * perkdecl list
  | Model of perkident * perkident list * perkdef list
  | Summon of perkident * perkident * expr list
  | Return of expr
  [@@deriving show]
