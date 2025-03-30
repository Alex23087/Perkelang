type perkident = string
[@@deriving show]

(* type of the perk -- giangpt *)
type perktype = Basetype of string | Funtype of (perktype list) * perktype | Pointertype of perktype | Arraytype of perktype * (int option) | Classtype of string
[@@deriving show]

type perkvardesc = perktype * perkident
[@@deriving show]

type perkdecl = perkvardesc
[@@deriving show]

type binop =
  | Add
  | Sub
  | Eq
  | Lt
  | Leq
  | Gt
  | Geq
(*  ... boolean and bitwise ops and all that  *)
[@@deriving show]

type unop = Neg | Not (*....*) [@@deriving show]

type perkdef = perkdecl * expr [@@deriving show]
  
(* return, name, args, body *)
and perkfun = Fun of perktype * perkident * (perkvardesc list) * command [@@deriving show]

(* name, attributes, methods *)
(* and perklass = Class of perkident * (perkdef list) * (perkfun list) [@@deriving show] *)

and expr = 
  | Int of int
  | Pointer of expr
  | Var of perkident
    (* classname, identifier *)
  (* | Ob of string * int *)
  | Apply of expr * (expr list)
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Lambda of perktype * (perkvardesc list) * command
  [@@deriving show]
  
(* Syntax of the language *)
and command =
  | InlineC of string
  | Block of command
  | Def of perkdef
  | Fundef of perkfun
  (* | Classdecl of perklass *)
  | Assign of (string * expr)
  | Seq of command * command
  | IfThenElse of expr * command * command
  | Whiledo of expr * command
  | Dowhile of expr * command
  | For of expr * expr * command * command
  | Expr of expr
  | Switch of expr * (expr * command) list
  | Skip
  | Return of expr
  [@@deriving show]