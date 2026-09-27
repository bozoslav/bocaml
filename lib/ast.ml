type binop =
  | Add
  | Sub
  | Mul
  | Div
  | ShiftLeft
  | ShiftRight
  | Lt
  | Gt
  | LtEq
  | GtEq
  | Equal
  | NotEqual
  | BitAnd
  | BitOr
  | Mod

type expr =
  | Int of int
  | Read of lvalue
  | Binop of binop * expr * expr
  | Neg of expr
  | Conditional of expr * expr * expr
  | Address of lvalue
  | Assign of lvalue * expr
  | Call of expr * expr list
  | Not of expr
  | PreIncrement of lvalue
  | PreDecrement of lvalue
  | PostIncrement of lvalue
  | PostDecrement of lvalue

and lvalue =
  | Variable of string
  | Dereference of expr
  | Subscript of expr * expr

type stmt =
  | Null
  | Expression of expr
  | Return of expr option
  | Block of stmt list
  | If of expr * stmt * stmt option
  | While of expr * stmt

type func = { name : string; params : string list; body : stmt }
type program = func list
