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

and lvalue =
  | Variable of string
  | Dereference of expr
  | Subscript of expr * expr
