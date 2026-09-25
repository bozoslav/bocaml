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
  | Binop of binop * expr * expr
  | Neg of expr
  | Conditional of expr * expr * expr
