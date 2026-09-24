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

type expr = Int of int | Binop of binop * expr * expr | Neg of expr
