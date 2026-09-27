type instruction = Push_int of int | Binary of Ast.binop
type t = instruction list
