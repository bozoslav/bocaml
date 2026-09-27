exception Unsupported_expression

let rec compile_expr = function
  | Ast.Int n -> [ Bytecode.Push_int n ]
  | Ast.Binop (op, left, right) ->
      compile_expr left @ compile_expr right @ [ Bytecode.Binary op ]
  | _ -> raise Unsupported_expression
