type result = { operand : string; instructions : string list; next_temp : int }

exception Unsupported_expression of Ast.expr
exception Unsupported_operator of Ast.binop

let llvm_operator = function
  | Ast.Add -> "add"
  | Ast.Sub -> "sub"
  | Ast.Mul -> "mul"
  | Ast.Div -> "sdiv"
  | Ast.Mod -> "srem"
  | op -> raise (Unsupported_operator op)

let rec compile_node expression next_temp =
  match expression with
  | Ast.Int n -> { operand = string_of_int n; instructions = []; next_temp }
  | Ast.Binop (op, left, right) ->
      let left_result = compile_node left next_temp in
      let right_result = compile_node right left_result.next_temp in
      let temp = Printf.sprintf "%%t%d" right_result.next_temp in
      let instruction =
        Printf.sprintf "  %s = %s i64 %s, %s" temp (llvm_operator op)
          left_result.operand right_result.operand
      in
      {
        operand = temp;
        instructions =
          left_result.instructions @ right_result.instructions @ [ instruction ];
        next_temp = right_result.next_temp + 1;
      }
  | _ -> raise (Unsupported_expression expression)

let compile_expr expression =
  let result = compile_node expression 0 in
  String.concat "\n"
    ([ "define i64 @main() {"; "entry:" ]
    @ result.instructions
    @ [ "  ret i64 " ^ result.operand; "}" ])
