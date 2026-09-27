open Bocaml

let compile source = source |> Lexer.lex |> Parser.parse |> Codegen.compile_expr
let test_integer_literal () = assert (compile "42" = [ Bytecode.Push_int 42 ])

let test_operator_order_respects_precedence () =
  assert (
    compile "2 + 3 * 4"
    = [
        Bytecode.Push_int 2;
        Bytecode.Push_int 3;
        Bytecode.Push_int 4;
        Bytecode.Binary Ast.Mul;
        Bytecode.Binary Ast.Add;
      ])

let test_left_associative_operators () =
  assert (
    compile "10 - 3 - 2"
    = [
        Bytecode.Push_int 10;
        Bytecode.Push_int 3;
        Bytecode.Binary Ast.Sub;
        Bytecode.Push_int 2;
        Bytecode.Binary Ast.Sub;
      ])

let test_unsupported_expression () =
  try
    ignore (compile "value");
    failwith "Expected variable expression code generation to be unsupported"
  with Codegen.Unsupported_expression -> ()

let () =
  test_integer_literal ();
  test_operator_order_respects_precedence ();
  test_left_associative_operators ();
  test_unsupported_expression ();
  print_endline "Codegen tests passed"
