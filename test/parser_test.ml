open Bocaml

let parse source = source |> Lexer.lex |> Parser.parse
let test_integer_expression () = assert (parse "42" = Ast.Int 42)

let test_multiplication_precedence () =
  assert (
    parse "2 + 3 * 4"
    = Ast.Binop (Ast.Add, Ast.Int 2, Ast.Binop (Ast.Mul, Ast.Int 3, Ast.Int 4)))

let test_parentheses_override_precedence () =
  assert (
    parse "(2 + 3) * 4"
    = Ast.Binop (Ast.Mul, Ast.Binop (Ast.Add, Ast.Int 2, Ast.Int 3), Ast.Int 4))

let test_operators_are_left_associative () =
  assert (
    parse "10 - 2 - 3"
    = Ast.Binop (Ast.Sub, Ast.Binop (Ast.Sub, Ast.Int 10, Ast.Int 2), Ast.Int 3))

let expect_parser_error source =
  match parse source with
  | _ -> assert false
  | exception Parser.Parser_error _ -> ()

let test_invalid_expressions () =
  expect_parser_error "";
  expect_parser_error "2 +";
  expect_parser_error "2 3";
  expect_parser_error "(2 + 3"

let () =
  test_integer_expression ();
  test_multiplication_precedence ();
  test_parentheses_override_precedence ();
  test_operators_are_left_associative ();
  test_invalid_expressions ();
  print_endline "Parser tests passed"
