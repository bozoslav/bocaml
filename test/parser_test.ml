open Bocaml

let parse source = source |> Lexer.lex |> Parser.parse
let parse_statement source = source |> Lexer.lex |> Parser.parse_statement
let parse_program source = source |> Lexer.lex |> Parser.parse_program
let test_integer_expression () = assert (parse "42" = Ast.Int 42)

let test_variable_expression () =
  assert (parse "count" = Ast.Read (Ast.Variable "count"));
  assert (
    parse "count + 1"
    = Ast.Binop (Ast.Add, Ast.Read (Ast.Variable "count"), Ast.Int 1))

let test_dereference_expression () =
  assert (
    parse "*ptr" = Ast.Read (Ast.Dereference (Ast.Read (Ast.Variable "ptr"))))

let test_address_of_expression () =
  assert (parse "&count" = Ast.Address (Ast.Variable "count"));
  assert (
    parse "&*ptr"
    = Ast.Address (Ast.Dereference (Ast.Read (Ast.Variable "ptr"))))

let test_array_subscript () =
  assert (
    parse "items[2]"
    = Ast.Read (Ast.Subscript (Ast.Read (Ast.Variable "items"), Ast.Int 2)));
  assert (
    parse "items[i + 1]"
    = Ast.Read
        (Ast.Subscript
           ( Ast.Read (Ast.Variable "items"),
             Ast.Binop (Ast.Add, Ast.Read (Ast.Variable "i"), Ast.Int 1) )));
  assert (
    parse "items[2][3]"
    = Ast.Read
        (Ast.Subscript
           ( Ast.Read
               (Ast.Subscript (Ast.Read (Ast.Variable "items"), Ast.Int 2)),
             Ast.Int 3 )))

let test_assignment_expression () =
  assert (parse "count = 7" = Ast.Assign (Ast.Variable "count", Ast.Int 7));
  assert (
    parse "count = other = 7"
    = Ast.Assign
        (Ast.Variable "count", Ast.Assign (Ast.Variable "other", Ast.Int 7)));
  assert (
    parse "items[i] = count + 1"
    = Ast.Assign
        ( Ast.Subscript
            (Ast.Read (Ast.Variable "items"), Ast.Read (Ast.Variable "i")),
          Ast.Binop (Ast.Add, Ast.Read (Ast.Variable "count"), Ast.Int 1) ))

let test_function_call_expression () =
  assert (parse "foo()" = Ast.Call (Ast.Read (Ast.Variable "foo"), []));
  assert (
    parse "foo(count, 2 + 3)"
    = Ast.Call
        ( Ast.Read (Ast.Variable "foo"),
          [
            Ast.Read (Ast.Variable "count");
            Ast.Binop (Ast.Add, Ast.Int 2, Ast.Int 3);
          ] ));
  assert (
    parse "foo(1)[i]"
    = Ast.Read
        (Ast.Subscript
           ( Ast.Call (Ast.Read (Ast.Variable "foo"), [ Ast.Int 1 ]),
             Ast.Read (Ast.Variable "i") )))

let test_logical_not () =
  assert (parse "!0" = Ast.Not (Ast.Int 0));
  assert (parse "!!count" = Ast.Not (Ast.Not (Ast.Read (Ast.Variable "count"))))

let test_increment_decrement () =
  assert (parse "++count" = Ast.PreIncrement (Ast.Variable "count"));
  assert (parse "--count" = Ast.PreDecrement (Ast.Variable "count"));
  assert (parse "count++" = Ast.PostIncrement (Ast.Variable "count"));
  assert (parse "count--" = Ast.PostDecrement (Ast.Variable "count"));
  assert (
    parse "--items[i]"
    = Ast.PreDecrement
        (Ast.Subscript
           (Ast.Read (Ast.Variable "items"), Ast.Read (Ast.Variable "i"))));
  assert (
    parse "items[i]++"
    = Ast.PostIncrement
        (Ast.Subscript
           (Ast.Read (Ast.Variable "items"), Ast.Read (Ast.Variable "i"))))

let test_multiplication_precedence () =
  assert (
    parse "2 + 3 * 4"
    = Ast.Binop (Ast.Add, Ast.Int 2, Ast.Binop (Ast.Mul, Ast.Int 3, Ast.Int 4)))

let test_modulo_is_multiplicative () =
  assert (parse "17 % 5" = Ast.Binop (Ast.Mod, Ast.Int 17, Ast.Int 5));
  assert (
    parse "10 + 17 % 5 * 2"
    = Ast.Binop
        ( Ast.Add,
          Ast.Int 10,
          Ast.Binop
            (Ast.Mul, Ast.Binop (Ast.Mod, Ast.Int 17, Ast.Int 5), Ast.Int 2) ));
  assert (
    parse "20 % 6 % 4"
    = Ast.Binop (Ast.Mod, Ast.Binop (Ast.Mod, Ast.Int 20, Ast.Int 6), Ast.Int 4))

let test_less_than () =
  assert (parse "1 < 2" = Ast.Binop (Ast.Lt, Ast.Int 1, Ast.Int 2))

let test_equality_operators () =
  assert (parse "1 == 2" = Ast.Binop (Ast.Equal, Ast.Int 1, Ast.Int 2));
  assert (parse "1 != 2" = Ast.Binop (Ast.NotEqual, Ast.Int 1, Ast.Int 2))

let test_equality_binds_less_tightly_than_relational () =
  assert (
    parse "1 < 2 == 3 >= 4"
    = Ast.Binop
        ( Ast.Equal,
          Ast.Binop (Ast.Lt, Ast.Int 1, Ast.Int 2),
          Ast.Binop (Ast.GtEq, Ast.Int 3, Ast.Int 4) ))

let test_equality_is_left_associative () =
  assert (
    parse "1 == 2 != 3"
    = Ast.Binop
        (Ast.NotEqual, Ast.Binop (Ast.Equal, Ast.Int 1, Ast.Int 2), Ast.Int 3))

let test_bitwise_operators () =
  assert (parse "1 & 2" = Ast.Binop (Ast.BitAnd, Ast.Int 1, Ast.Int 2));
  assert (parse "1 | 2" = Ast.Binop (Ast.BitOr, Ast.Int 1, Ast.Int 2))

let test_bitwise_precedence () =
  assert (
    parse "1 == 2 & 3"
    = Ast.Binop
        (Ast.BitAnd, Ast.Binop (Ast.Equal, Ast.Int 1, Ast.Int 2), Ast.Int 3));
  assert (
    parse "1 | 2 & 3"
    = Ast.Binop
        (Ast.BitOr, Ast.Int 1, Ast.Binop (Ast.BitAnd, Ast.Int 2, Ast.Int 3)))

let test_bitwise_operators_are_left_associative () =
  assert (
    parse "1 | 2 | 3"
    = Ast.Binop
        (Ast.BitOr, Ast.Binop (Ast.BitOr, Ast.Int 1, Ast.Int 2), Ast.Int 3))

let test_conditional_expression () =
  assert (parse "1 ? 2 : 3" = Ast.Conditional (Ast.Int 1, Ast.Int 2, Ast.Int 3))

let test_conditional_binds_less_tightly_than_bitwise_or () =
  assert (
    parse "1 | 2 ? 3 + 4 : 5"
    = Ast.Conditional
        ( Ast.Binop (Ast.BitOr, Ast.Int 1, Ast.Int 2),
          Ast.Binop (Ast.Add, Ast.Int 3, Ast.Int 4),
          Ast.Int 5 ))

let test_conditional_is_right_associative () =
  assert (
    parse "1 ? 2 : 3 ? 4 : 5"
    = Ast.Conditional
        (Ast.Int 1, Ast.Int 2, Ast.Conditional (Ast.Int 3, Ast.Int 4, Ast.Int 5)));
  assert (
    parse "1 ? 2 ? 3 : 4 : 5"
    = Ast.Conditional
        (Ast.Int 1, Ast.Conditional (Ast.Int 2, Ast.Int 3, Ast.Int 4), Ast.Int 5))

let test_addition_binds_tighter_than_less_than () =
  assert (
    parse "1 + 2 < 4"
    = Ast.Binop (Ast.Lt, Ast.Binop (Ast.Add, Ast.Int 1, Ast.Int 2), Ast.Int 4))

let test_shift_operators () =
  assert (parse "1 << 2" = Ast.Binop (Ast.ShiftLeft, Ast.Int 1, Ast.Int 2));
  assert (parse "8 >> 1" = Ast.Binop (Ast.ShiftRight, Ast.Int 8, Ast.Int 1))

let test_addition_binds_tighter_than_shift () =
  assert (
    parse "1 + 2 << 3"
    = Ast.Binop
        (Ast.ShiftLeft, Ast.Binop (Ast.Add, Ast.Int 1, Ast.Int 2), Ast.Int 3));
  assert (
    parse "8 >> 1 + 1"
    = Ast.Binop
        (Ast.ShiftRight, Ast.Int 8, Ast.Binop (Ast.Add, Ast.Int 1, Ast.Int 1)))

let test_shift_binds_tighter_than_relational () =
  assert (
    parse "1 << 2 < 8"
    = Ast.Binop
        (Ast.Lt, Ast.Binop (Ast.ShiftLeft, Ast.Int 1, Ast.Int 2), Ast.Int 8))

let test_shifts_are_left_associative () =
  assert (
    parse "16 >> 2 << 1"
    = Ast.Binop
        ( Ast.ShiftLeft,
          Ast.Binop (Ast.ShiftRight, Ast.Int 16, Ast.Int 2),
          Ast.Int 1 ))

let test_parentheses_override_precedence () =
  assert (
    parse "(2 + 3) * 4"
    = Ast.Binop (Ast.Mul, Ast.Binop (Ast.Add, Ast.Int 2, Ast.Int 3), Ast.Int 4))

let test_operators_are_left_associative () =
  assert (
    parse "10 - 2 - 3"
    = Ast.Binop (Ast.Sub, Ast.Binop (Ast.Sub, Ast.Int 10, Ast.Int 2), Ast.Int 3))

let test_unary_minus () =
  assert (parse "-42" = Ast.Neg (Ast.Int 42));
  assert (parse "-2 * 3" = Ast.Binop (Ast.Mul, Ast.Neg (Ast.Int 2), Ast.Int 3));
  assert (parse "-(2 + 3)" = Ast.Neg (Ast.Binop (Ast.Add, Ast.Int 2, Ast.Int 3)))

let expect_parser_error source =
  match parse source with
  | _ -> assert false
  | exception Parser.Parser_error _ -> ()

let test_invalid_expressions () =
  expect_parser_error "";
  expect_parser_error "2 +";
  expect_parser_error "2 3";
  expect_parser_error "(2 + 3";
  expect_parser_error "1 ? 2";
  expect_parser_error "1 ? 2 :";
  expect_parser_error "items[2";
  expect_parser_error "items[]";
  expect_parser_error "&1";
  expect_parser_error "1 = count";
  expect_parser_error "foo(1";
  expect_parser_error "foo(1,)";
  expect_parser_error "++1";
  expect_parser_error "--(count + 1)";
  expect_parser_error "1++";
  expect_parser_error "foo()--"

let test_expression_statements () =
  assert (
    parse_statement "count = 7;"
    = Ast.Expression (Ast.Assign (Ast.Variable "count", Ast.Int 7)));
  assert (
    parse_statement "print(count);"
    = Ast.Expression
        (Ast.Call
           (Ast.Read (Ast.Variable "print"), [ Ast.Read (Ast.Variable "count") ])))

let test_return_statements () =
  assert (parse_statement "return;" = Ast.Return None);
  assert (
    parse_statement "return (count + 1);"
    = Ast.Return
        (Some (Ast.Binop (Ast.Add, Ast.Read (Ast.Variable "count"), Ast.Int 1))))

let expect_statement_parser_error source =
  match parse_statement source with
  | _ -> assert false
  | exception Parser.Parser_error _ -> ()

let test_invalid_statements () =
  expect_statement_parser_error "count = 7";
  expect_statement_parser_error "return";
  expect_statement_parser_error "return count;";
  expect_statement_parser_error "return (count); extra;"

let test_block_statements () =
  assert (parse_statement "{}" = Ast.Block []);
  assert (
    parse_statement "{ count = 1; return (count); }"
    = Ast.Block
        [
          Ast.Expression (Ast.Assign (Ast.Variable "count", Ast.Int 1));
          Ast.Return (Some (Ast.Read (Ast.Variable "count")));
        ]);
  assert (
    parse_statement "{ { count = 1; } return; }"
    = Ast.Block
        [
          Ast.Block
            [ Ast.Expression (Ast.Assign (Ast.Variable "count", Ast.Int 1)) ];
          Ast.Return None;
        ])

let test_invalid_blocks () = expect_statement_parser_error "{ count = 1;"

let test_if_statements () =
  assert (
    parse_statement "if (count) count = count - 1;"
    = Ast.If
        ( Ast.Read (Ast.Variable "count"),
          Ast.Expression
            (Ast.Assign
               ( Ast.Variable "count",
                 Ast.Binop (Ast.Sub, Ast.Read (Ast.Variable "count"), Ast.Int 1)
               )),
          None ));
  assert (
    parse_statement "if (count) { count = count - 1; } else { return (count); }"
    = Ast.If
        ( Ast.Read (Ast.Variable "count"),
          Ast.Block
            [
              Ast.Expression
                (Ast.Assign
                   ( Ast.Variable "count",
                     Ast.Binop
                       (Ast.Sub, Ast.Read (Ast.Variable "count"), Ast.Int 1) ));
            ],
          Some
            (Ast.Block [ Ast.Return (Some (Ast.Read (Ast.Variable "count"))) ])
        ));
  assert (
    parse_statement "if (a) if (b) x = 1; else x = 2;"
    = Ast.If
        ( Ast.Read (Ast.Variable "a"),
          Ast.If
            ( Ast.Read (Ast.Variable "b"),
              Ast.Expression (Ast.Assign (Ast.Variable "x", Ast.Int 1)),
              Some (Ast.Expression (Ast.Assign (Ast.Variable "x", Ast.Int 2)))
            ),
          None ))

let test_while_statements () =
  assert (
    parse_statement "while (count) count--;"
    = Ast.While
        ( Ast.Read (Ast.Variable "count"),
          Ast.Expression (Ast.PostDecrement (Ast.Variable "count")) ));
  assert (
    parse_statement "while (count) { count--; }"
    = Ast.While
        ( Ast.Read (Ast.Variable "count"),
          Ast.Block
            [ Ast.Expression (Ast.PostDecrement (Ast.Variable "count")) ] ))

let test_invalid_if_and_while_statements () =
  expect_statement_parser_error "if count x = 1;";
  expect_statement_parser_error "if (count)";
  expect_statement_parser_error "else x = 2;";
  expect_statement_parser_error "while (count { count--; }"

let test_null_statements () =
  assert (parse_statement ";" = Ast.Null);
  assert (
    parse_statement "while (count);"
    = Ast.While (Ast.Read (Ast.Variable "count"), Ast.Null));
  assert (
    parse_statement "{ ; return; }" = Ast.Block [ Ast.Null; Ast.Return None ])

let test_auto_and_extrn_declarations () =
  assert (
    parse_statement "auto x, y; return (x + y);"
    = Ast.Auto
        ( [ "x"; "y" ],
          Ast.Return
            (Some
               (Ast.Binop
                  ( Ast.Add,
                    Ast.Read (Ast.Variable "x"),
                    Ast.Read (Ast.Variable "y") ))) ));
  assert (
    parse_statement "extrn putchar; putchar(65);"
    = Ast.Extrn
        ( [ "putchar" ],
          Ast.Expression
            (Ast.Call (Ast.Read (Ast.Variable "putchar"), [ Ast.Int 65 ])) ));
  assert (
    parse_program "main() { auto value; return (value); }"
    = [
        {
          Ast.name = "main";
          params = [];
          body =
            Ast.Block
              [
                Ast.Auto
                  ( [ "value" ],
                    Ast.Return (Some (Ast.Read (Ast.Variable "value"))) );
              ];
        };
      ])

let test_invalid_declarations () =
  expect_statement_parser_error "auto; return;";
  expect_statement_parser_error "auto x,; return;";
  expect_statement_parser_error "auto x";
  expect_statement_parser_error "auto x;";
  expect_statement_parser_error "extrn putchar return;"

let test_function_definitions () =
  assert (
    parse_program "main() { return (42); }"
    = [
        {
          Ast.name = "main";
          params = [];
          body = Ast.Block [ Ast.Return (Some (Ast.Int 42)) ];
        };
      ]);
  assert (
    parse_program "double(x) { return (x + x); }"
    = [
        {
          Ast.name = "double";
          params = [ "x" ];
          body =
            Ast.Block
              [
                Ast.Return
                  (Some
                     (Ast.Binop
                        ( Ast.Add,
                          Ast.Read (Ast.Variable "x"),
                          Ast.Read (Ast.Variable "x") )));
              ];
        };
      ]);
  assert (
    parse_program "first() ; second(a, b) return (a + b);"
    = [
        { Ast.name = "first"; params = []; body = Ast.Null };
        {
          Ast.name = "second";
          params = [ "a"; "b" ];
          body =
            Ast.Return
              (Some
                 (Ast.Binop
                    ( Ast.Add,
                      Ast.Read (Ast.Variable "a"),
                      Ast.Read (Ast.Variable "b") )));
        };
      ])

let expect_program_parser_error source =
  try
    ignore (parse_program source);
    failwith ("Expected a parser error for program: " ^ source)
  with Parser.Parser_error _ -> ()

let test_invalid_function_definitions () =
  expect_program_parser_error "";
  expect_program_parser_error "42";
  expect_program_parser_error "main { return; }";
  expect_program_parser_error "f(x,) { return; }";
  expect_program_parser_error "main() { return;"

let () =
  test_integer_expression ();
  test_variable_expression ();
  test_dereference_expression ();
  test_address_of_expression ();
  test_array_subscript ();
  test_assignment_expression ();
  test_function_call_expression ();
  test_logical_not ();
  test_increment_decrement ();
  test_multiplication_precedence ();
  test_modulo_is_multiplicative ();
  test_less_than ();
  test_equality_operators ();
  test_equality_binds_less_tightly_than_relational ();
  test_equality_is_left_associative ();
  test_bitwise_operators ();
  test_bitwise_precedence ();
  test_bitwise_operators_are_left_associative ();
  test_conditional_expression ();
  test_conditional_binds_less_tightly_than_bitwise_or ();
  test_conditional_is_right_associative ();
  test_addition_binds_tighter_than_less_than ();
  test_shift_operators ();
  test_addition_binds_tighter_than_shift ();
  test_shift_binds_tighter_than_relational ();
  test_shifts_are_left_associative ();
  test_parentheses_override_precedence ();
  test_operators_are_left_associative ();
  test_unary_minus ();
  test_invalid_expressions ();
  test_expression_statements ();
  test_return_statements ();
  test_invalid_statements ();
  test_block_statements ();
  test_invalid_blocks ();
  test_if_statements ();
  test_while_statements ();
  test_invalid_if_and_while_statements ();
  test_null_statements ();
  test_auto_and_extrn_declarations ();
  test_invalid_declarations ();
  test_function_definitions ();
  test_invalid_function_definitions ();
  print_endline "Parser tests passed"
