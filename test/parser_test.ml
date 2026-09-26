open Bocaml

let parse source = source |> Lexer.lex |> Parser.parse
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
  print_endline "Parser tests passed"
