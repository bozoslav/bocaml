open Bocaml

let compile source =
  source |> Lexer.lex |> Parser.parse |> Llvm_codegen.compile_expr

let test_integer_literal () =
  assert (compile "42" = "define i64 @main() {\nentry:\n  ret i64 42\n}")

let test_arithmetic_order () =
  assert (
    compile "2 + 3 * 4"
    = "define i64 @main() {\n\
       entry:\n\
      \  %t0 = mul i64 3, 4\n\
      \  %t1 = add i64 2, %t0\n\
      \  ret i64 %t1\n\
       }")

let test_left_associativity () =
  assert (
    compile "10 - 3 - 2"
    = "define i64 @main() {\n\
       entry:\n\
      \  %t0 = sub i64 10, 3\n\
      \  %t1 = sub i64 %t0, 2\n\
      \  ret i64 %t1\n\
       }")

let test_unsupported_expression () =
  try
    ignore (compile "count");
    failwith "Expected variable expression code generation to be unsupported"
  with Llvm_codegen.Unsupported_expression _ -> ()

let test_unsupported_operator () =
  try
    ignore (compile "1 < 2");
    failwith "Expected comparison code generation to be unsupported"
  with Llvm_codegen.Unsupported_operator _ -> ()

let () =
  test_integer_literal ();
  test_arithmetic_order ();
  test_left_associativity ();
  test_unsupported_expression ();
  test_unsupported_operator ();
  print_endline "LLVM codegen tests passed"
