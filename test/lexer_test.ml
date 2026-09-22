open Bocaml
open Token

let test_whitespace () =
  assert (Lexer.lex "" = [ Eof ]);
  assert (Lexer.lex " \t\r\n " = [ Eof ])

let test_integers () =
  assert (Lexer.lex "0" = [ Int 0; Eof ]);
  assert (Lexer.lex "123" = [ Int 123; Eof ]);
  assert (Lexer.lex "12+34" = [ Int 12; Plus; Int 34; Eof ]);
  assert (Lexer.lex "-42" = [ Minus; Int 42; Eof ]);
  assert (Lexer.lex (string_of_int max_int) = [ Int max_int; Eof ])

let test_names_and_keywords () =
  assert (Lexer.lex "automatic" = [ Name "automatic"; Eof ]);
  assert (Lexer.lex "_count2" = [ Name "_count2"; Eof ]);
  assert (Lexer.lex "r" = [ Name "r"; Eof ]);
  assert (Lexer.lex "Auto" = [ Name "Auto"; Eof ]);
  assert (
    Lexer.lex "auto extrn if else while switch case default goto return break"
    = [
        Auto;
        Extrn;
        If;
        Else;
        While;
        Switch;
        Case;
        Default;
        Goto;
        Return;
        Break;
        Eof;
      ]);
  assert (
    Lexer.lex "auto count2; count2 = 42;"
    = [ Auto; Name "count2"; Semi; Name "count2"; Eq; Int 42; Semi; Eof ])

let test_operators () =
  let singles =
    [
      ("+", Plus);
      ("-", Minus);
      ("*", Star);
      ("/", Slash);
      ("%", Percent);
      ("=", Eq);
      ("!", Bang);
      ("<", Lt);
      (">", Gt);
      ("&", Amp);
      ("|", Pipe);
      ("^", Caret);
      ("~", Tilde);
      ("(", LParen);
      (")", RParen);
      ("{", LBrace);
      ("}", RBrace);
      ("[", LBracket);
      ("]", RBracket);
      (";", Semi);
      (",", Comma);
      (":", Colon);
      ("?", Question);
    ]
  in
  List.iter
    (fun (source, token) -> assert (Lexer.lex source = [ token; Eof ]))
    singles;
  let doubles =
    [
      ("==", EqEq);
      ("!=", BangEq);
      ("++", PlusPlus);
      ("--", MinusMinus);
      ("<=", LtEq);
      (">=", GtEq);
      ("<<", LtLt);
      (">>", GtGt);
      ("&&", AmpAmp);
      ("||", PipePipe);
    ]
  in
  List.iter
    (fun (source, token) -> assert (Lexer.lex source = [ token; Eof ]))
    doubles;
  assert (Lexer.lex "= =" = [ Eq; Eq; Eof ]);
  assert (Lexer.lex "===" = [ EqEq; Eq; Eof ]);
  assert (Lexer.lex "+++" = [ PlusPlus; Plus; Eof ])

let test_comments () =
  assert (Lexer.lex "/**/" = [ Eof ]);
  assert (Lexer.lex "/* first */ /* second */" = [ Eof ]);
  assert (Lexer.lex "/* line one\nline two */" = [ Eof ]);
  assert (Lexer.lex "a/* hello */+b" = [ Name "a"; Plus; Name "b"; Eof ]);
  assert (Lexer.lex "8 / 2" = [ Int 8; Slash; Int 2; Eof ])

let test_strings () =
  assert (Lexer.lex "\"\"" = [ Str ""; Eof ]);
  assert (Lexer.lex "\"hello\";" = [ Str "hello"; Semi; Eof ]);
  assert (Lexer.lex "\"one\"\"two\"" = [ Str "one"; Str "two"; Eof ]);
  assert (Lexer.lex "\"auto + /* text */\"" = [ Str "auto + /* text */"; Eof ])

let expect_failure source expected_message =
  match Lexer.lex source with
  | _ -> assert false
  | exception Failure message -> assert (message = expected_message)

let test_errors () =
  expect_failure "/*" "Unterminated comment";
  expect_failure "/* unfinished" "Unterminated comment";
  expect_failure "/* unfinished*" "Unterminated comment";
  expect_failure "\"" "Unterminated string";
  expect_failure "\"unfinished" "Unterminated string";
  let too_large = string_of_int max_int ^ "0" in
  expect_failure ("  " ^ too_large)
    ("Integer out of range at position 2: " ^ too_large);
  match Lexer.lex "@" with
  | _ -> assert false
  | exception Lexer.Lexer_error c -> assert (c = '@')

let () =
  test_whitespace ();
  test_integers ();
  test_names_and_keywords ();
  test_operators ();
  test_comments ();
  test_strings ();
  test_errors ();
  print_endline "Lexer tests passed"
