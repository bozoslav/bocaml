module Ast = Bocaml.Ast

let string_of_binop = function
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Mod -> "%"
  | Ast.ShiftLeft -> "<<"
  | Ast.ShiftRight -> ">>"
  | Ast.Lt -> "<"
  | Ast.Gt -> ">"
  | Ast.LtEq -> "<="
  | Ast.GtEq -> ">="
  | Ast.Equal -> "=="
  | Ast.NotEqual -> "!="
  | Ast.BitAnd -> "&"
  | Ast.BitOr -> "|"

let rec string_of_expr = function
  | Ast.Int n -> Printf.sprintf "Int(%d)" n
  | Ast.Binop (op, left, right) ->
      Printf.sprintf "Binop(%s, %s, %s)" (string_of_binop op)
        (string_of_expr left) (string_of_expr right)
  | Ast.Neg expr -> Printf.sprintf "Neg(%s)" (string_of_expr expr)
  | Ast.Conditional (condition, when_true, when_false) ->
      Printf.sprintf "Conditional(%s, %s, %s)" (string_of_expr condition)
        (string_of_expr when_true)
        (string_of_expr when_false)

let string_of_token = function
  | Bocaml.Token.Int n -> "Int " ^ string_of_int n
  | Bocaml.Token.Str s -> Printf.sprintf "Str %S" s
  | Bocaml.Token.Name s -> "Name " ^ s
  | Bocaml.Token.Auto -> "Auto"
  | Bocaml.Token.Extrn -> "Extrn"
  | Bocaml.Token.If -> "If"
  | Bocaml.Token.Else -> "Else"
  | Bocaml.Token.While -> "While"
  | Bocaml.Token.Switch -> "Switch"
  | Bocaml.Token.Case -> "Case"
  | Bocaml.Token.Default -> "Default"
  | Bocaml.Token.Goto -> "Goto"
  | Bocaml.Token.Return -> "Return"
  | Bocaml.Token.Break -> "Break"
  | Bocaml.Token.Plus -> "Plus"
  | Bocaml.Token.Minus -> "Minus"
  | Bocaml.Token.Star -> "Star"
  | Bocaml.Token.Slash -> "Slash"
  | Bocaml.Token.Percent -> "Percent"
  | Bocaml.Token.Eq -> "Eq"
  | Bocaml.Token.EqEq -> "EqEq"
  | Bocaml.Token.Bang -> "Bang"
  | Bocaml.Token.BangEq -> "BangEq"
  | Bocaml.Token.Lt -> "Lt"
  | Bocaml.Token.LtEq -> "LtEq"
  | Bocaml.Token.Gt -> "Gt"
  | Bocaml.Token.GtEq -> "GtEq"
  | Bocaml.Token.Amp -> "Amp"
  | Bocaml.Token.Pipe -> "Pipe"
  | Bocaml.Token.Caret -> "Caret"
  | Bocaml.Token.Tilde -> "Tilde"
  | Bocaml.Token.LtLt -> "LtLt"
  | Bocaml.Token.GtGt -> "GtGt"
  | Bocaml.Token.AmpAmp -> "AmpAmp"
  | Bocaml.Token.PipePipe -> "PipePipe"
  | Bocaml.Token.PlusPlus -> "PlusPlus"
  | Bocaml.Token.MinusMinus -> "MinusMinus"
  | Bocaml.Token.LParen -> "LParen"
  | Bocaml.Token.RParen -> "RParen"
  | Bocaml.Token.LBrace -> "LBrace"
  | Bocaml.Token.RBrace -> "RBrace"
  | Bocaml.Token.LBracket -> "LBracket"
  | Bocaml.Token.RBracket -> "RBracket"
  | Bocaml.Token.Semi -> "Semi"
  | Bocaml.Token.Comma -> "Comma"
  | Bocaml.Token.Colon -> "Colon"
  | Bocaml.Token.Question -> "Question"
  | Bocaml.Token.Eof -> "Eof"

let () =
  let mode, input =
    match Sys.argv with
    | [| _; "--expr"; source |] -> (`Parse, `Source source)
    | [| _; "--tokens"; filename |] -> (`Tokens, `File filename)
    | [| _; argument |] ->
        if Sys.file_exists argument then (`Parse, `File argument)
        else (`Parse, `Source argument)
    | _ ->
        Printf.eprintf
          "Usage: %s <source-file> | --expr <source> | --tokens <source-file>\n"
          Sys.argv.(0);
        exit 1
  in

  try
    let source =
      match input with
      | `Source source -> source
      | `File filename -> In_channel.with_open_bin filename In_channel.input_all
    in
    let tokens = Bocaml.Lexer.lex source in
    match mode with
    | `Tokens ->
        List.iter (fun token -> print_endline (string_of_token token)) tokens
    | `Parse ->
        let expression = Bocaml.Parser.parse tokens in
        print_endline (string_of_expr expression)
  with
  | Sys_error message ->
      Printf.eprintf "File error: %s\n" message;
      exit 1
  | Bocaml.Lexer.Lexer_error c ->
      Printf.eprintf "Unexpected character: %C\n" c;
      exit 1
  | Bocaml.Parser.Parser_error message ->
      Printf.eprintf "Parse error: %s\n" message;
      exit 1
  | Failure message ->
      Printf.eprintf "Lexer error: %s\n" message;
      exit 1
