module Token = Bocaml.Token

(*let string_of_token = function
  | Token.Int n -> "Int " ^ string_of_int n
  | Token.Str s -> Printf.sprintf "Str %S" s
  | Token.Name s -> "Name " ^ s
  | Token.Auto -> "Auto"
  | Token.Extrn -> "Extrn"
  | Token.If -> "If"
  | Token.Else -> "Else"
  | Token.While -> "While"
  | Token.Switch -> "Switch"
  | Token.Case -> "Case"
  | Token.Default -> "Default"
  | Token.Goto -> "Goto"
  | Token.Return -> "Return"
  | Token.Break -> "Break"
  | Token.Plus -> "Plus"
  | Token.Minus -> "Minus"
  | Token.Star -> "Star"
  | Token.Slash -> "Slash"
  | Token.Percent -> "Percent"
  | Token.Eq -> "Eq"
  | Token.EqEq -> "EqEq"
  | Token.Bang -> "Bang"
  | Token.BangEq -> "BangEq"
  | Token.Lt -> "Lt"
  | Token.LtEq -> "LtEq"
  | Token.Gt -> "Gt"
  | Token.GtEq -> "GtEq"
  | Token.Amp -> "Amp"
  | Token.Pipe -> "Pipe"
  | Token.Caret -> "Caret"
  | Token.Tilde -> "Tilde"
  | Token.LtLt -> "LtLt"
  | Token.GtGt -> "GtGt"
  | Token.AmpAmp -> "AmpAmp"
  | Token.PipePipe -> "PipePipe"
  | Token.PlusPlus -> "PlusPlus"
  | Token.MinusMinus -> "MinusMinus"
  | Token.LParen -> "LParen"
  | Token.RParen -> "RParen"
  | Token.LBrace -> "LBrace"
  | Token.RBrace -> "RBrace"
  | Token.LBracket -> "LBracket"
  | Token.RBracket -> "RBracket"
  | Token.Semi -> "Semi"
  | Token.Comma -> "Comma"
  | Token.Colon -> "Colon"
  | Token.Question -> "Question"
  | Token.Eof -> "Eof"
*)

module Ast = Bocaml.Ast

let string_of_binop = function
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"

let rec string_of_expr = function
  | Ast.Int n -> Printf.sprintf "Int(%d)" n
  
  | Ast.Binop (op, left, right) ->
    Printf.sprintf "Binop(%s, %s, %s)" (string_of_binop op) (string_of_expr left) (string_of_expr right)

let () =
  (*
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <file.b>\n" Sys.argv.(0);
    exit 1);

  let filename = Sys.argv.(1) in

  try
    let source = In_channel.with_open_bin filename In_channel.input_all in
    let tokens = Bocaml.Lexer.lex source in
    List.iter (fun token -> print_endline (string_of_token token)) tokens
  with
  | Sys_error message ->
      Printf.eprintf "File error: %s\n" message;
      exit 1
  | Bocaml.Lexer.Lexer_error c ->
      Printf.eprintf "Unexpected character: %C\n" c;
      exit 1
  | Failure message ->
      Printf.eprintf "Lexer error: %s\n" message;
      exit 1
  *)

  let source = "10 - 2 - 3" in

  let tokens = Bocaml.Lexer.lex source in
  let expression = Bocaml.Parser.parse tokens in

  Printf.printf "Source: %s\n" source;
  Printf.printf "Ast: %s\n" (string_of_expr expression)