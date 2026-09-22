module Token = Bocaml.Token

let string_of_token = function
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

let () =
  let source = "\"hello\";" in
  let tokens = Bocaml.Lexer.lex source in

  List.iter (fun token -> print_endline (string_of_token token)) tokens
