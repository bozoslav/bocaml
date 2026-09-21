open Token

exception Lexer_error of char

let is_digit c = c >= '0' && c <= '9'
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alnum c = is_letter c || is_digit c

let lex source =
  let n = String.length source in

  let rec scan i tokens =
    if i >= n then List.rev (Eof :: tokens)
    else
      match source.[i] with
      (* whitespace *)
      | ' ' | '\n' | '\t' | '\r' -> scan (i + 1) tokens
      (* one char tokens *)
      | '+' -> scan (i + 1) (Plus :: tokens)
      | '-' -> scan (i + 1) (Minus :: tokens)
      | '*' -> scan (i + 1) (Star :: tokens)
      | '/' -> scan (i + 1) (Slash :: tokens)
      | '%' -> scan (i + 1) (Percent :: tokens)
      | '=' -> scan (i + 1) (Eq :: tokens)
      | '!' -> scan (i + 1) (Bang :: tokens)
      | '<' -> scan (i + 1) (Lt :: tokens)
      | '>' -> scan (i + 1) (Gt :: tokens)
      | '&' -> scan (i + 1) (Amp :: tokens)
      | '|' -> scan (i + 1) (Pipe :: tokens)
      | '^' -> scan (i + 1) (Caret :: tokens)
      | '~' -> scan (i + 1) (Tilde :: tokens)
      | '(' -> scan (i + 1) (LParen :: tokens)
      | ')' -> scan (i + 1) (RParen :: tokens)
      | '{' -> scan (i + 1) (LBrace :: tokens)
      | '}' -> scan (i + 1) (RBrace :: tokens)
      | '[' -> scan (i + 1) (LBracket :: tokens)
      | ']' -> scan (i + 1) (RBracket :: tokens)
      | ';' -> scan (i + 1) (Semi :: tokens)
      | ',' -> scan (i + 1) (Comma :: tokens)
      | ':' -> scan (i + 1) (Colon :: tokens)
      | '?' -> scan (i + 1) (Question :: tokens)
      | c -> raise (Lexer_error c)
  in

  scan 0 []
