open Token

exception Lexer_error of char

let is_digit c = c >= '0' && c <= '9'
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alnum c = is_letter c || is_digit c

let keyword_or_name word =
  match word with
  | "auto" -> Auto
  | "extrn" -> Extrn
  | "if" -> If
  | "else" -> Else
  | "while" -> While
  | "switch" -> Switch
  | "case" -> Case
  | "default" -> Default
  | "goto" -> Goto
  | "return" -> Return
  | "break" -> Break
  | _ -> Name word

let lex source =
  let n = String.length source in

  let rec number_end j =
    if j < n && is_digit source.[j] then number_end (j + 1) else j
  in

  let rec indentifier_end j =
    if j < n && is_alnum source.[j] then indentifier_end (j + 1) else j
  in

  let rec comment_end j =
    if j + 1 >= n then failwith "Unterminated comment"
    else if source.[j] = '*' && source.[j + 1] = '/' then j + 2
    else comment_end (j + 1)
  in

  let rec string_end j =
    if j >= n then failwith "Unterminated string"
    else if source.[j] = '"' then j
    else string_end (j + 1)
  in

  let rec scan i tokens =
    if i >= n then List.rev (Eof :: tokens)
    else
      match source.[i] with
      (* whitespace *)
      | ' ' | '\n' | '\t' | '\r' -> scan (i + 1) tokens
      (* one char tokens *)
      | '*' -> scan (i + 1) (Star :: tokens)
      | '%' -> scan (i + 1) (Percent :: tokens)
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
      (* two char tokens *)
      | '=' ->
          if i + 1 < n && source.[i + 1] = '=' then scan (i + 2) (EqEq :: tokens)
          else scan (i + 1) (Eq :: tokens)
      | '!' ->
          if i + 1 < n && source.[i + 1] = '=' then
            scan (i + 2) (BangEq :: tokens)
          else scan (i + 1) (Bang :: tokens)
      | '+' ->
          if i + 1 < n && source.[i + 1] = '+' then
            scan (i + 2) (PlusPlus :: tokens)
          else scan (i + 1) (Plus :: tokens)
      | '-' ->
          if i + 1 < n && source.[i + 1] = '-' then
            scan (i + 2) (MinusMinus :: tokens)
          else scan (i + 1) (Minus :: tokens)
      | '<' ->
          if i + 1 < n && source.[i + 1] = '=' then scan (i + 2) (LtEq :: tokens)
          else if i + 1 < n && source.[i + 1] = '<' then
            scan (i + 2) (LtLt :: tokens)
          else scan (i + 1) (Lt :: tokens)
      | '>' ->
          if i + 1 < n && source.[i + 1] = '=' then scan (i + 2) (GtEq :: tokens)
          else if i + 1 < n && source.[i + 1] = '>' then
            scan (i + 2) (GtGt :: tokens)
          else scan (i + 1) (Gt :: tokens)
      | '&' ->
          if i + 1 < n && source.[i + 1] = '&' then
            scan (i + 2) (AmpAmp :: tokens)
          else scan (i + 1) (Amp :: tokens)
      | '|' ->
          if i + 1 < n && source.[i + 1] = '|' then
            scan (i + 2) (PipePipe :: tokens)
          else scan (i + 1) (Pipe :: tokens)
      (* ints *)
      | c when is_digit c ->
          let stop = number_end i in
          let text = String.sub source i (stop - i) in
          let value =
            match int_of_string_opt text with
            | Some n -> n
            | None ->
                failwith
                  (Printf.sprintf "Integer out of range at position %d: %s" i
                     text)
          in
          scan stop (Int value :: tokens)
      (* keyword / name *)
      | c when is_letter c ->
          let stop = indentifier_end i in
          let word = String.sub source i (stop - i) in
          let token = keyword_or_name word in
          scan stop (token :: tokens)
      (* comments *)
      | '/' ->
          if i + 1 < n && source.[i + 1] = '*' then
            let stop = comment_end (i + 2) in
            scan stop tokens
          else scan (i + 1) (Slash :: tokens)
      (* string literals *)
      | '"' ->
          let stop = string_end (i + 1) in
          let text = String.sub source (i + 1) (stop - i - 1) in
          scan (stop + 1) (Str text :: tokens)
      | c -> raise (Lexer_error c)
  in

  scan 0 []
