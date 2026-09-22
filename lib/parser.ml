exception Parser_error of string

let rec parse_expr tokens =
  parse_additive tokens

and parse_additive tokens =
  let left, rest = parse_multiplicative tokens in
  parse_additive_rest left rest

and parse_additive_rest left tokens =
  match tokens with
  | Token.Plus :: rest ->
    let right, rest = parse_multiplicative rest in
    let expr = Ast.Binop (Ast.Add, left, right) in
    parse_additive_rest expr rest

  | Token.Minus :: rest ->
    let right, rest = parse_multiplicative rest in
    let expr = Ast.Binop (Ast.Sub, left, right) in
    parse_additive_rest expr rest

  | _ -> (left, tokens)

and parse_multiplicative tokens =
  let left, rest = parse_primary tokens in
  parse_multiplicative_rest left rest

and parse_multiplicative_rest left tokens =
  match tokens with
  | Token.Star :: rest ->
    let right, rest = parse_primary rest in
    let expr = Ast.Binop(Ast.Mul, left, right) in
    parse_multiplicative_rest expr rest
  
  | Token.Slash :: rest ->
    let right, rest = parse_primary rest in
    let expr = Ast.Binop(Ast.Div, left, right) in
    parse_multiplicative_rest expr rest
  
  | _ -> (left, tokens)

and parse_primary tokens =
  match tokens with
  | Token.Int n :: rest -> (Ast.Int n, rest)

  | Token.LParen :: rest ->
    let expr, rest = parse_expr rest in
    begin
      match rest with
      | Token.RParen :: rest -> (expr, rest)
      | _ -> raise (Parser_error "Expected ')")
    end

  | _ -> raise (Parser_error "Expected expression")

let parse tokens =
  let expr, rest = parse_expr tokens in

  match rest with
  | [Token.Eof] -> expr

  | _ -> raise (Parser_error "Unexpected tokens after expression")