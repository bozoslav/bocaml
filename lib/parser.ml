exception Parser_error of string

let rec parse_expr tokens = parse_conditional tokens

and parse_conditional tokens =
  let condition, rest = parse_bitwise_or tokens in
  match rest with
  | Token.Question :: rest ->
      let when_true, rest = parse_expr rest in
      begin match rest with
      | Token.Colon :: rest ->
          let when_false, rest = parse_conditional rest in
          (Ast.Conditional (condition, when_true, when_false), rest)
      | _ -> raise (Parser_error "Expected ':' in conditional expression")
      end
  | _ -> (condition, rest)

and parse_bitwise_or tokens =
  let left, rest = parse_bitwise_and tokens in
  parse_bitwise_or_rest left rest

and parse_bitwise_or_rest left tokens =
  match tokens with
  | Token.Pipe :: rest ->
      let right, rest = parse_bitwise_and rest in
      let expr = Ast.Binop (Ast.BitOr, left, right) in
      parse_bitwise_or_rest expr rest
  | _ -> (left, tokens)

and parse_bitwise_and tokens =
  let left, rest = parse_equality tokens in
  parse_bitwise_and_rest left rest

and parse_bitwise_and_rest left tokens =
  match tokens with
  | Token.Amp :: rest ->
      let right, rest = parse_equality rest in
      let expr = Ast.Binop (Ast.BitAnd, left, right) in
      parse_bitwise_and_rest expr rest
  | _ -> (left, tokens)

and parse_equality tokens =
  let left, rest = parse_relational tokens in
  parse_equality_rest left rest

and parse_equality_rest left tokens =
  match tokens with
  | Token.EqEq :: rest ->
      let right, rest = parse_relational rest in
      let expr = Ast.Binop (Ast.Equal, left, right) in
      parse_equality_rest expr rest
  | Token.BangEq :: rest ->
      let right, rest = parse_relational rest in
      let expr = Ast.Binop (Ast.NotEqual, left, right) in
      parse_equality_rest expr rest
  | _ -> (left, tokens)

and parse_relational tokens =
  let left, rest = parse_shift tokens in
  parse_relational_rest left rest

and parse_relational_rest left tokens =
  match tokens with
  | Token.Lt :: rest ->
      let right, rest = parse_shift rest in
      let expr = Ast.Binop (Ast.Lt, left, right) in
      parse_relational_rest expr rest
  | Token.Gt :: rest ->
      let right, rest = parse_shift rest in
      let expr = Ast.Binop (Ast.Gt, left, right) in
      parse_relational_rest expr rest
  | Token.LtEq :: rest ->
      let right, rest = parse_shift rest in
      let expr = Ast.Binop (Ast.LtEq, left, right) in
      parse_relational_rest expr rest
  | Token.GtEq :: rest ->
      let right, rest = parse_shift rest in
      let expr = Ast.Binop (Ast.GtEq, left, right) in
      parse_relational_rest expr rest
  | _ -> (left, tokens)

and parse_shift tokens =
  let left, rest = parse_additive tokens in
  parse_shift_rest left rest

and parse_shift_rest left tokens =
  match tokens with
  | Token.LtLt :: rest ->
      let right, rest = parse_additive rest in
      let expr = Ast.Binop (Ast.ShiftLeft, left, right) in
      parse_shift_rest expr rest
  | Token.GtGt :: rest ->
      let right, rest = parse_additive rest in
      let expr = Ast.Binop (Ast.ShiftRight, left, right) in
      parse_shift_rest expr rest
  | _ -> (left, tokens)

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
  let left, rest = parse_unary tokens in
  parse_multiplicative_rest left rest

and parse_multiplicative_rest left tokens =
  match tokens with
  | Token.Star :: rest ->
      let right, rest = parse_unary rest in
      let expr = Ast.Binop (Ast.Mul, left, right) in
      parse_multiplicative_rest expr rest
  | Token.Slash :: rest ->
      let right, rest = parse_unary rest in
      let expr = Ast.Binop (Ast.Div, left, right) in
      parse_multiplicative_rest expr rest
  | Token.Percent :: rest ->
      let right, rest = parse_unary rest in
      let expr = Ast.Binop (Ast.Mod, left, right) in
      parse_multiplicative_rest expr rest
  | _ -> (left, tokens)

and parse_unary tokens =
  match tokens with
  | Token.Minus :: rest ->
      let expr, rest = parse_unary rest in
      (Ast.Neg expr, rest)
  | Token.Star :: rest ->
      let expr, rest = parse_unary rest in
      (Ast.Read (Ast.Dereference expr), rest)
  | Token.Amp :: rest ->
      let expr, rest = parse_unary rest in
      begin match expr with
      | Ast.Read lvalue -> (Ast.Address lvalue, rest)
      | _ -> raise (Parser_error "Expected an lvalue after '&'")
      end
  | _ -> parse_primary tokens

and parse_primary tokens =
  let base, rest =
    match tokens with
    | Token.Int n :: rest -> (Ast.Int n, rest)
    | Token.Name name :: rest -> (Ast.Read (Ast.Variable name), rest)
    | Token.LParen :: rest ->
        let expr, rest = parse_expr rest in
        begin match rest with
        | Token.RParen :: rest -> (expr, rest)
        | _ -> raise (Parser_error "Expected ')")
        end
    | _ -> raise (Parser_error "Expected expression")
  in
  parse_postfix base rest

and parse_postfix base tokens =
  match tokens with
  | Token.LBracket :: rest ->
      let index, rest = parse_expr rest in
      begin match rest with
      | Token.RBracket :: rest ->
          let subscript = Ast.Read (Ast.Subscript (base, index)) in
          parse_postfix subscript rest
      | _ -> raise (Parser_error "expected ']' after subscript")
      end
  | _ -> (base, tokens)

let parse tokens =
  let expr, rest = parse_expr tokens in

  match rest with
  | [ Token.Eof ] -> expr
  | _ -> raise (Parser_error "Unexpected tokens after expression")
