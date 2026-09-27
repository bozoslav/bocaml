exception Parser_error of string

let rec parse_expr tokens = parse_assignment tokens

and parse_assignment tokens =
  let left, rest = parse_conditional tokens in
  match rest with
  | Token.Eq :: rest ->
      begin match left with
      | Ast.Read lvalue ->
          let right, rest = parse_assignment rest in
          (Ast.Assign (lvalue, right), rest)
      | _ -> raise (Parser_error "Expected an lvalue on the left of '='")
      end
  | _ -> (left, rest)

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
  | Token.Bang :: rest ->
      let expr, rest = parse_unary rest in
      (Ast.Not expr, rest)
  | Token.PlusPlus :: rest ->
      let expr, rest = parse_unary rest in
      begin match expr with
      | Ast.Read lvalue -> (Ast.PreIncrement lvalue, rest)
      | _ -> raise (Parser_error "Expected and lvalue after '++'")
      end
  | Token.MinusMinus :: rest ->
      let expr, rest = parse_unary rest in
      begin match expr with
      | Ast.Read lvalue -> (Ast.PreDecrement lvalue, rest)
      | _ -> raise (Parser_error "Expected an lvalue after '--'")
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
  | Token.LParen :: rest ->
      let arguments, rest = parse_arguments rest in
      parse_postfix (Ast.Call (base, arguments)) rest
  | Token.PlusPlus :: rest ->
      begin match base with
      | Ast.Read lvalue -> parse_postfix (Ast.PostIncrement lvalue) rest
      | _ -> raise (Parser_error "Expected an lvalue before '++'")
      end
  | Token.MinusMinus :: rest ->
      begin match base with
      | Ast.Read lvalue -> parse_postfix (Ast.PostDecrement lvalue) rest
      | _ -> raise (Parser_error "Expected an lvalue before '--")
      end
  | _ -> (base, tokens)

and parse_arguments tokens =
  match tokens with
  | Token.RParen :: rest -> ([], rest)
  | _ ->
      let first, rest = parse_expr tokens in
      parse_arguments_rest [ first ] rest

and parse_arguments_rest reversed_arguments tokens =
  match tokens with
  | Token.Comma :: rest ->
      let argument, rest = parse_expr rest in
      parse_arguments_rest (argument :: reversed_arguments) rest
  | Token.RParen :: rest -> (List.rev reversed_arguments, rest)
  | _ -> raise (Parser_error "Expected ',' or ')' in function arguments")

let rec parse_statement_rest tokens =
  match tokens with
  | Token.Semi :: rest -> (Ast.Null, rest)
  | Token.LBrace :: rest -> parse_block rest []
  | Token.Return :: Token.Semi :: rest -> (Ast.Return None, rest)
  | Token.Return :: Token.LParen :: rest ->
      let expr, rest = parse_expr rest in
      begin match rest with
      | Token.RParen :: Token.Semi :: rest -> (Ast.Return (Some expr), rest)
      | Token.RParen :: _ -> raise (Parser_error "Expected ';' after return")
      | _ -> raise (Parser_error "Expected ')' after return expression")
      end
  | Token.Return :: _ -> raise (Parser_error "Expected ';' or '(' after return")
  | Token.If :: Token.LParen :: rest ->
      let condition, rest = parse_expr rest in
      begin match rest with
      | Token.RParen :: rest ->
          let then_branch, rest = parse_statement_rest rest in
          begin match rest with
          | Token.Else :: rest ->
              let else_branch, rest = parse_statement_rest rest in
              (Ast.If (condition, then_branch, Some else_branch), rest)
          | _ -> (Ast.If (condition, then_branch, None), rest)
          end
      | _ -> raise (Parser_error "Expected ')' after if condition")
      end
  | Token.While :: Token.LParen :: rest ->
      let condition, rest = parse_expr rest in
      begin match rest with
      | Token.RParen :: rest ->
          let body, rest = parse_statement_rest rest in
          (Ast.While (condition, body), rest)
      | _ -> raise (Parser_error "Expected ')' after while condition")
      end
  | _ ->
      let expr, rest = parse_expr tokens in
      begin match rest with
      | Token.Semi :: rest -> (Ast.Expression expr, rest)
      | _ -> raise (Parser_error "Expected ';' after expression")
      end

and parse_block tokens reversed_statements =
  match tokens with
  | Token.RBrace :: rest -> (Ast.Block (List.rev reversed_statements), rest)
  | Token.Eof :: _ -> raise (Parser_error "Expected '}' before end of input")
  | [] -> raise (Parser_error "Expected '}' before end of input")
  | _ ->
      let statement, rest = parse_statement_rest tokens in
      parse_block rest (statement :: reversed_statements)

let parse_statement tokens =
  let statement, rest = parse_statement_rest tokens in
  match rest with
  | [ Token.Eof ] -> statement
  | _ -> raise (Parser_error "Unexpected tokens after statement")

let parse tokens =
  let expr, rest = parse_expr tokens in

  match rest with
  | [ Token.Eof ] -> expr
  | _ -> raise (Parser_error "Unexpected tokens after expression")
