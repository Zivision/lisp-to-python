exception ParseError of string

let parse_tokens tokens =
  let rec parse_expr tokens =
    match tokens with
    | [] -> raise (ParseError "Unexpected end of input")
    | Lexer.LParen :: rest ->
        let exprs, remaining = parse_list rest in
        (Ast.List exprs, remaining)
    | Lexer.RParen :: _ -> raise (ParseError "Unexpected ')'")
    | Lexer.IntLit n :: rest -> (Ast.Int n, rest)
    | Lexer.Sym s :: rest -> (Ast.Symbol s, rest)

  and parse_list tokens =
    match tokens with
    | [] -> raise (ParseError "Unclosed '('")
    | Lexer.RParen :: rest -> ([], rest)
    | _ ->
        let expr, rest1 = parse_expr tokens in
        let exprs, rest2 = parse_list rest1 in
        (expr :: exprs, rest2)
  in
  match parse_expr tokens with
  | (expr, []) -> expr
  | (_, _) -> raise (ParseError "Extra tokens after expression")

let parse input =
  let tokens = Lexer.tokenize input in
  parse_tokens tokens
