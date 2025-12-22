type operator = Negation | BitwiseComplement | LogicalNegation;;
(* type binary_operator = Addition | Multiplication | Division *)
type exp = Const of int | UnOp of operator * exp(* | BinOp of binary_operator * exp * exp*);;
type statement = Return of exp;;
type func_decl = Func of string * statement;;
type prog = Prog of func_decl;;

let rec parse_exp token_list =
  match token_list with
  | [] -> failwith "Invalid Syntax: Expression is incomplete"
  | token :: token_list ->
    (match token with 
    | Lexer.IntegerLiteral number -> (Const number, token_list)
    | Lexer.Negation ->
      (let exp, token_list = parse_exp token_list in
      (UnOp(Negation, exp), token_list))
    | Lexer.BitwiseComplement ->
      (let exp, token_list = parse_exp token_list in
      (UnOp(BitwiseComplement, exp), token_list))
    | Lexer.LogicalNegation ->
      (let exp, token_list = parse_exp token_list in
      (UnOp(LogicalNegation, exp), token_list))
    | _ -> failwith "Invalid Syntax: Expression doesn't contain an integer literal")

let parse_statement token_list =
  match token_list with
  | [] -> failwith "Invalid Syntax: Statement is incomplete"
  | token :: token_list ->
    (match token with
    | Lexer.Keyword "return" ->
      (let expression, token_list = parse_exp token_list in
      match token_list with
      | [] -> failwith "Invalid Syntax: Statement is incomplete"
      | token :: token_list ->
        (match token with
        | Lexer.Semicolon -> (Return expression, token_list)
        | _ -> failwith "Invalid Syntax: Statement doesn't end with ';'")
      )
    | _ -> failwith "Invalid Syntax: Statement doesn't start with return keyword")

let parse_func token_list =
  match token_list with
  | [] -> failwith "Invalid Syntax: Function is incomplete"
  | token :: token_list ->
    (match token with
    | Lexer.Keyword "int" -> 
      (match token_list with
      | [] -> failwith "Invalid Syntax: Function is incomplete"
      | token :: token_list ->
        (match token with
        | Lexer.Identifier func_name -> 
          (match token_list with
          | [] -> failwith "Invalid Syntax: Function is incomplete"
          | token :: token_list ->
            (match token with
            | Lexer.OpenParanthesis ->
              (match token_list with
              | [] -> failwith "Invalid Syntax: Function is incomplete"
              | token :: token_list ->
                (match token with
                | Lexer.CloseParanthesis ->
                  (match token_list with
                  | [] -> failwith "Invalid Syntax: Function is incomplete"
                  | token :: token_list ->
                    (match token with
                    | Lexer.OpenBrace ->
                      (let statement, token_list = parse_statement token_list in
                      match token_list with
                      | [] -> failwith "Invalid Syntax: Function is incomplete"
                      | token :: token_list ->
                        (match token with
                        | Lexer.CloseBrace -> (Func (func_name, statement), token_list)
                        | _ -> failwith "Invalid syntax: Invalid function definition"))
                    | _ ->  failwith "Invalid Syntax: Invalid function definition"))
                | _ -> failwith "Invalid Syntax: Invalid function definition"))
            | _ -> failwith "Invalid Syntax: Invalid function definition"))
        | _ -> failwith "Invalid Syntax: Invalid main function name"))
    | _ -> failwith "Invalid Syntax: Function return datatype incorrect")

let parse_prog token_list =
  let function_node, token_list = parse_func token_list in
  match token_list with
  | [] -> Prog function_node
  | _ -> failwith "Invalid Syntax: Program contains unnecessary tokens" 

let func_of_prog prog = 
  match prog with
  | Prog f -> f

let name_and_body_of_func func_decl =
  match func_decl with
  | Func (name, statement) -> (name, statement)

let expression_of_statemnt statement =
  match statement with
  | Return exp -> exp
