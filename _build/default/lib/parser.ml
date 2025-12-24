type operator = Negation | BitwiseComplement | LogicalNegation
type and_operator = And
type or_operator = Or
type comparative_operator = Less | LessEqual | Greater | GreaterEqual
type equality_operator = Equal | NotEqual
type additive_operator = Addition | Subtraction
type multiplicative_operator = Multiplication | Division
type fact = BracedExp of exp | UnOp of operator * fact | Const of int
and multiplicative_tokens = Operator of multiplicative_operator | MultiplicativeTerm of fact
and term = MultExp of multiplicative_tokens list
and additive_tokens = Operator of additive_operator | AdditiveTerm of term
and additive_exp = AdditiveExp of additive_tokens list
and comparative_tokens = Operator of comparative_operator | ComparativeTerm of additive_exp
and comparative_exp = ComparativeExp of comparative_tokens list
and equality_tokens = Operator of equality_operator | EqualityTerm of comparative_exp
and equality_exp = EqualityExp of equality_tokens list
and and_tokens = Operator of and_operator | AndTerm of equality_exp
and and_exp = AndExp of and_tokens list
and or_tokens = Operator of or_operator | OrTerm of and_exp
and exp = OrExp of or_tokens list
;;
type statement = Return of exp;;
type func_decl = Func of string * statement;;
type prog = Prog of func_decl;;

let rec parse_factor token_list =
  match token_list with
  | [] -> failwith "Invalid Syntax: Expression is incomplete"
  | token :: token_list ->
    (match token with 
    | Lexer.IntegerLiteral number -> (Const number, token_list)
    | Lexer.Negation ->
      (let exp, token_list = parse_factor token_list in
      (UnOp(Negation, exp), token_list))
    | Lexer.BitwiseComplement ->
      (let exp, token_list = parse_factor token_list in
      (UnOp(BitwiseComplement, exp), token_list))
    | Lexer.LogicalNegation ->
      (let exp, token_list = parse_factor token_list in
      (UnOp(LogicalNegation, exp), token_list))
    | Lexer.OpenParanthesis ->
      (let exp, token_list = parse_exp token_list in
      match token_list with
      | Lexer.CloseParanthesis :: token_list -> (BracedExp(exp), token_list)
      | _ -> failwith "Invalid Syntax: missing ')'")
    | _ -> failwith "Invalid Syntax: Expression doesn't contain an integer literal")

and parse_term token_list = 
  let (fact, token_list) = parse_factor token_list in
  match token_list with
  | Lexer.Multiplication :: token_list -> 
    ( let term, token_list = parse_term token_list in
      match term with
      | MultExp(term_list) -> (MultExp(MultiplicativeTerm(fact) :: Operator(Multiplication) :: term_list), token_list))
  | Lexer.Division :: token_list ->
    ( let term, token_list = parse_term token_list in
      match term with
      | MultExp(term_list) -> (MultExp(MultiplicativeTerm(fact) :: Operator(Division) :: term_list), token_list))
  | _ -> (MultExp([MultiplicativeTerm(fact)]), token_list)

and parse_additive_exp token_list =
  let (term, token_list) = parse_term token_list in
  match token_list with
  | Lexer.Addition :: token_list ->
    (let exp, token_list = parse_additive_exp token_list in
    match exp with
    | AdditiveExp(exp_list) -> (AdditiveExp(AdditiveTerm(term) :: Operator(Addition) :: exp_list), token_list))
  | Lexer.Negation :: token_list ->
    (let exp, token_list = parse_additive_exp token_list in
    match exp with
    | AdditiveExp(exp_list) -> (AdditiveExp(AdditiveTerm(term) :: Operator(Subtraction) :: exp_list), token_list))
  | _ -> (AdditiveExp([AdditiveTerm(term)]), token_list)

and parse_comparative_exp token_list =
  let (additive_exp, token_list) = parse_additive_exp token_list in  
  match token_list with
  | Lexer.Less :: token_list ->
    (let exp, token_list = parse_comparative_exp token_list in
    match exp with
    | ComparativeExp(exp_list) -> (ComparativeExp(ComparativeTerm(additive_exp) :: Operator(Less) :: exp_list), token_list))
  | Lexer.LessEqual :: token_list ->
    (let exp, token_list = parse_comparative_exp token_list in
    match exp with
    | ComparativeExp(exp_list) -> (ComparativeExp(ComparativeTerm(additive_exp) :: Operator(LessEqual) :: exp_list), token_list))
  | Lexer.Greater :: token_list ->
    (let exp, token_list = parse_comparative_exp token_list in
    match exp with
    | ComparativeExp(exp_list) -> (ComparativeExp(ComparativeTerm(additive_exp) :: Operator(Greater) :: exp_list), token_list))
  | Lexer.GreaterEqual :: token_list ->
    (let exp, token_list = parse_comparative_exp token_list in
    match exp with
    | ComparativeExp(exp_list) -> (ComparativeExp(ComparativeTerm(additive_exp) :: Operator(GreaterEqual) :: exp_list), token_list))
  | _ -> (ComparativeExp([ComparativeTerm(additive_exp)]), token_list)

and parse_equality_exp token_list =
  let (comparative_exp, token_list) = parse_comparative_exp token_list in  
  match token_list with
  | Lexer.Equal :: token_list ->
    (let exp, token_list = parse_equality_exp token_list in
    match exp with
    | EqualityExp(exp_list) -> (EqualityExp(EqualityTerm(comparative_exp) :: Operator(Equal) :: exp_list), token_list))
  | Lexer.NotEqual :: token_list ->
    (let exp, token_list = parse_equality_exp token_list in
    match exp with
    | EqualityExp(exp_list) -> (EqualityExp(EqualityTerm(comparative_exp) :: Operator(NotEqual) :: exp_list), token_list))
  | _ -> (EqualityExp([EqualityTerm(comparative_exp)]), token_list)

and parse_and_exp token_list =
  let (equality_exp, token_list) = parse_equality_exp token_list in  
  match token_list with
  | Lexer.And :: token_list ->
    (let exp, token_list = parse_and_exp token_list in
    match exp with
    | AndExp(exp_list) -> (AndExp(AndTerm(equality_exp) :: Operator(And) :: exp_list), token_list))
  | _ -> (AndExp([AndTerm(equality_exp)]), token_list)

and parse_exp token_list =
  let (and_exp, token_list) = parse_and_exp token_list in  
  match token_list with
  | Lexer.Or :: token_list ->
    (let exp, token_list = parse_exp token_list in
    match exp with
    | OrExp(exp_list) -> (OrExp(OrTerm(and_exp) :: Operator(Or) :: exp_list), token_list))
  | _ -> (OrExp([OrTerm(and_exp)]), token_list)
;;

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
