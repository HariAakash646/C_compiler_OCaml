type operator = Negation | BitwiseComplement | LogicalNegation;;
type additive_operator = Addition | Subtraction
type multiplicative_operator = Multiplication | Division
type fact = BracedExp of exp | UnOp of operator * fact | Const of int
and multiplicative_tokens = Operator of multiplicative_operator | MultiplicativeTerm of fact
and term = MultExp of multiplicative_tokens list
and additive_tokens = Operator of additive_operator | AdditiveTerm of term
and exp = AdditiveExp of additive_tokens list;;
type statement = Return of exp;;
type func_decl = Func of string * statement;;
type prog = Prog of func_decl;;

val parse_prog : Lexer.token list -> prog
val func_of_prog : prog -> func_decl
val name_and_body_of_func : func_decl -> string * statement
val expression_of_statemnt : statement -> exp
