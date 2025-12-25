type operator = Negation | BitwiseComplement | LogicalNegation
type and_operator = And
type or_operator = Or
type comparative_operator = Less | LessEqual | Greater | GreaterEqual
type equality_operator = Equal | NotEqual
type additive_operator = Addition | Subtraction
type multiplicative_operator = Multiplication | Division
type fact = BracedExp of exp | UnOp of operator * fact | Const of int | Var of string
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
and or_exp = OrExp of or_tokens list
and exp = LogicalOr of or_exp | Assign of string * exp 
;;
type statement = Return of exp | Declare of string * exp option | Exp of exp;;
type func_decl = Func of string * statement list;;
type prog = Prog of func_decl;;

val parse_prog : Lexer.token list -> prog
val func_of_prog : prog -> func_decl
val name_and_body_of_func : func_decl -> string * statement list
