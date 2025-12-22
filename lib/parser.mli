type operator = Negation | BitwiseComplement | LogicalNegation;;
type exp = Const of int | UnOp of operator * exp;;
type statement = Return of exp;;
type func_decl = Func of string * statement;;
type prog = Prog of func_decl;;

val parse_prog : Lexer.token list -> prog
val func_of_prog : prog -> func_decl
val name_and_body_of_func : func_decl -> string * statement
val expression_of_statemnt : statement -> exp
