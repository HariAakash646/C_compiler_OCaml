type token =
  | Keyword of string
  | OpenBrace
  | CloseBrace
  | OpenParanthesis
  | CloseParanthesis
  | Semicolon
  | IntegerLiteral of int
  | Identifier of string
  | UnknownToken
  | Negation
  | BitwiseComplement
  | LogicalNegation
  | Addition
  | Multiplication
  | Division
;;

val lex_file : string -> token list