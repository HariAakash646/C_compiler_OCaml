let file_name = Sys.argv.(1) 
let file_prefix = Filename.chop_extension file_name

let token_list = Lexer.lex_file file_name

let rec output_token_list token_list =
  match token_list with
  | [] -> ()
  | head :: suffix ->
    (match head with
    | Lexer.OpenBrace -> print_endline "{"
    | Lexer.CloseBrace -> print_endline "}"
    | Lexer.OpenParanthesis -> print_endline "("
    | Lexer.CloseParanthesis -> print_endline ")"
    | Lexer.Semicolon -> print_endline ";"
    | Lexer.Keyword keyw -> print_endline keyw
    | Lexer.Identifier ident -> print_endline ("ident" ^ ident)
    | Lexer.IntegerLiteral constant -> print_endline (string_of_int constant)
    | Lexer.NotEqual -> print_endline "!="
    | _ -> ())
    ;
    output_token_list suffix
;;

ignore output_token_list
(* output_token_list token_list *)

let ast = Parser.parse_prog token_list;;

Code_generator.generate_code ast file_prefix

let status = Sys.command ("gcc " ^ file_prefix ^ ".s -o " ^ file_prefix) ;;
ignore status
     