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
  | And
  | Or
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Assignment
;;

let lex_file file_name =
  let in_channel = open_in file_name in

  let rec process_line in_channel = 
    try
      let line = input_line in_channel in
      let rec extract_token line start =
        if start >= String.length line then []
        else
          let (token_value, start) = 
            if Str.string_partial_match (Str.regexp_string "{") line start 
              then (OpenBrace, start+1)
            else if Str.string_partial_match (Str.regexp_string "}") line start 
              then (CloseBrace, start+1)
            else if Str.string_partial_match (Str.regexp_string "(") line start 
              then (OpenParanthesis, start+1)
            else if Str.string_partial_match (Str.regexp_string ")") line start 
              then (CloseParanthesis, start+1)
            else if Str.string_partial_match (Str.regexp_string ";") line start 
              then (Semicolon, start+1)
            else if Str.string_partial_match (Str.regexp {|[A-Za-z][A-Za-z0-9_]*|}) line start
              then
                let s = Str.matched_string line in
                if s = "int" || s = "return" then
                  (Keyword s, start + String.length s)
                else
                  (Identifier s, start + String.length s)
            else if Str.string_partial_match (Str.regexp "[0-9]+") line start
              then
                let current_constant = Str.matched_string line in
                (IntegerLiteral (int_of_string current_constant), start + String.length current_constant)  
            else if Str.string_partial_match (Str.regexp_string "-") line start
              then (Negation, start+1)
            else if Str.string_partial_match (Str.regexp_string "~") line start
              then (BitwiseComplement, start+1)
            else if Str.string_partial_match (Str.regexp_string "!=") line start
              then (NotEqual, start+2)
            else if Str.string_partial_match (Str.regexp_string "!") line start
              then (LogicalNegation, start+1)
            else if Str.string_partial_match (Str.regexp_string "+") line start
              then (Addition, start+1)
            else if Str.string_partial_match (Str.regexp_string "*") line start
              then (Multiplication, start+1)
            else if Str.string_partial_match (Str.regexp_string "/") line start
              then (Division, start+1)
            else if Str.string_partial_match (Str.regexp_string "&&") line start
              then (And, start+2)
            else if Str.string_partial_match (Str.regexp_string "||") line start
              then (Or, start+2)
            else if Str.string_partial_match (Str.regexp_string "==") line start
              then (Equal, start+2)
            else if Str.string_partial_match (Str.regexp_string "<=") line start
              then (LessEqual, start+2)
            else if Str.string_partial_match (Str.regexp_string "<") line start
              then (Less, start+1)
            else if Str.string_partial_match (Str.regexp_string ">=") line start
              then (GreaterEqual, start+2)
            else if Str.string_partial_match (Str.regexp_string ">") line start
              then (Greater, start+1)
            else if Str.string_partial_match (Str.regexp_string "=") line start
              then (Assignment, start+1)
            else (UnknownToken, start+1)
          in
          match token_value with
          | UnknownToken -> extract_token line start
          | _ -> ([token_value] @ (extract_token line start))
      in
      let output_list = extract_token line 0 in
      (output_list @ (process_line in_channel))
    with End_of_file -> []
  in

  process_line in_channel