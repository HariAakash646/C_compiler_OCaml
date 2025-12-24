let writeline (oc : out_channel) (s : string) =
  output_string oc s;
  output_char oc '\n'

let clause_counter = ref 0
let end_counter = ref 0

let get_clause_counter () =
  incr clause_counter;
  clause_counter
;;

let get_end_counter () =
  incr end_counter;
  end_counter
;;

let rec generate_factor_code (factor : Parser.fact) out_channel =
  match factor with
  | Parser.Const num -> writeline out_channel ("movq $" ^ (string_of_int num) ^ ", %rax")
  | Parser.UnOp(operator, factor) ->
    (
      generate_factor_code factor out_channel;
      match operator with
      | Negation -> writeline out_channel ("negq %rax")
      | BitwiseComplement -> writeline out_channel ("notq %rax")
      | LogicalNegation ->
        (writeline out_channel ("cmpq $0, %rax");
        writeline out_channel "movq $0, %rax";
        writeline out_channel "sete %al")
    ) 
  | Parser.BracedExp(exp) -> generate_expression_code exp out_channel

and generate_term_code (term : Parser.term) out_channel =
  match term with
  | Parser.MultExp(fact_list) -> 
    (match fact_list with
    | [] -> ()
    | Parser.MultiplicativeTerm(fact) :: fact_list ->
      (generate_factor_code fact out_channel;
      generate_term_code (Parser.MultExp(fact_list)) out_channel)
    | Parser.Operator(operator) :: Parser.MultiplicativeTerm(fact) :: fact_list ->
      (writeline out_channel "pushq %rax";
      generate_factor_code fact out_channel;
      writeline out_channel "movq %rax, %rcx";
      writeline out_channel "popq %rax";
      (match operator with
      | Multiplication -> writeline out_channel "imulq %rcx"
      | Division ->
        (writeline out_channel "cqo";
        writeline out_channel "idivq %rcx"));
      generate_term_code (Parser.MultExp(fact_list)) out_channel)
    | _ -> failwith "Unexpected run in generate_term_code. This should never happen.")

and generate_additive_expression_code (expression : Parser.additive_exp) out_channel =
  match expression with
  | Parser.AdditiveExp(term_list) -> 
    (match term_list with
    | [] -> ()
    | Parser.AdditiveTerm(term) :: term_list ->
      (generate_term_code term out_channel;
      generate_additive_expression_code (Parser.AdditiveExp(term_list)) out_channel)
    | Parser.Operator(operator) :: Parser.AdditiveTerm(term) :: term_list ->
      (writeline out_channel "pushq %rax";
      generate_term_code term out_channel;
      writeline out_channel "movq %rax, %rcx";
      writeline out_channel "popq %rax";
      (match operator with
      | Addition -> writeline out_channel "addq %rcx, %rax"
      | Subtraction -> writeline out_channel "subq %rcx, %rax");
      generate_additive_expression_code (Parser.AdditiveExp(term_list)) out_channel)
    | _ -> failwith "Unexpected run in generate_additive_expression_code. This should never happen.")

and generate_comparative_expression_code (expression : Parser.comparative_exp) out_channel =
  match expression with
  | Parser.ComparativeExp(term_list) -> 
    (match term_list with
    | [] -> ()
    | Parser.ComparativeTerm(term) :: term_list ->
      (generate_additive_expression_code term out_channel;
      generate_comparative_expression_code (Parser.ComparativeExp(term_list)) out_channel)
    | Parser.Operator(operator) :: Parser.ComparativeTerm(term) :: term_list ->
      (writeline out_channel "pushq %rax";
      generate_additive_expression_code term out_channel;
      writeline out_channel "movq %rax, %rcx";
      writeline out_channel "popq %rax";
      writeline out_channel "cmpq %rcx, %rax";
      writeline out_channel "movq $0, %rax";
      (match operator with
      | Less -> writeline out_channel "setl %al"
      | LessEqual -> writeline out_channel "setle %al"
      | Greater -> writeline out_channel "setg %al"
      | GreaterEqual -> writeline out_channel "setge %al");
      generate_comparative_expression_code (Parser.ComparativeExp(term_list)) out_channel)
    | _ -> failwith "Unexpected run in generate_comparative_expression_code. This should never happen.")

and generate_equality_expression_code (expression : Parser.equality_exp) out_channel =
  match expression with
  | Parser.EqualityExp(term_list) -> 
    (match term_list with
    | [] -> ()
    | Parser.EqualityTerm(term) :: term_list ->
      (generate_comparative_expression_code term out_channel;
      generate_equality_expression_code (Parser.EqualityExp(term_list)) out_channel)
    | Parser.Operator(operator) :: Parser.EqualityTerm(term) :: term_list ->
      (writeline out_channel "pushq %rax";
      generate_comparative_expression_code term out_channel;
      writeline out_channel "movq %rax, %rcx";
      writeline out_channel "popq %rax";
      writeline out_channel "cmpq %rcx, %rax";
      writeline out_channel "movq $0, %rax";
      (match operator with
      | Equal -> writeline out_channel "sete %al"
      | NotEqual -> writeline out_channel "setne %al");
      generate_equality_expression_code (Parser.EqualityExp(term_list)) out_channel)
    | _ -> failwith "Unexpected run in generate_equality_expression_code. This should never happen.")

and generate_and_expression_code (expression : Parser.and_exp) out_channel =
  match expression with
  | Parser.AndExp(term_list) -> 
    (match term_list with
    | [] -> ()
    | Parser.AndTerm(term) :: term_list ->
      (generate_equality_expression_code term out_channel;
      generate_and_expression_code (Parser.AndExp(term_list)) out_channel)
    | Parser.Operator(operator) :: Parser.AndTerm(term) :: term_list ->
      (writeline out_channel "pushq %rax";
      generate_equality_expression_code term out_channel;
      writeline out_channel "movq %rax, %rcx";
      writeline out_channel "popq %rax";
      (match operator with
      | And -> 
        (writeline out_channel "cmpq $0, %rax";
        writeline out_channel ("jne _clause" ^ (string_of_int !(get_clause_counter ())));
        writeline out_channel "movq $0, %rax";
        writeline out_channel ("jmp _end" ^ (string_of_int !(get_end_counter ())));
        writeline out_channel ("_clause" ^ (string_of_int !clause_counter) ^ ":") ;
        writeline out_channel "cmpq $0, %rcx";
        writeline out_channel "movq $1, %rax";
        writeline out_channel ("jne _end" ^ (string_of_int !end_counter));
        writeline out_channel "movq $0, %rax";
        writeline out_channel ("_end" ^ (string_of_int !end_counter) ^ ":")));
      generate_and_expression_code (Parser.AndExp(term_list)) out_channel)
    | _ -> failwith "Unexpected run in generate_and_expression_code. This should never happen.")

and generate_expression_code (expression : Parser.exp) out_channel =
  match expression with
  | Parser.OrExp(term_list) -> 
    (match term_list with
    | [] -> ()
    | Parser.OrTerm(term) :: term_list ->
      (generate_and_expression_code term out_channel;
      generate_expression_code (Parser.OrExp(term_list)) out_channel)
    | Parser.Operator(operator) :: Parser.OrTerm(term) :: term_list ->
      (writeline out_channel "pushq %rax";
      generate_and_expression_code term out_channel;
      writeline out_channel "movq %rax, %rcx";
      writeline out_channel "popq %rax";
      (match operator with
      | Or -> 
        (writeline out_channel "cmpq $0, %rax";
        writeline out_channel ("je _clause" ^ (string_of_int !(get_clause_counter ())));
        writeline out_channel "movq $1, %rax";
        writeline out_channel ("jmp _end" ^ (string_of_int !(get_end_counter ())));
        writeline out_channel ("_clause" ^ (string_of_int !clause_counter) ^ ":");
        writeline out_channel "cmpq $0, %rcx";
        writeline out_channel "movq $0, %rax";
        writeline out_channel ("je _end" ^ (string_of_int !end_counter));
        writeline out_channel "movq $1, %rax";
        writeline out_channel ("_end" ^ (string_of_int !end_counter) ^ ":")));
      generate_expression_code (Parser.OrExp(term_list)) out_channel)
    | _ -> failwith "Unexpected run in generate_expression_code. This should never happen.")

;;
  

let generate_code prog file_name =
  let out_channel = open_out (file_name ^ ".s") in
  
  let func = Parser.func_of_prog prog in
  let name, statement = Parser.name_and_body_of_func func in
  writeline out_channel (".globl " ^ name);
  writeline out_channel (name ^ ":");
  let expression = Parser.expression_of_statemnt statement in
  generate_expression_code expression out_channel;

  writeline out_channel "retq";
  close_out out_channel
  
