module StringMap = Map.Make(String)

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

let tmp_file = ref ""

let gracefully_fail fail_msg out_channel =
  close_out out_channel;
  Sys.remove !tmp_file;
  failwith fail_msg

let rec generate_factor_code (factor : Parser.fact) out_channel var_map =
  match factor with
  | Parser.Const num -> writeline out_channel ("movq $" ^ (string_of_int num) ^ ", %rax")
  | Parser.UnOp(operator, factor) ->
    (
      generate_factor_code factor out_channel var_map;
      match operator with
      | Negation -> writeline out_channel ("negq %rax")
      | BitwiseComplement -> writeline out_channel ("notq %rax")
      | LogicalNegation ->
        (writeline out_channel ("cmpq $0, %rax");
        writeline out_channel "movq $0, %rax";
        writeline out_channel "sete %al")
    ) 
  | Parser.BracedExp(exp) -> generate_expression_code exp out_channel var_map
  | Parser.Var(name) -> 
    (match StringMap.find_opt name var_map with
    | None -> gracefully_fail ("Unknown variable named " ^ name) out_channel
    | Some var_offset -> writeline out_channel ("movq " ^ (string_of_int var_offset) ^ "(%rbp), %rax"))

and generate_term_code (term : Parser.term) out_channel var_map =
  match term with
  | Parser.MultExp(fact_list) -> 
    (match fact_list with
    | [] -> ()
    | Parser.MultiplicativeTerm(fact) :: fact_list ->
      (generate_factor_code fact out_channel var_map;
      generate_term_code (Parser.MultExp(fact_list)) out_channel var_map)
    | Parser.Operator(operator) :: Parser.MultiplicativeTerm(fact) :: fact_list ->
      (writeline out_channel "pushq %rax";
      generate_factor_code fact out_channel var_map;
      writeline out_channel "movq %rax, %rcx";
      writeline out_channel "popq %rax";
      (match operator with
      | Multiplication -> writeline out_channel "imulq %rcx"
      | Division ->
        (writeline out_channel "cqo";
        writeline out_channel "idivq %rcx"));
      generate_term_code (Parser.MultExp(fact_list)) out_channel var_map)
    | _ -> gracefully_fail "Unexpected run in generate_term_code. This should never happen." out_channel)

and generate_additive_expression_code (expression : Parser.additive_exp) out_channel var_map =
  match expression with
  | Parser.AdditiveExp(term_list) -> 
    (match term_list with
    | [] -> ()
    | Parser.AdditiveTerm(term) :: term_list ->
      (generate_term_code term out_channel var_map;
      generate_additive_expression_code (Parser.AdditiveExp(term_list)) out_channel var_map)
    | Parser.Operator(operator) :: Parser.AdditiveTerm(term) :: term_list ->
      (writeline out_channel "pushq %rax";
      generate_term_code term out_channel var_map;
      writeline out_channel "movq %rax, %rcx";
      writeline out_channel "popq %rax";
      (match operator with
      | Addition -> writeline out_channel "addq %rcx, %rax"
      | Subtraction -> writeline out_channel "subq %rcx, %rax");
      generate_additive_expression_code (Parser.AdditiveExp(term_list)) out_channel var_map)
    | _ -> gracefully_fail "Unexpected run in generate_additive_expression_code. This should never happen." out_channel)

and generate_comparative_expression_code (expression : Parser.comparative_exp) out_channel var_map =
  match expression with
  | Parser.ComparativeExp(term_list) -> 
    (match term_list with
    | [] -> ()
    | Parser.ComparativeTerm(term) :: term_list ->
      (generate_additive_expression_code term out_channel var_map;
      generate_comparative_expression_code (Parser.ComparativeExp(term_list)) out_channel var_map)
    | Parser.Operator(operator) :: Parser.ComparativeTerm(term) :: term_list ->
      (writeline out_channel "pushq %rax";
      generate_additive_expression_code term out_channel var_map;
      writeline out_channel "movq %rax, %rcx";
      writeline out_channel "popq %rax";
      writeline out_channel "cmpq %rcx, %rax";
      writeline out_channel "movq $0, %rax";
      (match operator with
      | Less -> writeline out_channel "setl %al"
      | LessEqual -> writeline out_channel "setle %al"
      | Greater -> writeline out_channel "setg %al"
      | GreaterEqual -> writeline out_channel "setge %al");
      generate_comparative_expression_code (Parser.ComparativeExp(term_list)) out_channel var_map)
    | _ -> gracefully_fail "Unexpected run in generate_comparative_expression_code. This should never happen." out_channel)

and generate_equality_expression_code (expression : Parser.equality_exp) out_channel var_map =
  match expression with
  | Parser.EqualityExp(term_list) -> 
    (match term_list with
    | [] -> ()
    | Parser.EqualityTerm(term) :: term_list ->
      (generate_comparative_expression_code term out_channel var_map;
      generate_equality_expression_code (Parser.EqualityExp(term_list)) out_channel var_map)
    | Parser.Operator(operator) :: Parser.EqualityTerm(term) :: term_list ->
      (writeline out_channel "pushq %rax";
      generate_comparative_expression_code term out_channel var_map;
      writeline out_channel "movq %rax, %rcx";
      writeline out_channel "popq %rax";
      writeline out_channel "cmpq %rcx, %rax";
      writeline out_channel "movq $0, %rax";
      (match operator with
      | Equal -> writeline out_channel "sete %al"
      | NotEqual -> writeline out_channel "setne %al");
      generate_equality_expression_code (Parser.EqualityExp(term_list)) out_channel var_map)
    | _ -> gracefully_fail "Unexpected run in generate_equality_expression_code. This should never happen." out_channel)

and generate_and_expression_code (expression : Parser.and_exp) out_channel var_map =
  match expression with
  | Parser.AndExp(term_list) -> 
    (match term_list with
    | [] -> ()
    | Parser.AndTerm(term) :: term_list ->
      (generate_equality_expression_code term out_channel var_map;
      generate_and_expression_code (Parser.AndExp(term_list)) out_channel var_map)
    | Parser.Operator(operator) :: Parser.AndTerm(term) :: term_list ->
      (
      (match operator with
      | And -> 
        (writeline out_channel "cmpq $0, %rax";
        let clause_val = string_of_int !(get_clause_counter ()) in
        let end_val = string_of_int !(get_end_counter ()) in 
        writeline out_channel ("jne _clause" ^ clause_val);
        writeline out_channel "movq $0, %rax";
        writeline out_channel ("jmp _end" ^ end_val);
        writeline out_channel ("_clause" ^ clause_val ^ ":") ;
        generate_equality_expression_code term out_channel var_map;
        writeline out_channel "cmpq $0, %rax";
        writeline out_channel "movq $1, %rax";
        writeline out_channel ("jne _end" ^ end_val);
        writeline out_channel "movq $0, %rax";
        writeline out_channel ("_end" ^ end_val ^ ":")));
      generate_and_expression_code (Parser.AndExp(term_list)) out_channel var_map)
    | _ -> gracefully_fail "Unexpected run in generate_and_expression_code. This should never happen." out_channel)

and generate_or_expression_code (expression : Parser.or_exp) out_channel var_map =
  match expression with
  | Parser.OrExp(term_list) -> 
    (match term_list with
    | [] -> ()
    | Parser.OrTerm(term) :: term_list ->
      (generate_and_expression_code term out_channel var_map;
      generate_or_expression_code (Parser.OrExp(term_list)) out_channel var_map)
    | Parser.Operator(operator) :: Parser.OrTerm(term) :: term_list ->
      (
      (match operator with
      | Or -> 
        (writeline out_channel "cmpq $0, %rax";
        let clause_val = string_of_int !(get_clause_counter ()) in
        let end_val = string_of_int !(get_end_counter ()) in 
        writeline out_channel ("je _clause" ^ clause_val);
        writeline out_channel "movq $1, %rax";
        writeline out_channel ("jmp _end" ^ end_val);
        writeline out_channel ("_clause" ^ clause_val ^ ":");
        generate_and_expression_code term out_channel var_map;
        writeline out_channel "cmpq $0, %rax";
        writeline out_channel "movq $0, %rax";
        writeline out_channel ("je _end" ^ end_val);
        writeline out_channel "movq $1, %rax";
        writeline out_channel ("_end" ^ end_val ^ ":")));
      generate_or_expression_code (Parser.OrExp(term_list)) out_channel var_map)
    | _ -> gracefully_fail "Unexpected run in generate_expression_code. This should never happen." out_channel)

and generate_conditional_code (expression : Parser.conditional_exp) out_channel var_map =
  match expression with
  | CondExp(or_exp, suffix) ->
    (generate_or_expression_code or_exp out_channel var_map;
    match suffix with
    | None -> ()
    | Some (exp, cond_exp) ->
      (let clause_val = string_of_int !(get_clause_counter ()) in
      let end_val = string_of_int !(get_clause_counter ()) in
      writeline out_channel "cmpq $0, %rax";
      writeline out_channel ("je _clause" ^ clause_val);
      generate_expression_code exp out_channel var_map;
      writeline out_channel ("jmp _end" ^ end_val);
      writeline out_channel ("_clause" ^ clause_val ^ ":");
      generate_conditional_code cond_exp out_channel var_map;
      writeline out_channel ("_end" ^ end_val ^ ":")))

and generate_expression_code (expression : Parser.exp) out_channel var_map =
  match expression with
  | Parser.CondExp(exp) -> generate_conditional_code exp out_channel var_map
  | Parser.Assign(name, exp) ->
    (generate_expression_code exp out_channel var_map;
    match StringMap.find_opt name var_map with
    | None -> gracefully_fail ("Unknown variable named " ^ name) out_channel
    | Some var_offset -> writeline out_channel ("movq %rax, " ^ (string_of_int var_offset) ^ "(%rbp)"))
;;
  
let rec generate_statement_code (statement : Parser.statement) out_channel var_map stack_index =
  match statement with
  | Return(exp) 
  | Exp(exp) -> 
    (generate_expression_code exp out_channel var_map;
    (var_map, stack_index))
  | If(exp, statement, opt_statement) ->
    (generate_expression_code exp out_channel var_map;
    let clause_val = string_of_int !(get_clause_counter ()) in
    let end_val = string_of_int !(get_clause_counter ()) in
    writeline out_channel "cmpq $0, %rax";
    writeline out_channel ("je _clause" ^ clause_val);
    let var_map, stack_index = generate_statement_code statement out_channel var_map stack_index in
    writeline out_channel ("jmp _end" ^ end_val);
    writeline out_channel ("_clause" ^ clause_val ^ ":");
    (match opt_statement with
    | None -> 
      (writeline out_channel ("_end" ^ end_val ^ ":");
      (var_map, stack_index))
    | Some statement -> 
      (let var_map, stack_index = generate_statement_code statement out_channel var_map stack_index in
      writeline out_channel ("_end" ^ end_val ^ ":");
      (var_map, stack_index)));
    )

let generate_declaration_code (declaration : Parser.declaration) out_channel var_map stack_index =
  match declaration with
  | Declare(name, exp) ->
    (match StringMap.find_opt name var_map with
    | Some _ -> gracefully_fail ("Cannot reinitialize variable " ^ name) out_channel
    | None ->
      ((match exp with
      | None -> ()
      | Some exp -> generate_expression_code exp out_channel var_map);
      writeline out_channel "pushq %rax";
      let var_map = StringMap.add name stack_index var_map in
      let stack_index = stack_index - 8 in
      (var_map, stack_index)
      ))

let generate_block_item_code (block_item : Parser.block_item) out_channel var_map stack_index = 
  match block_item with
  | Statement(statement) -> generate_statement_code statement out_channel var_map stack_index
  | Declaration(declaration) -> generate_declaration_code declaration out_channel var_map stack_index

let rec generate_block_item_list_code (block_item_list : Parser.block_item list) out_channel var_map stack_index =
  (match block_item_list with
  | _ :: [] -> writeline out_channel "movq $0, %rax"
  | _ -> ());
  match block_item_list with
  | [] -> (var_map, stack_index)
  | block_item :: block_item_list ->
    (let (var_map, stack_index) = generate_block_item_code block_item out_channel var_map stack_index in
    generate_block_item_list_code block_item_list out_channel var_map stack_index)

let generate_function_code (func : Parser.func_decl) out_channel =
  let name, block_item_list = Parser.name_and_body_of_func func in
  writeline out_channel (".globl " ^ name);
  writeline out_channel (name ^ ":");
  writeline out_channel "pushq %rbp";
  writeline out_channel "movq %rsp, %rbp";
  let var_map = StringMap.empty in
  let stack_index = -8 in
  writeline out_channel "movq $0, %rax";
  let _ = generate_block_item_list_code block_item_list out_channel var_map stack_index in
  writeline out_channel "movq %rbp, %rsp";
  writeline out_channel "popq %rbp";
  writeline out_channel "retq"

let generate_code prog file_name =
  let out_channel = open_out (file_name ^ "_tmp.s") in
  tmp_file := file_name ^ "_tmp.s";
  
  let func = Parser.func_of_prog prog in
  generate_function_code func out_channel;
  close_out out_channel;

  Sys.rename (file_name ^ "_tmp.s") (file_name ^ ".s")
  
