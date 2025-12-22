let writeline (oc : out_channel) (s : string) =
  output_string oc s;
  output_char oc '\n'

let rec generate_expression_code expression out_channel =
  match expression with
  | Parser.Const number -> writeline out_channel ("movq $" ^ (string_of_int number) ^ ", %rax")
  | Parser.UnOp(operator, expression) ->
    (
      generate_expression_code expression out_channel;
      match operator with
      | Negation -> writeline out_channel ("negq %rax")
      | BitwiseComplement -> writeline out_channel ("notq %rax")
      | LogicalNegation ->
        (writeline out_channel ("cmpq $0, %rax");
        writeline out_channel "movq $0, %rax";
        writeline out_channel "sete %al")
    ) 

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
  
