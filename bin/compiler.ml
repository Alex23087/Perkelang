open Perkelang.Ast
open Perkelang.Codegen
exception Syntax_error of int * int * string


let inchn = open_in Sys.argv.(1);;

let ast_of_channel inchn =
  let lexbuf = Sedlexing.Latin1.from_channel inchn in
  let lexer  = Sedlexing.with_tokenizer Perkelang.Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Perkelang.Parser.program in
  try (parser lexer) with
  | Perkelang.Parser.Error ->
    raise
      (Syntax_error
         ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
           (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
           - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol,
           "Syntax error" ));;


let out_file = 
  let input_file = Sys.argv.(1) in
  if Filename.check_suffix input_file ".perk" then
    (Filename.chop_suffix input_file ".perk") ^ ".c"
  else
    failwith "Input file must have a .perk extension";;

let ast = ast_of_channel inchn in
print_endline (show_command ast);
let oc = open_out out_file in
output_string oc (ast |> codegen_program);
close_out oc;
;;

close_in inchn
