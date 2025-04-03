open Perkelang.Ast
open Perkelang.Codegen
open Perkelang.Errors (*EWWOWS*)

let inchn = open_in Sys.argv.(1)

let ast_of_channel inchn =
  let lexbuf = Sedlexing.Latin1.from_channel inchn in
  let lexer = Sedlexing.with_tokenizer Perkelang.Lexer.token lexbuf in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Perkelang.Parser.program
  in
  try parser lexer
  with ParseError e ->
    raise
      (Syntax_error
         ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
           (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
           - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol,
           e ))

let out_file =
  let input_file = Sys.argv.(1) in
  if Filename.check_suffix input_file ".perk" then
    Filename.chop_suffix input_file ".perk" ^ ".c"
  else failwith "Input file must have a .perk extension"
;;

try
  let ast = ast_of_channel inchn in
  print_endline (show_command_a ast);
  let oc = open_out out_file in
  output_string oc (ast |> codegen_program);
  close_out oc
with
| Syntax_error (line, col, msg) ->
    Printf.eprintf "\027[31mSyntax error at line %d, column %d: %s\027[0m\n"
      line col msg;
    exit 1
| Lexing_error (line, col, msg) ->
    Printf.eprintf "\027[31mLexing error at line %d, column %d: %s\027[0m\n"
      line col msg;
    exit 1
;;

close_in inchn
