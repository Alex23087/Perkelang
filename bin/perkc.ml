open Perkelang.Ast
open Perkelang.Codegen
open Perkelang.Errors (*EWWOWS*)
open Perkelang.Typecheck

let rec compile_program input_file =
  let out_file = Filename.chop_suffix input_file ".perk" ^ ".c" in

  let out_ast_file = Filename.chop_suffix input_file ".perk" ^ ".ast" in
  try
    let ast, compiled = process_file input_file in

    let oaf = open_out out_ast_file in
    output_string oaf ast;
    let oc = open_out out_file in
    output_string oc compiled;
    close_out oc
  with
  | Syntax_error ((start_line, start_col), (end_line, end_col), msg) ->
      Printf.eprintf
        "\027[31mSyntax error at line %d, column %d: %s, ending at line %d, \
         column %d\027[0m\n"
        start_line start_col msg end_line end_col;
      exit 1
  | Lexing_error ((start_line, start_col), (end_line, end_col), msg) ->
      Printf.eprintf
        "\027[31mLexing error at line %d, column %d: %s, ending at line %d, \
         column %d\027[0m\n"
        start_line start_col msg end_line end_col;
      exit 1
  | Type_error ((start_line, start_col), (end_line, end_col), msg) ->
      Printf.eprintf
        "\027[31mType error at line %d, column %d: %s, ending at line %d, \
         column %d\027[0m\n"
        start_line start_col msg end_line end_col;
      exit 1
  | Perkelang.Parser.Error ->
      Printf.eprintf "\027[31mParsing error: unexpected token\027[0m\n";
      exit 1

and process_file (filename : string) : string * string =
  let inchn = open_in filename in
  let ast_of_channel inchn =
    let lexbuf = Sedlexing.Latin1.from_channel inchn in
    let lexer = Sedlexing.with_tokenizer Perkelang.Lexer.token lexbuf in
    let parser =
      MenhirLib.Convert.Simplified.traditional2revised Perkelang.Parser.program
    in
    try parser lexer with
    | ParseError e ->
        raise
          (Syntax_error
             ( ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
                 (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
                 - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol ),
               ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
                 (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
                 - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol ),
               e ))
    | Perkelang.Parser.Error ->
        raise
          (Syntax_error
             ( ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
                 (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
                 - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol ),
               ( (snd (Sedlexing.lexing_positions lexbuf)).pos_lnum,
                 (snd (Sedlexing.lexing_positions lexbuf)).pos_cnum
                 - (snd (Sedlexing.lexing_positions lexbuf)).pos_bol ),
               "Unhandled parsing error. If this happens to you, please open \
                an issue on https://github.com/Alex23087/Perkelang/issues" ))
  in

  let ast = ast_of_channel inchn in
  let ast = typecheck_program ast in
  let out =
    ( String.concat "\n" (List.map show_topleveldef_a ast),
      ast |> codegen_program )
  in
  close_in inchn;
  out

and check_file (filename : string) : unit =
  try process_file filename |> ignore with
  | Syntax_error ((start_line, start_col), (end_line, end_col), msg) ->
      Printf.printf
        "{\"error\": \"syntax\", \"start_line\": %d, \"start_col\": %d, \
         \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\"}\n"
        start_line start_col end_line end_col (String.escaped msg);
      exit 0
  | Lexing_error ((start_line, start_col), (end_line, end_col), msg) ->
      Printf.printf
        "{\"error\": \"lexing\", \"start_line\": %d, \"start_col\": %d, \
         \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\"}\n"
        start_line start_col end_line end_col (String.escaped msg);
      exit 0
  | Type_error ((start_line, start_col), (end_line, end_col), msg) ->
      Printf.printf
        "{\"error\": \"typecheck\", \"start_line\": %d, \"start_col\": %d, \
         \"end_line\": %d, \"end_col\": %d, \"message\": \"%s\"}\n"
        start_line start_col end_line end_col (String.escaped msg);
      exit 0

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: compiler <file.perk> or compiler --check";
    exit 1);

  let first_arg = Sys.argv.(1) in
  if first_arg = "--check" then (
    if Array.length Sys.argv < 3 then (
      prerr_endline "Usage: compiler --check <file.perk>";
      exit 1);
    let filename = Sys.argv.(2) in
    ignore (check_file filename);
    exit 0);

  compile_program first_arg
