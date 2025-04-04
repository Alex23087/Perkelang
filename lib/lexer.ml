open Parser
open Errors

let string_buffer = Buffer.create 512
let digit = [%sedlex.regexp? '0' .. '9']
let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let hex_number = [%sedlex.regexp? Plus hex_digit]
let dec_number = [%sedlex.regexp? Plus digit]
let oct_number = [%sedlex.regexp? Plus '0' .. '7']

let float_number =
  [%sedlex.regexp? Plus digit, '.', Star digit | Star digit, '.', Plus digit]
(* Allow for numbers all of the following formats: 0.5, .5, 1. *)

let character = [%sedlex.regexp? 0x20 .. 0x7E]

let identifier =
  [%sedlex.regexp?
    ('a' .. 'z' | 'A' .. 'Z' | '_'), Star ('a' .. 'z' | 'A' .. 'Z' | digit | '_')]

let white_space = [%sedlex.regexp? ' ' | '\t' | '\n' | '\r']

let escape =
  [%sedlex.regexp?
    '\\', ('0' | '\'' | '"' | 'b' | 'f' | 't' | '\\' | 'r' | 'n')]

let stringchar = [%sedlex.regexp? 0x00 .. 0xFF]

let unescape ch lexbuf =
  match ch with
  | '0' -> char_of_int 0x00
  | '\'' -> '\''
  | '"' -> '"'
  | 'b' -> '\b'
  | 'f' -> char_of_int 0x0C
  | 't' -> '\t'
  | '\\' -> '\\'
  | 'r' -> '\r'
  | 'n' -> '\n'
  | _ ->
      let line = (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum in
      let col =
        (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
        - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol
      in
      raise (Lexing_error (line, col, "Invalid escape character"))

let rec token lexbuf =
  match%sedlex lexbuf with
  | "BEGIN_C" ->
      Buffer.clear string_buffer;
      inlineC lexbuf;
      InlineC (Buffer.contents string_buffer)
  | "+" -> Plus
  | "==" -> Eq
  | "<" -> Lt
  | "<=" -> Leq
  | ">" -> Gt
  | ">=" -> Geq
  | "-" -> Minus
  | "!" -> Bang
  | "fun" -> Fun
  | "=" -> Assign
  | "++" -> PlusPlus
  | "--" -> MinusMinus
  | "&" -> Ampersand
  | "." -> Dot
  | "..." -> Ellipsis
  | "?" -> Question
  | "nothing" | "none" -> Nothing
  | "something" | "some" | "just" -> Something
  (* | "☠" | "forget" | "forgor" -> Forgor *)
  | "if" -> If
  | "else" -> Else
  | "while" -> While
  | "do" -> Do
  | "for" -> For
  | "return" -> Return
  (*| "switch"        -> Switch
  | "break"         -> Break
  | "continue"      -> Continue *)
  | "skip" -> Skip
  | "let" -> Let
  | "public" -> Public
  | "private" -> Private
  | "static" -> Static
  | "extern" -> Extern
  | "const" -> Const
  | "volatile" -> Volatile
  | "restrict" -> Restrict
  | "import" -> Import
  | "open" -> Open
  | "true" -> Integer 1
  | "false" -> Integer 0
  | "archetype" | "theory" | "interface" | "prototype" | "trait" | "typeclass"
    ->
      Archetype (* TODO reinvent the wheel*)
  | "model" | "impl" | "class" -> Model
  | "summon" -> Summon
  | "banish" -> Banish
  | "->" -> Arrow
  | "=>" -> Bigarrow
  | identifier -> Ident (Sedlexing.Latin1.lexeme lexbuf)
  | "0x", hex_number -> Integer (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
  | "0b", Plus ('0' | '1') ->
      Integer (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
  | "0o", oct_number ->
      Integer
        (int_of_string
           ("0o"
           ^
           let s = Sedlexing.Latin1.lexeme lexbuf in
           String.sub s 2 (String.length s - 2)))
  | dec_number -> Integer (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
  | float_number -> Float (float_of_string (Sedlexing.Latin1.lexeme lexbuf))
  | "'" -> char lexbuf
  | '"' ->
      Buffer.clear string_buffer;
      string_literal lexbuf;
      String (Buffer.contents string_buffer)
  | white_space -> token lexbuf
  | "," -> Comma
  | ";" -> Semicolon
  | ":" -> Colon
  | "(" -> LParen
  | ")" -> RParen
  | "{" -> LBrace
  | "}" -> RBrace
  | "[" -> LBracket
  | "]" -> RBracket
  | "!" -> Bang
  | "*" | "×" -> Star
  | "/" -> Div
  | "//" -> comment lexbuf
  | "/*" -> multiline_comment lexbuf
  | eof -> EOF
  (* | inline_c_content, "}" -> INLINEC_CONTENT (Sedlexing.Latin1.lexeme lexbuf) *)
  | any ->
      let line = (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum in
      let col =
        (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
        - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol
      in
      raise
        (Lexing_error
           ( line,
             col,
             Printf.sprintf "Unrecognised character: '%s'"
               (Sedlexing.Latin1.lexeme lexbuf) ))
  | _ -> failwith "Impossible!"

and comment lexbuf =
  match%sedlex lexbuf with
  | "\n" -> token lexbuf
  | eof -> EOF
  | any -> comment lexbuf
  | _ -> failwith "Impossible!"

and multiline_comment lexbuf =
  match%sedlex lexbuf with
  | "*/" -> token lexbuf
  | eof -> EOF
  | any -> multiline_comment lexbuf
  | _ -> failwith "Impossible!"

and char lexbuf =
  match%sedlex lexbuf with
  | character, "'" -> Character (Sedlexing.Latin1.lexeme lexbuf).[0]
  | _ ->
      let line = (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum in
      let col =
        (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
        - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol
      in
      raise (Lexing_error (line, col, "Character not closed by a quote!"))

and inlineC lexbuf =
  match%sedlex lexbuf with
  | "END_C" -> ()
  | any ->
      Buffer.add_string string_buffer (Sedlexing.Latin1.lexeme lexbuf);
      inlineC lexbuf
  | _ ->
      let line = (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum in
      let col =
        (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
        - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol
      in
      raise (Lexing_error (line, col, "Inline C not closed by END_C!"))

and string_literal lexbuf =
  match%sedlex lexbuf with
  | '"' -> ()
  | eof ->
      let line = (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum in
      let col =
        (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
        - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol
      in
      raise (Lexing_error (line, col, "Unterminated string"))
  | escape ->
      let chara = Sedlexing.Latin1.lexeme lexbuf in
      Buffer.add_char string_buffer
        (match chara.[0] with '\\' -> unescape chara.[1] lexbuf | c -> c);
      string_literal lexbuf
  | stringchar ->
      Buffer.add_string string_buffer (Sedlexing.Latin1.lexeme lexbuf);
      string_literal lexbuf
  | _ ->
      (* Handle invalid characters in string literal *)
      let line = (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum in
      let col =
        (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
        - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol
      in
      raise (Lexing_error (line, col, "Invalid character in string literal"))

let tokenize (lexbuf : Sedlexing.lexbuf) = token lexbuf
