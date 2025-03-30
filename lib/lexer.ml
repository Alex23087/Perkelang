open Parser

let string_buffer = Buffer.create 512

let digit = [%sedlex.regexp? '0' .. '9']
let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let hexNumber = [%sedlex.regexp? Plus hex_digit]
let number = [%sedlex.regexp? Plus digit]
let character = [%sedlex.regexp? 0x20 .. 0x7E]
let identifier = [%sedlex.regexp? ('a'..'z' | 'A'..'Z' | '_') , Star ('a'..'z' | 'A'..'Z' | digit | '_')]
let white_space = [%sedlex.regexp? ' ' | '\t' | '\n' | '\r']

let rec token lexbuf =
  match%sedlex lexbuf with
  | "BEGIN_C"       -> (
    Buffer.clear string_buffer;
    inlineC lexbuf;
    InlineC (Buffer.contents string_buffer)
  )
  | "+"             -> Plus
  | "=="            -> Eq
  | "<"             -> Lt
  | "<="            -> Leq
  | ">"             -> Gt
  | ">="            -> Geq
  | "-"             -> Minus
  | "!"             -> Bang
  | "fun"           -> Fun
  | "="             -> Assign
  | "if"            -> If
  | "else"          -> Else
  | "while"         -> While
  | "do"            -> Do
  | "for"           -> For
  | "return"        -> Return
  (*| "switch"        -> Switch
  | "break"         -> Break
  | "continue"      -> Continue *)
  | "skip"          -> Skip
  | "let"           -> Let
  | "public"        -> Public
  | "private"       -> Private
  | "static"        -> Static
  | "extern"        -> Extern
  | "const"         -> Const
  | "volatile"      -> Volatile
  | "restrict"      -> Restrict
  | "->"            -> Arrow
  | "=>"            -> Bigarrow
  | identifier      -> Ident (Sedlexing.Latin1.lexeme lexbuf)
  | number          -> Number (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
  | "0x", hexNumber -> Number (int_of_string ("0x" ^ (let x = Sedlexing.Latin1.lexeme lexbuf in String.sub x 2 (String.length x))))
  | "'"             -> char lexbuf
  | white_space     -> token lexbuf
  | ","             -> Comma
  | ";"             -> Semicolon
  | ":"             -> Colon
  | "("             -> LParen
  | ")"             -> RParen
  | "{"             -> LBrace
  | "}"             -> RBrace
  | "["             -> LBracket
  | "]"             -> RBracket
  | "!"             -> Bang
  | "*" | "×"       -> Star
  | "/"             -> Div
  | "//"            -> comment lexbuf
  | "/*"            -> multiline_comment lexbuf
  | eof             -> EOF
  (* | inline_c_content, "}" -> INLINEC_CONTENT (Sedlexing.Latin1.lexeme lexbuf) *)
  | any             -> failwith (Printf.sprintf "Unrecognised character: '%s'" (Sedlexing.Latin1.lexeme lexbuf))
  | _               -> failwith "Impossible!"

and comment lexbuf =
  match%sedlex lexbuf with
  | "\n"            -> token lexbuf
  | eof             -> EOF
  | any             -> comment lexbuf
  | _               -> failwith "Impossible!"

and multiline_comment lexbuf =
  match%sedlex lexbuf with
  | "*/"            -> token lexbuf
  | eof             -> EOF
  | any             -> multiline_comment lexbuf
  | _               -> failwith "Impossible!"

and char lexbuf =
  match%sedlex lexbuf with
  | character, "'"  -> Character (Sedlexing.Latin1.lexeme lexbuf).[0]
  | _               -> failwith "Character not closed by a quote!"

and inlineC lexbuf =
  match%sedlex lexbuf with
  | "END_C" -> ()
  | any -> (Buffer.add_string string_buffer (Sedlexing.Latin1.lexeme lexbuf); inlineC lexbuf)
  | _ -> failwith "Inline C not closed by END_C!"

let tokenize (lexbuf: Sedlexing.lexbuf) =
  token lexbuf
