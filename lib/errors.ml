exception ParseError of string
exception Syntax_error of (int * int) * (int * int) * string
exception Lexing_error of (int * int) * (int * int) * string
exception Type_error of (int * int) * (int * int) * string
exception Type_match_error of string
exception Double_declaration of string
exception Undeclared of string
