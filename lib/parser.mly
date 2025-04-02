%{
  open Errors
%}


/* Tokens declarations */
%token EOF
%token Plus Eq Lt Leq Gt Geq Minus Star Div Ampersand PlusPlus MinusMinus
%token Fun Assign If Else While Do For
%token <int> Number
%token <char> Character
%token <string> String
%token <string> Ident
%token Comma Semicolon Colon LParen RParen LBrace RBrace LBracket RBracket Bang
%token Arrow Bigarrow
%token Skip Return Let
%token Public Private Static Extern
%token Const Volatile Restrict
%token <string> InlineC
%token Import Open
%token Archetype Model

/* Precedence and associativity specification */
%left Plus Minus
%left Lt Leq Gt Geq
%left Eq
%left Arrow

/* Starting symbol */
%start program
%type <Ast.command> program
%type <Ast.command> command
%type <Ast.perkdef> perkdef
%type <Ast.perkvardesc> perkvardesc
%type <Ast.perkfun> perkfun
%type <Ast.perktype_complete> perktype_complete
%type <Ast.perktype> perktype
%type <Ast.perktype> perkfuntype
%type <Ast.binop> binop
%type <Ast.preunop> preunop
%type <Ast.postunop> postunop
%type <Ast.expr> expr
%type <Ast.perkident list> ident_list
%type <Ast.perktype_complete list> perktype_list

// %on_error_reduce command

%%

/* Grammar specification */

program:
  | c = command EOF { c }

command:
  | Import i = String                                                                                      { Ast.Import ("<" ^ i ^ ">") }
  | Open i = String                                                                                        { Ast.Import ("\"" ^ i ^ "\"") }
  | ic = InlineC                                                                                           { Ast.InlineC(ic) }
  | d = perkdef                                                                                            { Ast.Def d }
  | Fun pf = perkfun                                                                                       { Ast.Fundef pf }
  | l = expr Assign r = expr                                                                               { Ast.Assign (l, r) }
  | If LParen e = expr RParen LBrace c1 = command RBrace Else LBrace c2 = command RBrace                   { Ast.IfThenElse (e, c1, c2) }
  | If LParen e = expr RParen LBrace c1 = command RBrace                                                   { Ast.IfThenElse (e, c1, Ast.Skip) }
  | While LParen e = expr RParen LBrace c = command RBrace                                                 { Ast.Whiledo (e, c) }
  | Do LBrace c = command RBrace While LParen e = expr RParen                                              { Ast.Dowhile (e, c) }
  | For LParen c1 = command Semicolon e2 = expr Semicolon c3 = command RParen LBrace body = command RBrace { Ast.For (c1, e2, c3, body) }
  | LBrace c = command RBrace                                                                              { Ast.Block(c) }
  | e = expr                                                                                               { Ast.Expr(e) }
  | c1 = command Semicolon c2 = command                                                                    { Ast.Seq (c1, c2) }
  | c1 = command Semicolon                                                                                 { c1 }
  | Skip                                                                                                   { Ast.Skip }
  | Return e = expr                                                                                        { Ast.Return (e) }

  | Archetype i = Ident LBrace l = perkvardesc_list RBrace                                                 { Ast.Archetype (i, l) }
  | Model i = Ident Colon il = ident_list LBrace l = perkdef_list RBrace                                   { Ast.Model (i, il, l) }


  | error                                                                                                  { raise (ParseError("command expected")) }
  | command error                                                                                          { raise (ParseError("unexpected command (perhaps you are missing a ';'?)")) }
  | For LParen command Semicolon expr Semicolon command RParen error                                       { raise (ParseError("missing braces after for guard"))}
  | If LParen expr RParen LBrace command RBrace Else error                                                 { raise (ParseError("missing braces after else"))}
  | If LParen expr RParen error                                                                            { raise (ParseError("missing braces after if guard"))}
  | While LParen expr RParen error                                                                         { raise (ParseError("missing braces after while guard"))}
  | Do error                                                                                               { raise (ParseError("missing braces after do"))}
  


perkdef:
  | Let vd = perkvardesc Assign e = expr                                                                   { (vd, e) }
  | error { raise (ParseError("definition expected (e.g. let banana : int = 5)")) }

perkfun:
  | i = Ident LParen id_list = perkvardesc_list RParen Colon rt = perktype_complete LBrace c = command RBrace       { Ast.Fun (rt, i, id_list, c) }
  | i = Ident LParen RParen Colon rt = perktype_complete LBrace c = command RBrace                                  { Ast.Fun (rt, i, [], c) }
  

perkvardesc:
  | i = Ident Colon t = perktype_complete                                                                  { (t, i) }
  | error { raise (ParseError("variable descriptor expected (e.g. banana : int)")) }
  
%inline perkfuntype:
  | t1 = perktype_complete Arrow t2 = perktype_complete                                                    { Ast.Funtype ([t1], t2) }
  | LParen tl = perktype_list RParen Arrow tf = perktype_complete                                          { Ast.Funtype (tl, tf) }

expr:
  | Star e = expr                                                                                          { Ast.Pointer e }
  | e1 = expr LParen args = separated_list(Comma, expr) RParen                                             { Ast.Apply (e1, args) }
  | e1 = expr b = binop e2 = expr                                                                          { Ast.Binop (b, e1, e2) }
  | u = preunop e = expr                                                                                   { Ast.PreUnop (u, e) }
  | e = expr u = postunop                                                                                  { Ast.PostUnop (u, e) }
  | LParen id_list = perkvardesc_list RParen Colon ret = perktype_complete Bigarrow LBrace c = command RBrace       { Ast.Lambda (ret, id_list, c) }
  | LParen RParen Colon ret = perktype_complete Bigarrow LBrace c = command RBrace                                  { Ast.Lambda (ret, [], c) }
  | n = Number                                                                                             { Ast.Int (n) }
  | c = Character                                                                                          { Ast.Char (c) }
  | s = String                                                                                             { Ast.String (s) }
  | i = Ident                                                                                              { Ast.Var(i) }
  | LParen e = expr RParen                                                                                 { Ast.Parenthesised e }
  | e1 = expr LBracket e2 = expr RBracket                                                                  { Ast.Subscript (e1, e2) }
  | error                                                                                                  { raise (ParseError("expression expected")) }
  | expr error                                                                                             { raise (ParseError("unexpected expression")) }
  | Ident error                                                                                            { raise (ParseError("unexpected expression. Perhaps you tried to use C-style types?")) }

%inline perktype_attribute:
  | Public                                                                                                 { Ast.Public }
  | Private                                                                                                { Ast.Private }
  | Static                                                                                                 { Ast.Static }
  | Extern                                                                                                 { Ast.Extern }

%inline perktype_qualifier:
  | Const                                                                                                  { Ast.Const }
  | Volatile                                                                                               { Ast.Volatile }
  | Restrict                                                                                               { Ast.Restrict }

perktype_complete:
  | t = perktype q = list(perktype_qualifier)                                                              { ([], t, q) }
  | t = perkfuntype q = list(perktype_qualifier)                                                           { ([], t, q) }
  | LParen t = perktype_complete RParen                                                                    { t }
  | error                                                                                                  { raise (ParseError("type expected")) }

perktype:
  | i = Ident                                                                                              { Ast.Basetype i }
  | LBracket t = perktype_complete RBracket                                                                { Ast.Arraytype (t, None) }
  | LBracket t = perktype_complete n = Number RBracket                                                     { Ast.Arraytype (t, Some n) }
  | t = perktype_complete Star                                                                             { Ast.Pointertype t }
  | error                                                                                                  { raise (ParseError("type expected")) }

%inline binop:
  | Plus                                                                                                   { Ast.Add }
  | Minus                                                                                                  { Ast.Sub }
  | Star                                                                                                   { Ast.Mul }
  | Div                                                                                                    { Ast.Div }
  | Eq                                                                                                     { Ast.Eq }
  | Lt                                                                                                     { Ast.Lt }
  | Leq                                                                                                    { Ast.Leq }
  | Gt                                                                                                     { Ast.Gt }
  | Geq                                                                                                    { Ast.Geq }

%inline preunop:
  | Minus                                                                                                  { Ast.Neg }
  | Bang                                                                                                   { Ast.Not }
  | Ampersand                                                                                              { Ast.Reference }
  | Star                                                                                                   { Ast.Dereference }
  | PlusPlus                                                                                               { Ast.PreIncrement }
  | MinusMinus                                                                                             { Ast.PreDecrement }

%inline postunop:
  | PlusPlus                                                                                               { Ast.PostIncrement }
  | MinusMinus                                                                                             { Ast.PostDecrement }


/* New nonterminals for disambiguated lists */

ident_list:
  | i = Ident { [i] }
  | il = ident_list Comma i = Ident { il @ [i] }
  | error { raise (ParseError("identifier expected")) }
  | Ident error { raise (ParseError("unexpected identifier")) }

perktype_list:
  | t = perktype_complete { [t] }
  | tl = perktype_complete Star t = perktype_list { tl :: t }
  | error { raise (ParseError("type expected")) }
  | perktype_complete error { raise (ParseError("unexpected type")) }

perkdef_list:
  | t = perkdef { [t] }
  | tl = perkdef Comma t = perkdef_list { tl :: t }
  | error { raise (ParseError("definition expected")) }
  | perkdef error { raise (ParseError("unexpected definition")) }

perkvardesc_list:
  | t = perkvardesc { [t] }
  | tl = perkvardesc Comma t = perkvardesc_list { tl :: t }
  | error { raise (ParseError("variable descriptor expected")) }
  | perkvardesc error { raise (ParseError("unexpected variable descriptor")) }

spanish_inquisition:
  | error { raise (ParseError("Nobody expects the Spanish Inquisition!")) }
%%