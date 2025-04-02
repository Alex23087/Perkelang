/* Tokens declarations */
%token EOF
%token Plus Eq Lt Leq Gt Geq Minus Star Div Times
%token Fun Assign If Else While Do For
%token <int> Number
%token <char> Character
%token <string> Ident
%token Comma Semicolon Colon LParen RParen LBrace RBrace LBracket RBracket Bang
%token Arrow Bigarrow
%token Skip Return Let
%token <string> InlineC

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
%type <Ast.perktype> perktype
%type <Ast.perktype> perkfuntype
%type <Ast.binop> binop
%type <Ast.unop> unop
%type <Ast.expr> expr
%type <Ast.perkident list> ident_list
%type <Ast.perktype list> perktype_list

%%

/* Grammar specification */

program:
  | c = command EOF { c }

command:
  | ic = InlineC                                                                                           { Ast.InlineC(ic) }
  | d = perkdef                                                                                            { Ast.Def d }
  | Fun pf = perkfun                                                                                       { Ast.Fundef pf }
  | i = Ident Assign e = expr                                                                              { Ast.Assign (i, e) }
  | If LParen e = expr RParen LBrace c1 = command RBrace Else LBrace c2 = command RBrace                   { Ast.IfThenElse (e, c1, c2) }
  | If LParen e = expr RParen LBrace c1 = command RBrace                                                   { Ast.IfThenElse (e, c1, Ast.Skip) }
  | While LParen e = expr RParen LBrace c = command RBrace                                                 { Ast.Whiledo (e, c) }
  | Do LBrace c = command RBrace While LParen e = expr RParen                                              { Ast.Dowhile (e, c) }
  | For LParen e1 = expr Semicolon e2 = expr Semicolon c3 = command RParen LBrace c = command RBrace       { Ast.For (e1, e2, c3, c) }
  | LBrace c = command RBrace                                                                              { Ast.Block(c) }
  | e = expr                                                                                               { Ast.Expr(e) }
  | c1 = command Semicolon c2 = command                                                                    { Ast.Seq (c1, c2) }
  | Skip                                                                                                   { Ast.Skip }
  | Return e = expr                                                                                        { Ast.Return (e) }

perkdef:
  | Let vd = perkvardesc Assign e = expr                                                                   { (vd, e) }

perkfun:
  | i = Ident LParen id_list = perkvardesc_list RParen Colon rt = perktype LBrace c = command RBrace       { Ast.Fun (rt, i, id_list, c) }
  | i = Ident LParen RParen Colon rt = perktype LBrace c = command RBrace                                  { Ast.Fun (rt, i, [], c) }

perkvardesc:
  | i = Ident Colon t = perktype                                                                           { (t, i) }
  
perkfuntype:
  | t1 = perktype Arrow t2 = perktype                                                                      { Ast.Funtype ([t1], t2) }
  | LParen tl = perktype_list RParen Arrow tf = perktype                                                   { Ast.Funtype (tl, tf) }

expr:
  | Star e = expr                                                                                          { Ast.Pointer e }
  | e1 = expr LParen args = separated_list(Comma, expr) RParen                                             { Ast.Apply (e1, args) }
  | e1 = expr b = binop e2 = expr                                                                          { Ast.Binop (b, e1, e2) }
  | u = unop e = expr Comma                                                                                { Ast.Unop (u, e) }
  | LParen id_list = perkvardesc_list RParen Colon ret = perktype Bigarrow LBrace c = command RBrace       { Ast.Lambda (ret, id_list, c) }
  | LParen RParen Colon ret = perktype Bigarrow LBrace c = command RBrace                                  { Ast.Lambda (ret, [], c) }
  | n = Number                                                                                             { Ast.Int (n) }
  | i = Ident                                                                                              { Ast.Var(i) }
  | LParen e = expr RParen                                                                                 { e }

perktype:
  | LParen t = perktype RParen                                                                             { t }      
  | i = Ident                                                                                              { Ast.Basetype i }
  | LBracket t = perktype RBracket                                                                         { Ast.Arraytype (t, None) }
  | LBracket t = perktype n = Number RBracket                                                              { Ast.Arraytype (t, Some n) }
  | t = perktype Star                                                                                      { Ast.Pointertype t }
  | t = perkfuntype                                                                                        { t }

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

%inline unop:
  | Minus                                                                                                  { Ast.Neg }
  | Bang                                                                                                   { Ast.Not }


/* New nonterminals for disambiguated lists */

ident_list:
  | i = Ident { [i] }
  | il = ident_list Comma i = Ident { il @ [i] }

perktype_list:
  | t = perktype { [t] }
  | tl = perktype Star t = perktype_list { tl :: t }

perkvardesc_list:
  | t = perkvardesc { [t] }
  | tl = perkvardesc_list Comma t = perkvardesc { tl @ [t] }

%%
