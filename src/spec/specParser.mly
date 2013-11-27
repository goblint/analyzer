%{
(* necessary to open a different compilation unit
because exceptions directly defined here aren't visible outside
(e.g. SpecParser.Eof is raised, but Error: Unbound constructor
      if used to catch in a different module) *)
open SpecCore
%}

%token EOL EOF
/* operators */
%token LPAREN RPAREN LCURL RCURL LBRACK RBRACK
%token PLUS MINUS MUL DIV MOD
%token LT GT EQEQ NE LE GE AND OR NOT
%token EQ COMMA SEMICOLON
/* literals, identifiers */
%token <bool> BOOL
%token NULL
%token <int> INT
%token <string> STRING
%token <string> IDENT
/* spec */
%token UNDERS COLON
%token <string> VAR
%token <string> REGEX
%token <string * string> NODE
%token <string * string list * bool * string> EDGE

/* precedence groups from low to high */
%right EQ
%left OR
%left AND
%left EQEQ NE
%left LT GT LE GE
%left PLUS MINUS
%left MUL DIV MOD
%right NOT UPLUS UMINUS DEREF

%start file
%type <SpecCore.def> file
%%
file:
  | def EOL                  { $1 }
  | def EOF                  { $1 } /* no need for an empty line at the end */
  | EOL                      { raise Endl } /* empty line */
  | EOF                      { raise Eof }  /* end of file */
;

def:
  | NODE                     { Node($1) }
  | EDGE stmt                { let a, ws, fwd, b = $1 in
                               Edge(a, ws, fwd, b, $2) }
;

stmt:
  | lval EQ expr             { {lval = Some $1; exp = $3} } /* TODO expression would be better */
  | expr                     { {lval = None; exp = $1} }
;

lval:
  | MUL lval %prec DEREF     { Ptr $2 }
  | IDENT                    { Ident $1 } /* C identifier, e.g. foo, _foo, _1, but not 1b */
  | VAR                      { Var $1 }  /* spec variable, e.g. $foo, $123, $__ */
;

expr:
  | LPAREN expr RPAREN       { $2 }
  | REGEX                    { Regex $1 }
  | STRING                   { String $1 }
  | BOOL                     { Bool $1 }
  | lval                     { Lval $1 }
  | IDENT args               { Fun {fname=$1; args=$2} } /* function */
  | UNDERS                   { Exp_ }
  | nexpr                    { Int $1 }
/*  | nexpr LT    nexpr        { Bool ($1<$3) }
  | nexpr GT    nexpr        { Bool ($1>$3) }
  | nexpr EQEQ  nexpr        { Bool ($1=$3) }
  | nexpr NE    nexpr        { Bool ($1<>$3) }
  | nexpr LE    nexpr        { Bool ($1<=$3) }
  | nexpr GE    nexpr        { Bool ($1>=$3) } */
  | expr  OR    expr         { Binop ("||", $1, $3) }
  | expr  AND   expr         { Binop ("&&", $1, $3) }
  | expr  EQEQ  expr         { Binop ("==", $1, $3) }
  | expr  NE    expr         { Binop ("!=", $1, $3) }
  | expr  LT    expr         { Binop ("<",  $1, $3) }
  | expr  GT    expr         { Binop (">",  $1, $3) }
  | expr  LE    expr         { Binop ("<=", $1, $3) }
  | expr  GE    expr         { Binop (">=", $1, $3) }
  | expr  PLUS  expr         { Binop ("+",  $1, $3) }
  | expr  MINUS expr         { Binop ("-",  $1, $3) }
  | expr  MUL   expr         { Binop ("*",  $1, $3) }
  | expr  DIV   expr         { Binop ("/",  $1, $3) }
  | expr  MOD   expr         { Binop ("%",  $1, $3) }
  | NOT   expr               { Unop  ("!",  $2) }
;

nexpr:
  | INT                      { $1 }
  | MINUS nexpr %prec UMINUS { - $2 }
  | PLUS  nexpr %prec UPLUS  { $2 }
/*  | LPAREN nexpr RPAREN      { $2 }
  | nexpr PLUS nexpr         { $1 + $3 }
  | nexpr MINUS nexpr        { $1 - $3 }
  | nexpr MUL nexpr          { $1 * $3 }
  | nexpr DIV nexpr          { $1 / $3 } */
;

args:
  | LPAREN RPAREN            { [] }
  | LPAREN expr_list RPAREN  { $2 }
;

expr_list:
  | expr                     { [$1] }
  | expr COMMA expr_list     { $1 :: $3 }
;
