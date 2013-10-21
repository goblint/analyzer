%{
(* necessary to open a different compilation unit
because exceptions directly defined here aren't visible outside
(e.g. SpecParser.Eof is raised, but Error: Unbound constructor
      if used to catch in a different module) *)
open SpecCore
%}

%token LT GT EQ NE LE GE EQEQ
%token PLUS MINUS MUL DIV
%token LPAREN RPAREN LCURL RCURL LBRACK RBRACK
%token NULL COMMA SEMICOLON COLON UNDERS
%token EOL EOF VAR_
%token <string * string> NODE
%token <string * string list * bool * string> EDGE
%token <string> VAR
%token <string> IDENT
%token <string> REGEX
%token <string> STRING
%token <bool> BOOL
%token <int> INT
%left LT GT EQ NE LE GE EQEQ
%left PLUS MINUS        /* lowest precedence */
%left MUL DIV           /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

/* %{ type expr = String of string | Int of int %} */
%start file             /* the entry point */
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
  | VAR_                     { Var_ }     /* $_ */
  | VAR                      { Vari $1 }  /* e.g. $foo, $123, $__ */
  | IDENT                    { Ident $1 } /* e.g. foo, _foo, _1, but not 1b */
;

expr:
  | LPAREN expr RPAREN       { $2 }
  | REGEX                    { Regex $1 }
  | STRING                   { String $1 }
  | BOOL                     { Bool $1 }
  | lval                     { Var $1 }
  | IDENT args               { Fun {fname=$1; args=$2} } /* function */
  | UNDERS                   { Exp_ }
  | nexpr                    { Int $1 }
/*  | nexpr LT    nexpr        { Bool ($1<$3) }
  | nexpr GT    nexpr        { Bool ($1>$3) }
  | nexpr EQEQ  nexpr        { Bool ($1=$3) }
  | nexpr NE    nexpr        { Bool ($1<>$3) }
  | nexpr LE    nexpr        { Bool ($1<=$3) }
  | nexpr GE    nexpr        { Bool ($1>=$3) } */
  | expr  LT    expr         { Binop ("<",  $1, $3) }
  | expr  GT    expr         { Binop (">",  $1, $3) }
  | expr  EQEQ  expr         { Binop ("==", $1, $3) }
  | expr  NE    expr         { Binop ("!=", $1, $3) }
  | expr  LE    expr         { Binop ("<=", $1, $3) }
  | expr  GE    expr         { Binop (">=", $1, $3) }
  | expr  PLUS  expr         { Binop ("+",  $1, $3) }
  | expr  MINUS expr         { Binop ("-",  $1, $3) }
  | expr  MUL   expr         { Binop ("*",  $1, $3) }
  | expr  DIV   expr         { Binop ("/",  $1, $3) }
;

nexpr:
  | INT                      { $1 }
  | MINUS nexpr %prec UMINUS { - $2 }
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
