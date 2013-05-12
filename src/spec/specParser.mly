%{
(* necessary to open a different compilation unit
because exceptions directly defined here aren't visible outside
(e.g. SpecParser.Eof is raised, but Error: Unbound constructor
      if used to catch in a different module) *)
open SpecCore
%}

%token LT GT EQ NE LE GE
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
/* %token <Big_int.big_int> NUMBER */
%left LT GT EQ NE LE GE
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
  | EDGE stmts               { let a,ws,fwd,b = $1 in
                               Edge(a, ws, fwd, b, $2) }
;

stmts:
  | var EQ expr              { {lval = Some $1; exp = $3} } /* TODO expression would be better */
  | expr                     { {lval = None; exp = $1} }
/*  | stmts SEMICOLON expr     { (* $1^"; "^ *) $3 } */
;

var:
  | VAR_                     { Var_ }     /* $_ */
  | VAR                      { Vari $1 }  /* e.g. $foo, $123, $__ */
  | IDENT                    { Ident $1 } /* e.g. foo, _foo, _1, but not 1b */
;

expr:
  | LPAREN expr RPAREN       { $2 }
  | REGEX                    { Regex $1 }
  | STRING                   { String $1 }
  | BOOL                     { Bool $1 }
  | nexpr                    { Int $1 }
  | var                      { Var $1 }
  | IDENT args               { Fun {fname=$1; args=$2} } /* function */
  | UNDERS                   { Exp_ }
  | nexpr LT    nexpr        { Bool ($1<$3) }
  | nexpr GT    nexpr        { Bool ($1>$3) }
  | nexpr EQ EQ nexpr        { Bool ($1=$4) }
  | nexpr NE    nexpr        { Bool ($1<>$3) }
  | nexpr LE    nexpr        { Bool ($1<=$3) }
  | nexpr GE    nexpr        { Bool ($1>=$3) }
;

nexpr:
/*  | NUMBER                   { Big_int.int_of_big_int $1 } */
  | LPAREN nexpr RPAREN      { $2 }
  | INT                      { $1 }
  | nexpr PLUS nexpr         { $1 + $3 }
  | nexpr MINUS nexpr        { $1 - $3 }
  | nexpr MUL nexpr          { $1 * $3 }
  | nexpr DIV nexpr          { $1 / $3 }
  | MINUS nexpr %prec UMINUS { - $2 }
;

args:
  | LPAREN RPAREN            { [] }
  | LPAREN elems RPAREN      { $2 }
;

elems:
  | expr                     { [$1] }
  | elems COMMA expr         { $1 @ [$3] }
;
