%{
(* necessary to open a different compilation unit
because exceptions directly defined here aren't visible outside
(e.g. SpecParser.Eof is raised, but Error: Unbound constructor
      if used to catch in a different module) *)
open Def
%}

%token LT GT EQ NE LE GE
%token PLUS MINUS MUL DIV
%token LPAREN RPAREN LCURL RCURL LBRACK RBRACK
%token NULL COMMA SEMICOLON COLON UNDERS
%token EOL EOF VAR_
%token <string * string> NODE
%token <string * string> EDGE
%token <string> VAR
%token <string> IDENT
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
%type <Def.def> file
%%
file:
  | def EOL                  { $1 }
  | def EOF                  { $1 } /* no need for an empty line at the end */
  | EOL                      { raise Endl } /* empty line */
  | EOF                      { raise Eof }  /* end of file */
;

def:
  | NODE                     { Node($1) }
  | EDGE stmts               { Edge(fst $1, snd $1, $2) }
;

stmts:
  | expr                     { $1 }
  | stmts SEMICOLON expr     { $1^"; "^$3 }
;

var:
  | VAR                      { "$"^$1 }
  | VAR_                     { "$_" }
  | IDENT                    { $1 }
;

expr:
  | LPAREN expr RPAREN       { $2 }
  | STRING                   { "\""^$1^"\"" }
  | BOOL                     { string_of_bool $1 }
  | nexpr                    { string_of_int $1 }
  | var                      { $1 }
  | var EQ expr              { $1^" = "^$3 }   /* no need for extra stmt (avoids shift/reduce conflict) */
  | IDENT args               { $1^"("^$2^")" } /* function */
  | UNDERS                   { "_" }
  | nexpr LT    nexpr        { string_of_bool($1<$3) }
  | nexpr GT    nexpr        { string_of_bool($1>$3) }
  | nexpr EQ EQ nexpr        { string_of_bool($1=$4) }
  | nexpr NE    nexpr        { string_of_bool($1<>$3) }
  | nexpr LE    nexpr        { string_of_bool($1<=$3) }
  | nexpr GE    nexpr        { string_of_bool($1>=$3) }
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
  | LPAREN RPAREN            { "" }
  | LPAREN elems RPAREN      { $2 }
;

elems:
  | expr                     { $1 }
  | elems COMMA expr         { $1^", "^$3 }
;
