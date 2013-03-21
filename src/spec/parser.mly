%{
(* necessary to open a different compilation unit
because exceptions directly defined here aren't visible outside
(e.g. Parser.Eof is raised, but Error: Unbound constructor 
      if used to catch in a different module) *)
open Exc
%}

%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN LCURL RCURL LBRACK RBRACK
%token ASSIGN NULL COMMA SEMICOLON COLON UNDERS
%token EOL EOF VAR_
%token <string * string> NODE
%token <string * string> ARROW
%token <string> VAR
%token <string> IDENT
%token <string> STRING 
%token <bool> BOOL
%token <int> INT
/* %token <Big_int.big_int> NUMBER */
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

/* %{ type expr = String of string | Int of int %} */
%start file             /* the entry point */
%type <string> file
%%
file:
  | def EOL                  { $1 }
  | def EOF                  { $1 } /* no need for an empty line at the end */
  | EOL                      { raise Endl }
  | EOF                      { raise Eof }
;

def:
  | NODE                     { fst $1^"\t\""^snd $1^"\""}
  | ARROW stmt               { fst $1^" -> "^snd $1^"\t"^$2}
;

stmt:
  | expr                     { $1 }
  | var ASSIGN expr          { $1^" = "^$3 }
;

var:
  | VAR                      { "$"^$1 }
  | VAR_                     { "$_" }
  | IDENT                    { $1 }
;

expr:
  | LPAREN expr RPAREN       { $2 }
  | STRING                   { "\""^$1^"\"" }
  | nexpr                    { string_of_int $1 }
  | var                      { $1 }
  | IDENT args               { $1^"("^$2^")" } /* function */
  | UNDERS                   { "_" }
; 

nexpr:
/*  | NUMBER                   { Big_int.int_of_big_int $1 } */
  | LPAREN nexpr RPAREN      { $2 }
  | INT                      { $1 }
  | nexpr PLUS nexpr         { $1 + $3 }
  | nexpr MINUS nexpr        { $1 - $3 }
  | nexpr TIMES nexpr        { $1 * $3 }
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