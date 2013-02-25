/* File parser.mly */
%{
    exception Eof
%}
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token ASSIGN NULL COMMA SEMICOLON COLON
%token LCURL RCURL LBRACK RBRACK
%token UNDERS VAR IDENT EOF
%token <string> IDENT
%token <string> STRING 
%token <bool> BOOL
%token <int> INT
/* %token <Big_int.big_int> NUMBER */
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

/* %{ type expr = String of string | Int of int %} */
%start file             /* the entry point */
%type <string> file
%%
file:
  | stmt SEMICOLON endl      { $1 }
  | EOF                      { raise Eof }
;

endl:
  | EOL                      { "" }
  | EOF                      { "" }
;

var:
  | VAR INT                  { "$"^(string_of_int $2) }
  | VAR UNDERS               { "$_" }
  | IDENT                    { $1 }
;

stmt:
  | expr                     { $1 }
  | var ASSIGN expr          { $1^" = "^$3 }
;

args:
  | LPAREN RPAREN           { "" }
  | LPAREN elems RPAREN     { $2 }
;

elems:
  | expr                     { $1 }
  | elems COMMA expr         { $1^", "^$3 }
;

expr:
  | STRING                   { "\""^$1^"\"" }
  | nexpr                    { string_of_int $1 }
  | LPAREN expr RPAREN       { $2 }
  | var                      { $1 }
  | IDENT args { $1^"("^$2^")" } /* function */
; 

nexpr:
/*  | NUMBER                   { Big_int.int_of_big_int $1 } */
  | INT                      { $1 }
  | nexpr PLUS nexpr         { $1 + $3 }
  | nexpr MINUS nexpr        { $1 - $3 }
  | nexpr TIMES nexpr        { $1 * $3 }
  | nexpr DIV nexpr          { $1 / $3 }
  | MINUS nexpr %prec UMINUS { - $2 }
;