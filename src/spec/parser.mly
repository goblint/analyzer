%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN LCURL RCURL LBRACK RBRACK
%token ASSIGN NULL COMMA SEMICOLON COLON UNDERS
%token EOL VAR_
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
  | EOL                      { "" }
  | def EOL                  { $1 }
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