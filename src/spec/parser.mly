/* File parser.mly */
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token ASSIGN NULL COMMA SEMICOLON COLON
%token LCURL RCURL LBRACK RBRACK
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
  | expr EOL                 { $1 }
; 

expr:
  | STRING                   { $1 }
  | nexpr                    { string_of_int $1 }
  | LPAREN expr RPAREN       { $2 }
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