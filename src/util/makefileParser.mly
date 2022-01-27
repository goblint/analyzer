%token <string> IDENTIFIER
%token COLON
%token NEWLINE
%token EOF

%type <(string list * string list) list> deps

%start deps

%%

deps:
  | identifier_nonempty_list COLON identifier_list NEWLINE deps { ($1, $3) :: $5 }
  | NEWLINE identifier_nonempty_list COLON identifier_list NEWLINE deps { ($2, $4) :: $6 }
  | EOF { [] }
  ;

identifier_list:
  | { [] }
  | identifier_nonempty_list { $1 }
  ;

identifier_nonempty_list:
  | IDENTIFIER { [$1] }
  | IDENTIFIER identifier_nonempty_list { $1 :: $2 }
  ;
