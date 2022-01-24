%token <string> IDENTIFIER
%token COLON
%token NEWLINE
%token EOF

%type <string * string list> deps

%start deps

%%

deps:
  | IDENTIFIER COLON identifier_list NEWLINE EOF { ($1, $3) }
  ;

identifier_list:
  | { [] }
  | identifier_nonempty_list { $1 }
  ;

identifier_nonempty_list:
  | IDENTIFIER { [$1] }
  | IDENTIFIER identifier_nonempty_list { $1 :: $2 }
  ;
