(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
open Big_int
exception Eof
}

let nl = '\r'?'\n'
let endlinecomment = "//" [^'\n']* nl
let multlinecomment = "/*"([^'*']|('*'+[^'*''/'])|nl)*'*'+'/'
let comments = endlinecomment|multlinecomment

rule token = parse
  | [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | comments       { token lexbuf }     (* skip comments *)
  | ['\n' ]        { EOL }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | "="            { ASSIGN  }
  | "true"         { BOOL(true)   }
  | "false"        { BOOL(false)  }
  | "null"         { NULL   }
  | ","            { COMMA  }
  | ";"            { SEMICOLON  }
  | ":"            { COLON  }
  | "{"            { LCURL  }
  | "}"            { RCURL  }
  | "["            { LBRACK }
  | "]"            { RBRACK }
  | "_"            { UNDERS }
  | ('\"'[^'\"']*'\"') | ('\''[^'\'']*'\'')
      { let str = Lexing.lexeme lexbuf in
        let sl  = String.length str in
        STRING (String.sub str 1 (sl-2))
      }
(*  | ['0'-'9']*'.'?['0'-'9']*(('e'|'E')('+'|'-')?['0'-'9']+)?
      { NUMBER (big_int_of_string (Lexing.lexeme lexbuf)) } *)
  | "$"             { VAR }
  | ['a'-'z' 'A'-'Z' '_']+ as lxm { IDENT(lxm) }
  | eof            { EOF }