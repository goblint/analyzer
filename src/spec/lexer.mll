{
open Parser        (* The type token is defined in parser.mli *)
open Big_int
exception Eof
exception Token of string
let line = ref 1
}

let nl = '\r'?'\n'
let ws = [' ' '\t']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum = alpha | digit
let word  = '_' | alnum
let endlinecomment = "//" [^'\n']* nl
let multlinecomment = "/*"([^'*']|('*'+[^'*''/'])|nl)*'*'+'/'
let comments = endlinecomment|multlinecomment

rule token = parse
  | ws+            { token lexbuf }     (* skip blanks *)
  | comments       { token lexbuf }     (* skip comments *)
  | nl             { incr line; EOL }
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
  | (word+ as n) ws* '\"'(([^'\"']|"\\\"")* as m)'\"'  { NODE(n, m)}
  | (word+ as a) ws* "->" ws* (word+ as b) ws+         { ARROW(a, b) }
  | ('\"'([^'\"']|"\\\"")*'\"') | ('\''([^'\'']|"\\'")*'\'')
      { let str = Lexing.lexeme lexbuf in
        let sl  = String.length str in
        STRING (String.sub str 1 (sl-2))
      }
(*  | ['0'-'9']*'.'?['0'-'9']*(('e'|'E')('+'|'-')?['0'-'9']+)?
      { NUMBER (big_int_of_string (Lexing.lexeme lexbuf)) } *)
  | "$_"           { VAR_ }
  | "$"(word+ as x) { VAR(x) }
  | ('_'|alpha) word* as lxm { IDENT(lxm) }
  | eof            { raise Eof }
  | _ as x         { raise(Token (Char.escaped x^": unknown token in line "^string_of_int !line)) }