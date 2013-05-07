{
open SpecParser        (* The type token is defined in specParser.mli *)
open Big_int
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
let str = ('\"'(([^'\"']|"\\\"")* as s)'\"') | ('\''(([^'\'']|"\\'")* as s)'\'')

rule token = parse
  | ws             { token lexbuf }     (* skip blanks *)
  | comments       { token lexbuf }     (* skip comments *)
  | nl             { incr line; EOL }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '+'            { PLUS  }
  | '-'            { MINUS }
  | '*'            { MUL }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '<'            { LT }
  | '>'            { GT }
  | '='            { EQ }
  | "!="           { NE }
  | "<="           { LE }
  | ">="           { GE }
  | ','            { COMMA  }
  | ';'            { SEMICOLON  }
  | ':'            { COLON  }
  | '{'            { LCURL  }
  | '}'            { RCURL  }
  | '['            { LBRACK }
  | ']'            { RBRACK }
  | '_'            { UNDERS }
  | "true"         { BOOL(true)   }
  | "false"        { BOOL(false)  }
  | "null"         { NULL   }
  | (word+ as n) ws+ str
                   { NODE(n, s) }
  | (word+ as a) ws* "-" ((word+ ("," word+)*)? as ws) (">"? as fwd) ">" ws* (word+ as b) ws+
                   { EDGE(a, BatString.nsplit ws ",", fwd=">", b) }
  | str            { STRING(s) }
(*  | ['0'-'9']*'.'?['0'-'9']*(('e'|'E')('+'|'-')?['0'-'9']+)?
      { NUMBER (big_int_of_string (Lexing.lexeme lexbuf)) } *)
  | "$_"           { VAR_ }
  | "$"(word+ as x) { VAR(x) }
  | ('_'|alpha) word* as lxm { IDENT(lxm) }
  | eof            { EOF }
  | _ as x         { raise(Token (Char.escaped x^": unknown token in line "^string_of_int !line)) }