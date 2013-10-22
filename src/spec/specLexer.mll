{
open SpecParser        (* The type token is defined in specParser.mli *)
open Big_int
exception Token of string
let line = ref 1
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let nl = '\r'?'\n'            (* new line *)
let s  = [' ' '\t']           (* whitespace *)
let w  = '_' | alpha | digit  (* word *)
let endlinecomment = "//" [^'\n']*
let multlinecomment = "/*"([^'*']|('*'+[^'*''/'])|nl)*'*'+'/'
let comments = endlinecomment | multlinecomment
let str = ('\"'(([^'\"']|"\\\"")* as s)'\"') | ('\''(([^'\'']|"\\'")* as s)'\'')

rule token = parse
  | s              { token lexbuf }     (* skip blanks *)
  | comments       { token lexbuf }     (* skip comments *)
  | nl             { incr line; EOL }

  (* operators *)
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '['            { LBRACK }
  | ']'            { RBRACK }
  | '{'            { LCURL  }
  | '}'            { RCURL  }
(*  | '.'            { DOT } *)
(*  | "->"           { ARROW } *)
  | '+'            { PLUS  }
  | '-'            { MINUS }
  | '*'            { MUL }
  | '/'            { DIV }
  | '%'            { MOD }
  | '<'            { LT }
  | '>'            { GT }
  | "=="           { EQEQ }
  | "!="           { NE }
  | "<="           { LE }
  | ">="           { GE }
  | "&&"           { AND }
  | "||"           { OR }
  | '!'            { NOT }
  | '='            { EQ }
  | ','            { COMMA  }
  | ';'            { SEMICOLON  }

  (* literals, identifiers *)
  | "true"         { BOOL(true)   }
  | "false"        { BOOL(false)  }
  | "null"         { NULL   }
  | digit+ as x    { INT(int_of_string x) }
  | str            { STRING(s) }
  | ('_'|alpha) w* as x    { IDENT(x) }

  (* spec *)
  | '_'            { UNDERS }
  | ':'            { COLON  }
  | "$_"           { VAR_ }
  | "$"(w+ as x)   { VAR(x) }
  | "r" str        { REGEX(s) }
  | (w+ as n) s+ str
                   { NODE(n, s) }
  | (w+ as a) s* "-" ((w+ ("," w+)*)? as ws) (">"? as fwd) ">" s* (w+ as b) s+
                   { EDGE(a, BatString.nsplit ws ",", fwd=">", b) }

  | eof            { EOF }
  | _ as x         { raise(Token (Char.escaped x^": unknown token in line "^string_of_int !line)) }