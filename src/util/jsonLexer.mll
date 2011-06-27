{
  open Big_int
  open Json
  open JsonParser
  
}

rule token = parse
  | ['\t' '\n' ' ']   { token lexbuf }	
  | "//" [^'\n'] '\n' { token lexbuf }	
  | "/*" _* "*/"      { token lexbuf }	
  | "true"            { TRUE   }
  | "false"           { FALSE  }
  | "null"            { NULL   }
  | ","               { COMMA  }
  | ":"               { COLON  }
  | "{"               { LCURL  }
  | "}"               { RCURL  }
  | "["               { LBRACK }
  | "]"               { RBRACK }
  | ('\"'[^'\"']*'\"') | ('\''[^'\'']*'\'')
      { let str = Lexing.lexeme lexbuf in
        let sl  = String.length str in
        STRING (String.sub str 1 (sl-2))
      }
  | ['0'-'9']*'.'?['0'-'9']*(('e'|'E')('+'|'-')?['0'-'9']+)?
      { NUMBER (big_int_of_string (Lexing.lexeme lexbuf)) }

  