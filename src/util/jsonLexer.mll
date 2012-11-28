{
  open Big_int
  open Json
  open JsonParser
  
}

let nl = '\r'?'\n'
let endlinecomment = "//" [^'\n']* nl
let multlinecomment = "/*"([^'*']|('*'+[^'*''/'])|nl)*'*'+'/'
let comments = endlinecomment|multlinecomment

rule token = parse
  | ['\t' '\n' '\r' ' ']    { token lexbuf }	
  | comments		    { token lexbuf }
  | "true"                  { TRUE   }
  | "false"                 { FALSE  }
  | "null"                  { NULL   }
  | ","                     { COMMA  }
  | ":"                     { COLON  }
  | "{"                     { LCURL  }
  | "}"                     { RCURL  }
  | "["                     { LBRACK }
  | "]"                     { RBRACK }
  | ('\"'[^'\"']*'\"') | ('\''[^'\'']*'\'')
      { let str = Lexing.lexeme lexbuf in
        let sl  = String.length str in
        STRING (String.sub str 1 (sl-2))
      }
  | ['0'-'9']*'.'?['0'-'9']*(('e'|'E')('+'|'-')?['0'-'9']+)?
      { NUMBER (big_int_of_string (Lexing.lexeme lexbuf)) }

  