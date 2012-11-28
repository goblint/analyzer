{
  open OilParser
}

let nl = '\r'?'\n'
let endlinecomment = "//" [^'\n']* nl
let multlinecomment = "/*"([^'*']|('*'+[^'*''/'])|nl)*'*'+'/'
let comments = endlinecomment|multlinecomment
let include = "#include" [^'\n']* nl

let hex_digit = ['0'-'9''a'-'f''A'-'F']
let zero_digit = '0'
let dec_digit = ['0'-'9']
let pos_digit = ['1'-'9']
let dec_digits = (dec_digit)+
let int_digits = (zero_digit | pos_digit | pos_digit (dec_digits))
let exponent = ('e'|'E')?
let sign = ('+'|'-')?

let hex_number = ("0x"|"0X")(hex_digit)+
let float = ((sign)(dec_digits)'.'(dec_digits) (exponent))
let dec_number = (sign)(int_digits)
let number = (dec_number | hex_number)
let true = ('t'|'T')('r'|'R')('u'|'U')('e'|'E')
let false = ('f'|'F')('a'|'A')('l'|'L')('s'|'S')('e'|'E')
let name = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*
let oil_string = '\"'[^'\"']*'\"'
let object = "OS" | "TASK" | "COUNTER" | "ALARM" | "RESOURCE" | "EVENT" | "ISR" | "MESSAGE" | "COM" | "NM" | 
"APPMODE" | "IPDU"
let object_ref_type = "OS_TYPE" | "TASK_TYPE" | "COUNTER_TYPE" | "ALARM_TYPE" | "RESOURCE_TYPE" | "EVENT_TYPE" | "ISR_TYPE" | "MESSAGE_TYPE" | "COM_TYPE" | "NM_TYPE" | "APPMODE_TYPE" | "IPDU_TYPE"



rule token = parse
  | ['\t'' '] | nl  	{ token lexbuf }	
  | comments   		{ token lexbuf }
  | include		{ token lexbuf }	
  | "="               	{ ASSIGN  }
  | ","               	{ COMMA  }
  | ":"               	{ COLON  }
  | ";"               	{ SEMICOLON  }
  | "{"               	{ LCURL  }
  | "}"               	{ RCURL  }
  | "["               	{ LBRACK }
  | "]"               	{ RBRACK }
  | "CPU"	     	{ CPU }
  | "AUTO"	     	{ AUTO }
  | "NO_DEFAULT"	{ NO_DEFAULT }
  | ".."		{ DOTS }
  | "WITH_AUTO"		{ WITH_AUTO }
  | "BOOLEAN"	     	{ BOOLEAN }
  | "STRING"	     	{ STRING }
  | "ENUM"		{ ENUM }
  | "FLOAT"		{ FLOAT }
  | "INT64"		{ INT64 }
  | "UINT64"	     	{ UINT64 }
  | "INT32"	     	{ INT32 }
  | "UINT32"		{ UINT32 }
  | "IMPLEMENTATION"	{ IMPLEMENTATION }
  | "OIL_VERSION"	{ OIL_VERSION }
  | number	      	{ NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | float    	      	{ OIL_FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | true	      	{ BOOL (true) }
  | false	      	{ BOOL (false) }
  | object	      	{ OBJECT (Lexing.lexeme lexbuf) }
  | object_ref_type   	{ OBJECT_TYPE (Lexing.lexeme lexbuf) }
  | name	      	{ NAME (Lexing.lexeme lexbuf) }
  | oil_string	      	{ OIL_STRING (Lexing.lexeme lexbuf) }
  | _ as c { Printf.printf "Unrecognized character: %c\n" c; raise (Failure "") }
  | eof			{ EOF  }