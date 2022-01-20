{
  open MakefileParser
}

rule token = parse
  | ' ' { token lexbuf }
  | "\\\n" { token lexbuf }
  | '\n' { NEWLINE }
  | eof { EOF }
  | ':' { COLON }
  | [^':'' ''\n']+ { IDENTIFIER (Lexing.lexeme lexbuf) }
