type token =
  | RBRACK
  | LBRACK
  | RCURL
  | LCURL
  | COLON
  | COMMA
  | TRUE
  | FALSE
  | NULL
  | STRING of (string)
  | NUMBER of (Big_int.big_int)

val value :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Json.jvalue
