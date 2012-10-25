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

open Parsing;;
# 2 "jsonParser.mly"
  open Json
# 18 "jsonParser.ml"
let yytransl_const = [|
  257 (* RBRACK *);
  258 (* LBRACK *);
  259 (* RCURL *);
  260 (* LCURL *);
  261 (* COLON *);
  262 (* COMMA *);
  263 (* TRUE *);
  264 (* FALSE *);
  265 (* NULL *);
    0|]

let yytransl_block = [|
  266 (* STRING *);
  267 (* NUMBER *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\003\000\003\000\004\000\004\000\005\000\005\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\003\000\002\000\003\000\005\000\002\000\003\000\001\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\013\000\014\000\015\000\009\000\
\010\000\016\000\011\000\012\000\005\000\007\000\000\000\002\000\
\000\000\000\000\006\000\000\000\000\000\001\000\000\000\008\000\
\003\000\000\000\000\000\004\000"

let yydgoto = "\002\000\
\010\000\011\000\018\000\012\000\015\000"

let yysindex = "\002\000\
\012\255\000\000\000\255\002\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\255\000\000\
\001\255\023\255\000\000\012\255\012\255\000\000\003\255\000\000\
\000\000\010\255\012\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\253\255\000\000\000\000\000\000\000\000"

let yytablesize = 30
let yytable = "\014\000\
\013\000\003\000\001\000\004\000\016\000\021\000\005\000\006\000\
\007\000\008\000\009\000\017\000\026\000\003\000\027\000\004\000\
\024\000\025\000\005\000\006\000\007\000\008\000\009\000\028\000\
\019\000\022\000\000\000\000\000\023\000\020\000"

let yycheck = "\003\000\
\001\001\002\001\001\000\004\001\003\001\005\001\007\001\008\001\
\009\001\010\001\011\001\010\001\010\001\002\001\005\001\004\001\
\020\000\021\000\007\001\008\001\009\001\010\001\011\001\027\000\
\001\001\003\001\255\255\255\255\006\001\006\001"

let yynames_const = "\
  RBRACK\000\
  LBRACK\000\
  RCURL\000\
  LCURL\000\
  COLON\000\
  COMMA\000\
  TRUE\000\
  FALSE\000\
  NULL\000\
  "

let yynames_block = "\
  STRING\000\
  NUMBER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pmembers) in
    Obj.repr(
# 15 "jsonParser.mly"
                         ( Object (ref _2) )
# 105 "jsonParser.ml"
               : 'pobject))
; (fun __caml_parser_env ->
    Obj.repr(
# 16 "jsonParser.mly"
                         ( Object (ref Object.empty) )
# 111 "jsonParser.ml"
               : 'pobject))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Json.jvalue) in
    Obj.repr(
# 20 "jsonParser.mly"
                                      ( Object.add _1 (ref _3) Object.empty )
# 119 "jsonParser.ml"
               : 'pmembers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'pmembers) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Json.jvalue) in
    Obj.repr(
# 21 "jsonParser.mly"
                                      ( Object.add _3 (ref _5) _1 )
# 128 "jsonParser.ml"
               : 'pmembers))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "jsonParser.mly"
                             ( Array (ref [])            )
# 134 "jsonParser.ml"
               : 'parray))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pelements) in
    Obj.repr(
# 26 "jsonParser.mly"
                             ( Array (ref (List.rev _2)) )
# 141 "jsonParser.ml"
               : 'parray))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Json.jvalue) in
    Obj.repr(
# 30 "jsonParser.mly"
                          ( (ref _1) :: [] )
# 148 "jsonParser.ml"
               : 'pelements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pelements) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Json.jvalue) in
    Obj.repr(
# 31 "jsonParser.mly"
                          ( (ref _3) :: _1 )
# 156 "jsonParser.ml"
               : 'pelements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "jsonParser.mly"
            ( String _1 )
# 163 "jsonParser.ml"
               : Json.jvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Big_int.big_int) in
    Obj.repr(
# 36 "jsonParser.mly"
            ( Number _1 )
# 170 "jsonParser.ml"
               : Json.jvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pobject) in
    Obj.repr(
# 37 "jsonParser.mly"
            ( _1        )
# 177 "jsonParser.ml"
               : Json.jvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parray) in
    Obj.repr(
# 38 "jsonParser.mly"
            ( _1        )
# 184 "jsonParser.ml"
               : Json.jvalue))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "jsonParser.mly"
            ( True      )
# 190 "jsonParser.ml"
               : Json.jvalue))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "jsonParser.mly"
            ( False     )
# 196 "jsonParser.ml"
               : Json.jvalue))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "jsonParser.mly"
            ( Null      )
# 202 "jsonParser.ml"
               : Json.jvalue))
(* Entry value *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let value (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Json.jvalue)
