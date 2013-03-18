(* File spec.ml *)
let _ =
  try
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    while true do
      let result = Parser.file Lexer.token lexbuf in
        print_string result; print_newline(); flush stdout
    done
  with Lexer.Eof ->
    exit 0