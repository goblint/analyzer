let _ =
  (* running interactively (= reading from stdin)  *)
  let repl = Array.length Sys.argv = 1 in
  let cin = if repl then stdin else open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel cin in
  while true do
    try
      let result = Parser.file Lexer.token lexbuf in
        print_string result; print_newline(); flush stdout
    with
      (* just a new line -> won't be printed *)
      | Exc.Endl  -> ()
      (* done *)
      | Exc.Eof   -> exit 0
      (* catch and print in repl-mode *)
      | e when repl -> print_endline (Printexc.to_string e)
  done