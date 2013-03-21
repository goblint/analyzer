let _ =
  (* running interactively (= reading from stdin)  *)
  let repl = Array.length Sys.argv = 1 in
  let cin = if repl then stdin else open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel cin in
  while true do
    try
      let result = Parser.file Lexer.token lexbuf in
        print_endline (Def.to_string result); flush stdout
    with
      (* just a new line -> won't be printed *)
      | Def.Endl  -> ()
      (* done *)
      | Def.Eof   -> exit 0
      (* catch and print in repl-mode *)
      | e when repl -> print_endline (Printexc.to_string e)
  done