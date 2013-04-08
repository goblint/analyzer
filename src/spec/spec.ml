open Batteries

let _ =
  (* running interactively (= reading from stdin)  *)
  let repl = Array.length Sys.argv = 1 in
  let cin = if repl then stdin else open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel cin in
  let defs = ref [] in
  while true do
    try
      let result = Parser.file Lexer.token lexbuf in
      defs := !defs@[result];
      print_endline (Def.to_string result); flush stdout
    with
      (* just an empty line -> don't print *)
      | Def.Endl  -> ()
      (* done *)
      | Def.Eof   ->
          let nodes = List.filter (function Def.Node _ -> true | _ -> false) !defs in
          let edges = List.filter (function Def.Edge _ -> true | _ -> false) !defs in
          print_newline ();
          Printf.printf "#Definitions: %i, #Nodes: %i, #Edges: %i\n"
            (List.length !defs) (List.length nodes) (List.length edges);
          let dot = Def.dot !defs in
          output_file "file.dot" dot;
          print_endline "saved graph as file.dot";
          exit 0
      (* catch and print in repl-mode *)
      | e when repl -> print_endline (Printexc.to_string e)
  done

(* print ~first:"[" ~sep:", " ~last:"]" print_any stdout @@ 5--10 *)