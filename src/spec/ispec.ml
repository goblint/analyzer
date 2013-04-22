open Batteries

(* config *)
let save_dot = false

let parse ?repl:(repl=false) ?print:(print=false) cin =
  let lexbuf = Lexing.from_channel cin in
  let defs = ref [] in
  try
    while true do (* loop over all lines *)
      try
        let result = SpecParser.file SpecLexer.token lexbuf in
        defs := !defs@[result];
        if print then (print_endline (Def.to_string result); flush stdout)
      with
        (* just an empty line -> don't print *)
        | Def.Endl  -> ()
        (* catch and print in repl-mode *)
        | e when repl -> print_endline (Printexc.to_string e)
    done;
    ([], []) (* never happens, but ocaml needs it for type *)
  with
    (* done *)
    | Def.Eof   ->
(*         let nodes = List.filter (function Def.Node _ -> true | _ -> false) !defs in
        let edges = List.filter (function Def.Edge _ -> true | _ -> false) !defs in *)
        let nodes = List.filter_map (function Def.Node x -> Some x | _ -> None) !defs in
        let edges = List.filter_map (function Def.Edge x -> Some x | _ -> None) !defs in
        if print then Printf.printf "\n#Definitions: %i, #Nodes: %i, #Edges: %i\n"
          (List.length !defs) (List.length nodes) (List.length edges);
        if save_dot then (
          let dot = Def.dot !defs in
          output_file "file.dot" dot;
          print_endline ("saved graph as "^Sys.getcwd ()^"/file.dot");
        );
        (nodes, edges)

let parseFile filename = parse (open_in filename)

(* print ~first:"[" ~sep:", " ~last:"]" print_any stdout @@ 5--10 *)