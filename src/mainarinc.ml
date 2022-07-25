open Prelude
open GobConfig

let save_all ch =
  ArincUtil.unmarshal ch;
  ArincUtil.print_actions ();
  ArincUtil.save_dot_graph ();
  ArincUtil.save_promela_model ()

let _ =
  let conf, cin = match Array.to_list Sys.argv with
    | [_; conf; "-"] -> conf, stdin
    | [_; conf; path] -> conf, open_in_bin path
    | _ -> print_endline @@ "usage: " ^ Sys.argv.(0) ^ " <conf.json> <arinc.out or - for stdin>"; exit 1
  in
  merge_file (Fpath.v conf);
  save_all cin
