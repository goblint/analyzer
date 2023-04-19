open GoblintCil

let f = Printf.sprintf
let pf fmt = Printf.ksprintf print_endline fmt
let df fmt = Pretty.gprintf (Pretty.sprint ~width:max_int) fmt
let dpf fmt = Pretty.gprintf (fun doc -> print_endline @@ Pretty.sprint ~width:max_int doc) fmt
