open GoblintCil

let () =
  Printf.printf "%B" (Option.is_some GoblintCil.Machdep.gcc32 && Option.is_some GoblintCil.Machdep.gcc64)
