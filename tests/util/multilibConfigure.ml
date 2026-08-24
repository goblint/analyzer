open GoblintCil

let () =
  Printf.printf "%B" (Option.is_some Machdep.gcc32 && Option.is_some Machdep.gcc64)
