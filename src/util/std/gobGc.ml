(* print GC statistics; taken from Cil.Stats.print which also includes timing; there's also Gc.print_stat, but it's in words instead of MB and more info than we want (also slower than quick_stat since it goes through the heap) *)
let print_quick_stat chn =
  let gc = Gc.quick_stat () in
  let printM (w: float) : string =
    let coeff = float_of_int (Sys.word_size / 8) in
    Printf.sprintf "%.2fMB" (w *. coeff /. 1000000.0)
  in
  Printf.fprintf chn
    "Memory statistics: total=%s, max=%s, minor=%s, major=%s, promoted=%s\n    minor collections=%d  major collections=%d compactions=%d\n"
    (printM (gc.Gc.minor_words +. gc.Gc.major_words
             -. gc.Gc.promoted_words))
    (printM (float_of_int gc.Gc.top_heap_words))
    (printM gc.Gc.minor_words)
    (printM gc.Gc.major_words)
    (printM gc.Gc.promoted_words)
    gc.Gc.minor_collections
    gc.Gc.major_collections
    gc.Gc.compactions;
  gc
