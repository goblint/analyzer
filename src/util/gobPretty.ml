open GoblintCil

let sprint f x = Pretty.sprint ~width:max_int (f () x)

let sprintf (fmt: ('a, unit, Pretty.doc, string) format4): 'a =
  Pretty.gprintf (Pretty.sprint ~width:max_int) fmt
