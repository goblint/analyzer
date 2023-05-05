open GoblintCil

let show = Pretty.sprint ~width:max_int

let sprint f x = show (f () x)

let sprintf (fmt: ('a, unit, Pretty.doc, string) format4): 'a =
  Pretty.gprintf show fmt
