open GoblintCil

let sprint f x = Pretty.sprint ~width:max_int (f () x)
