open BatCache

type 'a t = (unit, 'a) manual_cache

let from_fun f = make_map ~gen:f

let force cache = cache.get ()

let reset cache = cache.del ()
