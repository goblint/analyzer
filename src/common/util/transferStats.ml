let count = ref 0
let reset () = count := 0
let bump () = incr count
let get () = !count
