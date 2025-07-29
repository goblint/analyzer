type t = unit
type 'a promise = 'a

let run pool f = f ()

let create n = ()

let add_work pool f = f ()

let await_all pool promises = ()

let finished_with pool = ()
