module Thread_pool =
struct

  type t = unit
  type 'a promise = 'a Domain.t

  let run pool f = f ()

  let create n = ()

  let add_work pool f =
    let promise = Domain.spawn f in
    promise

  let await_all pool promises =
    List.iter (fun promise -> Domain.join promise) promises

  let finished_with pool = ()
end
