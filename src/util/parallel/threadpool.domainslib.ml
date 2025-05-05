module Thread_pool =
struct
  module T = Domainslib.Task

  type t = T.pool
  type 'a promise = 'a T.promise

  let run = T.run

  let create n = T.setup_pool ~num_domains:n ()

  let add_work pool f = T.async pool f 

  let await_all pool promises = 
    List.iter (T.await pool) promises

  let finished_with pool = T.teardown_pool pool
end
