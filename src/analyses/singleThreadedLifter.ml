(** This lifter takes an analysis that only works for single-threaded code and allows it to run on multi-threaded programs by returning top when the code might be multi-threaded.
*)

open Analyses

module SingleThreadedLifter (S: MCPSpec) =
struct
  include S

  let is_multithreaded (ask:Queries.ask) =
    not @@ ask.f (MustBeSingleThreaded {since_start = true})

  let query ctx =
    let return_top (type a) (q: a Queries.t) =
      Queries.Result.top q
    in
    if is_multithreaded (ask_of_ctx ctx) then
      return_top
    else
      query ctx

  let assign ctx lval expr =
    if is_multithreaded (ask_of_ctx ctx) then
      D.top ()
    else
      assign ctx lval expr

  let branch ctx e pos =
    if is_multithreaded (ask_of_ctx ctx) then
      D.top ()
    else
      branch ctx e pos

  let body ctx f =
    if is_multithreaded (ask_of_ctx ctx) then
      D.top ()
    else
      body ctx f

  let return ctx exp_opt f =
    if is_multithreaded (ask_of_ctx ctx) then
      D.top ()
    else
      return ctx exp_opt f

  let special ctx var_opt v exprs =
    if is_multithreaded (ask_of_ctx ctx) then
      D.top ()
    else
      special ctx var_opt v exprs

  let enter ctx var_opt f args =
    if is_multithreaded (ask_of_ctx ctx) then
      [D.top (),D.top ()]
    else
      enter ctx var_opt f args

  let combine_env ctx var_opt expr f exprs t_context_opt t (ask: Queries.ask) =
    if is_multithreaded (ask_of_ctx ctx) then
      D.top ()
    else
      combine_env ctx var_opt expr f exprs t_context_opt t ask

  let combine_assign ctx var_opt expr f args t_context_opt t (ask: Queries.ask) =
    if is_multithreaded (ask_of_ctx ctx) then
      D.top ()
    else
      combine_assign ctx var_opt expr f args t_context_opt t ask

  let threadenter ctx ~multiple lval f args =
    [D.top ()]

  let threadspawn ctx ~multiple lval f args fctx =
    D.top ()
end
