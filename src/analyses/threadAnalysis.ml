(** Thread-id analyses. *)
open Prelude.Ana
open Analyses

module T  = ConcDomain.Thread
module TS = ConcDomain.ThreadSet

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "thread"
  module D = ConcDomain.ThreadSet
  module C = D
  module G = ConcDomain.ThreadCreation

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ctx.local
  let branch ctx (exp:exp) (tv:bool) : D.t =  ctx.local
  let body ctx (f:fundec) : D.t =  ctx.local
  let return ctx (exp:exp option) (f:fundec) : D.t = ctx.local
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = [ctx.local,ctx.local]
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t = au
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t = ctx.local

  let query ctx (q: Queries.t) =
    match q with
    | Queries.IsNotUnique -> begin
        let rec check_one tid =
          let (rep, parents) = ctx.global tid in
          let n = TS.cardinal parents in
          (* A thread is not unique if it is
           * a) repeatedly created,
           * b) created in multiple threads, or
           * c) created by a thread that is itself multiply created.
           * Note that starting threads have empty ancestor sets! *)
          rep || n > 1 || n > 0 && check_one (TS.choose parents)
        in
        let tid = ThreadId.get_current ctx in
        match tid with
        | `Lifted tid -> `Bool (check_one tid)
        | _ -> `Bool (true)
      end
    | _ -> Queries.Result.top ()

  let startstate v = D.bot ()
  let threadenter ctx f args = D.bot ()
  let threadcombine ctx f args fctx =
    let l = !Tracing.current_loc in
    let creator = ThreadId.get_current ctx in
    let tid = T.spawn_thread l f in (* TODO: get new thread id from threadid analysis *)
    let repeated = D.mem tid ctx.local in
    let eff =
      match creator with
      | `Lifted ctid -> (repeated, TS.singleton ctid)
      | `Top         -> (true,     TS.bot ())
      | `Bot         -> (false,    TS.bot ())
    in
    ctx.sideg tid eff;
    D.singleton tid
  let exitstate  v = D.bot ()
end

(* really stupid thread-ids *)
module StartLocIDs =
struct
  include Analyses.DefaultSpec

  let name () = "thread-id-location"
  module D = ConcDomain.ThreadStringSet
  module C = D
  module G = Lattice.Unit

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ctx.local
  let branch ctx (exp:exp) (tv:bool) : D.t =  ctx.local
  let body ctx (f:fundec) : D.t =  ctx.local
  let return ctx (exp:exp option) (f:fundec) : D.t = ctx.local
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = [ctx.local,ctx.local]
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t = ctx.local
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t = ctx.local

  let main = D.singleton "main"
  let startstate v = main
  let exitstate  v = D.top ()

  let threadenter ctx f args =
    let location x = let l = !Tracing.current_loc in l.file ^ ":" ^ string_of_int l.line ^ ":" ^ x.vname in
    D.singleton (location f)

  let threadcombine ctx f args fctx = D.bot ()
end

let _ = MCP.register_analysis (module StartLocIDs : Spec)
let _ = MCP.register_analysis ~dep:["threadid"] (module Spec : Spec)
