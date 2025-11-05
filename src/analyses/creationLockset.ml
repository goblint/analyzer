open Analyses
module TID = ThreadIdDomain.ThreadLifted
module LID = LockDomain.MustLock

(** 
    collects for each thread t_n pairs of must-ancestors and locks (t_0,l):
    when t_n or a must-ancestor t_1 of t_n was created, the parent t_0 must have held l.
    TODO: check if this requirement can be loosened
*)
module Spec = struct
  include IdentityUnitContextsSpec (* no context necessary(?) *)
  module D = Lattice.Unit (* flow-insensitive analysis *)

  module V = struct
    include TID
    include StdV
  end

  module G = SetDomain.Make (Printable.ProdSimple (TID) (LID))
  (* 2^{T\times L}. TODO: Prod or ProdSimple? *)

  let name () = "creationLockset"
  let startstate _ = D.bot ()
  let exitstate _ = D.bot ()

  (* create(t_1) in t_0 with lockset L *)
  let threadspawn man ~multiple lval f args fman =
    let ask = Analyses.ask_of_man man in
    let tid = ask.f Queries.CurrentThreadId in
    let child_ask = Analyses.ask_of_man fman in
    let child_tid = child_ask.f Queries.CurrentThreadId in
    let lockset = ask.f Queries.MustLockset in
    (* contribution (t_1, l) to global of t_0 for all l in L: *)
    (* TODO also register for transitive descendants of t_1! *)
    let contribute_lock lock = man.sideg child_tid (G.singleton (tid, lock)) in
    LockDomain.MustLockset.iter contribute_lock lockset
  ;;
  (* TODO: consider edge cases (most likely in creation lockset analysis)!
     - `ana.threads.include-node` is false. Two threads created with different locksets may have the same id that way!
     - child thread is not unique and thus could also ancestor of ego thread. In this case, it can also be created with a different lockset!
     - more? *)
end

let _ = MCP.register_analysis ~dep:[ "threadid"; "mutex" ] (module Spec : MCPSpec)
