open Prelude.Ana

type t =
  | Lock of LockDomain.Addr.t  (** This is only emitted if the mutex was not previously held *)
  | Unlock of LockDomain.Addr.t
  | Escape of EscapeDomain.EscapedVars.t
  | EnterMultiThreaded
  | SplitBranch of exp * bool (** Used to simulate old branch-based split. *)
  | AssignSpawnedThread of lval * ThreadIdDomain.Thread.t (** Assign spawned thread's ID to lval. *)
  | UpdateExpSplit of exp (** Used by expsplit analysis to evaluate [exp] on post-state. *)

let pretty () = function
  | Lock m -> dprintf "Lock %a" LockDomain.Addr.pretty m
  | Unlock m -> dprintf "Unock %a" LockDomain.Addr.pretty m
  | Escape escaped -> dprintf "Escape %a" EscapeDomain.EscapedVars.pretty escaped
  | EnterMultiThreaded -> text "EnterMultiThreaded"
  | SplitBranch (exp, tv) -> dprintf "SplitBranch (%a, %B)" d_exp exp tv
  | AssignSpawnedThread (lval, tid) -> dprintf "AssignSpawnedThread (%a, %a)" d_lval lval ThreadIdDomain.Thread.pretty tid
  | UpdateExpSplit exp -> dprintf "UpdateExpSplit %a" d_exp exp
