open Prelude.Ana

type t =
  | Lock of LockDomain.Lockset.Lock.t
  | Unlock of LockDomain.Addr.t
  | Escape of EscapeDomain.EscapedVars.t
  | EnterMultiThreaded
  | SplitBranch of exp * bool (** Used to simulate old branch-based split. *)
  | AssignSpawnedThread of lval * ThreadIdDomain.Thread.t (** Assign spawned thread's ID to lval. *)
  | Access of {exp: CilType.Exp.t; lvals: Queries.LS.t; kind: AccessKind.t; reach: bool}
  | Assign of {lval: CilType.Lval.t; exp: CilType.Exp.t} (** Used to simulate old [ctx.assign]. *)
  | UpdateExpSplit of exp (** Used by expsplit analysis to evaluate [exp] on post-state. *)
  | Unassume of {exp: CilType.Exp.t; uuids: string list}

let pretty () = function
  | Lock m -> dprintf "Lock %a" LockDomain.Lockset.Lock.pretty m
  | Unlock m -> dprintf "Unlock %a" LockDomain.Addr.pretty m
  | Escape escaped -> dprintf "Escape %a" EscapeDomain.EscapedVars.pretty escaped
  | EnterMultiThreaded -> text "EnterMultiThreaded"
  | SplitBranch (exp, tv) -> dprintf "SplitBranch (%a, %B)" d_exp exp tv
  | AssignSpawnedThread (lval, tid) -> dprintf "AssignSpawnedThread (%a, %a)" d_lval lval ThreadIdDomain.Thread.pretty tid
  | Access {exp; lvals; kind; reach} -> dprintf "Access {exp=%a; lvals=%a; kind=%a; reach=%B}" CilType.Exp.pretty exp Queries.LS.pretty lvals AccessKind.pretty kind reach
  | Assign {lval; exp} -> dprintf "Assign {lval=%a, exp=%a}" CilType.Lval.pretty lval CilType.Exp.pretty exp
  | UpdateExpSplit exp -> dprintf "UpdateExpSplit %a" d_exp exp
  | Unassume {exp; uuids} -> dprintf "Unassume {exp=%a; uuids=%a}" d_exp exp (docList Pretty.text) uuids
