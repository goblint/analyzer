open Prelude.Ana

type t =
  | Lock of LockDomain.Lockset.Lock.t
  | Unlock of LockDomain.Addr.t
  | Escape of EscapeDomain.EscapedVars.t
  | EnterMultiThreaded
  | SplitBranch of CilType.Exp.t * bool (** Used to simulate old branch-based split. *)
  | AssignSpawnedThread of CilType.Lval.t * ThreadIdDomain.Thread.t (** Assign spawned thread's ID to lval. *)
  | Access of {var_opt: CilType.Varinfo.t option; kind: AccessKind.t} (** Access varinfo (unknown if None). *)
  | Assign of {lval: CilType.Lval.t; exp: CilType.Exp.t} (** Used to simulate old [ctx.assign]. *)
[@@deriving show]
