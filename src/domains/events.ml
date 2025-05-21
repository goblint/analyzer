(** Events. *)

open GoblintCil
open Pretty

type t =
  | Lock of LockDomain.AddrRW.t
  | Unlock of LockDomain.Addr.t
  | Escape of EscapeDomain.EscapedVars.t
  | EnterMultiThreaded
  | SplitBranch of exp * bool (** Used to simulate old branch-based split. *)
  | AssignSpawnedThread of lval * ThreadIdDomain.Thread.t (** Assign spawned thread's ID to lval. *)
  | Access of {exp: CilType.Exp.t; ad: Queries.AD.t; kind: AccessKind.t; reach: bool}
  | Assign of {lval: CilType.Lval.t; exp: CilType.Exp.t} (** Used to simulate old [man.assign]. *) (* TODO: unused *)
  | UpdateExpSplit of exp (** Used by expsplit analysis to evaluate [exp] on post-state. *)
  | Assert of exp
  | Unassume of {exp: CilType.Exp.t; tokens: WideningToken.t list}
  | Longjmped of {lval: CilType.Lval.t option}

(** Should event be emitted after transfer function raises [Deadcode]? *)
let emit_on_deadcode = function
  | Unlock _ (* Privatization must still publish. *)
  | Escape _ (* Privatization must still handle escapes. *)
  | EnterMultiThreaded (* Privatization must still publish. *)
  | Access _ -> (* Protection and races must still consider access. *)
    true
  | Lock _ (* Doesn't need to publish. *)
  | SplitBranch _ (* only emitted in split, which is never dead. *)
  | AssignSpawnedThread _ (* Happens only after live thread spawn. *)
  | Assign _
  | UpdateExpSplit _ (* Pointless to split on dead. *)
  | Unassume _ (* Avoid spurious writes. *)
  | Assert _ (* Pointless to refine dead. *)
  | Longjmped _ ->
    false

let pretty () = function
  | Lock m -> dprintf "Lock %a" LockDomain.AddrRW.pretty m
  | Unlock m -> dprintf "Unlock %a" LockDomain.Addr.pretty m
  | Escape escaped -> dprintf "Escape %a" EscapeDomain.EscapedVars.pretty escaped
  | EnterMultiThreaded -> text "EnterMultiThreaded"
  | SplitBranch (exp, tv) -> dprintf "SplitBranch (%a, %B)" d_exp exp tv
  | AssignSpawnedThread (lval, tid) -> dprintf "AssignSpawnedThread (%a, %a)" d_lval lval ThreadIdDomain.Thread.pretty tid
  | Access {exp; ad; kind; reach} -> dprintf "Access {exp=%a; ad=%a; kind=%a; reach=%B}" CilType.Exp.pretty exp Queries.AD.pretty ad AccessKind.pretty kind reach
  | Assign {lval; exp} -> dprintf "Assign {lval=%a, exp=%a}" CilType.Lval.pretty lval CilType.Exp.pretty exp
  | UpdateExpSplit exp -> dprintf "UpdateExpSplit %a" d_exp exp
  | Assert exp -> dprintf "Assert %a" d_exp exp
  | Unassume {exp; tokens} -> dprintf "Unassume {exp=%a; tokens=%a}" d_exp exp (d_list ", " WideningToken.pretty) tokens
  | Longjmped {lval} -> dprintf "Longjmped {lval=%a}" (docOpt (CilType.Lval.pretty ())) lval
