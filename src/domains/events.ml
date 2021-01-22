(* open Deriving.Cil *)
open Prelude.Ana

type t =
  | Lock of LockDomain.Addr.t
  | Unlock of LockDomain.Addr.t
  | Escape of EscapeDomain.EscapedVars.t
  | EnterMultiThreaded
  | SplitBranch of exp * bool (** Used to simulate old branch-based split. *)

let pretty () = function
  | Lock m -> dprintf "Lock %a" LockDomain.Addr.pretty m
  | Unlock m -> dprintf "Unock %a" LockDomain.Addr.pretty m
  | Escape escaped -> dprintf "Escape %a" EscapeDomain.EscapedVars.pretty escaped
  | EnterMultiThreaded -> text "EnterMultiThreaded"
  | SplitBranch (exp, tv) -> dprintf "SplitBranch (%a, %B)" d_exp exp tv
