open Deriving.Cil

type t =
  | Lock of LockDomain.Addr.t
  | Unlock of LockDomain.Addr.t
  | Escape of varinfo (* TODO: use *)
  | SplitBranch of exp * bool (** Used to simulate old branch-based split. *)
