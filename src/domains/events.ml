open Deriving.Cil

type t =
  | Lock of LockDomain.Addr.t
  | Unlock of LockDomain.Addr.t
  | Escape of varinfo
