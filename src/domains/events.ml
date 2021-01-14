open Deriving.Cil

type t =
  | Lock of varinfo
  | Unlock of varinfo
  | Escape of varinfo
