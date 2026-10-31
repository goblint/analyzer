(** Kinds of memory accesses. *)

type t =
  | Write  (** argument may be written to *)
  | Read   (** argument may be read *)
  | Free   (** argument may be freed *)
  | Call   (** argument may be called *)
  | Spawn  (** argument may be spawned *)
[@@deriving eq, ord, hash]
(** Specifies what is known about an argument. *)

let show: t -> string = function
  | Write -> "write"
  | Read -> "read"
  | Free -> "free"
  | Call -> "call"
  | Spawn -> "spawn"

[%%functor include Printable.SimpleShow]
