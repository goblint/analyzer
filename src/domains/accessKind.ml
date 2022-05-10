type t =
  | Write  (** argument may be read or written to *)
  | Read   (** argument may be read *)
  | Free   (** argument may be freed *)
[@@deriving eq, ord, hash]
(** Specifies what is known about an argument. *)

let show: t -> string = function
  | Write -> "write"
  | Read -> "read"
  | Free -> "free"

include Printable.SimpleShow (
  struct
    type nonrec t = t
    let show = show
  end
  )
