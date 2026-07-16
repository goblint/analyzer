(** Widening token for {!WideningTokenLifter}. *)

module Uuid =
struct
  include Basetype.RawStrings
  let name () = "uuid"
end

module Index =
struct
  include Printable.Option (IntDomain.Integers (IntOps.NIntOps)) (struct let name = "None" end)
  let name () = "index"
end

(* Change to variant type if need other tokens than witness UUIDs. *)
include Printable.Prod (Uuid) (Index)
