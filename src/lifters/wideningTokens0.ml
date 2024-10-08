(** Widening token. *)
module Token =
struct
  (* Change to variant type if need other tokens than witness UUIDs. *)
  include Printable.Prod (Basetype.RawStrings) (Printable.Option (IntDomain.Integers (IntOps.NIntOps)) (struct let name = "None" end))
end
