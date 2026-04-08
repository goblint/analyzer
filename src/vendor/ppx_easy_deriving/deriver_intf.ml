module type S =
sig
  val register: unit -> Ppxlib.Deriving.t
  (** Register deriver with ppxlb. *)
end

module type Deriver =
sig
  module type S = S

  module Make (_: Intf.S): S
  (** Make registerable deriver. *)
end
