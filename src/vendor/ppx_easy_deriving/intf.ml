(** Main interfaces. *)

open Ppxlib

(** Deriver name interface. *)
module type Name =
sig
  val name: string
  (** Deriver name.

      For example, with the name "equal":
      + Use [[@@deriving equal]] after a type definition.
      + The derived value/function is [val equal: ...] (if the type is named [t]) or [val equal_ty: ...] (otherwise if the type is named [ty]).
      + Use [[@equal ...]] after a type expression to override the underlying value/function used for it.
      + Use [[%equal: ty]] as an expression for the value/function of type [ty]. *)
end

(** Deriver base interface. *)
module type Base =
sig
  include Name
  val typ: loc:location -> core_type -> core_type
  (** Derived value/function type for a given type.

      For example, "equal" deriver would map [t] to [t -> t -> bool]. *)
end

module Tuple =
struct

  (** Tuple deriver interface. *)
  module type S =
  sig
    include Base
    val tuple: loc:location -> expression list -> expression
    (** Compose derived values/functions for tuple elements into derived value/function for the tuple. *)
  end
end

module Record =
struct

  (** Record deriver interface. *)
  module type S =
  sig
    include Base
    val record: loc:location -> (longident * expression) list -> expression
    (** Compose derived values/functions for record fields into derived value/function for the record. *)
  end
end

module Full =
struct

  (** Full deriver interface. *)
  module type S =
  sig
    include Tuple.S
    include Record.S
  end
end

module type S = Full.S
(** Deriver interface. *)
