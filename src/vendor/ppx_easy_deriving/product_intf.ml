open Ppxlib

module type S =
sig
  include Intf.Base
  val product: loc:location -> pe_create:(prefix:string -> Pat_exp.t) -> expression list -> expression
  (** Compose derived values/functions for product elements into derived value/function for the product.

      @param pe_create factory for patterns/expressions of the actual type. *)
end

module Reduce =
struct
  module type S =
  sig
    include Intf.Base
    val unit: loc:location -> expression
    (** Derived value/function for [unit] type. *)

    val both: loc:location -> expression -> expression -> expression
    (** Compose derived values/functions in a product into derived value/function for the pair. *)
  end

  module Conjunctive =
  struct
    module type S = Intf.Name
  end
end

module Reduce1 =
struct
  module type S = Reduce.S
end

module Reduce2 =
struct
  module type S = Reduce.S
end

module Create =
struct
  module type S =
  sig
    include Intf.Base
  end
end

module Map1 =
struct
  module type S = Intf.Name
end

module Map2 =
struct
  module type S = Intf.Name
end

module type Product =
sig
  module type S = S
  (** Product deriver interface. *)

  module Make (P: S): Intf.S
  (** Make deriver from product deriver. *)

  (** Reductions for reducing derivers. *)
  module Reduce :
  sig
    module type S = Reduce.S
    (** Reduction interface. *)

    (** Conjunctive reduction. *)
    module Conjunctive :
    sig
      module type S = Reduce.Conjunctive.S
      (** Conjunctive reduction interface. *)

      module Make (C: S): Reduce.S
      (** Make reduction from conjunctive reduction. *)
    end
  end

  (** Unary reducing deriver. *)
  module Reduce1 :
  sig
    module type S = Reduce1.S
    (** Unary reducing deriver interface. *)

    module Make (R1: S): Intf.S
    (** Make deriver from unary reducing deriver. *)
  end

  (** Binary reducing deriver. *)
  module Reduce2 :
  sig
    module type S = Reduce2.S
    (** Binary reducing deriver interface. *)

    module Make (R2: S): Intf.S
    (** Make deriver from binary reducing deriver. *)
  end

  (** Unary creating deriver. *)
  module Create :
  sig
    module type S = Create.S
    (** Unary creating deriver interface. *)

    module Make (C: S): Intf.S
    (** Make deriver from unary creating deriver. *)
  end

  (** Unary mapping deriver. *)
  module Map1 :
  sig
    module type S = Map1.S
    (** Unary mapping deriver interface. *)

    module Make (M1: S): Intf.S
    (** Make deriver from unary mapping deriver. *)
  end

  (** Binary mapping deriver. *)
  module Map2 :
  sig
    module type S = Map2.S
    (** Binary mapping deriver interface. *)

    module Make (M2: S): Intf.S
    (** Make deriver from binary mapping deriver. *)
  end
end
