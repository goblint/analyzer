open Pretty
open Messages

module A = Array
module GU = Goblintutil

module type S =
sig
  include Lattice.S
  type idx
  type value

  val get: t -> idx -> value
  val set: t -> idx -> value -> t
  val make: int -> value -> t
  val length: t -> int option
end


module Trivial (Val: Lattice.S) (Idx: Lattice.S): S with type value = Val.t and type idx = Idx.t =
struct
  let name () = "trivial arrays"
  include Val
  type idx = Idx.t
  type value = Val.t

  let short w x = "Array: " ^ Val.short (w - 7) x
  let pretty () x = text "Array: " ++ pretty_f short () x
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let toXML m = toXML_f short m
  let get a i = a
  let set a i v = join a v
  let make i v = v
  let length _ = None

  let set_inplace = set
  let copy a = a
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml x
end

module TrivialFragmented (Val: Lattice.S) (Idx: Lattice.S): S with type value = Val.t and type idx = Idx.t =
struct
  let name () = "trivial arrays"
  include Lattice.Prod3 (Val) (Val) (Val)
  type idx = Idx.t
  type value = Val.t

  let short w (xl, xm, xr) = "Array: (" ^ Val.short (w - 7) xl ^ "," ^ Val.short (w - 7) xm ^ "," ^ Val.short (w - 7) xr ^ ")" (* TODO w-7 needs to be replaced here *)
  let pretty () x = text "Array: " ++ pretty_f short () x
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let toXML m = toXML_f short m
  let get (xl, xm, xr) i = xl
  let set (xl, xm, xr) i a = (Val.join xl a, xm, xr)
  let make i v = (v, v, v)
  let length _ = None

  let set_inplace = set
  let copy a = a
  let printXml f (xl, xm, xr) = BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml xl
end


module TrivialWithLength (Val: Lattice.S) (Idx: IntDomain.S): S with type value = Val.t and type idx = Idx.t =
struct
  module Base = Trivial (Val) (Idx)
  include Lattice.Prod (Base) (Idx)
  type idx = Idx.t
  type value = Val.t
  let get (x ,l) i = Base.get x i (* TODO check if in-bounds *)
  let set (x,l) i v = Base.set x i v, l
  let make l x = Base.make l x, Idx.of_int (Int64.of_int l)
  let length (_,l) = BatOption.map Int64.to_int (Idx.to_int l)
end


module TrivialFragmentedWithLength (Val: Lattice.S) (Idx: IntDomain.S): S with type value = Val.t and type idx = Idx.t =
struct
  module Base = TrivialFragmented (Val) (Idx)
  include Lattice.Prod (Base) (Idx)
  type idx = Idx.t
  type value = Val.t
  let get (x,l) i = Base.get x i (* TODO check if in-bounds *)
  let set (x,l) i v = Base.set x i v, l
  let make l x = Base.make l x, Idx.of_int (Int64.of_int l)
  let length (_,l) = BatOption.map Int64.to_int (Idx.to_int l)
end