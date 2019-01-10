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
  let name () = "trivial fragmented arrays"
  module Base = Lattice.Prod3 (Val) (Val) (Val)
  module Expp = Idx
  include Lattice.ProdSimple(Expp) (Base)
  type idx = Idx.t
  type value = Val.t

  let short w (e,(xl, xm, xr)) = "Array (partitioned by " ^ Expp.short (w-7) e ^ "): (" ^ Val.short (w - 7) xl ^ "," ^ Val.short (w - 7) xm ^ "," ^ Val.short (w - 7) xr ^ ")" (* TODO w-7 needs to be replaced here *)
  let pretty () x = text "Array: " ++ pretty_f short () x
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let toXML m = toXML_f short m

  (* For set&get we later need to distinguish between must & may equality to see *)
  (* decide whether to apply a least upper bound or not *)

  let get (e, (xl, xm, xr)) i =    (* This is currently under the assumption that we *)
    if Idx.equal e i then xm
    (* TODO: else if all the other ways in which e and i might relate *)
    else Val.join (Val.join xl xm) xr (* The case in which we don't know anything *)

  let set (e, (xl, xm, xr)) i a =
    begin
      Messages.report (Expp.short 20 i);
      let newExp = if Expp.is_bot e then i else e in
      let lub = Val.join a in
      if Idx.equal e i then (newExp, (xl, a, xr))
      (* TODO: else if all the other cases *)
      else (newExp, (lub xl, lub xm, lub xr))
    end

  let make i v = (Expp.bot(), (Val.top(), v, Val.top()))    (* TODO: We need to see whether we need to modify the bottom element from the Prod3 domain here *)
                                                            (* TODO: It would also seem we need to provide the expression that we are suing to split it here *)
  let length _ = None

  let move (e, (xl, xm, xr)) (i:int) =     (* Under the assumption that we always get exact information about how much it moved *)
    match i with
    | 0   -> (e, (xl, xm, xr))
    | 1   -> (e, (Val.join xl xm, xr, xr)) (* moved one to the right *)
    | -1  -> (e, (xl, xl, Val.join xm xr)) (* moved one to the left  *)
    | _ when i > 1
      -> (e, (Val.join (Val.join xl xm) xr, xr, xr)) (* moved more than one to the right *)
    | _ when i < -1
      -> (e, (xl, xl, Val.join (Val.join xl xm) xr)) (* moved more than one to the left *)
    | _ -> top()

  let set_inplace = set
  let copy a = a
  let printXml f (e, (xl, xm, xr)) = BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%a\n</map>\n</value>\n" Val.printXml xl
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