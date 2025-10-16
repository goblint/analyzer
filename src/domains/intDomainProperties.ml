(** QCheck properties for {!IntDomain}. *)

open GoblintCil

(* TODO: deduplicate with IntDomain *)
module type OldS =
sig
  include Lattice.S
  include IntDomain.Arith with type t := t
  val of_int: Z.t -> t
  val to_int: t -> Z.t option
  val of_bool: bool -> t
  val to_bool: t -> bool option
  val of_excl_list: Cil.ikind -> Z.t list -> t
  val is_excl_list: t -> bool
  val to_excl_list: t -> (Z.t list * (int * int)) option
end

module type OldSWithIkind =
sig
  include OldS
  module Ikind: IntDomain.Ikind
end

module type S = IntDomain.S with type int_t = Z.t
module type S2 = IntDomain.S2 with type int_t = Z.t

module MakeS2 (I: S): S2 =
struct
  include I

  let of_int ?suppress_ovwarn ik x = of_int ik x
  let starting ?suppress_ovwarn ik x = starting ik x
  let ending ?suppress_ovwarn ik x = ending ik x
  let of_interval ?suppress_ovwarn ik x = of_interval ik x
end

(* TODO: deduplicate with IntDomain, extension of IntDomWithDefaultIkind, inverse of OldDomainFacade? *)
module WithIkind (I: S2) (Ik: IntDomain.Ikind): OldSWithIkind =
struct
  include I
  module Ikind = Ik
  let join = join (Ik.ikind ())
  let meet = meet (Ik.ikind ())
  let widen = widen (Ik.ikind ())
  let narrow = narrow (Ik.ikind ())
  let neg = neg (Ik.ikind ())
  let add = add (Ik.ikind ())
  let sub = sub (Ik.ikind ())
  let mul = mul (Ik.ikind ())
  let div = div (Ik.ikind ())
  let rem = rem (Ik.ikind ())
  let lt = lt (Ik.ikind ())
  let gt = gt (Ik.ikind ())
  let le = le (Ik.ikind ())
  let ge = ge (Ik.ikind ())
  let eq = eq (Ik.ikind ())
  let ne = ne (Ik.ikind ())
  let lognot = lognot (Ik.ikind ())
  let logand = logand (Ik.ikind ())
  let logor = logor (Ik.ikind ())
  let logxor = logxor (Ik.ikind ())
  let shift_left = shift_left (Ik.ikind ())
  let shift_right = shift_right (Ik.ikind ())
  let c_lognot = c_lognot (Ik.ikind ())
  let c_logand = c_logand (Ik.ikind ())
  let c_logor = c_logor (Ik.ikind ())

  let of_int = of_int (Ik.ikind ())
  let of_bool = of_bool (Ik.ikind ())

  let bot () = bot_of (Ik.ikind ())
  let top () = top_of (Ik.ikind ())
  let is_top = is_top_of (Ik.ikind ())

  let name () = GobPretty.sprintf "%s (%a)" (name ()) Cil.d_ikind (Ik.ikind ())

  let arbitrary () = arbitrary (Ik.ikind ())
end

(* TODO: add ikinds to operators, arbitrary instead? *)
module IntegerSet (Ikind: IntDomain.Ikind) =
struct
  module Base =
  struct
    include IntDomain.Integers(IntOps.BigIntOps)
    let arbitrary () = QCheck.map_same_type (IntDomain.Size.cast (Ikind.ikind ())) (arbitrary ())
  end

  include SetDomain.Make(Base)

  let name () = "integerset"

  let lift1 = map
  let lift2 f x y = BatList.cartesian_product (elements x) (elements y) |> List.map (Batteries.uncurry f) |> of_list

  let neg  = lift1 Base.neg
  let add  = lift2 Base.add
  let sub  = lift2 Base.sub
  let mul  = lift2 Base.mul
  let div  = lift2 Base.div
  let rem  = lift2 Base.rem

  let lt = lift2 Base.lt
  let gt = lift2 Base.gt
  let le = lift2 Base.le
  let ge = lift2 Base.ge
  let eq = lift2 Base.eq
  let ne = lift2 Base.ne

  let lognot = lift1 Base.lognot
  let logand = lift2 Base.logand
  let logor  = lift2 Base.logor
  let logxor = lift2 Base.logxor
  let shift_left  = lift2 Base.shift_left
  let shift_right = lift2 Base.shift_right

  let c_lognot = lift1 Base.c_lognot
  let c_logand = lift2 Base.c_logand
  let c_logor  = lift2 Base.c_logor
end


module Valid (CD: (module type of IntegerSet (IntDomain.PtrDiffIkind))) (AD: OldSWithIkind) (AF: AbstractionDomainProperties.AbstractFunction with type c := CD.t and type a := AD.t): DomainProperties.S =
struct
  include AbstractionDomainProperties.ValidTest (CD) (AD) (AF)

  let not_bot a = not (CD.is_empty a)
  let none_bot (a,b) = not_bot a && not_bot b

  let valid_neg = make_valid1 ~name:"neg" ~cond:not_bot CD.neg AD.neg
  let valid_add = make_valid2 ~name:"add" ~cond:none_bot CD.add AD.add
  let valid_sub = make_valid2 ~name:"sub" ~cond:none_bot CD.sub AD.sub
  let valid_mul = make_valid2 ~name:"mul" ~cond:none_bot CD.mul AD.mul

  let snd_not_0 (a, b) = none_bot (a,b) && not (CD.mem Z.zero b) (* CD (IntegerSet) can't handle because no top *)
  let valid_div = make_valid2 ~name:"div" ~cond:snd_not_0 CD.div AD.div
  let valid_rem = make_valid2 ~name:"rem" ~cond:snd_not_0 CD.rem AD.rem

  let valid_lt = make_valid2 ~name:"lt" ~cond:none_bot CD.lt AD.lt
  let valid_gt = make_valid2 ~name:"gt" ~cond:none_bot CD.gt AD.gt
  let valid_le = make_valid2 ~name:"le" ~cond:none_bot CD.le AD.le
  let valid_ge = make_valid2 ~name:"ge" ~cond:none_bot CD.ge AD.ge
  let valid_eq = make_valid2 ~name:"eq" ~cond:none_bot CD.eq AD.eq
  let valid_ne = make_valid2 ~name:"ne" ~cond:none_bot CD.ne AD.ne

  let valid_lognot = make_valid1 ~name:"lognot" ~cond:not_bot CD.lognot AD.lognot
  let valid_logand = make_valid2 ~name:"logand" ~cond:none_bot CD.logand AD.logand
  let valid_logor = make_valid2 ~name:"logor" ~cond:none_bot CD.logor AD.logor
  let valid_logxor = make_valid2 ~name:"logxor" ~cond:none_bot CD.logxor AD.logxor

  let defined_shift (a, b) =
    let max_shift = Z.of_int @@ snd @@ IntDomain.Size.bits (AD.Ikind.ikind ()) in
    CD.for_all (fun x -> Z.compare Z.zero x <= 0 && Z.compare x max_shift <= 0) b
  let shift_cond p = none_bot p && defined_shift p
  let valid_shift_left = make_valid2 ~name:"shift_left" ~cond:shift_cond CD.shift_left AD.shift_left
  let valid_shift_right = make_valid2 ~name:"shift_right" ~cond:shift_cond CD.shift_right AD.shift_right

  let valid_c_lognot = make_valid1 ~name:"c_lognot" ~cond:not_bot CD.c_lognot AD.c_lognot
  let valid_c_logand = make_valid2 ~name:"c_logand" ~cond:none_bot CD.c_logand AD.c_logand
  let valid_c_logor = make_valid2 ~name:"c_logor" ~cond:none_bot CD.c_logor AD.c_logor

  let tests = [
    valid_neg;
    valid_add;
    valid_sub;
    valid_mul;
    valid_div;
    valid_rem;

    valid_lt;
    valid_gt;
    valid_le;
    valid_ge;
    valid_eq;
    valid_ne;

    valid_lognot;
    valid_logand;
    valid_logor;
    valid_logxor;
    valid_shift_left;
    valid_shift_right;

    valid_c_lognot;
    valid_c_logand;
    valid_c_logor
  ]
end

module All (D: OldSWithIkind): DomainProperties.S =
struct
  module A = DomainProperties.All (D)

  (* TODO: deduplicate *)
  module AD = D
  module CD = IntegerSet (AD.Ikind)
  module AF =
  struct
    let abstract s = CD.fold (fun c a -> AD.join (AD.of_int c) a) s (AD.bot ())
    let check_leq s x  = CD.for_all (fun c -> AD.leq (AD.of_int c) x) s
  end

  module M = AbstractionDomainProperties.Monotone (CD) (D) (AF)
  module V = Valid (CD) (D) (AF)

  let tests = A.tests @ M.tests @ V.tests
end

module AllNonAssoc (D: OldSWithIkind): DomainProperties.S =
struct
  module A = DomainProperties.AllNonAssoc (D)

  module AD = D
  module CD = IntegerSet (AD.Ikind)
  module AF =
  struct
    let abstract s = CD.fold (fun c a -> AD.join (AD.of_int c) a) s (AD.bot ())
    let check_leq s x  = CD.for_all (fun c -> AD.leq (AD.of_int c) x) s
  end

  module M = AbstractionDomainProperties.Monotone (CD) (D) (AF)
  module V = Valid (CD) (D) (AF)

  let tests = A.tests @ M.tests @ V.tests
end
