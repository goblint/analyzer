module IntegerSet =
struct
  module Base = IntDomain.Integers

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

  let bitnot = lift1 Base.bitnot
  let bitand = lift2 Base.bitand
  let bitor  = lift2 Base.bitor
  let bitxor = lift2 Base.bitxor
  let shift_left  = lift2 Base.shift_left
  let shift_right = lift2 Base.shift_right

  let lognot = lift1 Base.lognot
  let logand = lift2 Base.logand
  let logor  = lift2 Base.logor
end

module CD = IntegerSet
module AF (AD: IntDomain.S) =
struct
  let abstract s = CD.fold (fun c a -> AD.join (AD.of_int c) a) s (AD.bot ())
end

module Valid (AD: IntDomain.S): DomainProperties.S =
struct
  include AbstractionDomainProperties.ValidTest (CD) (AD) (AF (AD))

  let valid_neg = make_valid1 ~name:"neg" CD.neg AD.neg
  let valid_add = make_valid2 ~name:"add" CD.add AD.add
  let valid_sub = make_valid2 ~name:"sub" CD.sub AD.sub
  let valid_mul = make_valid2 ~name:"mul" CD.mul AD.mul

  let snd_not_0 (_, b) = not (CD.mem 0L b) (* CD (IntegerSet) can't handle because no top *)
  let valid_div = make_valid2 ~name:"div" ~cond:snd_not_0 CD.div AD.div
  let valid_rem = make_valid2 ~name:"rem" ~cond:snd_not_0 CD.rem AD.rem

  let valid_lt = make_valid2 ~name:"lt" CD.lt AD.lt
  let valid_gt = make_valid2 ~name:"gt" CD.gt AD.gt
  let valid_le = make_valid2 ~name:"le" CD.le AD.le
  let valid_ge = make_valid2 ~name:"ge" CD.ge AD.ge
  let valid_eq = make_valid2 ~name:"eq" CD.eq AD.eq
  let valid_ne = make_valid2 ~name:"ne" CD.ne AD.ne

  let valid_bitnot = make_valid1 ~name:"bitnot" CD.bitnot AD.bitnot
  let valid_bitand = make_valid2 ~name:"bitand" CD.bitand AD.bitand
  let valid_bitor = make_valid2 ~name:"bitor" CD.bitor AD.bitor
  let valid_bitxor = make_valid2 ~name:"bitxor" CD.bitxor AD.bitxor
  let valid_shift_left = make_valid2 ~name:"shift_left" CD.shift_left AD.shift_left
  let valid_shift_right = make_valid2 ~name:"shift_right" CD.shift_right AD.shift_right

  let valid_lognot = make_valid1 ~name:"lognot" CD.lognot AD.lognot
  let valid_logand = make_valid2 ~name:"logand" CD.logand AD.logand
  let valid_logor = make_valid2 ~name:"logor" CD.logor AD.logor

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

    valid_bitnot;
    valid_bitand;
    valid_bitor;
    valid_bitxor;
    valid_shift_left;
    valid_shift_right;

    valid_lognot;
    valid_logand;
    valid_logor
  ]
end

module All (D: IntDomain.S): DomainProperties.S =
struct
  module A = DomainProperties.All (D)
  module M = AbstractionDomainProperties.Monotone (CD) (D) (AF (D))
  module V = Valid (D)

  let tests = A.tests @ M.tests @ V.tests
end
