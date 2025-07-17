open ZExt
open Batteries
open StringUtils

(**
   Stores functions and types for single intervals $\mathbb{Z}^\infty$
   according to the pentagon domains semantics. Beware, this module is NOT generic.
*)
module Intv =
struct
  include ZExtOps

  type t = (ZExt.t * ZExt.t) [@@deriving eq, hash, ord]

  let to_string ((lb, ub): t) = Printf.sprintf "[%s,%s]" (StringUtils.int32_bound_str lb) (StringUtils.int32_bound_str ub)

  let top () = ((ZExt.NegInfty, ZExt.PosInfty): t)

  let bot () = ((ZExt.PosInfty, ZExt.NegInfty): t)

  let is_top (x:t) = ((ZExt.NegInfty, ZExt.PosInfty) = x)

  let is_bot ((l, u): t) = u <* l

  let mem a (l, u) = l <=* a && u >=* a

  (** Checks for bot interval. *)
  let sup (i: t) = if is_bot i then ZExt.NegInfty else snd i;;

  (** Checks for bot interval. *)
  let inf (i: t) = if is_bot i then ZExt.PosInfty else fst i;;

  (** Intv intersection *)
  let inter ((l1, u1): t) ((l2, u2): t) =
    let max_lb = if l1 <=* l2 then l2 else l1 in
    let min_ub = if u1 <=* u2 then u1 else u2 in
    ((max_lb, min_ub): t)

  let add ((l1, u1): t) ((l2, u2): t) =
    let ( + ) = ZExt.add_opt in
    ((Option.default ZExt.NegInfty (l1 + l2), Option.default ZExt.PosInfty (u1 + u2)): t)

  let neg intv = (ZExt.neg (sup intv), ZExt.neg (inf intv))

  let sub intv_1 intv_2 =
    let intv_2 = neg intv_2 in
    add intv_1 intv_2

  (** Taken from module IArith *)
  let mul ((x1, x2): t) ((y1, y2): t) =
    let ( * ) = ZExt.mul in
    ((
      ZExt.min4 (x1 * y1) (x1 * y2) (x2 * y1) (x2 * y2),
      ZExt.max4 (x1 * y1) (x1 * y2) (x2 * y1) (x2 * y2)
    ): t)

  (** Taken from module IArith *)
  let div ((x1, x2): t) ((y1, y2): t) =
    if y1 <=* ZExt.zero && y2 >=* ZExt.zero then top() else
      let ( / ) = ZExt.div in
      ((
        ZExt.min4 (x1 / y1) (x1 / y2) (x2 / y1) (x2 / y2),
        ZExt.max4 (x1 / y1) (x1 / y2) (x2 / y1) (x2 / y2)
      ): t)

  let no_lowerbound (i: t) = ZExt.NegInfty = inf i

  let no_upperbound (i: t) = ZExt.PosInfty = sup i

  (* taken from https://people.eng.unimelb.edu.au/pstuckey/papers/toplas15.pdf *)
  let rec rem (i1: t) i2 =
    let (l2, u2) = i2 in
    let zero = ZExt.zero in
    if inf i2 <=* zero && sup i2 >=* zero then top () (* cannot rule out division by zero *)
    else if no_lowerbound i2 || no_upperbound i2 then i1
    else if inf i1 <* zero && sup i1 >* zero then (* split the interval into negative and nonnegative parts *)
      let (l, _), (_, u) = (rem (inf i1, ZExt.of_int (-1)) i2), (rem (zero, sup i1) i2)
      in (l, u)
    else
      let ( - ), ( * ), ( / ) = sub, mul, div in
      let quotient = i1 / i2 in
      if inf quotient =* sup quotient (* if every possible division yields the same result, we can use the remainders *)
      then i1 - (quotient * i2)
      else
        let ub_minus_1 = ZExt.sub (ZExt.max (ZExt.abs l2) (ZExt.abs u2)) (ZExt.of_int 1) in
        if ZExt.sign (sup i1) <= 0 then (ZExt.max (ZExt.neg ub_minus_1) (inf i1), zero)
        else (zero, ZExt.min ub_minus_1 (sup i1))


  (**
     We assume that i1 and i2 are well-formed, i.e. not bot/empty.
  *)
  let pow ((l1, u1): t) ((l2, u2): t) =
    if l2 <* ZExt.zero then top () (* x ^ (-1) is unsupported operation on ints ==> we treat it as undefined behavior, same as division by 0 *)
    else
      match u2 with
      | PosInfty -> if l1 <=* ZExt.of_int (-2) then top () (* can create arbitrarily big numbers with (-2) ^ x *)
        else if l1 >=* ZExt.zero then (ZExt.pow l1 l2, ZExt.pow u1 PosInfty)
        else (* l1 = -1 *)
        if u1 =* ZExt.of_int (-1) then (ZExt.of_int (-1), ZExt.of_int 1)
        else (ZExt.of_int (-1), ZExt.pow u1 PosInfty)
      | NegInfty -> failwith "Intv.pow should not happen"
      | Arb u2z when l1 <* ZExt.zero ->
        if l2 =* u2 then (* special case because we don't have an even AND an odd number ==> either impossible to mirror negative numbers or everything gets nonnegative *)
          let exp = l2 in
          if exp =* ZExt.zero then (ZExt.of_int 1, ZExt.of_int 1) else
          if Z.is_even u2z then
            if u1 >=* ZExt.zero then
              (* i1 contains negative and nonnegative numbers, exp != 0 is even ==> lb = 0, ub depends on greater abs value of bounds *)
              let max_abs = ZExt.max (ZExt.abs l1) u1 in
              let u = ZExt.pow max_abs exp in
              (ZExt.zero, u) else
              (* x -> x ^ n is monotonically decreasing for even n and negative x *)
              let l = ZExt.pow u1 exp in
              let u = ZExt.pow l1 exp in
              (l, u)
          else (* exp is odd *)
            (* x -> x ^ n is monotonically increasing for odd n *)
            (ZExt.pow l1 exp, ZExt.pow u1 exp)
        else
          (* we have at least one even and one odd number in the exponent ==> negative numbers can be mirrored if needed *)
          let greatest_even = if Z.is_even u2z then u2 else ZExt.sub u2 (ZExt.of_int 1) in
          let greatest_odd = if Z.is_odd u2z then u2 else ZExt.sub u2 (ZExt.of_int 1) in
          let l = ZExt.pow l1 greatest_odd in
          let u' = ZExt.pow l1 greatest_even in
          let u'' = if ZExt.sign u1 > 0 then ZExt.pow u1 u2 else u' in
          (l, ZExt.max u' u'')
      | _ -> (* i1 is nonnegative ==> no special cases here :) *)
        let l = ZExt.pow l1 l2 in
        let u = ZExt.pow u1 u2 in
        (l, u)


  (**
     Creates a single interval from the supplied integer values.
  *)
  let create i1 i2 = (ZExt.of_int i1, ZExt.of_int i2)

  let create_const i = (ZExt.of_int i, ZExt.of_int i)

  let create_const_of_z z = (ZExt.Arb z, ZExt.Arb z)

  let leq ((l1, u1): t) ((l2, u2): t) = l2 <=* l1 && u1 <=* u2

  let union ((l1, u1): t) ((l2, u2): t) = (ZExt.min l1 l2, ZExt.max u1 u2)

  let widen (l1, u1) (l2, u2) =
    let l = if l1 <=* l2 then l2 else ZExt.NegInfty in
    let u = if u2 <=* u1 then u2 else ZExt.PosInfty in
    (l, u)

  (*
    Taken from Compiler Design: Analysis and Transformation -
    https://link.springer.com/book/10.1007/978-3-642-17548-0.
   *)
  let narrow (l1, u1) (l2, u2) = 
    let l = if l1 =* ZExt.NegInfty then l2 else l1 in
    let u = if u2 =* ZExt.PosInfty then u2 else u1 in
    (l, u)

  let lt ((_, ub1):t) ((lb2, _):t) = 
    ub1 < lb2

  let gt ((lb1, _):t) ((_, ub2):t) =
    lb1 > ub2


  let to_z_opt_intv (z_ext_intv : t) =
    match z_ext_intv with
    | ZExt.Arb s1, ZExt.Arb s2 -> Some(s1), Some(s2)
    | ZExt.Arb s1, _ -> Some(s1), None
    | _, ZExt.Arb s2 -> None, Some(s2)
    | _, _ -> None, None

end