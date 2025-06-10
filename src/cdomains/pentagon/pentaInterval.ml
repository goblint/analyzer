open ZExt
open Batteries

(**
   Stores functions and types for single intervals $\mathbb{Z}^\infty$
   according to the pentagon domains semantics. Beware, this module is NOT generic.
*)
module Interval =
struct
  type t = (ZExt.t * ZExt.t) [@@deriving eq, hash, ord]

  let top () = ((ZExt.NegInfty, ZExt.PosInfty): t)

  let bot () = ((ZExt.PosInfty, ZExt.NegInfty): t)

  let is_top (x:t) = ((ZExt.NegInfty, ZExt.PosInfty) = x)

  let is_bot ((l, u): t) = l > u

  (** Interval intersection *)
  let inter ((l1, u1): t) ((l2, u2): t) =
    let max_lb = if l1 <= l2 then l2 else l1 in
    let min_ub = if u1 <= u2 then u1 else u2 in
    ((max_lb, min_ub): t)

  let add ((l1, u1): t) ((l2, u2): t) =
    let ( + ) = ZExt.add_opt in
    ((Option.default ZExt.NegInfty (l1 + l2), Option.default ZExt.PosInfty (u1 + u2)): t)

  (** Taken from module IArith *)
  let mul ((x1, x2): t) ((y1, y2): t) =
    let ( * ) = ZExt.mul in
    ((
      ZExt.min4 (x1 * y1) (x1 * y2) (x2 * y1) (x2 * y2),
      ZExt.max4 (x1 * y1) (x1 * y2) (x2 * y1) (x2 * y2)
    ): t)

  (** Taken from module IArith *)
  let div ((x1, x2): t) ((y1, y2): t) =
    if y1 <= ZExt.zero && y2 >= ZExt.zero then top() else
      let ( / ) = ZExt.div in
      ((
        ZExt.min4 (x1 / y1) (x1 / y2) (x2 / y1) (x2 / y2),
        ZExt.max4 (x1 / y1) (x1 / y2) (x2 / y1) (x2 / y2)
      ): t)


  (** Checks for bot interval. *)
  let sup (i: t) = if is_bot i then ZExt.NegInfty else snd i;;

  (** Checks for bot interval. *)
  let inf (i: t) = if is_bot i then ZExt.PosInfty else fst i;;

  (** Checks whether the lower bound is -infty, i.e., unbound *)
  (**
     TODO: Verfiy that `inf` is correct here. Alternative `fst`.
  *)
  let no_lowerbound (i: t) = ZExt.NegInfty = inf i

  (** Checks whether the upper bound is +infty, i.e., unbound *)
  (**
     TODO: Verfiy that `sup` is correct here. Alternative `snd`.
  *)
  let no_upperbound (i: t) = ZExt.PosInfty = sup i

  let rem (i1: t) i2 =
    (* i1 % i2 *)
    let (l2, u2) = i2 in
    if l2 <= ZExt.zero && u2 >= ZExt.zero then
      top() 
    else if no_lowerbound i2 || no_upperbound i2 then
      i1
    else
      let ub_minus_1 = ZExt.add_unsafe (ZExt.max (ZExt.abs l2) (ZExt.abs u2)) (ZExt.of_int (-1)) in
      inter i1 ((ZExt.neg ub_minus_1, ub_minus_1): t)


  (**
     We assume that i1 and i2 are well-formed, i.e. not bot/empty.
  *)
  let pow ((l1, u1): t) ((l2, u2): t) =
    if l2 < ZExt.zero then top () (* x ^ (-1) is unsupported operation on ints ==> we treat it as undefined behavior, same as division by 0 *)
    else
      match u2 with
      | PosInfty -> if l1 <= ZExt.of_int (-2) then top () (* can create arbitrarily big numbers with (-2) ^ x *)
        else if l1 >= ZExt.zero then (ZExt.pow l1 l2, ZExt.pow u1 PosInfty)
        else (* l1 = -1 *)
        if u1 = ZExt.of_int (-1) then (ZExt.of_int (-1), ZExt.of_int 1)
        else (ZExt.of_int (-1), ZExt.pow u1 PosInfty)
      | NegInfty -> failwith "Interval.pow should not happen"
      | Arb u2z when l1 < ZExt.zero ->
        if l2 = u2 then (* special case because we don't have an even AND an odd number ==> may be impossible to mirror negative numbers *)
          let exp = l2 in
          if exp = ZExt.zero then (ZExt.of_int 1, ZExt.of_int 1) else
          if Z.is_even u2z then
            if u1 >= ZExt.zero then
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
        if Z.is_even u2z then
          let l = ZExt.pow l1 (ZExt.add_unsafe u2 (ZExt.of_int (-1))) in
          let max_abs = ZExt.max (ZExt.abs l1) (ZExt.abs u1) in
          let u = ZExt.pow max_abs u2 in
          (l, u)
        else
          let l = ZExt.pow l1 u2 in
          let u' = ZExt.pow l1 (ZExt.add_unsafe u2 (ZExt.of_int (-1))) in
          let u'' = if u1 > (ZExt.of_int 0) then ZExt.pow u1 u2 else u' in
          let u = ZExt.max u' u'' in
          (l, u)
      | _ -> (* i1 is nonnegative ==> no special cases here :) *)
        let l = ZExt.pow l1 l2 in
        let u = ZExt.pow u1 u2 in
        (l, u)

  (**
     Creates a single interval from the supplied integer values.
  *)
  let create i1 i2 = (ZExt.of_int i1, ZExt.of_int i2)

  let leq ((l1, u1): t) ((l2, u2): t) = l2 <= l1 && u1 <= u2

  let union ((l1, u1): t) ((l2, u2): t) = (ZExt.min l1 l2, ZExt.max u1 u2)

  let widen (l1, u1) (l2, u2) =
    let l = if l1 <= l2 then l2 else ZExt.NegInfty in
    let u = if u2 <= u1 then u2 else ZExt.PosInfty in
    (l, u)

  let narrow (i1: t) (i2: t) = 
    inter i1 i2

end