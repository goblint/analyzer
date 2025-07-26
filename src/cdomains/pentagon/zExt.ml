open Z
(** 
   Extension of the Zarith types and funcitons.
   The values represent arbitrary precision integers and also negative or positive infinity.
*)
module ZExt =
struct
  type t = PosInfty | NegInfty | Arb of Z.t

  let hash (z: t) = 
    match z with
    | PosInfty -> 0x186e81bd (* random constant *)
    | NegInfty -> 0xc8590e8 (* random constant *)
    | Arb(z) -> Z.hash z

  let equal (z1: t) (z2: t) = 
    match z1, z2 with
    | PosInfty, PosInfty -> true
    | NegInfty, NegInfty -> true
    | Arb(z1), Arb(z2) -> Z.equal z1 z2
    | _ -> false 

  let compare (z1: t) (z2: t) = 
    match z1, z2 with
    | NegInfty, NegInfty -> 0
    | PosInfty, PosInfty -> 0
    | NegInfty, _ -> -1 
    | _, NegInfty -> 1
    | PosInfty, _ -> 1
    | _, PosInfty -> -1
    | Arb(z1), Arb(z2) -> Z.compare z1 z2

  let (<*) z1 z2 = compare z1 z2 < 0
  let (>*) z1 z2 = compare z1 z2 > 0
  let (=*) z1 z2 = compare z1 z2 = 0
  let (<=*) z1 z2 = compare z1 z2 <= 0
  let (>=*) z1 z2 = compare z1 z2 >= 0
  let (<>*) z1 z2 = compare z1 z2 <> 0


  let of_int i = Arb(Z.of_int i)

  let of_float f =
    if Float.is_nan f then 
      failwith "ZExt.of_float: Tried to convert Nan." 
    else if Float.is_finite f then 
      Arb(Z.of_float f) 
    else if Float.sign_bit f then 
      NegInfty
    else 
      PosInfty

  let zero = of_int 0

  let to_string = function
    | NegInfty -> "-∞"
    | PosInfty -> "+∞"
    | Arb z -> Z.to_string z

  let neg = function
    | NegInfty -> PosInfty
    | PosInfty -> NegInfty
    | Arb z -> Arb(Z.neg z)

  let sign = function
    | NegInfty -> -1
    | PosInfty -> +1
    | Arb z -> Z.sign z

  let add_opt z1 z2 =
    match z1, z2 with
    | PosInfty, NegInfty -> None
    | NegInfty, PosInfty -> None
    | Arb z1, Arb z2 -> Some(Arb(Z.add z1 z2))
    | PosInfty, _ -> Some(PosInfty)
    | NegInfty, _ -> Some(NegInfty)
    | _, PosInfty -> Some(PosInfty)
    | _, NegInfty -> Some(NegInfty)

  let add_unsafe z1 z2 =
    match add_opt z1 z2 with
    | None -> failwith "ZExt.add_unsafe: Cannot add PosInfty and NegInfty or vice versa."
    | Some(s) -> s

  (** Alias for add_unsafe *)
  let add = add_unsafe

  (** Alias for add z1 (neg z2) *)
  let sub z1 z2 = add z1 (neg z2)

  let rem zext1 zext2 = 
    match zext1, zext2 with
    | Arb(z1), Arb(z2) ->
      Arb (Z.rem z1 z2)
    | _ -> failwith "ZExt.rem: Only applicable for two Arb values."

  let rem_add zext1 zext2 =
    match zext1, zext2 with
    | Arb(z1), Arb(z2) ->
      let rem = Z.rem z1 z2 in
      if Z.sign rem < 0 then 
        Arb (Z.add rem z2)
      else
        Arb(rem)
    | _ -> failwith "ZExt.rem_add: Only applicable for two Arb values."

  let rec mul z1 z2 =
    match z1, z2 with
    | Arb z1, Arb z2 -> Arb(Z.mul z1 z2)
    | Arb(z1), z2 -> mul z2 (Arb z1)
    (** z1 is definitely a infty *)
    | z1, z2 ->
      if sign z2 < 0 then
        neg z1
      else
      if z2 =* zero then
        zero
      else
        z1

  let rec div z1 z2 =
    match z1, z2 with
    | Arb z1, Arb z2 -> Arb(Z.div z1 z2)
    | Arb(z1), z2 -> div z2 (Arb z1)
    (** z1 is definitely a infty *)
    | z1, z2 ->
      if sign z2 < 0 then
        neg z1
      else
      if z2 =* zero then
        zero
      else
        z1

  let pow z1 z2 =
    if sign z2 < 0 then failwith "ZExt.pow: z2 should be non negative" else
      match z1, z2 with
      | Arb z1, Arb z2 when Z.sign z1 < 0 && not (Z.fits_nativeint z2) -> 
        if Z.is_even z2 then PosInfty else NegInfty
      | Arb z1, Arb z2 when Z.sign z1 > 0 && not (Z.fits_nativeint z2) -> PosInfty
      | _, Arb z when Z.zero = z -> (of_int 1)
      | Arb z, _ when Z.zero = z -> zero 
      | Arb z, _ when Z.one = z -> (of_int 1)
      | Arb z1, Arb z2 -> (Arb(Z.pow z1 (Z.to_int z2)))
      | z1, PosInfty when sign z1 < 0 -> failwith "ZExt.pow: Cannot determine whether result is NegInfty or PosInfty (or -1 or 1 for z1 = -1) -> depends on the side of the interval"
      | PosInfty, _ | _, PosInfty -> PosInfty
      | NegInfty, Arb z -> if Z.is_even z then PosInfty else NegInfty
      | _, NegInfty -> failwith "This shouldn't happen (caught in second line of ZExt.pow)"

  let abs z1 = if z1 <* zero then neg z1 else z1

  let max z1 z2 = if z1 >* z2 then z1 else z2

  let min z1 z2 = if z1 <* z2 then z1 else z2

  (** Taken from module IArith *)
  let min4 a b c d = min (min a b) (min c d)

  (** Taken from module IArith *)
  let max4 a b c d = max (max a b) (max c d)

end

module ZExtOps =
struct 

  let (<*) = ZExt.(<*)
  let (>*) = ZExt.(>*)
  let (=*) = ZExt.(=*)
  let (<=*) = ZExt.(<=*)
  let (>=*) = ZExt.(>=*)
  let (<>*) = ZExt.(<>*)

end