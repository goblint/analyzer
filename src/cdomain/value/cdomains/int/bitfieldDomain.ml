open IntDomain0

module InfixIntOps (Ints_t : IntOps.IntOps) = struct
  let (&:) = Ints_t.logand
  let (|:) = Ints_t.logor
  let (^:) = Ints_t.logxor
  let (!:) = Ints_t.lognot
  let (<<:) = Ints_t.shift_left
  let (>>:) = Ints_t.shift_right
  let (<:) = fun a b -> Ints_t.compare a b < 0
  let (=:) = fun a b -> Ints_t.compare a b = 0
  let (>:) = fun a b -> Ints_t.compare a b > 0
  let (<>:) = fun a b -> Ints_t.compare a b <> 0

  let (+:) = Ints_t.add
  let (-:) = Ints_t.sub
  let ( *: ) = Ints_t.mul
  let (/:) = Ints_t.div
  let (%:) = Ints_t.rem

  let (>>.) = fun a b -> a >>: b |: !:((Ints_t.one <<: b) -: Ints_t.one)
end

(*
  Operations in the abstract domain mostly based on

  "Abstract Domains for Bit-Level Machine Integer and Floating-point Operations"
  of Antoine Miné
  https://doi.org/10.29007/b63g

  and

  the bachelor thesis "Integer Abstract Domains"
  of Tomáš Brukner
  https://is.muni.cz/th/kasap/thesis.pdf
*)

(* Bitfield arithmetic, without any overflow handling etc. *)
module BitfieldArith (Ints_t : IntOps.IntOps) = struct

  include InfixIntOps (Ints_t)

  let zero_mask = Ints_t.zero

  (* one_mask corresponds to (-1). It has an infinite amount of 1 bits *)
  let one_mask = !:zero_mask

  let of_int x = (!:x, x)

  let join (z1,o1) (z2,o2) = (z1 |: z2, o1 |: o2)
  let meet (z1,o1) (z2,o2) = (z1 &: z2, o1 &: o2)

  let one = of_int Ints_t.one
  let zero = of_int Ints_t.zero
  let top_bool = join one zero

  let bits_known (z,o) = z ^: o
  let bits_invalid (z,o) = !:(z |: o)

  let is_const (z,o) = (z ^: o) =: one_mask

  let is_invalid (z,o) =
    not (!:(z |: o) = Ints_t.zero)

  let nabla x y= if x =: (x |: y) then x else one_mask

  let widen (z1,o1) (z2,o2) = (nabla z1 z2, nabla o1 o2)

  let lognot (z,o) = (o,z)

  let logxor (z1,o1)  (z2,o2) = ((z1 &: z2) |: (o1 &: o2),
                                 (z1 &: o2) |: (o1 &: z2))

  let logand (z1,o1) (z2,o2) = (z1 |: z2, o1 &: o2)

  let logor (z1,o1)  (z2,o2) = (z1 &: z2, o1 |: o2)

  let bitmask_up_to n = (Ints_t.one <<: n) -: Ints_t.one

  let nth_bit p n = Ints_t.one &: (p >>: n) =: Ints_t.one

  let min ik (z,o) =
    (* checking whether the MSB is set *)
    let signBit = Ints_t.one <<: ((Size.bit ik) - 1) in
    let isNegative = signBit &: o <>: Ints_t.zero in
    (* mask to set all bits outside of ik to 1 *)
    let signMask = !: (Ints_t.of_bigint (snd (Size.range ik))) in
    if GoblintCil.isSigned ik && isNegative then
      (* maximal number of 0 with correct sign extension *)
      Ints_t.to_bigint(signMask |: (!: z))
    else
      (* maximal number of 0 with correct sign extension *)
      Ints_t.to_bigint(!: z)

  let max ik (z,o) =
    (* checking whether the MSB is set *)
    let signBit = Ints_t.one <<: ((Size.bit ik) - 1) in
    let isPositive = signBit &: z <>: Ints_t.zero in
    (* mask to set all bits outside of ik to 1 *)
    let signMask = Ints_t.of_bigint (snd (Size.range ik)) in
    if GoblintCil.isSigned ik && isPositive then
      (* maximal number of 1 with correct sign extension*)
      Ints_t.to_bigint(signMask &: o)
    else
      (* maximal number of 1 *)
      Ints_t.to_bigint o

  (** [concretize bf] computes the set of all possible integer values represented by the bitfield [bf].

      @param (z,o) The bitfield to concretize.

      @info By default, the function generates all possible values that the bitfield can represent,
            which results in an exponential complexity of O(2^n) where [n] is the width of [ik].
            It is recommended to constrain the number of bits that are concretized to avoid non-termination.
  *)
  let rec concretize (z,o) =
    if is_const (z,o) then [o]
    else
      let bit = o &: Ints_t.one in
      concretize (z >>. 1, o >>: 1)
      |>
      match nth_bit z 0, nth_bit o 0 with
      | true, true -> List.concat_map (fun c -> [c <<: 1; (c <<: 1) |: Ints_t.one])
      | false, false -> failwith "Should not have happened: Invalid bit during concretization of a bitfield."
      | _ -> List.map (fun c -> c <<: 1 |: bit)

  let concretize bf = List.map Ints_t.to_int (concretize bf)

  let bit_width_of ik = snd @@ Size.bits ik

  let constrain_to_bit_width_of ik (z,o) =
    let mask = bitmask_up_to (Int.succ @@ Z.log2up @@ Z.of_int @@ bit_width_of ik) in
    (z |: !:mask, o &: mask)

  let shift_right ik (z,o) c =
    let msb_pos = Int.max 0 (Size.bit ik - c) in
    let sign_mask = !:(bitmask_up_to msb_pos) in
    if GoblintCil.isSigned ik && o <: Ints_t.zero then
      ((z >>: c) |: sign_mask, (o >>: c) |: sign_mask) (* sign extension in sar is impl. defined *)
    else
      ((z >>: c) |: sign_mask, o >>: c)

  let shift_left _ (z,o) c =
    let zero_mask = bitmask_up_to c in
    ((z <<: c) |: zero_mask, o <<: c)

  let join_shifts shift ik bf (z2,o2) =
    match is_const (z2,o2) with
    | true -> shift ik bf (Ints_t.to_int o2)
    | false ->
      (* Only values leq then inf_c {c | bf >> c = 0} are relevant. *)
      let defined_shifts = constrain_to_bit_width_of ik (z2, o2) in
      let shift_amounts = concretize defined_shifts in (* O(n) *)
      List.fold_left (fun acc c ->
          join acc @@ shift ik bf c
        ) (zero_mask, zero_mask) shift_amounts

  let shift_left = join_shifts shift_left

  let shift_right = join_shifts shift_right

  let nth_bit p n = if nth_bit p n then Ints_t.one else Ints_t.zero

  let is_power_of_two x = (x &: (x -: Ints_t.one) = Ints_t.zero)

  let has_neg_values ik b = Z.compare (min ik b) Z.zero < 0

  let has_only_neg_values ik b = Z.compare (max ik b) Z.zero < 0

  let exceeds_bit_width_of ik b = Z.compare (min ik b) (Z.of_int @@ bit_width_of ik) > 0

  let equals_bit_width_of ik b = Z.compare (min ik b) (Z.of_int @@ bit_width_of ik) = 0
end

module BitfieldFunctor (Ints_t : IntOps.IntOps): Bitfield_SOverflow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) = struct

  include InfixIntOps (Ints_t)

  let name () = "bitfield"
  type int_t = Ints_t.t

  (* the bitfield is represented as a tuple of two bitmasks zs and os. *)
  (* zs is the mask of all bits that may be zero, os is the mask of all bits that may be one *)
  (* Example: (zs, os) = (−1, 7) = (...1111,...0111) =...0??? represents the bitmask, *)
  (* where the last three bits are unknown, and all other bits are known to be 0 *)
  type t = (Ints_t.t * Ints_t.t) [@@deriving eq, ord, hash]

  module BArith = BitfieldArith (Ints_t)

  (* top = all bis are unknown*)
  let top () = (BArith.one_mask, BArith.one_mask)

  (* bot = all bits are invalid *)
  let bot () = (BArith.zero_mask, BArith.zero_mask)

  let top_of ?bitfield ik = (* TODO: use bitfield *)
    if GoblintCil.isSigned ik then top ()
    else (BArith.one_mask, Ints_t.of_bigint (snd (Size.range ik)))

  let bot_of ik = bot ()

  let to_pretty_bits (z,o) =
    let known_bitmask = BArith.bits_known (z,o) in
    let invalid_bitmask = BArith.bits_invalid (z,o) in

    (* converts the (zs,os) mask representation to a human readable string of the form 0b(0|1|?|⊥)...(0|1|?|⊥)+. *)
    (* Example: 0b0...01? should mean that the last bit is unknown, while all other bits are exactly known *)
    (* The ... (dots) are used to indicate an infinte repetition of the previous bit *)
    let rec create_pretty_bf_string o_mask z_mask known_bitmask invalid_bitmask acc =
      let current_bit_known = (known_bitmask &: Ints_t.one) = Ints_t.one in
      let current_bit_invalid = (invalid_bitmask &: Ints_t.one) = Ints_t.one in
      let bit_value = o_mask &: Ints_t.one in
      let bit =
        if current_bit_invalid then "⊥"
        else if not current_bit_known then "?"
        else Ints_t.to_string bit_value
      in
      if (o_mask = Ints_t.of_int (-1) || o_mask = Ints_t.zero) && (z_mask = Ints_t.of_int (-1) || z_mask = Ints_t.zero) then
        let prefix = bit ^ "..." ^ bit in
        prefix ^ acc
      else
        create_pretty_bf_string (o_mask >>: 1) (z_mask >>: 1) (known_bitmask >>: 1) (invalid_bitmask >>: 1) (bit ^ acc)
    in
    "0b" ^ create_pretty_bf_string o z known_bitmask invalid_bitmask ""

  let show t =
    if t = bot () then "⊥" else
      let (z,o) = t in
      if GobConfig.get_bool "dbg.full-output" then
        Printf.sprintf "{%s, (zs:%s, os:%s)}" (to_pretty_bits t) (Ints_t.to_string z) (Ints_t.to_string o)
      else
        to_pretty_bits t

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let maximal (z,o) =
    if (z <: Ints_t.zero) <> (o <: Ints_t.zero) then Some o
    else None

  let minimal (z,o) =
    if (z <: Ints_t.zero) <> (o <: Ints_t.zero) then Some (!:z)
    else None

  (* setting all bits outside of the ik range to the correct sign bit *)
  let wrap ik (z,o) =
    let (min_ik, max_ik) = Size.range ik in
    if GoblintCil.isSigned ik then
      let newz = (z &: (Ints_t.of_bigint max_ik)) |: ((Ints_t.of_bigint min_ik) *: (BArith.nth_bit z (Size.bit ik - 1))) in
      let newo = (o &: (Ints_t.of_bigint max_ik)) |: ((Ints_t.of_bigint min_ik) *: (BArith.nth_bit o (Size.bit ik - 1))) in
      (newz,newo)
    else
      let newz = z |: !:(Ints_t.of_bigint max_ik) in
      let newo = o &: (Ints_t.of_bigint max_ik) in
      (newz,newo)

  let norm ?(ov=false) ik (z,o) =
    if BArith.is_invalid (z,o) then
      bot ()
    else
      let new_bitfield = wrap ik (z,o) in
      if not ov || should_wrap ik then
        new_bitfield
      else
        top_of ik

  let cast_to ?torg ?(no_ov=false) ik (z,o) =
    let (min_ik, max_ik) = Size.range ik in
    let (underflow, overflow) = match torg with
      | None -> (false, false) (* ik does not change *)
      | Some (GoblintCil.Cil.TInt (old_ik, _) | TEnum ({ekind = old_ik; _}, _)) ->
        let underflow = Z.compare (BArith.min old_ik (z,o)) min_ik < 0 in
        let overflow = Z.compare max_ik (BArith.max old_ik (z,o)) < 0 in
        (underflow, overflow)
      | _ ->
        let isPos = z <: Ints_t.zero in
        let isNeg = o <: Ints_t.zero in
        let underflow = if GoblintCil.isSigned ik then (((Ints_t.of_bigint min_ik) &: z) <>: Ints_t.zero) && isNeg else isNeg in
        let overflow = (((!:(Ints_t.of_bigint max_ik)) &: o) <>: Ints_t.zero) && isPos in
        (underflow, overflow)
    in
    let overflow_info = {underflow; overflow} in
    (norm ~ov:(underflow || overflow) ik (z,o), overflow_info)

  let cast_to ~kind ?torg ?(no_ov=false) ik (z,o) =
    if ik = GoblintCil.IBool then (
      let may_zero =
        if Ints_t.equal z BArith.one_mask then (* zero bit may be in every position (one_mask) *)
          BArith.zero
        else
          bot () (* must be non-zero, so may not be zero *)
      in
      let may_one =
        if Ints_t.equal o BArith.zero_mask then (* one bit may be in no position (zero_mask) *)
          bot () (* must be zero, so may not be one *)
        else
          BArith.one
      in
      (BArith.join may_zero may_one, {underflow=false; overflow=false})
    )
    else
      cast_to ?torg ~no_ov ik (z,o)

  let join ik b1 b2 = norm ik @@ (BArith.join b1 b2)

  let meet ik x y = norm ik @@ (BArith.meet x y)

  let leq (z1,o1) (z2,o2) =
    (* If a bit can have a certain value in parameter 1, it must be able to have the same value in parameter 2. *)
    (* This corresponds to bitwise implication. *)
    let implies a b = Ints_t.equal (!:a |: b) BArith.one_mask in
    implies z1 z2 && implies o1 o2

  let widen ik x y = norm ik @@ BArith.widen x y

  let narrow ik x y = meet ik x y

  let of_int ik (x: int_t) =
    let (min_ik, max_ik) = Size.range ik in
    let y = Ints_t.to_bigint x in
    let underflow = Z.compare y min_ik < 0 in
    let overflow = Z.compare max_ik y < 0 in
    let overflow_info = {underflow=underflow; overflow=overflow} in
    (norm ~ov:(underflow || overflow) ik (BArith.of_int x), overflow_info)

  let to_int (z,o) = if is_bot (z,o) then None else
    if BArith.is_const (z,o) then Some o
    else None

  let equal_to i bf =
    if BArith.of_int i = bf then `Eq
    else if leq (BArith.of_int i) bf then `Top
    else `Neq

  (* Conversions *)

  type bit_status = Zero | One | Top

  let of_interval ik (x,y) =
    let (min_ik, max_ik) = Size.range ik in
    let startv = Ints_t.max x (Ints_t.of_bigint min_ik) in
    let endv= Ints_t.min y (Ints_t.of_bigint max_ik) in

    (* constructs a bitfield of the interval: for each bit check if the smallest number that is greater than startv and has the bit flipped is still in the interval *)
    (* If the flipped value is still in the interval, the bit can be 0 or 1 which means it must be described as top, otherwise the bit cant change and is the same as in startv *)
    (* Runtime: O(bits) *)
    let rec construct_bitfield pos (acc_z, acc_o) =
      if pos < 0 then (acc_z, acc_o)
      else
        let position_mask = Ints_t.shift_left Ints_t.one pos in
        let mask = Ints_t.sub position_mask Ints_t.one in
        let remainder = Ints_t.logand startv mask in

        let smallest_number_with_flipped_bit = Ints_t.add (Ints_t.sub startv remainder) position_mask in

        let bit_status =
          if Ints_t.compare smallest_number_with_flipped_bit endv <= 0 then
            Top
          else
            (* bit can't change inside the interval -> it's the same as in startv *)
          if Ints_t.equal (Ints_t.logand (Ints_t.shift_right startv pos) Ints_t.one) Ints_t.one then
            One
          else
            Zero
        in

        (* set bit in masks depending on bit_status *)
        let new_acc =
          match bit_status with
          | Top-> (Ints_t.logor position_mask acc_z, Ints_t.logor position_mask acc_o)
          | One-> (Ints_t.logand (Ints_t.lognot position_mask) acc_z, Ints_t.logor position_mask acc_o)
          | Zero-> (Ints_t.logor position_mask acc_z, Ints_t.logand (Ints_t.lognot position_mask) acc_o)
        in
        construct_bitfield (pos - 1) new_acc
    in
    let result = construct_bitfield (Size.bit ik - 1) (bot()) in
    let casted = (Ints_t.of_bigint (Size.cast ik ((Ints_t.to_bigint (fst result)))), Ints_t.of_bigint (Size.cast ik ((Ints_t.to_bigint (snd result)))))
    in (wrap ik casted, {underflow=false; overflow=false})

  let of_bool _ik = function true -> BArith.one | false -> BArith.zero

  let to_bool d =
    if not (leq BArith.zero d) then Some true
    else if equal d BArith.zero then Some false
    else None

  let of_bitfield ik x = norm ik x

  let to_bitfield ik x = norm ik x

  let of_congruence ik (c,m) =
    if m = Ints_t.zero then
      of_int ik c |> fst
    else if BArith.is_power_of_two m && Ints_t.one <>: m then
      let mod_mask = !:(m -: Ints_t.one) in
      let z = mod_mask |: (!: c) in
      let o = mod_mask |: c in
      norm ik (z,o)
    else top_of ik

  (* Logic *)

  let log1 f ik i1 = match to_bool i1 with
    | None -> top_of ik
    | Some x -> of_bool ik (f x)

  let log2 f ~annihilator ik i1 i2 = match to_bool i1, to_bool i2 with
    | Some x, _ when x = annihilator -> of_bool ik annihilator
    | _, Some y when y = annihilator -> of_bool ik annihilator
    | Some x, Some y -> of_bool ik (f x y)
    | _              -> top_of ik

  let c_logor = log2 (||) ~annihilator:true

  let c_logand = log2 (&&) ~annihilator:false

  let c_lognot ik i1 = log1 not ik i1


  (* Bitwise *)

  let logxor ik i1 i2 = BArith.logxor i1 i2 |> norm ik

  let logand ik i1 i2 = BArith.logand i1 i2 |> norm ik

  let logor  ik i1 i2 = BArith.logor i1 i2 |> norm ik

  let lognot ik i1 = BArith.lognot i1 |> norm ik

  let top_on_undefined_shift ?(is_shift_left=false) ik a b do_shift =
    let no_ov = {underflow=false; overflow=false} in
    if BArith.exceeds_bit_width_of GoblintCil.ILongLong b || BArith.equals_bit_width_of GoblintCil.ILongLong b then
      (top_of ik,
       match is_shift_left, GoblintCil.isSigned ik && BArith.has_neg_values ik a with
       | true, false -> {underflow=false; overflow=true}
       | true, true when BArith.has_only_neg_values ik a -> {underflow=true; overflow=false}
       | true, true -> {underflow=true; overflow=true}
       | _ -> no_ov
      )
    else
    if GoblintCil.isSigned ik && BArith.has_only_neg_values ik b then (top_of ik, no_ov) else do_shift ()

  let shift_right ik a b = match is_bot a, is_bot b with
    | true, true -> bot_of ik, {underflow=false; overflow=false}
    | true,_ | _,true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s >> %s" (show a) (show b)))
    | _ ->
      top_on_undefined_shift ik a b @@ fun () ->
      (norm ik (BArith.shift_right ik a b), {underflow=false; overflow=false})

  let shift_left ik a b = match is_bot a, is_bot b with
    | true, true -> bot_of ik, {underflow=false; overflow=false}
    | true,_ | _,true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s << %s" (show a) (show b)))
    | _ ->
      top_on_undefined_shift ~is_shift_left:true ik a b @@ fun () ->
      let max_shift = if Z.fits_int (BArith.max ik b) then Z.to_int (BArith.max ik b) else Int.max_int in
      let (min_ik, max_ik) = Size.range ik in
      let min_res = if max_shift < 0 then Z.pred min_ik else Z.shift_left (BArith.min ik a) max_shift in
      let max_res = if max_shift < 0 then Z.succ max_ik else Z.shift_left (BArith.max ik a) max_shift in
      let underflow = Z.compare min_res min_ik < 0 in
      let overflow = Z.compare max_ik max_res < 0 in
      (norm ~ov:(underflow || overflow) ik (BArith.shift_left ik a b), {underflow=underflow; overflow=overflow})

  (* Arith *)

  (*
  add, sub and mul based on the paper
  "Sound, Precise, and Fast Abstract Interpretation with Tristate Numbers"
  of Vishwanathan et al.
  https://doi.org/10.1109/CGO53902.2022.9741267
  *)

  let add_paper pv pm qv qm =
    let sv = pv +: qv in
    let sm = pm +: qm in
    let sigma = sv +: sm in
    let chi = sigma ^: sv in
    let mu = pm |: qm |: chi in
    let rv = sv &: !:mu in
    let rm = mu in
    (rv, rm)

  let add ?no_ov ik (z1, o1) (z2, o2) =
    let pv = o1 &: !:z1 in
    let pm = o1 &: z1 in
    let qv = o2 &: !:z2 in
    let qm = o2 &: z2 in
    let (rv, rm) = add_paper pv pm qv qm in
    let o3 = rv |: rm in
    let z3 = !:rv |: rm in
    let (max1, max2) = (BArith.max ik (z1, o1), BArith.max ik (z2, o2)) in
    let (min1, min2) = (BArith.min ik (z1, o1), BArith.min ik (z2, o2)) in
    let (min_ik, max_ik) = Size.range ik in
    let underflow = Z.compare (Z.add min1 min2) min_ik < 0 in
    let overflow = Z.compare max_ik (Z.add max1 max2) < 0 in
    (norm ~ov:(overflow || underflow) ik (z3,o3), {underflow=underflow; overflow=overflow})

  let sub ?no_ov ik (z1, o1) (z2, o2) =
    let pv = o1 &: !:z1 in
    let pm = o1 &: z1 in
    let qv = o2 &: !:z2 in
    let qm = o2 &: z2 in
    let dv = pv -: qv in
    let alpha = dv +: pm in
    let beta = dv -: qm in
    let chi = alpha ^: beta in
    let mu = pm |: qm |: chi in
    let rv = dv &: !:mu in
    let rm = mu in
    let o3 = rv |: rm in
    let z3 = !:rv |: rm in
    let (max1, max2) = (BArith.max ik (z1, o1), BArith.max ik (z2, o2)) in
    let (min1, min2) = (BArith.min ik (z1, o1), BArith.min ik (z2, o2)) in
    let (min_ik, max_ik) = Size.range ik in
    let underflow = Z.compare (Z.sub min1 max2) min_ik < 0 in
    let overflow = Z.compare max_ik (Z.sub max1 min2) < 0 in
    (norm ~ov:(overflow || underflow) ik (z3, o3), {underflow=underflow; overflow=overflow})

  let neg ?no_ov ik x =
    if M.tracing then M.trace "bitfield" "neg";
    sub ?no_ov ik BArith.zero x

  let mul ?no_ov ik (z1, o1) (z2, o2) =
    let pm = ref (z1 &: o1) in
    let pv = ref (o1 &: !:z1) in
    let qm = ref (z2 &: o2) in
    let qv = ref (o2 &: !:z2) in
    let accv = ref BArith.zero_mask in
    let accm = ref BArith.zero_mask in
    let size = if GoblintCil.isSigned ik then Size.bit ik - 1 else Size.bit ik in
    let bitmask = Ints_t.of_bigint (fst (Size.range ik)) in
    let signBitUndef1 = z1 &: o1 &: bitmask in
    let signBitUndef2 = z2 &: o2 &: bitmask in
    let signBitUndef = signBitUndef1 |: signBitUndef2 in
    let signBitDefO = (o1 ^: o2) &: bitmask in
    let signBitDefZ = !:(o1 ^: o2) &: bitmask in
    for _ = size downto 0 do
      (if !pm &: Ints_t.one == Ints_t.one then
         accm := snd(add_paper Ints_t.zero !accm Ints_t.zero (!qv |: !qm))
       else if !pv &: Ints_t.one == Ints_t.one then
         accv := fst(add_paper !accv Ints_t.zero !qv Ints_t.zero);
       accm := snd(add_paper Ints_t.zero !accm Ints_t.zero !qm));

      pv := !pv >>: 1;
      pm := !pm >>: 1;
      qv := !qv <<: 1;
      qm := !qm <<: 1;
    done;
    let (rv, rm) = add_paper !accv Ints_t.zero Ints_t.zero !accm in
    let o3 = rv |: rm in
    let z3 = !:rv |: rm in
    let (max1, max2) = (BArith.max ik (z1, o1), BArith.max ik (z2, o2)) in
    let (min1, min2) = (BArith.min ik (z1, o1), BArith.min ik (z2, o2)) in
    let (min_ik, max_ik) = Size.range ik in
    let min_res = Z.min (Z.mul min1 max2) (Z.mul max1 min2) in
    let max_res = Z.max (Z.mul min1 min2) (Z.mul max1 max2) in
    let underflow = Z.compare min_res min_ik < 0 in
    let overflow = Z.compare max_ik max_res < 0 in
    let z3 = if GoblintCil.isSigned ik then signBitUndef |: signBitDefZ |: z3 else z3 in
    let o3 = if GoblintCil.isSigned ik then signBitUndef |: signBitDefO |: o3 else o3 in
    (norm ~ov:(overflow || underflow) ik (z3, o3), {underflow=underflow; overflow=overflow})


  let div ?no_ov ik (z1, o1) (z2, o2) =
    if o2 = Ints_t.zero then
      (top_of ik, {underflow=false; overflow=false})
    else
      let res =
        if BArith.is_const (z1, o1) && BArith.is_const (z2, o2) then
          (let tmp = o1 /: o2 in (!:tmp, tmp))
        else if BArith.is_const (z2, o2) && BArith.is_power_of_two o2 then
          let exp = Z.trailing_zeros (Ints_t.to_bigint o2) in
          (z1 >>: exp, o1 >>: exp)
        else
          top_of ik
      in
      let min_ik = Size.range ik |> fst |> Ints_t.of_bigint in
      (* div can only overflow for divisions like -(INT_MIN) / (-1) *)
      let overflow = GoblintCil.isSigned ik && leq (!: min_ik, min_ik) (z1, o1) && leq (Ints_t.zero, BArith.one_mask) (z2, o2) in
      (norm ~ov:overflow ik res, {underflow=false; overflow=overflow})

  let rem ik (z1, o1) (z2, o2) =
    if o2 = Ints_t.zero then top_of ik else
    if BArith.is_const (z1, o1) && BArith.is_const (z2, o2) then
      let tmp = o1 %: o2 in (!:tmp, tmp)
    else if BArith.is_const (z2, o2) && BArith.is_power_of_two o2 then
      let mask = Ints_t.sub o2 Ints_t.one in
      let newz = Ints_t.logor z1 (Ints_t.lognot mask) in
      let newo = Ints_t.logand o1 mask in
      norm ik (newz, newo)
    else
      top_of ik

  let eq ik x y =
    if Z.compare (BArith.max ik x) (BArith.min ik y) <= 0 && Z.compare (BArith.min ik x) (BArith.max ik y) >= 0 then
      of_bool ik true
    else if Z.compare (BArith.min ik x) (BArith.max ik y) > 0 || Z.compare (BArith.max ik x) (BArith.min ik y) < 0 then
      of_bool ik false
    else
      BArith.top_bool

  let ne ik x y = match eq ik x y with
    | t when t = of_bool ik true -> of_bool ik false
    | t when t = of_bool ik false -> of_bool ik true
    | _ -> BArith.top_bool

  let le ik x y =
    if Z.compare (BArith.max ik x) (BArith.min ik y) <= 0 then
      of_bool ik true
    else if Z.compare (BArith.min ik x) (BArith.max ik y) > 0 then
      of_bool ik false
    else
      BArith.top_bool

  let ge ik x y = le ik y x

  let lt ik x y =
    if Z.compare (BArith.max ik x) (BArith.min ik y) < 0 then
      of_bool ik true
    else if Z.compare (BArith.min ik x) (BArith.max ik y) >= 0 then
      of_bool ik false
    else
      BArith.top_bool

  let gt ik x y = lt ik y x

  (* Invariant *)

  let invariant_ikind e ik (z, o) =
    assert (not (BArith.is_invalid (z, o)));
    let open GoblintCil.Cil in
    let ik_type = TInt (ik, []) in
    let i1 =
      let def0 = z &: (!: o) in
      if def0 =: BArith.zero_mask then
        Invariant.none
      else (
        let def0 = Ints_t.to_bigint def0 in
        if fitsInInt ik def0 then ( (* At least for _Bool and unsigned types, kintegerCilint can give an incorrect mask if doesn't fit. See https://github.com/goblint/analyzer/pull/1897/changes#r2610251390. *)
          let def0 = kintegerCilint ik def0 in
          Invariant.of_exp (BinOp (Eq, (BinOp (BAnd, e, def0, ik_type)), kintegerCilint ik Z.zero, intType))
        )
        else
          Invariant.none
      )
    in
    let i2 =
      let def1 = o &: (!: z) in
      if def1 =: BArith.zero_mask then
        Invariant.none
      else (
        let def1 = Ints_t.to_bigint def1 in
        if fitsInInt ik def1 then ( (* At least for _Bool and unsigned types, kintegerCilint can give an incorrect mask if doesn't fit. See https://github.com/goblint/analyzer/pull/1897/changes#r2610251390. *)
          let def1 = kintegerCilint ik def1 in
          Invariant.of_exp (BinOp (Eq, (BinOp (BAnd, e, def1, ik_type)), def1, intType))
        )
        else
          Invariant.none
      )
    in
    Invariant.(i1 && i2)

  let starting ik n =
    let (min_ik, max_ik) = Size.range ik in
    of_interval ik (n, Ints_t.of_bigint max_ik)

  let ending ik n =
    let (min_ik, max_ik) = Size.range ik in
    of_interval ik (Ints_t.of_bigint min_ik, n)

  (* Refinements *)

  let refine_with_congruence ik bf ((cong) : (int_t * int_t ) option) : t =
    match cong with
    | Some (c, m) -> meet ik bf (of_congruence ik (c,m))
    | _ -> norm ik bf

  let refine_with_interval ik t itv =
    match itv with
    | None -> norm ik t
    | Some (l, u) -> meet ik t (of_interval ik (l, u) |> fst)

  let refine_with_bitfield ik x y = meet ik x y

  let refine_with_excl_list ik t (excl : (int_t list * (int * int)) option) : t = norm ik t

  let refine_with_incl_list ik t (incl : (int_t list) option) : t =
    let joined =match incl with
      | None -> top_of ik
      | Some ls ->
        List.fold_left (fun acc i -> BArith.join acc (BArith.of_int i)) (bot_of ik) ls
    in
    meet ik t joined


  let refine_bor (az, ao) (bz, bo) (cz, co) =
    let cDef0 = cz &: (!: co) in
    let cDef1 = co &: (!: cz) in
    let aDef0 = az &: (!: ao) in
    let bDef0 = bz &: (!: bo) in
    (* if a bit is definitely 0 in b and definitely 1 in c, the same bit must be definitely 1 in a *)
    (* example (with t for top): (tttt) | (t010) = (1011) *)
    (* we can refine (tttt) to (ttt1) because the lowest 1 of c must come from a *)
    let az = az &: (!: (bDef0 &: cDef1)) in
    let bz = bz &: (!: (aDef0 &: cDef1)) in
    (* if a bit is definitely 0 in c, the same bit must be definitely 0 in a too *)
    (* example (with t for top): (ttt1) | (t010) = (1011) *)
    (* we can refine (ttt1) to (t0t1) because the second bit of a cannot be a 1 *)
    let ao = ao &: (!: cDef0) in
    let bo = bo &: (!: cDef0) in
    ((az, ao), (bz, bo))

  let refine_band (az, ao) (bz, bo) (cz, co) =
    let cDef0 = cz &: (!: co) in
    let cDef1 = co &: (!: cz) in
    let aDef1 = ao &: (!: az) in
    let bDef1 = bo &: (!: bz) in
    (* if a bit is definitely 1 in c, the same bit must be definitely 1 in a too *)
    (* example (with t for top): (tttt) & (t010) = (1011) *)
    (* we can refine (tttt) to (1t11) *)
    let az = az &: (!: cDef1) in
    let bz = bz &: (!: cDef1) in
    (* if a bit is definitely 1 in b and definitely 0 in c, the same bit must be definitely 0 in a *)
    (* example (with t for top): (tttt) & (t110) = (1011) *)
    (* we can refine (tttt) to (t0tt) *)
    let ao = ao &: (!: (bDef1 &: cDef0)) in
    let bo = bo &: (!: (aDef1 &: cDef0)) in
    ((az, ao), (bz, bo))

  let arbitrary ik =
    let open QCheck.Iter in
    let int_arb = QCheck.map ~rev:Ints_t.to_int64 Ints_t.of_int64 GobQCheck.Arbitrary.int64 in
    let pair_arb = QCheck.pair int_arb int_arb in
    let shrink bf =
      (GobQCheck.shrink pair_arb bf
       >|= (fun (zs, os) ->
           (* Shrinking works by setting some unsure bits to 0. This reduces the number of possible values, and makes the decimal representation of the masks smaller *)
           let random_mask = Ints_t.of_int64 (Random.int64 (Int64.of_int (Size.bits ik |> snd))) in
           let unsure_bitmask= zs &: os in
           let pruned_bits= unsure_bitmask &: random_mask in
           (* set the pruned bits to 1 in the zs-mask and to 0 in the os-mask *)
           let flipped_z = zs |: pruned_bits in
           let flipped_o = os &: !:pruned_bits in
           norm ik (flipped_z, flipped_o)
         ))
    in
    QCheck.(set_shrink shrink @@ set_print show @@ map (fun (i1,i2) -> norm ik (i1,i2)) pair_arb)

  let project ik p t = t

end

module Bitfield = BitfieldFunctor (IntOps.BigIntOps)