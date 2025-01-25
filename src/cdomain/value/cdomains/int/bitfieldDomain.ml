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

  let bitmask_up_to n =
    let top_bit = Ints_t.one <<: n in
    if top_bit = Ints_t.zero
    then Ints_t.zero
    else
      Ints_t.sub top_bit Ints_t.one

  let nth_bit p n = Ints_t.one &: (p >>: n) =: Ints_t.one

  let min ik (z,o) = 
    let signBit = Ints_t.one <<: ((Size.bit ik) - 1) in 
    let signMask = !: (Ints_t.of_bigint (snd (Size.range ik))) in
    let isNegative = signBit &: o <>: Ints_t.zero in
    if GoblintCil.isSigned ik && isNegative then Ints_t.to_bigint(signMask |: (!: z))
    else Ints_t.to_bigint(!: z)

  let max ik (z,o) =
    let signBit = Ints_t.one <<: ((Size.bit ik) - 1) in 
    let signMask = Ints_t.of_bigint (snd (Size.range ik)) in
    let isPositive = signBit &: z <>: Ints_t.zero in
    if GoblintCil.isSigned ik && isPositive then Ints_t.to_bigint(signMask &: o)
    else Ints_t.to_bigint o 

  (*
    This function is exclusively used inside the shift functions. The invariant for the second
    parameter is that it's size is bounded by O(log2 n) ensuring that no exponential blowup happens.
   *)
  let rec concretize (z,o) = (* O(2^n) *)
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

  let shift_right ik (z,o) c =
    let msb_pos = (Size.bit ik - c) in
    let msb_pos = if msb_pos < 0 then 0 else msb_pos in
    let sign_mask = !:(bitmask_up_to msb_pos) in
    if GoblintCil.isSigned ik && o <: Ints_t.zero then
      (z >>: c, (o >>: c) |: sign_mask)
    else
      ((z >>: c) |: sign_mask, o >>: c)

  let shift_right ik bf (z2, o2) = 
    if is_const (z2, o2) 
    then 
      shift_right ik bf (Ints_t.to_int o2)
    else
      let shift_counts = concretize (z2, o2) in
      List.fold_left (fun acc c ->
          let next = shift_right ik bf c in
          join acc next
        ) (zero_mask, zero_mask) shift_counts

  let shift_left _ (z,o) c =
    let zero_mask = bitmask_up_to c in
    ((z <<: c) |: zero_mask, o <<: c)

  let shift_left ik bf (z2, o2) = 
    if is_const (z2, o2) 
    then 
      shift_left ik bf (Ints_t.to_int o2)
    else
      let shift_counts = concretize (z2, o2) in
      List.fold_left (fun acc c ->
          let next = shift_left ik bf c in
          join acc next
        ) (zero_mask, zero_mask) shift_counts

  let nth_bit p n = if nth_bit p n then Ints_t.one else Ints_t.zero
end

module BitfieldFunctor (Ints_t : IntOps.IntOps): Bitfield_SOverflow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) = struct

  include InfixIntOps (Ints_t)

  let name () = "bitfield"
  type int_t = Ints_t.t
  type t = (Ints_t.t * Ints_t.t) [@@deriving eq, ord, hash]

  module BArith = BitfieldArith (Ints_t)

  let top () = (BArith.one_mask, BArith.one_mask)
  let bot () = (BArith.zero_mask, BArith.zero_mask)

  let top_of ik = 
    if GoblintCil.isSigned ik then top () 
    else (BArith.one_mask, Ints_t.of_bigint (snd (Size.range ik)))

  let bot_of ik = bot ()

  let to_pretty_bits (z,o) = 
    let known_bitmask = (BArith.bits_known (z,o)) in
    let invalid_bitmask = (BArith.bits_invalid (z,o)) in
    let o_mask = o in
    let z_mask = z in

    let rec to_pretty_bits' o_mask z_mask known_bitmask invalid_bitmask acc =
      let current_bit_known = (known_bitmask &: Ints_t.one) = Ints_t.one in
      let current_bit_impossible = (invalid_bitmask &: Ints_t.one) = Ints_t.one in
      let bit_value = o_mask &: Ints_t.one in
      let bit =
        if current_bit_impossible then "⊥"
        else if not current_bit_known then "⊤"
        else Ints_t.to_string bit_value 
      in
      if (o_mask = Ints_t.of_int (-1) || o_mask = Ints_t.zero ) && (z_mask = Ints_t.of_int (-1) || z_mask = Ints_t.zero) then
        let prefix = bit ^ "..." ^ bit in
        prefix ^ acc
      else
        to_pretty_bits' (o_mask >>: 1) (z_mask >>: 1) (known_bitmask >>: 1) (invalid_bitmask >>: 1) (bit ^ acc)
    in
    "0b" ^ to_pretty_bits' o_mask z_mask known_bitmask invalid_bitmask ""

  let show t = 
    if t = bot () then "bot" else
    if t = top () then "top" else
      let (z,o) = t in
      Format.sprintf "{%s, (zs:%s, os:%s)}" (to_pretty_bits t) (Ints_t.to_string z) (Ints_t.to_string o) 

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let maximal (z,o) =     
    if (z <: Ints_t.zero) <> (o <: Ints_t.zero) then Some o
    else None

  let minimal (z,o) =     
    if (z <: Ints_t.zero) <> (o <: Ints_t.zero) then Some (!:z)
    else None

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

  let norm ?(suppress_ovwarn=false) ik (z,o) = 
    if BArith.is_invalid (z,o) then 
      (bot (), {underflow=false; overflow=false})
    else      
      let (min_ik, max_ik) = Size.range ik in
      let isPos = z <: Ints_t.zero in 
      let isNeg = o <: Ints_t.zero in
      let underflow = if GoblintCil.isSigned ik then (((Ints_t.of_bigint min_ik) &: z) <>: Ints_t.zero) && isNeg else isNeg in     
      let overflow = (((!:(Ints_t.of_bigint max_ik)) &: o) <>: Ints_t.zero) && isPos in      
      let new_bitfield = wrap ik (z,o)
      in
      let overflow_info = if suppress_ovwarn then {underflow=false; overflow=false} else  {underflow=underflow; overflow=overflow} in      
      if not (underflow || overflow) then
        ((z,o), overflow_info)
      else if should_wrap ik then
        (new_bitfield, overflow_info)
      else if should_ignore_overflow ik then 
        (top_of ik, overflow_info)
      else 
        (top_of ik, overflow_info)

  let cast_to ?(suppress_ovwarn=false) ?torg ?no_ov ik x = norm ~suppress_ovwarn:(suppress_ovwarn || x = top ()) ik x

  let join ik b1 b2 = fst @@ norm ik @@ (BArith.join b1 b2)

  let meet ik x y = fst @@ norm ik @@ (BArith.meet x y)

  let leq (x:t) (y:t) = (BArith.join x y) = y

  let widen ik x y = fst @@ norm ik @@ BArith.widen x y

  let narrow ik x y = meet ik x y

  let of_int ik (x: int_t) = (norm ik @@ BArith.of_int x) 

  let to_int (z,o) = if is_bot (z,o) then None else
    if BArith.is_const (z,o) then Some o
    else None

  let equal_to i bf = 
    if BArith.of_int i = bf then `Eq
    else if leq (BArith.of_int i) bf then `Top
    else `Neq

  (* Conversions *)

  let of_interval ?(suppress_ovwarn=false) ik (x,y) =
    let (min_ik, max_ik) = Size.range ik in
    let startv = Ints_t.max x (Ints_t.of_bigint min_ik) in
    let endv= Ints_t.min y (Ints_t.of_bigint max_ik) in

    let rec analyze_bits pos (acc_z, acc_o) =
      if pos < 0 then (acc_z, acc_o)
      else
        let position = Ints_t.shift_left Ints_t.one pos in
        let mask = Ints_t.sub position Ints_t.one in
        let remainder = Ints_t.logand startv mask in

        let without_remainder = Ints_t.sub startv remainder in
        let bigger_number = Ints_t.add without_remainder position in

        let bit_status =
          if Ints_t.compare bigger_number endv <= 0 then
            `top
          else 
          if Ints_t.equal (Ints_t.logand (Ints_t.shift_right startv pos) Ints_t.one) Ints_t.one then
            `one
          else
            `zero
        in

        let new_acc = 
          match bit_status with
          | `top -> (Ints_t.logor position acc_z, Ints_t.logor position acc_o)
          | `one -> (Ints_t.logand (Ints_t.lognot position) acc_z, Ints_t.logor position acc_o)
          | `zero -> (Ints_t.logor position acc_z, Ints_t.logand (Ints_t.lognot position) acc_o)

        in 
        analyze_bits (pos - 1) new_acc
    in      
    let result = analyze_bits (Size.bit ik - 1) (bot()) in
    let casted = (Ints_t.of_bigint (Size.cast ik ((Ints_t.to_bigint (fst result)))), Ints_t.of_bigint (Size.cast ik ((Ints_t.to_bigint (snd result))))) 
    in (wrap ik casted, {underflow=false; overflow=false})

  let of_bool _ik = function true -> BArith.one | false -> BArith.zero

  let to_bool d =
    if not (leq BArith.zero d) then Some true
    else if d = BArith.zero then Some false
    else None

  let of_bitfield ik x = norm ik x |> fst

  let to_bitfield ik x = norm ik x |> fst

  let is_power_of_two x = (x &: (x -: Ints_t.one) = Ints_t.zero) 

  let of_congruence ik (c,m) = 
    if m = Ints_t.zero then 
      of_int ik c |> fst
    else if is_power_of_two m && Ints_t.one <>: m then 
      let mod_mask = !:(m -: Ints_t.one) in 
      let z = mod_mask |: (!: c) in 
      let o = mod_mask |: c in 
      norm ik (z,o) |> fst
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

  let logxor ik i1 i2 = BArith.logxor i1 i2 |> norm ik |> fst

  let logand ik i1 i2 = BArith.logand i1 i2 |> norm ik |> fst

  let logor  ik i1 i2 = BArith.logor i1 i2 |> norm ik |> fst

  let lognot ik i1 = BArith.lognot i1 |> norm ik |> fst

  let precision ik = snd @@ Size.bits ik
  let cap_bitshifts_to_precision ik (z,o) =
    let mask = BArith.bitmask_up_to (Int.succ @@ Z.log2up @@ Z.of_int @@ precision ik) in
    (z |: !:mask, o &: mask)

  let is_invalid_shift_operation ik a b = BArith.is_invalid b || BArith.is_invalid a

  let has_neg_values ik b = Z.compare (BArith.min ik b) Z.zero < 0
  let has_only_neg_values ik b = Z.compare (BArith.max ik b) Z.zero < 0

  let check_if_undefined_shift_operation ?(is_shift_left=false) ik a b =
    let ov_info = if is_shift_left
      then {underflow=false; overflow=true}
      else {underflow=true; overflow=false}
    in
    let no_ov = {underflow=false; overflow=false} in
    let min_val = BArith.min ik b in
    if GoblintCil.isSigned ik && has_only_neg_values ik b then true, no_ov else
      let exceeds_bit_width =
        if Z.fits_int min_val then Z.to_int min_val >= Sys.word_size else true
      in
      if exceeds_bit_width
      then true, ov_info else
        let causes_signed_overflow = GoblintCil.isSigned ik && ((is_shift_left && Z.to_int min_val >= precision ik) || (not is_shift_left && has_neg_values ik a && Z.to_int min_val > precision ik))
        in
        if causes_signed_overflow
        then true, ov_info else false, no_ov

  let shift_right ik a b = 
    if M.tracing then M.trace "bitfield" "%a >> %a" pretty a pretty b; 
    if is_invalid_shift_operation ik a b
    then
      (bot (), {underflow=false; overflow=false})
    else
      let is_undefined_shift_operation, ov_info = check_if_undefined_shift_operation ik a b
      in
      if is_undefined_shift_operation then (top_of ik, ov_info)
      else
        let defined_shifts = cap_bitshifts_to_precision ik b in (* O(2^(log n)) *)
        norm ik @@ BArith.shift_right ik a defined_shifts

  let shift_left ik a b =
    if M.tracing then M.trace "bitfield" "%a << %a" pretty a pretty b;
    if is_invalid_shift_operation ik a b
    then
      (bot (), {underflow=false; overflow=false})
    else
      let is_undefined_shift_operation, ov_info = check_if_undefined_shift_operation ~is_shift_left:true ik a b
      in
      if is_undefined_shift_operation then (top_of ik, ov_info)
      else
        let defined_shifts = cap_bitshifts_to_precision ik b in (* O(2^(log n)) *)
        norm ik @@ BArith.shift_left ik a defined_shifts

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
    norm ik (z3,o3)

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
    norm ik (z3, o3)

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
    let o3 = ref(rv |: rm) in 
    let z3 = ref(!:rv |: rm) in
    if GoblintCil.isSigned ik then z3 := signBitUndef |: signBitDefZ |: !z3;
    if GoblintCil.isSigned ik then o3 := signBitUndef |: signBitDefO |: !o3;
    norm ik (!z3, !o3)

  let div ?no_ov ik (z1, o1) (z2, o2) =
    if o2 = Ints_t.zero then (top_of ik, {underflow=false; overflow=false}) else
      let res = 
        if BArith.is_const (z1, o1) && BArith.is_const (z2, o2) then (let tmp = o1 /: o2 in (!:tmp, tmp)) 
        else if BArith.is_const (z2, o2) && is_power_of_two o2 then 
          let exp = Z.trailing_zeros (Ints_t.to_bigint o2) in 
          (z1 >>: exp, o1 >>: exp)
        else top_of ik in
      norm ik res

  let rem ik (z1, o1) (z2, o2) = 
    if o2 = Ints_t.zero then top_of ik else
    if BArith.is_const (z1, o1) && BArith.is_const (z2, o2) then (
      let tmp = o1 %: o2 in (!:tmp, tmp)      
    )
    else if BArith.is_const (z2, o2) && is_power_of_two o2 then (
      let mask = Ints_t.sub o2 Ints_t.one in
      let newz = Ints_t.logor z1 (Ints_t.lognot mask) in
      let newo = Ints_t.logand o1 mask in
      norm ik (newz, newo) |> fst
    )
    else top_of ik

  let eq ik x y =
    if Z.compare (BArith.max ik x) (BArith.min ik y) <= 0 && Z.compare (BArith.min ik x) (BArith.max ik y) >= 0 then of_bool ik true 
    else if Z.compare (BArith.min ik x) (BArith.max ik y) > 0 || Z.compare (BArith.max ik x) (BArith.min ik y) < 0 then of_bool ik false 
    else BArith.top_bool

  let ne ik x y = match eq ik x y with
    | t when t = of_bool ik true -> of_bool ik false
    | t when t = of_bool ik false -> of_bool ik true
    | _ -> BArith.top_bool

  let le ik x y = 
    if Z.compare (BArith.max ik x) (BArith.min ik y) <= 0 then of_bool ik true 
    else if Z.compare (BArith.min ik x) (BArith.max ik y) > 0 then of_bool ik false 
    else BArith.top_bool

  let ge ik x y = le ik y x

  let lt ik x y = if Z.compare (BArith.max ik x) (BArith.min ik y) < 0 then of_bool ik true 
    else if Z.compare (BArith.min ik x) (BArith.max ik y) >= 0 then of_bool ik false 
    else BArith.top_bool

  let gt ik x y = lt ik y x

  (* Invariant *)

  let invariant_ikind e ik (z,o) = 
    if z =: BArith.one_mask && o =: BArith.one_mask then 
      Invariant.top ()
    else if  BArith.is_invalid (z,o) then 
      Invariant.none
    else      
      let open GoblintCil.Cil in
      let def0 = z &: (!: o) in 
      let def1 = o &: (!: z) in 
      let (def0, def1) = BatTuple.Tuple2.mapn (kintegerCilint ik) (Ints_t.to_bigint !:def0, Ints_t.to_bigint def1) in
      Invariant.of_exp (BinOp (Eq, (BinOp (BOr, (BinOp (BAnd, e, def0, TInt(ik,[]))), def1, TInt(ik,[]))), e, intType))

  let starting ?(suppress_ovwarn=false) ik n = 
    let (min_ik, max_ik) = Size.range ik in
    of_interval ~suppress_ovwarn ik (n, Ints_t.of_bigint max_ik) 

  let ending ?(suppress_ovwarn=false) ik n =
    let (min_ik, max_ik) = Size.range ik in
    of_interval ~suppress_ovwarn ik (Ints_t.of_bigint min_ik, n)

  (* Refinements *)

  let refine_with_congruence ik bf ((cong) : (int_t * int_t ) option) : t =
    match cong with
    | Some (c, m) -> meet ik bf (of_congruence ik (c,m))
    | _ -> norm ik bf |> fst

  let refine_with_interval ik t itv = 
    match itv with
    | None -> norm ik t |> fst
    | Some (l, u) -> meet ik t (of_interval ik (l, u) |> fst)  

  let refine_with_bitfield ik x y = meet ik x y

  let refine_with_excl_list ik t (excl : (int_t list * (int64 * int64)) option) : t = norm ik t |> fst

  let refine_with_incl_list ik t (incl : (int_t list) option) : t =
    let joined =match incl with
      | None -> top_of ik
      | Some ls -> 
        List.fold_left (fun acc i -> BArith.join acc (BArith.of_int i)) (bot_of ik) ls
    in
    meet ik t joined


  let refine_bor (az, ao) (bz, bo) (cz, co) = 
    (* bits that are definitely 1 in c*)
    let cDef0 = cz &: (!: cz) in 
    let cDef1 = co &: (!: cz) in     
    (* bits that are definitely 0 in a*)
    let aDef0 = az &: (!: ao) in
    (* bits that are definitely 0 in b*)
    let bDef0 = bz &: (!: bo) in
    (* bits that are definitely 0 in b and 1 in c must be definitely 1 in a, i.e. the zero bit cannot be set *)
    let az = az &: (!: (bDef0 &: cDef1)) in 
    let ao = ao &: (!: cDef0) in 
    (* bits that are definitely 0 in a and 1 in c must be definitely 1 in b, i.e. the zero bit cannot be set *)
    let bz = bz &: (!: (aDef0 &: cDef1)) in
    let bo = bo &: (!: cDef0) in 
    ((az, ao), (bz, bo))

  let refine_band (az, ao) (bz, bo) (cz, co) = 
    (* bits that are definitely 0 in c*)
    let cDef0 = cz &: (!: co) in 
    let cDef1 = co &: (!: cz) in 
    (* bits that are definitely 1 in a*)
    let aDef1 = ao &: (!: az) in
    (* bits that are definitely 1 in b*)
    let bDef1 = bo &: (!: bz) in
    let az = az &: (!: cDef1) in 
    (* bits that are definitely 1 in b and 0 in c must be definitely 0 in a, i.e. the one bit cannot be set *)
    let ao = ao &: (!: (bDef1 &: cDef0)) in 
    let bz = bz &: (!: cDef1) in 
    (* bits that are definitely 1 in a and 0 in c must be definitely 0 in a, i.e. the one bit cannot be set *)
    let bo = bo &: (!: (aDef1 &: cDef0)) in
    ((az, ao), (bz, bo))

  (* Unit Tests *)

  let arbitrary ik = 
    let open QCheck.Iter in
    let int_arb = QCheck.map ~rev:Ints_t.to_int64 Ints_t.of_int64 GobQCheck.Arbitrary.int64 in
    let pair_arb = QCheck.pair int_arb int_arb in
    let shrink (z, o) =
      (GobQCheck.shrink pair_arb (z, o) 
       >|= (fun (new_z, new_o) -> 
           (* Randomly flip bits to be opposite *)
           let random_mask = Ints_t.of_int64 (Random.int64 (Int64.of_int (Size.bits ik |> snd))) in
           let unsure_bitmask= new_z &: new_o in
           let canceled_bits= unsure_bitmask &: random_mask in
           let flipped_z = new_z |: canceled_bits in
           let flipped_o = new_o &: !:canceled_bits in
           norm ik (flipped_z, flipped_o) |> fst
         ))
    in
    QCheck.(set_shrink shrink @@ set_print show @@ map (fun (i1,i2) -> norm ik (i1,i2) |> fst ) pair_arb)

  let project ik p t = t

end

module Bitfield = BitfieldFunctor (IntOps.BigIntOps)