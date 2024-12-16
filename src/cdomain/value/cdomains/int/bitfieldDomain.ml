open IntDomain0
open GoblintCil

module BitfieldInfixOps (Ints_t : IntOps.IntOps) = struct
  let (&:) = Ints_t.logand
  let (|:) = Ints_t.logor
  let (^:) = Ints_t.logxor
  let (!:) = Ints_t.lognot
  let (<<:) = Ints_t.shift_left
  let (>>:) = Ints_t.shift_right
  let (<:) = fun a b -> Ints_t.compare a b < 0
  let (=:) = fun a b -> Ints_t.compare a b = 0
  let (>:) = fun a b -> Ints_t.compare a b > 0

  let (+:) = Ints_t.add
  let (-:) = Ints_t.sub
  let ( *: ) = Ints_t.mul
  let (/:) = Ints_t.div
  let (%:) = Ints_t.rem

  let (>>.) = fun a b -> a >>: b |: !:((Ints_t.one <<: b) -: Ints_t.one)
end

(* Bitfield arithmetic, without any overflow handling etc. *)
module BitfieldArith (Ints_t : IntOps.IntOps) = struct

  include BitfieldInfixOps (Ints_t)

  let zero_mask = Ints_t.zero
  let one_mask = !:zero_mask

  let of_int x = (!:x, x) 

  let join (z1,o1) (z2,o2) = (z1 |: z2, o1 |: o2)
  let meet (z1,o1) (z2,o2) = (z1 &: z2, o1 &: o2)

  let one = of_int Ints_t.one
  let zero = of_int Ints_t.zero
  let top_bool = join one zero

  let bits_known (z,o) = z ^: o
  let bits_unknown (z,o) = z &: o
  let bits_set bf = (snd bf) &: (bits_known bf)
  let bits_invalid (z,o) = !:(z |: o)

  let is_const (z,o) = (z ^: o) =: one_mask
  let is_invalid ik (z,o) = 
    let mask = !:(Ints_t.of_bigint (snd (Size.range ik))) in
    not ((!:(z |: o |: mask)) = Ints_t.zero)

  let nabla x y= if x =: (x |: y) then x else one_mask

  let widen (z1,o1) (z2,o2) = (nabla z1 z2, nabla o1 o2)            

  let lognot (z,o) = (o,z)

  let logxor (z1,o1)  (z2,o2) = ((z1 &: z2) |: (o1 &: o2), 
                                 (z1 &: o2) |: (o1 &: z2))

  let logand (z1,o1) (z2,o2) = (z1 |: z2, o1 &: o2)

  let logor (z1,o1)  (z2,o2) = (z1 &: z2, o1 |: o2)

  let make_bitone_msk pos = Ints_t.one <<: pos
  let make_lsb_bitmask pos =
    let bitmsk = make_bitone_msk pos in
    if bitmsk =: Ints_t.zero then Ints_t.zero
    else Ints_t.sub bitmsk Ints_t.one
  let make_msb_bitmask pos = !:(make_lsb_bitmask pos)

  let get_bit bf pos = Ints_t.one &: (bf >>: pos)

  let min ik (z,o) = 
    let signBit = Ints_t.one <<: ((Size.bit ik) - 1) in 
    let signMask = !: (Ints_t.of_bigint (snd (Size.range ik))) in
    let isNegative = signBit &: o <> Ints_t.zero in
    if isSigned ik && isNegative then Ints_t.to_bigint(signMask |: (!: z))
    else Ints_t.to_bigint(!: z)

  let max ik (z,o) =
    let signBit = Ints_t.one <<: ((Size.bit ik) - 1) in 
    let signMask = Ints_t.of_bigint (snd (Size.range ik)) in
    let isPositive = signBit &: z <> Ints_t.zero in
    if isSigned ik && isPositive then Ints_t.to_bigint(signMask &: o)
    else Ints_t.to_bigint o 

  (* Worst Case asymptotic runtime: O(2^n). *)
  let rec concretize (z,o) =
    if is_const (z,o) then [o]
    else
      let is_bit_unknown = not ((bits_unknown (z,o) &: Ints_t.one) =: Ints_t.zero) in
      let bit = o &: Ints_t.one in
      let shifted_z, shifted_o = (z >>. 1, o >>: 1) in
      if is_bit_unknown
        then concretize (shifted_z, shifted_o) |> List.concat_map (fun c -> [c <<: 1; (c <<: 1) |: Ints_t.one])
        else concretize (shifted_z, shifted_o) |> List.map (fun c -> c <<: 1 |: bit)

  let concretize bf = List.map Ints_t.to_int (concretize bf)

  let get_o (_,o) = Ints_t.to_int o

  let shift_right_action ik (z,o) c =
    let sign_msk = make_msb_bitmask (Size.bit ik - c) in
    if (isSigned ik) && (o <: Ints_t.zero) then
      (z >>: c, (o >>: c) |: sign_msk)
    else
      ((z >>: c) |: sign_msk, o >>: c)

  let shift_right ik (z1, o1) (z2, o2) = 
    if is_const (z2, o2) 
      then 
        shift_right_action ik (z1, o1) (Ints_t.to_int o2)
    else
      let max_bit = Z.log2up (Z.of_int (Size.bit ik)) in
      let mask_usefull_bits = !:(one_mask<<:max_bit) in
      let concrete_values = concretize ((z2 &: mask_usefull_bits), (o2 &: mask_usefull_bits)) in
      if (((o2 &: mask_usefull_bits) == Ints_t.of_int 0) && (z2 != one_mask)) || (List.length concrete_values) == 0 
        then 
          (one_mask, zero_mask)
      else 
        let (v1, v2) = (ref zero_mask, ref zero_mask) in
        List.iter (fun x -> let (a, b) = (shift_right_action ik (z1, o1) x) in
          v1 := !v1 |: a;
          v2 := !v2 |: b
        ) concrete_values;
        (!v1, !v2)

  let shift_left_action _ (z,o) c =
    let z_msk = make_lsb_bitmask c in
    ((z <<: c) |: z_msk, o <<: c)

  let shift_left ik (z1, o1) (z2, o2) = 
    (* (one_mask, Ints_t.of_int (Size.bit ik)) *)
    if is_const (z2, o2) 
      then 
        shift_left_action ik (z1, o1) (Ints_t.to_int o2)
    else
      let max_bit = Z.log2up (Z.of_int (Size.bit ik)) in
      let mask_usefull_bits = !:(one_mask <<: max_bit) in
      let concrete_values = concretize ((z2 &: mask_usefull_bits), (o2 &: mask_usefull_bits)) in
      if (((o2 &: mask_usefull_bits) == Ints_t.of_int 0) && (z2 != one_mask)) || (List.length concrete_values) == 0 
      then 
        (one_mask, zero_mask)
      else 
        let (v1, v2) = (ref zero_mask, ref zero_mask) in
        List.iter (fun x -> let (a, b) = (shift_left_action ik (z1, o1) x) in
          v1 := !v1 |: a;
          v2 := !v2 |: b
        ) concrete_values;
        (!v1, !v2)

end

module BitfieldFunctor (Ints_t : IntOps.IntOps): SOverflow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) = struct

  include BitfieldInfixOps (Ints_t)

  let name () = "bitfield"
  type int_t = Ints_t.t
  type t = (Ints_t.t * Ints_t.t) [@@deriving eq, ord, hash]

  module BArith = BitfieldArith (Ints_t)

  let top () = (BArith.one_mask, BArith.one_mask)
  let bot () = (BArith.zero_mask, BArith.zero_mask)
  let top_of ik = top ()
  let bot_of ik = bot ()

  let to_pretty_bits (z,o) = 
    let known_bitmask = ref (BArith.bits_known (z,o)) in
    let invalid_bitmask = ref (BArith.bits_invalid (z,o)) in
    let o_mask = ref o in
    let z_mask = ref z in

    let rec to_pretty_bits' acc =
        let current_bit_known = (!known_bitmask &: Ints_t.one) = Ints_t.one in
        let current_bit_impossible = (!invalid_bitmask &: Ints_t.one) = Ints_t.one in

        let bit_value = !o_mask &: Ints_t.one in
        let bit =
          if current_bit_impossible then "⊥"
          else if not current_bit_known then "⊤"
          else Ints_t.to_string bit_value 
        in

        if (!o_mask = Ints_t.of_int (-1) || !o_mask = Ints_t.zero ) && (!z_mask = Ints_t.of_int (-1) || !z_mask = Ints_t.zero) then
          let prefix = bit ^ "..." ^ bit in
          prefix ^ acc
        else
          (known_bitmask := !known_bitmask >>: 1;
          invalid_bitmask := !invalid_bitmask >>: 1;
          o_mask := !o_mask >>: 1;
          z_mask := !z_mask >>: 1;
          to_pretty_bits' (bit ^ acc))
    in
    "0b" ^ to_pretty_bits' ""

  let show t = 
    if t = bot () then "bot" else
    if t = top () then "top" else
      let (z,o) = t in
        Format.sprintf "{%s, (zs:%s, os:%s)}" (to_pretty_bits t) (Ints_t.to_string z) (Ints_t.to_string o) 

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let range ik bf = (BArith.min ik bf, BArith.max ik bf)

  let maximal (z,o) = let isPositive = z < Ints_t.zero in
    if o < Ints_t.zero && isPositive then (match Ints_t.upper_bound with Some maxVal -> Some (maxVal &: o) | None -> None )
    else Some o 

  let minimal (z,o) = let isNegative = o < Ints_t.zero in
    if z < Ints_t.zero && isNegative then (match Ints_t.lower_bound with Some minVal -> Some (minVal |: (!:z)) | None -> None )
    else Some (!:z)

  let norm ?(suppress_ovwarn=false) ik (z,o) = 
    if BArith.is_invalid ik (z,o) then 
      (bot (), {underflow=false; overflow=false})
    else      
      let (min_ik, max_ik) = Size.range ik in
      let wrap ik (z,o) = 
        if isSigned ik then
          let newz = (z &: (Ints_t.of_bigint max_ik)) |: ((Ints_t.of_bigint min_ik) *: (BArith.get_bit z (Size.bit ik - 1))) in
          let newo = (o &: (Ints_t.of_bigint max_ik)) |: ((Ints_t.of_bigint min_ik) *: (BArith.get_bit o (Size.bit ik - 1))) in
          (newz,newo)
        else
          let newz = z |: !:(Ints_t.of_bigint max_ik) in
          let newo = o &: (Ints_t.of_bigint max_ik) in
          (newz,newo)
      in
      let (min,max) = range ik (z,o) in
      let underflow = Z.compare min min_ik < 0 in
      let overflow = Z.compare max max_ik > 0 in
      let new_bitfield = wrap ik (z,o)
      in
      if suppress_ovwarn then (new_bitfield, {underflow=false; overflow=false})
      else (new_bitfield, {underflow=underflow; overflow=overflow})

  let cast_to ?(suppress_ovwarn=false) ?torg ?no_ov t = norm ~suppress_ovwarn t

  let join ik b1 b2 = (norm ik @@ (BArith.join b1 b2) ) |> fst

  let meet ik x y = (norm ik @@ (BArith.meet x y)) |> fst

  let leq (x:t) (y:t) = (BArith.join x y) = y

  let widen ik x y = (norm ik @@ BArith.widen x y) |> fst

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
    let casted = (Ints_t.of_bigint (Size.cast ik ((Ints_t.to_bigint (fst result)))), Ints_t.of_bigint (Size.cast ik ((Ints_t.to_bigint (snd result))))) in
    norm ~suppress_ovwarn ik casted


  let of_bool _ik = function true -> BArith.one | false -> BArith.zero

  let to_bool d =
    if not (leq BArith.zero d) then Some true
    else if d = BArith.zero then Some false
    else None

  let of_bitfield ik x = norm ik x |> fst

  let to_bitfield ik x = norm ik x |> fst

  let is_power_of_two x = (x &: (x -: Ints_t.one) = Ints_t.zero) 

  let of_congruence ik (c,m) = 
    if m = Ints_t.zero then of_int ik c |> fst
    else if is_power_of_two m then 
      let mod_mask = m -: Ints_t.one in 
      let z = !: c in 
      let o = !:mod_mask |: c in 
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

  let shift_right ik a b = 
    M.trace "bitfield" "shift_right";
    if BArith.is_invalid ik b || BArith.is_invalid ik a || (isSigned ik && BArith.min ik b < Z.zero) then (bot (), {underflow=false; overflow=false})
    else norm ik (BArith.shift_right ik a b)

  let shift_left ik a b =
    M.trace "bitfield" "shift_left";
    if BArith.is_invalid ik b || BArith.is_invalid ik a || (isSigned ik && BArith.min ik b < Z.zero) then (bot (), {underflow=false; overflow=false})
    else norm ik (BArith.shift_left ik a b)

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
    (* let _ = print_endline (show (z3, o3)) in 
    let _ = (match maximal (z3,o3) with Some k -> print_endline (Ints_t.to_string k) | None -> print_endline "None") in
    let _ = (match minimal (z3,o3) with Some k -> print_endline (Ints_t.to_string k) | None -> print_endline "None") in
    let _ = (match Size.range ik with (a,b) -> print_endline ("(" ^ Z.to_string a ^ "; " ^ Z.to_string b ^ ")")) in  *)
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
    M.trace "bitfield" "neg";
    sub ?no_ov ik BArith.zero x

  let mul ?no_ov ik (z1, o1) (z2, o2) =
    let pm = ref (z1 &: o1) in
    let pv = ref (o1 &: !:z1) in 
    let qm = ref (z2 &: o2) in
    let qv = ref (o2 &: !:z2) in 
    let accv = ref BArith.zero_mask in 
    let accm = ref BArith.zero_mask in 
    let size = if isSigned ik then Size.bit ik - 1 else Size.bit ik in
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
    if isSigned ik then z3 := signBitUndef |: signBitDefZ |: !z3;
    if isSigned ik then o3 := signBitUndef |: signBitDefO |: !o3;
    norm ik (!z3, !o3)

  let div ?no_ov ik (z1, o1) (z2, o2) =
    let res = if BArith.is_const (z1, o1) && BArith.is_const (z2, o2) then (let tmp = z1 /: z2 in (!:tmp, tmp)) else top_of ik in
    norm ik res

  let rem ik x y = 
    if BArith.is_const x && BArith.is_const y then (
      let def_x = Option.get (to_int x) in
      let def_y = Option.get (to_int y) in
      fst (of_int ik (Ints_t.rem def_x def_y))
    )
    else if BArith.is_const y && is_power_of_two (snd y) then (
      let mask = Ints_t.sub (snd y) Ints_t.one in
      let newz = Ints_t.logor (fst x) (Ints_t.lognot mask) in
      let newo = Ints_t.logand (snd x) mask in
      norm ik (newz, newo) |> fst
    )
    else top_of ik

  let eq ik x y =
    if (BArith.max ik x) <= (BArith.min ik y) && (BArith.min ik x) >= (BArith.max ik y) then of_bool ik true 
    else if (BArith.min ik x) > (BArith.max ik y) || (BArith.max ik x) < (BArith.min ik y) then of_bool ik false 
    else BArith.top_bool

  let ne ik x y = match eq ik x y with
    | t when t = of_bool ik true -> of_bool ik false
    | t when t = of_bool ik false -> of_bool ik true
    | _ -> BArith.top_bool

  let le ik x y = 
    if (BArith.max ik x) <= (BArith.min ik y) then of_bool ik true 
    else if (BArith.min ik x) > (BArith.max ik y) then of_bool ik false 
    else BArith.top_bool

  let ge ik x y = le ik y x

  let lt ik x y = if (BArith.max ik x) < (BArith.min ik y) then of_bool ik true 
    else if (BArith.min ik x) >= (BArith.max ik y) then of_bool ik false 
    else BArith.top_bool

  let gt ik x y = lt ik y x

  (* Invariant *)

  let invariant_ikind e ik (z,o) = 
    let range = range ik (z,o) in
    IntInvariant.of_interval e ik range

  let starting ?(suppress_ovwarn=false) ik n = 
    let (min_ik, max_ik) = Size.range ik in
    of_interval ~suppress_ovwarn ik (n, Ints_t.of_bigint max_ik)

  let ending ?(suppress_ovwarn=false) ik n =
    let (min_ik, max_ik) = Size.range ik in
    of_interval ~suppress_ovwarn ik (Ints_t.of_bigint min_ik, n)

  (* Refinements *)

  let refine_with_congruence ik bf ((cong) : (int_t * int_t ) option) : t =
    match bf, cong with
    | (z,o), Some (c, m) when m = Ints_t.zero -> norm ik (!: c, c) |> fst
    | (z,o), Some (c, m) when is_power_of_two m && m <> Ints_t.one ->
      let congruenceMask = !:m in
      let newz = (!:congruenceMask &: z) |: (congruenceMask &: !:c) in
      let newo = (!:congruenceMask &: o) |: (congruenceMask &: c) in
      norm ik (newz, newo) |> fst
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