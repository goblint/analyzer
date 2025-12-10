open IntDomain0
open GoblintCil


module Congruence : S with type int_t = Z.t and type t = (Z.t * Z.t) option =
struct
  let name () = "congruences"
  type int_t = Z.t

  (* represents congruence class of c mod m, None is bot *)
  type t = (Z.t * Z.t) option [@@deriving eq, ord, hash]

  let ( *: ) = Z.mul
  let (+:) = Z.add
  let (-:) = Z.sub
  let (%:) = Z.rem
  let (/:) = Z.div
  let (=:) = Z.equal
  let (<:) x y = Z.compare x y < 0
  let (>:) x y = Z.compare x y > 0
  let (<=:) x y = Z.compare x y <= 0
  let (>=:) x y = Z.compare x y >= 0
  (* a divides b *)
  let ( |: ) a b =
    if a =: Z.zero then false else (b %: a) =: Z.zero

  let normalize ik x =
    match x with
    | None -> None
    | Some (c, m) ->
      if m =: Z.zero then
        if should_wrap ik then
          Some (Size.cast ik c, m)
        else
          Some (c, m)
      else
        let m' = Z.abs m in
        let c' = c %: m' in
        if c' <: Z.zero then
          Some (c' +: m', m')
        else
          Some (c' %: m', m')

  let range ik = Size.range ik

  let top () = Some (Z.zero, Z.one)
  let top_of ?bitfield ik = Some (Z.zero, Z.one)
  let bot () = None
  let bot_of ik = bot ()

  let show = function ik -> match ik with
    | None -> "⟂"
    | Some (c, m) when (c, m) = (Z.zero, Z.zero) -> Z.to_string c
    | Some (c, m) ->
      let a = if c =: Z.zero then "" else Z.to_string c in
      let b = if m =: Z.zero then "" else if m = Z.one then "ℤ" else Z.to_string m^"ℤ" in
      let c = if a = "" || b = "" then "" else "+" in
      a^c^b

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let is_top x = x = top ()

  let equal_to i = function
    | None -> failwith "unsupported: equal_to with bottom"
    | Some (a, b) when b =: Z.zero -> if a =: i then `Eq else `Neq
    | Some (a, b) ->  if i %: b =: a then `Top else `Neq

  let leq (x:t) (y:t) =
    match x, y with
    | None, _ -> true
    | Some _, None -> false
    | Some (c1,m1), Some (c2,m2) when m2 =: Z.zero && m1 =: Z.zero -> c1 =: c2
    | Some (c1,m1), Some (c2,m2) when m2 =: Z.zero -> c1 =: c2 && m1 =: Z.zero
    | Some (c1,m1), Some (c2,m2) -> m2 |: Z.gcd (c1 -: c2) m1
  (* Typo in original equation of P. Granger (m2 instead of m1): gcd (c1 -: c2) m2
     Reference: https://doi.org/10.1080/00207168908803778 Page 171 corollary 3.3*)

  let leq x y =
    let res = leq x y in
    if M.tracing then M.trace "congruence" "leq %a %a -> %a " pretty x pretty y pretty (Some (Z.of_int (Bool.to_int res), Z.zero)) ;
    res

  let join ik (x:t) y =
    match x, y with
    | None, z | z, None -> z
    | Some (c1,m1), Some (c2,m2) ->
      let m3 = Z.gcd m1 (Z.gcd m2 (c1 -: c2)) in
      normalize ik (Some (c1, m3))

  let join ik (x:t) y =
    let res = join ik x y in
    if M.tracing then M.trace "congruence" "join %a %a -> %a" pretty x pretty y pretty res;
    res


  let meet ik x y =
    (* if it exists, c2/a2 is solution to a*x ≡ c (mod m) *)
    let congruence_series a c m =
      let rec next a1 c1 a2 c2 =
        if a2 |: a1 then (a2, c2)
        else next a2 c2 (a1 %: a2) (c1 -: (c2 *: (a1 /: a2)))
      in next m Z.zero a c
    in
    let simple_case i c m =
      if m |: (i -: c)
      then Some (i, Z.zero) else None
    in
    match x, y with
    | Some (c1, m1), Some (c2, m2) when m1 =: Z.zero && m2 =: Z.zero -> if c1 =: c2 then Some (c1, Z.zero) else None
    | Some (c1, m1), Some (c2, m2) when m1 =: Z.zero -> simple_case c1 c2 m2
    | Some (c1, m1), Some (c2, m2) when m2 =: Z.zero -> simple_case c2 c1 m1
    | Some (c1, m1), Some (c2, m2) when (Z.gcd m1 m2) |: (c1 -: c2) ->
      let (c, m) = congruence_series m1 (c2 -: c1 ) m2 in
      normalize ik (Some(c1 +: (m1 *: (m /: c)), m1 *: (m2 /: c)))
    | _  -> None

  let meet ik x y =
    let res = meet ik x y in
    if M.tracing then M.trace "congruence" "meet %a %a -> %a" pretty x pretty y pretty res;
    res

  let to_int = function Some (c, m) when m =: Z.zero -> Some c | _ -> None
  let of_int ik (x: int_t) = normalize ik @@ Some (x, Z.zero)
  let zero = Some (Z.zero, Z.zero)
  let one  = Some (Z.one, Z.zero)
  let top_bool = top()

  let of_bool _ik = function true -> one | false -> zero

  let to_bool (a: t) = match a with
    | None -> None
    | x when equal zero x -> Some false
    | x -> if leq zero x then None else Some true

  let starting ik n = top()

  let ending = starting
  let of_interval ik x = of_interval ik x (* cast away optional suppress_ovwarn argument *)

  let of_congruence ik (c,m) = normalize ik @@ Some(c,m)

  let of_bitfield ik (z,o) =
    match BitfieldDomain.Bitfield.to_int (z,o) with
    | Some x -> normalize ik (Some (x, Z.zero))
    | _ ->
      (* get position of first top bit *)
      let tl_zeros = Z.trailing_zeros (Z.logand z o) in
      let ik_bits = Size.bit ik in
      let m = if tl_zeros > ik_bits then Z.one else Z.pow Z.one tl_zeros in
      let c = Z.logand o (m -: Z.one) in
      normalize ik (Some (c, m))

  let to_bitfield ik x =
    let x = normalize ik x in
    match x with
    | None -> (Z.zero, Z.zero)
    | Some (c,m) -> BitfieldDomain.Bitfield.of_congruence ik (c,m)

  let maximal t = match t with
    | Some (x, y) when y =: Z.zero -> Some x
    | _ -> None

  let minimal t = match t with
    | Some (x,y) when y =: Z.zero -> Some x
    | _ -> None

  (* cast from original type to ikind, set to top if the value doesn't fit into the new type *)
  let cast_to ?(suppress_ovwarn=false) ~kind ?torg ?(no_ov=false) t x =
    match x with
    | None -> None
    | Some (c, m) when m =: Z.zero ->
      let c' = Size.cast t c in
      (* When casting into a signed type and the result does not fit, the behavior is implementation-defined. (C90 6.2.1.2, C99 and C11 6.3.1.3) *)
      (* We go with GCC behavior here: *)
      (*  For conversion to a type of width N, the value is reduced modulo 2^N to be within range of the type; no signal is raised. *)
      (*   (https://gcc.gnu.org/onlinedocs/gcc/Integers-implementation.html)   *)
      (* Clang behaves the same but they never document that anywhere *)
      Some (c', m)
    | _ ->
      let (min_t, max_t) = range t in
      let p ikorg =
        let (min_ikorg, max_ikorg) = range ikorg in
        ikorg = t || (max_t >=: max_ikorg && min_t <=: min_ikorg)
      in
      match Option.map Cil.unrollType torg with
      | Some (Cil.TInt (ikorg, _) | TEnum ({ekind = ikorg; _}, _)) when p ikorg ->
        if M.tracing then M.trace "cong-cast" "some case";
        x
      | _ -> top ()


  let cast_to ?(suppress_ovwarn=false) ~kind ?torg ?no_ov (t : Cil.ikind) x =
    let pretty_bool _ x = Pretty.text (string_of_bool x) in
    let res = cast_to ~kind ?torg ?no_ov t x in
    if M.tracing then M.trace "cong-cast" "Cast %a to %a (no_ov: %a) = %a" pretty x Cil.d_ikind t (Pretty.docOpt (pretty_bool ())) no_ov pretty res;
    res

  let widen = join

  let widen ik x y =
    let res = widen ik x y in
    if M.tracing then M.trace "congruence" "widen %a %a -> %a" pretty x pretty y pretty res;
    res

  let narrow = meet

  let log f ik i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, true -> bot_of ik
    | true, _
    | _   , true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show i1) (show i2)))
    | _ ->
      match to_bool i1, to_bool i2 with
      | Some x, Some y -> of_bool ik (f x y)
      | _              -> top_of ik

  let c_logor = log (||)
  let c_logand = log (&&)

  let log1 f ik i1 =
    if is_bot i1 then
      bot_of ik
    else
      match to_bool i1 with
      | Some x -> of_bool ik (f ik x)
      | _      -> top_of ik

  let c_lognot = log1 (fun _ik -> not)

  let shift_right _ _ _ = top()

  let shift_right ik x y =
    let res = shift_right ik x y in
    if M.tracing then  M.trace "congruence" "shift_right : %a %a becomes %a " pretty x pretty y pretty res;
    res

  let shift_left ik x y =
    (* Naive primality test *)
    (* let is_prime n =
         let n = Z.abs n in
         let rec is_prime' d =
           (d *: d >: n) || ((not ((n %: d) =: Z.zero)) && (is_prime' [@tailcall]) (d +: Z.one))
         in
         not (n =: Z.one) && is_prime' (Z.of_int 2)
       in *)
    match x, y with
    | None, None -> None
    | None, _
    | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c, m), Some (c', m') when Cil.isSigned ik || c <: Z.zero || c' <: Z.zero -> top_of ik
    | Some (c, m), Some (c', m') ->
      let (_, max_ik) = range ik in
      if m =: Z.zero && m' =: Z.zero then
        normalize ik @@ Some (Z.logand max_ik (Z.shift_left c (Z.to_int c')), Z.zero)
      else
        let x = Z.logand max_ik (Z.shift_left Z.one (Z.to_int c')) in (* 2^c' *)
        (* TODO: commented out because fails test with _Bool *)
        (* if is_prime (m' +: Z.one) then
             normalize ik @@ Some (x *: c, Z.gcd (x *: m) ((c *: x) *: (m' +: Z.one)))
           else *)
        normalize ik @@ Some (x *: c, Z.gcd (x *: m) (c *: x))

  let shift_left ik x y =
    let res = shift_left ik x y in
    if M.tracing then  M.trace "congruence" "shift_left : %a %a becomes %a " pretty x pretty y pretty res;
    res

  (* Handle unsigned overflows.
     From n === k mod (2^a * b), we conclude n === k mod 2^a, for a <= bitwidth.
     The congruence modulo b may not persist on an overflow. *)
  let handle_overflow ik (c, m) =
    if m =: Z.zero then
      normalize ik (Some (c, m))
    else
      (* Find largest m'=2^k (for some k) such that m is divisible by m' *)
      let tz = Z.trailing_zeros m in
      let m' = Z.shift_left Z.one tz in

      let max = (snd (Size.range ik)) +: Z.one in
      if m' >=: max then
        (* if m' >= 2 ^ {bitlength}, there is only one value in range *)
        let c' = c %: max in
        Some (c', Z.zero)
      else
        normalize ik (Some (c, m'))

  let mul ?(no_ov=false) ik x y =
    let no_ov_case (c1, m1) (c2, m2) =
      c1 *: c2, Z.gcd (c1 *: m2) (Z.gcd (m1 *: c2) (m1 *: m2))
    in
    match x, y with
    | None, None -> bot ()
    | None, _ | _, None ->
      raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1, m1), Some (c2, m2) when no_ov ->
      Some (no_ov_case (c1, m1) (c2, m2))
    | Some (c1, m1), Some (c2, m2) when m1 =: Z.zero && m2 =: Z.zero && not (Cil.isSigned ik) ->
      let (_, max_ik) = range ik in
      Some ((c1 *: c2) %: (max_ik +: Z.one), Z.zero)
    | Some a, Some b when not (Cil.isSigned ik) ->
      handle_overflow ik (no_ov_case a b )
    | _ -> top ()

  let mul ?no_ov ik x y =
    let res = mul ?no_ov ik x y in
    if M.tracing then  M.trace "congruence" "mul : %a %a -> %a " pretty x pretty y pretty res;
    res

  let neg ?(no_ov=false) ik x =
    match x with
    | None -> bot()
    | Some _ -> mul ~no_ov ik (of_int ik (Z.of_int (-1))) x

  let add ?(no_ov=false) ik x y =
    let no_ov_case (c1, m1) (c2, m2) =
      c1 +: c2, Z.gcd m1 m2
    in
    match (x, y) with
    | None, None -> bot ()
    | None, _ | _, None ->
      raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some a, Some b when no_ov ->
      normalize ik (Some (no_ov_case a b))
    | Some (c1, m1), Some (c2, m2) when m1 =: Z.zero && m2 =: Z.zero && not (Cil.isSigned ik) ->
      let (_, max_ik) = range ik in
      Some((c1 +: c2) %: (max_ik +: Z.one), Z.zero)
    | Some a, Some b when not (Cil.isSigned ik) ->
      handle_overflow ik (no_ov_case a b)
    | _ -> top ()


  let add ?no_ov ik x y =
    let res = add ?no_ov ik x y in
    if M.tracing then
      M.trace "congruence" "add : %a %a -> %a" pretty x pretty y
        pretty res ;
    res

  let sub ?(no_ov=false) ik x y =
    let no_ov_case (c1, m1) (c2, m2) =
      c1 -: c2, Z.gcd m1 m2
    in
    match (x, y) with
    | None, None -> bot ()
    | None, _
    | _, None ->
      raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some a, Some b when no_ov ->
      normalize ik (Some (no_ov_case a b))
    | Some (c1, m1), Some (c2, m2) when m1 =: Z.zero && m2 =: Z.zero ->
      let min_ik, max_ik = range ik in
      let m_ikind = max_ik +: Z.one in
      if Cil.isSigned ik then
        let c = c1 -: c2 in
        if c >=: min_ik && c <= max_ik then
          Some (c, Z.zero)
        else
          top_of ik
      else
        let c = (c1 -: c2 +: m_ikind) %: m_ikind  in
        Some (c, Z.zero)
    | Some a, Some b when not (Cil.isSigned ik) ->
      handle_overflow ik (no_ov_case a b)
    | _ -> top ()

  let sub ?no_ov ik x y =
    let res = sub ?no_ov ik x y in
    if M.tracing then
      M.trace "congruence" "sub : %a %a -> %a" pretty x pretty y
        pretty res ;
    res

  let lognot ik x = match x with
    | None -> None
    | Some (c, m) ->
      if (Cil.isSigned ik) then
        sub ik (neg ik x) one
      else
        let (_, max_ik) = range ik in
        Some (Z.sub max_ik c, m)

  (** The implementation of the bit operations could be improved based on the master’s thesis
      'Abstract Interpretation and Abstract Domains' written by Stefan Bygde.
      see: http://www.es.mdh.se/pdf_publications/948.pdf *)
  let bit2 f ik x y = match x, y with
    | None, None -> None
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c, m), Some (c', m') ->
      if m =: Z.zero && m' =: Z.zero then Some (f c c', Z.zero)
      else top ()

  let logor ik x y = bit2 Z.logor ik x y

  let logand ik x y =  match x, y with
    | None, None -> None
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c, m), Some (c', m') ->
      if m =: Z.zero && m' =: Z.zero then
        (* both arguments constant *)
        Some (Z.logand c c', Z.zero)
      else if m' =: Z.zero && c' =: Z.one && Z.rem m (Z.of_int 2) =: Z.zero then
        (* x & 1  and  x == c (mod 2*z) *)
        (* Value is equal to LSB of c *)
        Some (Z.logand c c', Z.zero)
      else
        top ()

  let logxor ik x y = bit2 Z.logxor ik x y

  let rem ik x y =
    match x, y with
    | None, None -> bot()
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1, m1), Some(c2, m2) ->
      if m2 =: Z.zero then
        if (c2 |: m1) && (c1 %: c2 =: Z.zero || m1 =: Z.zero || not (Cil.isSigned ik)) then
          Some (c1 %: c2, Z.zero)
        else
          normalize ik (Some (c1, (Z.gcd m1 c2)))
      else
        normalize ik (Some (c1, Z.gcd m1 (Z.gcd c2 m2)))

  let rem ik x y = let res = rem ik x y in
    if M.tracing then  M.trace "congruence" "rem : %a %a -> %a " pretty x pretty y pretty res;
    res

  let div ?(no_ov=false) ik x y =
    match x,y with
    | None, None -> bot ()
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | _, x when leq zero x -> top ()
    | Some(c1, m1), Some(c2, m2) when not no_ov && m2 =: Z.zero && c2 =: Z.neg Z.one -> top ()
    | Some(c1, m1), Some(c2, m2) when m1 =: Z.zero && m2 =: Z.zero -> Some (c1 /: c2, Z.zero)
    | Some(c1, m1), Some(c2, m2) when m2 =: Z.zero && c2 |: m1 && c2 |: c1 -> Some (c1 /: c2, m1 /: c2)
    | _, _ -> top ()


  let div ?no_ov ik x y =
    let res = div ?no_ov ik x y in
    if M.tracing then
      M.trace "congruence" "div : %a %a -> %a" pretty x pretty y pretty
        res ;
    res

  let ne ik (x: t) (y: t) = match x, y with
    | Some (c1, m1), Some (c2, m2) when (m1 =: Z.zero) && (m2 =: Z.zero) -> of_bool ik (not (c1 =: c2 ))
    | x, y -> if meet ik x y = None then of_bool ik true else top_bool

  let eq ik (x: t) (y: t) = match x, y with
    | Some (c1, m1), Some (c2, m2) when (m1 =: Z.zero) && (m2 =: Z.zero) -> of_bool ik (c1 =: c2)
    | x, y -> if meet ik x y <> None then top_bool else of_bool ik false

  let comparison ik op x y = match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1, m1), Some (c2, m2) ->
      if m1 =: Z.zero && m2 =: Z.zero then
        if op c1 c2 then of_bool ik true else of_bool ik false
      else
        top_bool

  let ge ik x y = comparison ik (>=:) x y

  let ge ik x y =
    let res = ge ik x y in
    if M.tracing then  M.trace "congruence" "greater or equal : %a %a -> %a " pretty x pretty y pretty res;
    res

  let le ik x y = comparison ik (<=:) x y

  let le ik x y =
    let res = le ik x y in
    if M.tracing then  M.trace "congruence" "less or equal : %a %a -> %a " pretty x pretty y pretty res;
    res

  let gt ik x y = comparison ik (>:) x y

  let gt ik x y =
    let res = gt ik x y in
    if M.tracing then  M.trace "congruence" "greater than : %a %a -> %a " pretty x pretty y pretty res;
    res

  let lt ik x y = comparison ik (<:) x y

  let lt ik x y =
    let res = lt ik x y in
    if M.tracing then  M.trace "congruence" "less than : %a %a -> %a " pretty x pretty y pretty res;
    res

  let invariant_ikind e ik x =
    match x with
    | x when is_top x -> Invariant.top ()
    | Some (c, m) when m =: Z.zero ->
      IntInvariant.of_int e ik c
    | Some (c, m) ->
      let open Cil in
      let (c, m) = BatTuple.Tuple2.mapn (fun a -> kintegerCilint ik a) (c, m) in
      Invariant.of_exp (BinOp (Eq, (BinOp (Mod, e, m, TInt(ik,[]))), c, intType))
    | None -> Invariant.none

  let arbitrary ik =
    let open QCheck in
    let int_arb = map ~rev:Z.to_int64 Z.of_int64 GobQCheck.Arbitrary.int64 in
    let cong_arb = pair int_arb int_arb in
    let of_pair ik p = normalize ik (Some p) in
    let to_pair = Option.get in
    set_print show (map ~rev:to_pair (of_pair ik) cong_arb)

  let refine_with_interval ik (cong : t) (intv : (int_t * int_t ) option) : t =
    match intv, cong with
    | Some (x, y), Some (c, m) ->
      if m =: Z.zero then
        if c <: x || c >: y then None else Some (c, Z.zero)
      else
        let rcx = x +: ((c -: x) %: Z.abs m) in
        let lcy = y -: ((y -: c) %: Z.abs m) in
        if rcx >: lcy then None
        else if rcx =: lcy then Some (rcx, Z.zero)
        else cong
    | _ -> None

  let refine_with_interval ik (cong : t) (intv : (int_t * int_t) option) : t =
    let pretty_intv _ i =
      match i with
      | Some (l, u) -> Pretty.dprintf "[%a,%a]" GobZ.pretty l GobZ.pretty u
      | _ -> Pretty.text ("Display Error") in
    let refn = refine_with_interval ik cong intv in
    if M.tracing then M.trace "refine" "cong_refine_with_interval %a %a -> %a" pretty cong pretty_intv intv pretty refn;
    refn

  let refine_with_congruence ik a b = meet ik a b

  let refine_with_bitfield ik a (z,o) =
    let a = normalize ik a in
    meet ik a (of_bitfield ik (z,o))

  let refine_with_excl_list ik a b = a

  let refine_with_incl_list ik a b = a

  let project ik p t = t
end
