open Pretty
open PrecisionUtil
open FloatOps
open Cil

module type FloatArith = sig
  type t

  val neg : t -> t
  (** Negating an flaot value: [-x] *)
  val add : t -> t -> t
  (** Addition: [x + y] *)
  val sub : t -> t -> t
  (** Subtraction: [x - y] *)
  val mul : t -> t -> t
  (** Multiplication: [x * y] *)
  val div : t -> t -> t
  (** Division: [x / y] *)

  (** {b Comparison operators} *)
  val lt : t -> t -> IntDomain.IntDomTuple.t
  (** Less than: [x < y] *)
  val gt : t -> t -> IntDomain.IntDomTuple.t
  (** Greater than: [x > y] *)
  val le : t -> t -> IntDomain.IntDomTuple.t
  (** Less than or equal: [x <= y] *)
  val ge : t -> t -> IntDomain.IntDomTuple.t
  (** Greater than or equal: [x >= y] *)
  val eq : t -> t -> IntDomain.IntDomTuple.t
  (** Equal to: [x == y] *)
  val ne : t -> t -> IntDomain.IntDomTuple.t
  (** Not equal to: [x != y] *)

  (** {unary functions returning int} *)
  val isfinite : t -> IntDomain.IntDomTuple.t
  (** __builtin_isfinite(x) *)
  val isinf : t -> IntDomain.IntDomTuple.t
  (** __builtin_isinf(x) *)
  val isnan : t -> IntDomain.IntDomTuple.t
  (** __builtin_isnan(x) *)
  val isnormal : t -> IntDomain.IntDomTuple.t
  (** __builtin_isnormal(x) *)
  val signbit : t -> IntDomain.IntDomTuple.t
  (** __builtin_signbit(x) *)
end

module type FloatDomainBase = sig
  include Lattice.S
  include FloatArith with type t := t

  val to_int : Cil.ikind -> t -> IntDomain.IntDomTuple.t

  val of_const : float -> t
  val of_interval : float * float -> t
  val of_string : string -> t
  val of_int: IntDomain.IntDomTuple.t -> t

  val ending : float -> t
  val starting : float -> t

  val minimal: t -> float option
  val maximal: t -> float option

  val is_exact : t -> bool
end

module FloatIntervalImpl(Float_t : CFloatType) = struct
  include Printable.Std (* for default invariant, tag and relift *)
  type t = (Float_t.t * Float_t.t) option [@@deriving eq, ord, to_yojson]

  let hash = Hashtbl.hash

  let to_int ik = function
    | None -> IntDomain.IntDomTuple.top_of ik
    | Some (l, h) -> 
      (* as converting from float to integer is (exactly) defined as leaving out the fractional part,
         (value is truncated towrad zero) we do not require specific rounding here *)
      IntDomain.IntDomTuple.of_interval ik (Float_t.to_big_int l, Float_t.to_big_int h)

  let of_int x = 
    match IntDomain.IntDomTuple.minimal x, IntDomain.IntDomTuple.maximal x with
    | Some l, Some h when l >= Float_t.to_big_int Float_t.lower_bound && h <= Float_t.to_big_int Float_t.upper_bound ->
      let l' = Float_t.of_float Down (Big_int_Z.float_of_big_int l) in
      let h' = Float_t.of_float Up (Big_int_Z.float_of_big_int h) in
      if not (Float_t.is_finite l' && Float_t.is_finite h') then
        None
      else
        Some (l', h')
    | _, _ -> None

  let show = function
    | None -> Float_t.name ^ ": [Top]"
    | Some (low, high) -> Printf.sprintf "%s:[%s,%s]" Float_t.name (Float_t.to_string low) (Float_t.to_string high)

  let pretty () x = text (show x)

  let printXml f (x : t) =
    BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)

  let name () = "FloatInterval"

  (** If [leq x y = false], then [pretty_diff () (x, y)] should explain why. *)
  let pretty_diff () (x, y) =
    Pretty.dprintf "%a instead of %a" pretty x pretty y

  let bot () = failwith "no bot exists"

  let is_bot _ = false

  let top () = None

  let is_top = Option.is_none

  let neg = Option.map (fun (low, high) -> (Float_t.neg high, Float_t.neg low))

  let norm v = 
    let normed = match v with
      | Some (low, high) -> 
        if Float_t.is_finite low && Float_t.is_finite high then 
          if low > high then failwith "invalid Interval"
          else v
        else None
      | _ -> None
    in if is_top normed then
      Messages.warn ~category:Messages.Category.FloatMessage ~tags:[CWE 189; CWE 739] 
        "Float could be +/-infinity or Nan";
    normed

  (**just for norming the arbitraries, so correct intervals get created, but no failwith if low > high*)
  let norm_arb v = 
    match v with
    | Some (f1, f2) ->
      let f1' = Float_t.of_float Nearest f1 in
      let f2' = Float_t.of_float Nearest f2 in
      if Float_t.is_finite f1' && Float_t.is_finite f2' 
      then Some(min f1' f2', max f1' f2') 
      else None
    | _ -> None

  (**for QCheck: should describe how to generate random values and shrink possible counter examples *)
  let arbitrary () = QCheck.map norm_arb (QCheck.option (QCheck.pair QCheck.float QCheck.float)) 

  let of_interval' interval = norm @@ Some interval
  let of_interval (l, h) = of_interval' (Float_t.of_float Down (min l h), Float_t.of_float Up (max l h))
  let of_string s = of_interval' (Float_t.atof Down s, Float_t.atof Up s)
  let of_const f = of_interval (f, f)

  let ending e = of_interval' (Float_t.lower_bound, Float_t.of_float Up e)
  let starting s = of_interval' (Float_t.of_float Down s, Float_t.upper_bound)

  let minimal x = Option.bind x (fun (l, _) -> Float_t.to_float l)
  let maximal x = Option.bind x (fun (_, h) -> Float_t.to_float h)

  let is_exact = function Some (l, v) -> l = v | _ -> false

  let leq v1 v2 = 
    match v1, v2 with
    | _, None -> true
    | None, Some _ -> false
    | Some (l1, h1), Some (l2, h2) -> l1 >= l2 && h1 <= h2

  let join v1 v2 = 
    match v1, v2 with
    | None, _ | _, None -> None
    | Some (l1, h1), Some (l2, h2) -> Some (min l1 l2, max h1 h2)

  let meet v1 v2 = 
    match v1, v2 with
    | None, v | v, None -> v
    | Some (l1, h1), Some (l2, h2) -> 
      let (l, h) = (max l1 l2, min h1 h2) in
      if l <= h 
      then Some (l, h) 
      else failwith "meet results in empty Interval"

  (** [widen x y] assumes [leq x y]. Solvers guarantee this by calling [widen old (join old new)]. *)
  let widen v1 v2 = (**TODO: support 'threshold_widening' option *)
    match v1, v2 with
    | None, _ | _, None -> None
    | Some (l1, h1), Some (l2, h2) ->
      (**If we widen and we know that neither interval contains +-inf or nan, it is ok to widen only to +-max_float, 
         because a widening with +-inf/nan will always result in the case above -> Top *)
      let low = if l1 <= l2 then l1 else Float_t.lower_bound in 
      let high = if h1 >= h2 then h1 else Float_t.upper_bound in
      norm @@ Some (low, high)

  let narrow v1 v2 = (**TODO: support 'threshold_narrowing' and 'narrow_by_meet' option *)
    match v1, v2 with (**we cannot distinguish between the lower bound beeing -inf or the upper bound beeing inf. Also there is nan *)
    | None, _ -> v2
    | _, _ -> v1

  (** evaluation of the binary operations *)
  let eval_binop eval_operation op1 op2 =
    norm @@ match (op1, op2) with 
    | Some v1, Some v2 -> eval_operation v1 v2
    | _ -> None

  let eval_int_binop eval_operation (op1: t) op2 =
    let a, b =
      match (op1, op2) with 
      | Some v1, Some v2 -> eval_operation v1 v2 
      | _ -> (0, 1)
    in
    IntDomain.IntDomTuple.of_interval IBool
      (Big_int_Z.big_int_of_int a, Big_int_Z.big_int_of_int b)

  let eval_add (l1, h1) (l2, h2) = 
    Some (Float_t.add Down l1 l2, Float_t.add Up h1 h2)

  let eval_sub (l1, h1) (l2, h2) = 
    Some (Float_t.sub Down l1 h2, Float_t.sub Up h1 l2)

  let eval_mul (l1, h1) (l2, h2) =
    let mul1u = Float_t.mul Up l1 l2 in
    let mul2u = Float_t.mul Up l1 h2 in
    let mul3u = Float_t.mul Up h1 l2 in
    let mul4u = Float_t.mul Up h1 h2 in
    let mul1d = Float_t.mul Down l1 l2 in
    let mul2d = Float_t.mul Down l1 h2 in
    let mul3d = Float_t.mul Down h1 l2 in
    let mul4d = Float_t.mul Down h1 h2 in
    let high = max (max (max mul1u mul2u) mul3u) mul4u in
    let low = min (min (min mul1d mul2d) mul3d) mul4d in
    Some (low, high)

  let eval_div (l1, h1) (l2, h2) =
    if l2 <= Float_t.zero && h2 >= Float_t.zero then None
    else
      let div1u = Float_t.div Up l1 l2 in
      let div2u = Float_t.div Up l1 h2 in
      let div3u = Float_t.div Up h1 l2 in
      let div4u = Float_t.div Up h1 h2 in
      let div1d = Float_t.div Down l1 l2 in
      let div2d = Float_t.div Down l1 h2 in
      let div3d = Float_t.div Down h1 l2 in
      let div4d = Float_t.div Down h1 h2 in
      let high = max (max (max div1u div2u) div3u) div4u in
      let low = min (min (min div1d div2d) div3d) div4d in
      Some (low, high)


  let eval_lt (l1, h1) (l2, h2) = 
    if h1 < l2 then (1, 1)
    else if l1 >= h2 then (0, 0) 
    else (0, 1)

  let eval_gt (l1, h1) (l2, h2) =
    if l1 > h2 then (1, 1)
    else if h1 <= l2 then (0, 0) 
    else (0, 1)

  let eval_le (l1, h1) (l2, h2) =
    if h1 <= l2 then (1, 1)
    else if l1 > h2 then (0, 0) 
    else (0, 1)

  let eval_ge (l1, h1) (l2, h2) =
    if l1 >= h2 then (1, 1)
    else if h1 < l2 then (0, 0) 
    else (0, 1)

  let eval_eq (l1, h1) (l2, h2) =
    if h1 < l2 || h2 < l1 then (0, 0)
    else if h1 = l1 && h2 = l2 && l1 = l2 then (1, 1)
    else (0, 1)

  let eval_ne (l1, h1) (l2, h2) =
    if h1 < l2 || h2 < l1 then (1, 1)
    else if h1 = l1 && h2 = l2 && l1 = l2 then (0, 0)
    else (0, 1)

  let add = eval_binop eval_add

  let sub = eval_binop eval_sub

  let mul = eval_binop eval_mul

  let div = eval_binop eval_div

  let lt = eval_int_binop eval_lt

  let gt = eval_int_binop eval_gt

  let le = eval_int_binop eval_le

  let ge = eval_int_binop eval_ge

  let eq = eval_int_binop eval_eq

  let ne = eval_int_binop eval_ne

  let true_nonZero_IInt = IntDomain.IntDomTuple.of_excl_list IInt [(Big_int_Z.big_int_of_int 0)]
  let false_zero_IInt = IntDomain.IntDomTuple.of_int IInt (Big_int_Z.big_int_of_int 0)
  let unknown_IInt = IntDomain.IntDomTuple.top_of IInt

  let isfinite op =
    match op with 
    | Some v -> true_nonZero_IInt
    | None -> unknown_IInt

  let isinf op =
    match op with 
    | Some v -> false_zero_IInt
    | None -> unknown_IInt

  let isnan = isinf (**currently we cannot decide if we are NaN or +-inf; both are only in Top*)

  let isnormal op =
    match op with 
    | Some (l, h) -> 
      if l >= Float_t.smallest || h <= (Float_t.neg (Float_t.smallest)) then
        true_nonZero_IInt
      else if l > (Float_t.neg (Float_t.smallest)) && h < Float_t.smallest then
        false_zero_IInt
      else 
        unknown_IInt
    | None -> unknown_IInt

  (**it seems strange not to return a explicit 1 for negative numbers, but in c99 signbit is defined as: *)
  (**<<The signbit macro returns a nonzero value if and only if the sign of its argument value is negative.>> *)
  let signbit op = 
    match op with
    | Some (_, h) when h < Float_t.zero -> true_nonZero_IInt
    | Some (l, _) when l > Float_t.zero -> false_zero_IInt
    | Some _ -> unknown_IInt (**any interval containing zero has to fall in this case, because we do not distinguish between 0. and -0. *)
    | None -> unknown_IInt

end

module F64Interval = FloatIntervalImpl(CDouble)
module F32Interval = FloatIntervalImpl(CFloat)

module type FloatDomain = sig
  include Lattice.S
  include FloatArith with type t := t

  val to_int : Cil.ikind -> t -> IntDomain.IntDomTuple.t
  val cast_to : Cil.fkind -> t -> t

  val of_const : Cil.fkind -> float -> t
  val of_interval : Cil.fkind -> float*float -> t
  val of_string : Cil.fkind -> string -> t
  val of_int: Cil.fkind -> IntDomain.IntDomTuple.t -> t

  val top_of: Cil.fkind -> t
  val bot_of: Cil.fkind -> t

  val ending : Cil.fkind -> float -> t
  val starting : Cil.fkind -> float -> t

  val minimal: t -> float option
  val maximal: t -> float option

  val is_exact : t -> bool
  val precision : t -> Cil.fkind
end

module FloatIntervalImplLifted = struct
  include Printable.Std (* for default invariant, tag and relift *)
  type t = F32 of F32Interval.t | F64 of F64Interval.t [@@deriving to_yojson, eq, ord, hash]

  module F1 = F32Interval
  module F2 = F64Interval

  let lift2 (op32, op64) x y = match x, y with
    | F32 a, F32 b -> F32 (op32 a b)
    | F64 a, F64 b -> F64 (op64 a b)
    | F32 a, F64 b -> failwith ("fkinds float and double are incompatible. Values: " ^ Prelude.Ana.sprint F32Interval.pretty a ^ " and " ^ Prelude.Ana.sprint F64Interval.pretty b)
    | F64 a, F32 b -> failwith ("fkinds double and float are incompatible. Values: " ^ Prelude.Ana.sprint F64Interval.pretty a ^ " and " ^ Prelude.Ana.sprint F32Interval.pretty b)

  let lift2_cmp (op32, op64) x y = match x, y with
    | F32 a, F32 b -> op32 a b
    | F64 a, F64 b -> op64 a b
    | F32 a, F64 b -> failwith ("fkinds float and double are incompatible. Values: " ^ Prelude.Ana.sprint F32Interval.pretty a ^ " and " ^ Prelude.Ana.sprint F64Interval.pretty b)
    | F64 a, F32 b -> failwith ("fkinds double and float are incompatible. Values: " ^ Prelude.Ana.sprint F64Interval.pretty a ^ " and " ^ Prelude.Ana.sprint F32Interval.pretty b)

  let lift (op32, op64) x = match x with
    | F32 a -> F32 (op32 a)
    | F64 a -> F64 (op64 a)

  let dispatch (op32, op64) x = match x with
    | F32 a -> op32 a
    | F64 a -> op64 a

  let dispatch_fkind fkind (op32, op64) = match fkind with
    | FFloat -> F32 (op32 ())
    | FDouble -> F64 (op64 ()) 
    | _ -> 
      (* this sould never be reached, as we have to check for invalid fkind elsewhere, 
         however we could instead of crashing also return top_of some fkind to vaid this and nonetheless have no actual information about anything*)
      failwith "unsupported fkind" 

  let neg = lift (F1.neg, F2.neg)
  let add = lift2 (F1.add, F2.add)
  let sub = lift2 (F1.sub, F2.sub)
  let mul = lift2 (F1.mul, F2.mul)
  let div = lift2 (F1.div, F2.div)
  let lt = lift2_cmp (F1.lt, F2.lt)
  let gt = lift2_cmp (F1.gt, F2.gt)
  let le = lift2_cmp (F1.le, F2.le)
  let ge = lift2_cmp (F1.ge, F2.ge)
  let eq = lift2_cmp (F1.eq, F2.eq)
  let ne = lift2_cmp (F1.ne, F2.ne)
  let isfinite = dispatch (F1.isfinite, F2.isfinite)
  let isinf = dispatch (F1.isinf, F2.isinf)
  let isnan = dispatch (F1.isnan, F2.isnan)
  let isnormal = dispatch (F1.isnormal, F2.isnormal)
  let signbit = dispatch (F1.signbit, F2.signbit)

  let bot_of fkind = dispatch_fkind fkind (F1.bot, F2.bot)
  let bot () = failwith "bot () is not implemented for FloatIntervalImplLifted."
  let is_bot = dispatch (F1.is_bot, F2.is_bot)
  let top_of fkind = dispatch_fkind fkind (F1.top, F2.top)
  let top () = failwith "top () is not implemented for FloatIntervalImplLifted."
  let is_top = dispatch (F1.is_bot, F2.is_bot)

  let precision = dispatch ((fun _ -> FFloat), (fun _ -> FDouble))

  let leq = lift2_cmp (F1.leq, F2.leq)
  let join = lift2 (F1.join, F2.join)
  let meet = lift2 (F1.meet, F2.meet)
  let widen = lift2 (F1.widen, F2.widen)
  let narrow = lift2 (F1.narrow, F2.narrow)
  let is_exact = dispatch (F1.is_exact, F2.is_exact)

  let show = dispatch (F1.show, F2.show)  (* TODO add fkind to output *)
  let pretty = (fun () -> dispatch (F1.pretty (), F2.pretty ())) (* TODO add fkind to output *)

  let pretty_diff () (x, y) = lift2_cmp ((fun a b -> F1.pretty_diff () (a, b)), (fun a b -> F2.pretty_diff () (a, b))) x y(* TODO add fkind to output *)
  let printXml o = dispatch (F1.printXml o, F2.printXml o) (* TODO add fkind to output *)

  (* This is for debugging *)
  let name () = "FloatIntervalImplLifted(F32|F64)"
  let to_yojson = dispatch (F1.to_yojson, F2.to_yojson)
  let tag = dispatch (F1.tag, F2.tag)
  let arbitrary fk = failwith @@ "Arbitrary not implement for " ^ (name ()) ^ "."

  let of_const fkind x = dispatch_fkind fkind ((fun () -> F1.of_const x), (fun () -> F2.of_const x))
  let of_string fkind str = dispatch_fkind fkind ((fun () -> F1.of_string str), (fun () -> F2.of_string str))
  let of_int fkind i = dispatch_fkind fkind ((fun () -> F1.of_int i), (fun () -> F2.of_int i))
  let of_interval fkind i = dispatch_fkind fkind ((fun () -> F1.of_interval i), (fun () -> F2.of_interval i))
  let starting fkind s = dispatch_fkind fkind ((fun () -> F1.starting s), (fun () -> F2.starting s))
  let ending fkind e = dispatch_fkind fkind ((fun () -> F1.ending e), (fun () -> F2.ending e))
  let minimal = dispatch (F1.minimal, F2.minimal)
  let maximal = dispatch (F1.maximal, F2.maximal)
  let to_int ikind = dispatch (F1.to_int ikind, F2.to_int ikind)
  let cast_to fkind x =
    let create_interval fkind l h = 
      match l, h with 
      | Some l, Some h -> of_interval fkind (l,h)
      | Some l, None -> starting fkind l
      | None, Some h -> ending fkind h
      | _ -> top_of fkind
    in 
    match x, fkind with
    | F32 a, FDouble -> create_interval FDouble (F1.minimal a) (F1.maximal a)
    | F64 a, FFloat -> create_interval FFloat (F2.minimal a) (F2.maximal a)
    | _ -> x
end

module FloatDomTupleImpl = struct
  include Printable.Std (* for default invariant, tag, ... *)
  module F1 = FloatIntervalImplLifted
  open Batteries

  type t = F1.t option [@@deriving to_yojson, eq, ord]

  let name () = "floatdomtuple"

  type 'a m = (module FloatDomain with type t = 'a)
  (* only first-order polymorphism on functions 
     -> use records to get around monomorphism restriction on arguments (Same trick as used in intDomain) *)
  type 'b poly_in = { fi : 'a. 'a m -> 'b -> 'a }
  type 'b poly_pr = { fp : 'a. 'a m -> 'a -> 'b }
  type 'b poly2_pr = { f2p : 'a. 'a m -> 'a -> 'a -> 'b }
  type poly1 = { f1 : 'a. 'a m -> 'a -> 'a }
  type poly2 = { f2 : 'a. 'a m -> 'a -> 'a -> 'a }

  let create r x (f1 : float_precision) =
    let f b g = if b then Some (g x) else None in
    f f1 @@ r.fi (module F1)

  let create r x =
    (* use where values are introduced *)
    create r x (float_precision_from_node_or_config ())

  let opt_map2 f =
    curry @@ function Some x, Some y -> Some (f x y) | _ -> None

  let exists = Option.default false
  let for_all = Option.default true

  let mapp r = BatOption.map (r.fp (module F1))

  let map r a = BatOption.map (r.f1 (module F1)) a
  let map2 r xa ya = opt_map2 (r.f2 (module F1)) xa ya
  let map2p r xa ya = opt_map2 (r.f2p (module F1)) xa ya

  let map2int r xa ya =
    Option.map_default identity
      (IntDomain.IntDomTuple.top_of IBool) (opt_map2 (r.f2p (module F1)) xa ya)

  let map1int r xa =
    Option.map_default identity
      (IntDomain.IntDomTuple.top_of IInt) (BatOption.map (r.fp (module F1)) xa)

  let ( %% ) f g x = f % g x

  let show x =
    Option.map_default identity ""
      (mapp { fp= (fun (type a) (module F : FloatDomain with type t = a) x -> F.name () ^ ":" ^ F.show x); } x)

  let to_yojson =
    [%to_yojson: Yojson.Safe.t option]
    % mapp { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.to_yojson); }
  let hash x =
    Option.map_default identity 0
      (mapp { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.hash); } x)

  let of_const fkind =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.of_const fkind); }

  let of_interval fkind =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.of_interval fkind); }
  let ending fkind =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.ending fkind); }
  let starting fkind =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.starting fkind); }

  let of_string fkind =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.of_string fkind); }

  let top =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.top); }
  let bot =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.bot); }
  let top_of =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.top_of); }
  let bot_of =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.bot_of); }

  let is_bot =
    exists
    % mapp { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.is_bot); }
  let is_exact =
    exists
    % mapp { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.is_exact); }
  let is_top =
    for_all
    % mapp { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.is_top); }

  let precision = Option.map_default F1.precision FDouble

  let minimal x = Option.bind x F1.minimal
  let maximal x = Option.bind x F1.maximal

  let of_int fkind =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.of_int fkind); }

  let to_int ik = Option.map_default (F1.to_int ik) (IntDomain.IntDomTuple.top_of ik)

  let cast_to fkind =
    map { f1= (fun (type a) (module F : FloatDomain with type t = a) -> (fun x -> F.cast_to fkind x)); }

  let leq =
    for_all
    %% map2p { f2p= (fun (type a) (module F : FloatDomain with type t = a) -> F.leq); }

  let pretty () x =
    Option.map_default identity nil
      (mapp { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.pretty ()); } x)

  (* f1: one and only unary op *)
  let neg =
    map { f1= (fun (type a) (module F : FloatDomain with type t = a) -> F.neg); }

  (* f2: binary ops *)
  let join =
    map2 { f2= (fun (type a) (module F : FloatDomain with type t = a) -> F.join); }
  let meet =
    map2 { f2= (fun (type a) (module F : FloatDomain with type t = a) -> F.meet); }
  let widen =
    map2 { f2= (fun (type a) (module F : FloatDomain with type t = a) -> F.widen); }
  let narrow =
    map2 { f2= (fun (type a) (module F : FloatDomain with type t = a) -> F.narrow); }
  let add =
    map2 { f2= (fun (type a) (module F : FloatDomain with type t = a) -> F.add); }
  let sub =
    map2 { f2= (fun (type a) (module F : FloatDomain with type t = a) -> F.sub); }
  let mul =
    map2 { f2= (fun (type a) (module F : FloatDomain with type t = a) -> F.mul); }
  let div =
    map2 { f2= (fun (type a) (module F : FloatDomain with type t = a) -> F.div); }

  (* f2p: binary ops which return an integer *)
  let lt =
    map2int { f2p= (fun (type a) (module F : FloatDomain with type t = a) -> F.lt); }
  let gt =
    map2int { f2p= (fun (type a) (module F : FloatDomain with type t = a) -> F.gt); }
  let le =
    map2int { f2p= (fun (type a) (module F : FloatDomain with type t = a) -> F.le); }
  let ge =
    map2int { f2p= (fun (type a) (module F : FloatDomain with type t = a) -> F.ge); }
  let eq =
    map2int { f2p= (fun (type a) (module F : FloatDomain with type t = a) -> F.eq); }
  let ne =
    map2int { f2p= (fun (type a) (module F : FloatDomain with type t = a) -> F.ne); }

  (* fp: unary functions which return an integer *)
  let isfinite =
    map1int { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.isfinite); }
  let isinf =
    map1int { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.isinf); }
  let isnan =
    map1int { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.isnan); }
  let isnormal =
    map1int { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.isnormal); }
  let signbit =
    map1int { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.signbit); }

  let pretty_diff () (x, y) = dprintf "%a instead of %a" pretty x pretty y

  let printXml f x =
    BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)
end
