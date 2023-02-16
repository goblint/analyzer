open GoblintCil
open Pretty
open PrecisionUtil
open FloatOps

exception ArithmeticOnFloatBot of string

module type FloatArith = sig
  type t

  val neg : t -> t
  (** Negating an float value: [-x] *)
  val add : t -> t -> t
  (** Addition: [x + y] *)
  val sub : t -> t -> t
  (** Subtraction: [x - y] *)
  val mul : t -> t -> t
  (** Multiplication: [x * y] *)
  val div : t -> t -> t
  (** Division: [x / y] *)
  val fmax : t -> t -> t
  (** Maximum *)
  val fmin : t -> t -> t
  (** Minimum *)

  (** {unary functions} *)
  val ceil: t -> t
  val floor: t -> t
  val fabs : t -> t
  (** fabs(x) *)
  val acos : t -> t
  (** acos(x) *)
  val asin : t -> t
  (** asin(x) *)
  val atan : t -> t
  (** atan(x) *)
  val cos : t -> t
  (** cos(x) *)
  val sin : t -> t
  (** sin(x) *)
  val tan : t -> t
  (** tan(x) *)


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
  val unordered: t -> t -> IntDomain.IntDomTuple.t
  (** Unordered *)

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

  val nan: unit -> t

  val of_const : float -> t
  val of_interval : float * float -> t
  val of_string : string -> t
  val of_int: IntDomain.IntDomTuple.t -> t

  val ending : float -> t
  val starting : float -> t
  val ending_before : float -> t
  val starting_after : float -> t

  val minimal: t -> float option
  val maximal: t -> float option

  val is_exact : t -> bool
end

module FloatIntervalImpl(Float_t : CFloatType) = struct
  include Printable.Std (* for default invariant, tag and relift *)
  type t = Top | Bot | NaN | PlusInfinity | MinusInfinity | Interval of (Float_t.t * Float_t.t) [@@deriving eq, ord, to_yojson, hash]

  let show = function
    | Top -> "[Top]"
    | Bot -> "[Bot]"
    | NaN -> "[NaN]"
    | PlusInfinity -> "[+infinity]"
    | MinusInfinity -> "[-infinity]"
    | Interval (low, high) -> Printf.sprintf "[%s,%s]" (Float_t.to_string low) (Float_t.to_string high)

  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )

  let name () = "FloatInterval"

  (** If [leq x y = false], then [pretty_diff () (x, y)] should explain why. *)
  let pretty_diff () (x, y) =
    Pretty.dprintf "%a instead of %a" pretty x pretty y

  let to_int ik = function
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "to_int %s" (show Bot)))
    | Top | NaN | MinusInfinity | PlusInfinity -> IntDomain.IntDomTuple.top_of ik
    (* special treatment for booleans as those aren't "just" truncated *)
    | Interval (l, h) when ik = IBool && (l > Float_t.zero || h < Float_t.zero) -> IntDomain.IntDomTuple.of_bool IBool true
    | Interval (l, h) when ik = IBool && l = h && l = Float_t.zero -> IntDomain.IntDomTuple.of_bool IBool false
    | Interval (l, h) when ik = IBool -> IntDomain.IntDomTuple.top_of IBool
    | Interval (l, h) ->
      (* as converting from float to integer is (exactly) defined as leaving out the fractional part,
         (value is truncated toward zero) we do not require specific rounding here *)
      IntDomain.IntDomTuple.of_interval ik (Float_t.to_big_int l, Float_t.to_big_int h)

  let of_int x =
    match IntDomain.IntDomTuple.minimal x, IntDomain.IntDomTuple.maximal x with
    | Some l, Some h when l >= Float_t.to_big_int Float_t.lower_bound && h <= Float_t.to_big_int Float_t.upper_bound ->
      let l' = Float_t.of_float Down (Big_int_Z.float_of_big_int l) in
      let h' = Float_t.of_float Up (Big_int_Z.float_of_big_int h) in
      if not (Float_t.is_finite l' && Float_t.is_finite h') then
        Top
      else
        Interval (l', h')
    | _, _ -> Top

  let bot () = Bot

  let is_bot = function
    | Bot -> true
    | _ -> false

  let top () = Top

  let is_top = function
    | Top -> true
    | _ -> false

  let nan () = NaN

  let is_nan = function
    | NaN -> true
    | _ -> false

  let inf () = PlusInfinity

  let minus_inf () = MinusInfinity

  let is_inf = function
    | PlusInfinity -> true
    | _ -> false

  let is_minus_inf = function
    | MinusInfinity -> true
    | _ -> false

  let norm = function
    | Interval (low, high) as x ->
      if Float_t.is_finite low && Float_t.is_finite high then
        if low > high then failwith "invalid Interval"
        else x
      else Top
    | tb -> tb

  (**converts "(Float_t.t * Float_t.t) option" arbitraries to "Top | Bot | Interval of (Float_t.t * Float_t.t)". Does not create Bot*)
  let convert_arb v =
    match v with
    | Some (f1, f2) ->
      let f1' = Float_t.of_float Nearest f1 in
      let f2' = Float_t.of_float Nearest f2 in
      if Float_t.is_finite f1' && Float_t.is_finite f2'
      then Interval (min f1' f2', max f1' f2')
      else Top
    | _ -> Top

  (**for QCheck: should describe how to generate random values and shrink possible counter examples *)
  let arbitrary () = QCheck.map convert_arb (QCheck.option (QCheck.pair QCheck.float QCheck.float))

  let of_interval' interval =
    let x = norm @@ Interval interval
    in if is_top x then
      Messages.warn ~category:Messages.Category.Float ~tags:[CWE 189; CWE 739]
        "Float could be +/-infinity or Nan";
    x

  let of_interval (l, h) = of_interval' (Float_t.of_float Down (min l h), Float_t.of_float Up (max l h))
  let of_string s = of_interval' (Float_t.atof Down s, Float_t.atof Up s)
  let of_const f = of_interval (f, f)

  let ending e = of_interval' (Float_t.lower_bound, Float_t.of_float Up e)
  let ending_before e = of_interval' (Float_t.lower_bound, Float_t.pred @@ Float_t.of_float Up e)
  let starting s = of_interval' (Float_t.of_float Down s, Float_t.upper_bound)
  let starting_after s = of_interval' (Float_t.succ @@ Float_t.of_float Down s, Float_t.upper_bound)

  let minimal = function
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "minimal %s" (show Bot)))
    | Interval (l, _) -> Float_t.to_float l
    | _ -> None

  let maximal = function
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "maximal %s" (show Bot)))
    | Interval (_, h) -> Float_t.to_float h
    | _ -> None

  let is_exact = function
    | Interval (l, v) -> l = v
    | _ -> false

  let leq v1 v2 =
    match v1, v2 with
    | _, Top -> true
    | Top, _ -> false
    | Bot, _ -> true
    | _, Bot -> false
    | Interval (l1, h1), Interval (l2, h2) -> l1 >= l2 && h1 <= h2
    | NaN, NaN
    | MinusInfinity, MinusInfinity
    | PlusInfinity, PlusInfinity -> true
    | _ -> false

  let join v1 v2 =
    match v1, v2 with
    | Top, _ | _, Top -> Top
    | Bot, v | v, Bot -> v
    | Interval (l1, h1), Interval (l2, h2) -> Interval (min l1 l2, max h1 h2)
    | NaN, NaN -> NaN
    | MinusInfinity, MinusInfinity -> MinusInfinity
    | PlusInfinity, PlusInfinity -> PlusInfinity
    | _ -> Top

  let meet v1 v2 =
    match v1, v2 with
    | Bot, _ | _, Bot -> Bot
    | Top, v | v, Top -> v
    | Interval (l1, h1), Interval (l2, h2) ->
      let (l, h) = (max l1 l2, min h1 h2) in
      if l <= h
      then Interval (l, h)
      else Bot
    | NaN, NaN -> NaN
    | MinusInfinity, MinusInfinity -> MinusInfinity
    | PlusInfinity, PlusInfinity -> PlusInfinity
    | _ -> Bot

  (** [widen x y] assumes [leq x y]. Solvers guarantee this by calling [widen old (join old new)]. *)
  let widen v1 v2 = (**TODO: support 'threshold_widening' option *)
    match v1, v2 with
    | Top, _ | _, Top -> Top
    | Bot, v | v, Bot -> v
    | Interval (l1, h1), Interval (l2, h2) ->
      (**If we widen and we know that neither interval contains +-inf or nan, it is ok to widen only to +-max_float,
         because a widening with +-inf/nan will always result in the case above -> Top *)
      let low = if l1 <= l2 then l1 else Float_t.lower_bound in
      let high = if h1 >= h2 then h1 else Float_t.upper_bound in
      norm @@ Interval (low, high)
    | NaN, NaN -> NaN
    | MinusInfinity, MinusInfinity -> MinusInfinity
    | PlusInfinity, PlusInfinity -> PlusInfinity
    | _ -> Top

  let narrow v1 v2 =
    match v1, v2 with (**we cannot distinguish between the lower bound beeing -inf or the upper bound beeing inf. Also there is nan *)
    | Bot, _ | _, Bot -> Bot
    | Top, _ -> v2
    | Interval (l1, h1), Interval (l2, h2) ->
      let low = if l1 = Float_t.lower_bound then l2 else l1 in
      let high = if h1 = Float_t.upper_bound then h2 else h1 in
      norm @@ Interval (low, high)
    | Interval _, Top -> v1
    | NaN, NaN -> NaN
    | MinusInfinity, MinusInfinity -> MinusInfinity
    | PlusInfinity, PlusInfinity -> PlusInfinity
    | _ -> Bot

  let warn_on_special name opname op =
    let warn = Messages.warn ~category:Messages.Category.Float ~tags:[CWE 189; CWE 739] in
    if is_top op then
      warn "%s of %s could be +/-infinity or Nan" name opname
    else if op = NaN then
      warn "%s of %s is Nan" name opname
    else if op = PlusInfinity then
      warn "%s of %s is +infinity" name opname
    else if op = MinusInfinity then
      warn "%s of %s is -infinity" name opname

  let warn_on_specials_unop =
    warn_on_special "Operand" "unary expression"
  let warn_on_specials_binop op1 op2 =
    warn_on_special "First operand" "arithmetic operation" op1;
    warn_on_special "Second operand" "arithmetic operation" op2
  let warn_on_specials_comparison op1 op2 =
    warn_on_special "First operand" "comparison" op1;
    warn_on_special "Second operand" "comparison" op2

  (** evaluation of the unary and binary operations *)
  let eval_unop onTop eval_operation op =
    warn_on_specials_unop op;
    match op with
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "unop %s" (show op)))
    | Interval v -> eval_operation v
    | Top -> onTop
    | _ -> onTop (* TODO: Do better *)

  let eval_binop eval_operation v1 v2 =
    let is_exact_before = is_exact (Interval v1) && is_exact (Interval v2) in
    let result = norm @@ eval_operation v1 v2 in
    (match result with
     | Interval (r1, r2) when not (is_exact result) && is_exact_before ->
       Messages.warn
         ~category:Messages.Category.Float
         ~tags:[CWE 1339]
         "The result of this operation is not exact, even though the inputs were exact";
     | Top ->
       Messages.warn
         ~category:Messages.Category.Float
         ~tags:[CWE 1339]
         "The result of this operation could be +/-infinity or Nan";
     | _ -> ());
    result

  let eval_comparison_binop min max sym eval_operation (op1: t) op2 =
    warn_on_specials_comparison op1 op2;
    let a, b =
      match (op1, op2) with
      | Bot, _ | _, Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "%s op %s" (show op1) (show op2)))
      | Interval v1, Interval v2 -> eval_operation v1 v2
      | NaN, NaN -> (0,0)
      | NaN, _ | _, NaN -> (0,0)
      | Top, _ | _, Top -> (0,1) (*neither of the arguments is Top/Bot/NaN*)
      | v1, v2 when v1 = min -> if v2 <> min || sym then (1,1) else (0,0)
      | _, v2 when v2 = min -> (0,0) (* first argument cannot be min *)
      | v1, v2 when v1 = max -> if v2 <> max || sym then (0,0) else (0,0)
      | _, v2 when v2 = max -> (1,1) (* first argument cannot be max *)
      | _ -> (0, 1)
    in
    IntDomain.IntDomTuple.of_interval IBool
      (Big_int_Z.big_int_of_int a, Big_int_Z.big_int_of_int b)


  let eval_neg = function
    | (low, high) -> Interval (Float_t.neg high, Float_t.neg low)

  let eval_add (l1, h1) (l2, h2) =
    Interval (Float_t.add Down l1 l2, Float_t.add Up h1 h2)

  let eval_sub (l1, h1) (l2, h2) =
    Interval (Float_t.sub Down l1 h2, Float_t.sub Up h1 l2)

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
    Interval (low, high)

  let eval_div (l1, h1) (l2, h2) =
    if (l1 = Float_t.zero &&  h1 = Float_t.zero) && (l2 = Float_t.zero && h2 = Float_t.zero) then
      NaN
    else if l2 <= Float_t.zero && h2 >= Float_t.zero then
      (* even if it is exactly zero, we cannot do anything as we do not distinguish -/+ 0*)
      Top
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
      Interval (low, high)


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

  let eval_fmax (l1, h1) (l2, h2) = (max l1 l2, max h1 h2)
  let eval_fmin (l1, h1) (l2, h2) = (min l1 l2, min h2 h2)

  let neg op =
    warn_on_specials_unop op;
    match op with
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "unop %s" (show op)))
    | Top -> Top
    | Interval v -> eval_neg v
    | NaN -> NaN
    | PlusInfinity -> MinusInfinity
    | MinusInfinity -> PlusInfinity

  let add op1 op2 =
    warn_on_specials_binop op1 op2;
    match op1, op2 with
    | Bot, _ | _, Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "%s op %s" (show op1) (show op2)))
    | NaN, _ | _, NaN -> NaN
    | Top, _ | _, Top -> Top (* Bot, Top, NaN are handled*)
    | Interval v1, Interval v2 -> eval_binop eval_add v1 v2
    | PlusInfinity, PlusInfinity
    | PlusInfinity, Interval _
    | Interval _, PlusInfinity -> PlusInfinity
    | MinusInfinity, MinusInfinity
    | MinusInfinity, Interval _
    | Interval _, MinusInfinity -> MinusInfinity
    | MinusInfinity, PlusInfinity -> NaN
    | PlusInfinity, MinusInfinity -> NaN

  let sub op1 op2 =
    warn_on_specials_binop op1 op2;
    match op1, op2 with
    | Bot, _ | _, Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "%s op %s" (show op1) (show op2)))
    | NaN, _ | _, NaN -> NaN
    | Top, _ | _, Top -> Top (* Bot, Top, NaN are handled*)
    | Interval v1, Interval v2 -> eval_binop eval_sub v1 v2
    | PlusInfinity, MinusInfinity
    | PlusInfinity, Interval _
    | Interval _, MinusInfinity -> PlusInfinity
    | MinusInfinity, Interval _
    | MinusInfinity, PlusInfinity
    | Interval _, PlusInfinity -> MinusInfinity
    | PlusInfinity, PlusInfinity -> NaN
    | MinusInfinity, MinusInfinity -> NaN

  let mul op1 op2 =
    warn_on_specials_binop op1 op2;
    match op1, op2 with
    | Bot, _ | _, Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "%s op %s" (show op1) (show op2)))
    | NaN, _ | _, NaN -> NaN
    | Top, _ | _, Top -> Top (* Bot, Top, NaN are handled*)
    | Interval v1, Interval v2 -> eval_binop eval_mul v1 v2
    | PlusInfinity, PlusInfinity
    | MinusInfinity, MinusInfinity -> PlusInfinity
    | PlusInfinity, MinusInfinity
    | MinusInfinity, PlusInfinity -> MinusInfinity
    | (PlusInfinity | MinusInfinity), Interval (l,u) when l = Float_t.zero && u = Float_t.zero -> NaN
    | PlusInfinity, Interval (l, _)
    | Interval (l, _), PlusInfinity when l > Float_t.zero -> PlusInfinity
    | Interval (_,u), MinusInfinity
    | MinusInfinity, Interval (_,u) when u < Float_t.zero -> PlusInfinity
    | MinusInfinity, Interval (l, _)
    | Interval (l, _), MinusInfinity when l > Float_t.zero -> MinusInfinity
    | Interval (_,u), PlusInfinity
    | PlusInfinity, Interval (_,u) when u < Float_t.zero -> MinusInfinity
    | _ -> Top


  let div op1 op2 =
    warn_on_specials_binop op1 op2;
    match op1, op2 with
    | Bot, _ | _, Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "%s op %s" (show op1) (show op2)))
    | NaN, _ | _, NaN -> NaN
    | Top, _ | _, Top -> Top (* Bot, Top, NaN are handled*)
    | Interval v1, Interval v2 -> eval_binop eval_div v1 v2
    | (PlusInfinity | MinusInfinity), (PlusInfinity | MinusInfinity) -> NaN
    | PlusInfinity, Interval (l, u) when u < Float_t.zero -> MinusInfinity
    | MinusInfinity, Interval (l, u) when l > Float_t.zero -> MinusInfinity
    | PlusInfinity, Interval (l, u) when l > Float_t.zero -> PlusInfinity
    | MinusInfinity, Interval (l, u) when u < Float_t.zero -> PlusInfinity
    | Interval (l,u), (PlusInfinity | MinusInfinity) -> Interval (Float_t.zero, Float_t.zero)
    | _ -> Top

  let fmax op1 op2 =
    warn_on_specials_binop op1 op2;
    match op1, op2 with
    | Bot, _ | _, Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "%s op %s" (show op1) (show op2)))
    | Top, _ | _, Top -> Top
    | NaN, NaN -> NaN
    | v, NaN | NaN, v -> v (* Bot, Top, NaN are handled*)
    | PlusInfinity, _ | _, PlusInfinity -> PlusInfinity
    | v, MinusInfinity | MinusInfinity, v -> v
    | Interval v1, Interval v2 -> Interval (eval_fmax v1 v2)

  let fmin op1 op2 =
    warn_on_specials_binop op1 op2;
    match op1, op2 with
    | Bot, _ | _, Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "%s op %s" (show op1) (show op2)))
    | Top, _ | _, Top -> Top
    | NaN, NaN -> NaN
    | v, NaN | NaN, v -> v (* Bot, Top, NaN are handled*)
    | MinusInfinity, _ | _, MinusInfinity -> MinusInfinity
    | v, PlusInfinity | PlusInfinity, v -> v
    | Interval v1, Interval v2 -> Interval (eval_fmin v1 v2)

  let lt = eval_comparison_binop MinusInfinity PlusInfinity false eval_lt
  let gt = eval_comparison_binop PlusInfinity MinusInfinity false eval_gt
  let le = eval_comparison_binop MinusInfinity PlusInfinity true eval_le
  let ge = eval_comparison_binop PlusInfinity MinusInfinity true eval_ge
  let eq a b =
    Messages.warn
      ~category:Messages.Category.Float
      ~tags:[CWE 1077]
      "Equality/Inequality between `double` is dangerous!";
    warn_on_specials_comparison a b;
    let l, u =
      match (a, b) with
      | Bot, _ | _, Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "%s op %s" (show a) (show b)))
      | Interval v1, Interval v2 -> eval_eq v1 v2
      | NaN, NaN -> (0,0)
      | NaN, _ | _, NaN -> (0,0)
      | Top, _ | _, Top -> (0,1) (*neither of the arguments is Top/Bot/NaN*)
      | PlusInfinity, PlusInfinity -> (1,1)
      | MinusInfinity, MinusInfinity -> (1,1)
      | _ -> (0, 0)
    in
    IntDomain.IntDomTuple.of_interval IBool
      (Big_int_Z.big_int_of_int l, Big_int_Z.big_int_of_int u)

  let ne a b =
    Messages.warn
      ~category:Messages.Category.Float
      ~tags:[CWE 1077]
      "Equality/Inequality between `double` is dangerous!";
    warn_on_specials_comparison a b;
    let l, u =
      match (a, b) with
      | Bot, _ | _, Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "%s op %s" (show a) (show b)))
      | Interval v1, Interval v2 -> eval_ne v1 v2
      | NaN, NaN -> (1,1)
      | NaN, _ | _, NaN -> (0,0)
      | Top, _ | _, Top -> (0,1) (*neither of the arguments is Top/Bot/NaN*)
      | PlusInfinity, PlusInfinity -> (0,0)
      | MinusInfinity, MinusInfinity -> (0,0)
      | _ -> (1, 1)
    in
    IntDomain.IntDomTuple.of_interval IBool
      (Big_int_Z.big_int_of_int l, Big_int_Z.big_int_of_int u)

  let unordered op1 op2 =
    let a, b =
      match (op1, op2) with
      | Bot, _ | _, Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "%s op %s" (show op1) (show op2)))
      | NaN, _ | _, NaN -> (1,1)
      | Top, _ | _, Top -> (0,1) (*neither of the arguments is Top/Bot/NaN*)
      | _ -> (0, 0)
    in
    IntDomain.IntDomTuple.of_interval IBool (Big_int_Z.big_int_of_int a, Big_int_Z.big_int_of_int b)

  let true_nonZero_IInt () = IntDomain.IntDomTuple.of_excl_list IInt [(Big_int_Z.big_int_of_int 0)]
  let false_zero_IInt () = IntDomain.IntDomTuple.of_int IInt (Big_int_Z.big_int_of_int 0)
  let unknown_IInt () = IntDomain.IntDomTuple.top_of IInt

  let eval_isnormal = function
    | (l, h) ->
      if l >= Float_t.smallest || h <= (Float_t.neg (Float_t.smallest)) then
        true_nonZero_IInt ()
      else if l > (Float_t.neg (Float_t.smallest)) && h < Float_t.smallest then
        false_zero_IInt ()
      else
        unknown_IInt ()

  (**it seems strange not to return an explicit 1 for negative numbers, but in c99 signbit is defined as: *)
  (**<<The signbit macro returns a nonzero value if and only if the sign of its argument value is negative.>> *)
  let eval_signbit = function
    | (_, h) when h < Float_t.zero -> true_nonZero_IInt ()
    | (l, _) when l > Float_t.zero -> false_zero_IInt ()
    | _ -> unknown_IInt () (**any interval containing zero has to fall in this case, because we do not distinguish between 0. and -0. *)

  (**This Constant overapproximates pi to use as bounds for the return values of trigonometric functions *)
  let overapprox_pi = 3.1416

  let eval_fabs = function
    | (l, h) when l > Float_t.zero -> Interval (l, h)
    | (l, h) when h < Float_t.zero -> neg (Interval (l, h))
    | (l, h) -> Interval (Float_t.zero, max (Float_t.fabs l) (Float_t.fabs h))

  let eval_floor (l,h) =
    let lf, hf = Float_t.floor l, Float_t.floor h in
    if Float_t.fabs (Float_t.sub Nearest l lf) < (Float_t.of_float Nearest 1.0) && (Float_t.sub Nearest h hf) < (Float_t.of_float Nearest 1.0) then
      (* if the difference is less than 1, there is no nearer int and even a more precise type would have the same values *)
      Interval (lf,hf)
    else
      Top

  let eval_ceil (l,h) =
    let lc, hc = Float_t.ceil l, Float_t.ceil h in
    if Float_t.fabs (Float_t.sub Nearest l lc) < (Float_t.of_float Nearest 1.0) && (Float_t.sub Nearest h hc) < (Float_t.of_float Nearest 1.0) then
      (* if the difference is less than 1, there is no nearer int and even a more precise type would have the same values *)
      Interval (lc,hc)
    else
      Top

  let eval_acos = function
    | (l, h) when l = h && l = Float_t.of_float Nearest 1. -> of_const 0. (*acos(1) = 0*)
    | (l, h) ->
      if l < (Float_t.of_float Down (-.1.)) || h > (Float_t.of_float Up 1.) then
        Messages.warn ~category:Messages.Category.Float "Domain error might occur: acos argument might be outside of [-1., 1.]";
      of_interval (0., (overapprox_pi)) (**could be more exact *)

  let eval_asin = function
    | (l, h) when l = h && l = Float_t.zero -> of_const 0. (*asin(0) = 0*)
    | (l, h) ->
      if l < (Float_t.of_float Down (-.1.)) || h > (Float_t.of_float Up 1.) then
        Messages.warn ~category:Messages.Category.Float "Domain error might occur: asin argument might be outside of [-1., 1.]";
      div (of_interval ((-. overapprox_pi), overapprox_pi)) (of_const 2.) (**could be more exact *)

  let eval_atan = function
    | (l, h) when l = h && l = Float_t.zero -> of_const 0. (*atan(0) = 0*)
    | _ -> div (of_interval ((-. overapprox_pi), overapprox_pi)) (of_const 2.) (**could be more exact *)

  let eval_cos = function
    | (l, h) when l = h && l = Float_t.zero -> of_const 1. (*cos(0) = 1*)
    | _ -> of_interval (-. 1., 1.) (**could be exact for intervals where l=h, or even for Interval intervals *)

  let eval_sin = function
    | (l, h) when l = h && l = Float_t.zero -> of_const 0. (*sin(0) = 0*)
    | _ -> of_interval (-. 1., 1.) (**could be exact for intervals where l=h, or even for some intervals *)

  let eval_tan = function
    | (l, h) when l = h && l = Float_t.zero -> of_const 0. (*tan(0) = 0*)
    | _ -> top () (**could be exact for intervals where l=h, or even for some intervals *)

  let isfinite op =
    match op with
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "unop %s" (show op)))
    | Top -> unknown_IInt ()
    | Interval _  -> true_nonZero_IInt ()
    | NaN | PlusInfinity | MinusInfinity -> false_zero_IInt ()

  let isinf op =
    match op with
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "unop %s" (show op)))
    | Top -> unknown_IInt ()
    | PlusInfinity | MinusInfinity -> true_nonZero_IInt ()
    | Interval _ | NaN -> false_zero_IInt ()

  let isnan op =
    match op with
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "unop %s" (show op)))
    | Top -> unknown_IInt ()
    | Interval _ | PlusInfinity | MinusInfinity -> false_zero_IInt ()
    | NaN -> true_nonZero_IInt ()

  let isnormal op =
    match op with
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "unop %s" (show op)))
    | Top -> unknown_IInt ()
    | Interval i -> eval_isnormal i
    | PlusInfinity | MinusInfinity | NaN -> false_zero_IInt ()

  let signbit op =
    match op with
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "unop %s" (show op)))
    | Top | NaN -> unknown_IInt ()
    | Interval i -> eval_signbit i
    | PlusInfinity -> false_zero_IInt ()
    | MinusInfinity  -> true_nonZero_IInt ()

  let fabs op =
    warn_on_specials_unop op;
    match op with
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "unop %s" (show op)))
    | Top -> Top
    | Interval v -> eval_fabs v
    | NaN -> NaN
    | PlusInfinity -> PlusInfinity
    | MinusInfinity -> PlusInfinity

  let floor op =
    warn_on_specials_unop op;
    match op with
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "unop %s" (show op)))
    | Top -> Top
    | Interval v -> eval_floor v
    | NaN -> NaN
    | PlusInfinity -> PlusInfinity
    | MinusInfinity -> MinusInfinity

  let ceil op =
    warn_on_specials_unop op;
    match op with
    | Bot -> raise (ArithmeticOnFloatBot (Printf.sprintf "unop %s" (show op)))
    | Top -> Top
    | Interval v -> eval_ceil v
    | NaN -> NaN
    | PlusInfinity -> PlusInfinity
    | MinusInfinity -> MinusInfinity

  let acos = eval_unop (top ()) eval_acos
  let asin = eval_unop (top ()) eval_asin
  let atan = eval_unop (top ()) eval_atan
  let cos = eval_unop (top ()) eval_cos
  let sin = eval_unop (top ()) eval_sin
  let tan = eval_unop (top ()) eval_tan

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

  val nan_of: Cil.fkind -> t
  val inf_of: Cil.fkind -> t
  val minus_inf_of: Cil.fkind -> t

  val ending : Cil.fkind -> float -> t
  val starting : Cil.fkind -> float -> t
  val ending_before : Cil.fkind -> float -> t
  val starting_after : Cil.fkind -> float -> t

  val minimal: t -> float option
  val maximal: t -> float option

  val is_exact : t -> bool
  val get_fkind : t -> Cil.fkind
  val invariant: Cil.exp -> t -> Invariant.t
end

module FloatIntervalImplLifted = struct
  include Printable.Std (* for default invariant, tag and relift *)

  module F1 = F32Interval
  module F2 = F64Interval

  (* As described in [Relational Abstract Domains for the Detection of Floating-Point Run-Time Errors](https://www-apr.lip6.fr/~mine/publi/article-mine-esop04.pdf)
     the over-approximating interval analysis can do the abstract computations with less precision than the concrete ones. We make use of this in order to also
     provide a domain for long doubles although in the abstract we "only" use double precision
  *)
  type t =
    | F32 of F1.t
    | F64 of F2.t
    | FLong of F2.t [@@deriving to_yojson, eq, ord, hash]

  let show = function
    | F32 a -> "float: " ^ F1.show a
    | F64 a -> "double: " ^ F2.show a
    | FLong a -> "long double: " ^ F2.show a

  let lift2 (op32, op64) x y = match x, y with
    | F32 a, F32 b -> F32 (op32 a b)
    | F64 a, F64 b -> F64 (op64 a b)
    | FLong a, FLong b -> FLong (op64 a b)
    | _ -> failwith ("fkinds do not match. Values: " ^ show x ^ " and " ^ show y)

  let lift2_cmp (op32, op64) x y = match x, y with
    | F32 a, F32 b -> op32 a b
    | F64 a, F64 b -> op64 a b
    | FLong a, FLong b -> op64 a b
    | _ -> failwith ("fkinds do not match. Values: " ^ show x ^ " and " ^ show y)

  let lift (op32, op64) = function
    | F32 a -> F32 (op32 a)
    | F64 a -> F64 (op64 a)
    | FLong a -> FLong (op64 a)

  let dispatch (op32, op64) = function
    | F32 a -> op32 a
    | F64 a | FLong a -> op64 a

  let dispatch_fkind fkind (op32, op64) = match fkind with
    | FFloat -> F32 (op32 ())
    | FDouble -> F64 (op64 ())
    | FLongDouble -> FLong (op64 ())
    | _ ->
      (* this should never be reached, as we have to check for invalid fkind elsewhere,
         however we could instead of crashing also return top_of some fkind to avoid this and nonetheless have no actual information about anything*)
      failwith "unsupported fkind"

  let neg = lift (F1.neg, F2.neg)
  let fabs = lift (F1.fabs, F2.fabs)
  let floor = lift (F1.floor, F2.floor)
  let ceil = lift (F1.ceil, F2.ceil)
  let acos = lift (F1.acos, F2.acos)
  let asin = lift (F1.asin, F2.asin)
  let atan = lift (F1.atan, F2.atan)
  let cos = lift (F1.cos, F2.cos)
  let sin = lift (F1.sin, F2.sin)
  let tan = lift (F1.tan, F2.tan)
  let add = lift2 (F1.add, F2.add)
  let sub = lift2 (F1.sub, F2.sub)
  let mul = lift2 (F1.mul, F2.mul)
  let div = lift2 (F1.div, F2.div)
  let fmax = lift2 (F1.fmax, F2.fmax)
  let fmin = lift2 (F1.fmin, F2.fmin)
  let lt = lift2_cmp (F1.lt, F2.lt)
  let gt = lift2_cmp (F1.gt, F2.gt)
  let le = lift2_cmp (F1.le, F2.le)
  let ge = lift2_cmp (F1.ge, F2.ge)
  let eq = lift2_cmp (F1.eq, F2.eq)
  let ne = lift2_cmp (F1.ne, F2.ne)
  let unordered = lift2_cmp (F1.unordered, F2.unordered)
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

  let nan_of fkind = dispatch_fkind fkind (F1.nan, F2.nan)
  let is_nan = dispatch (F1.is_nan, F2.is_nan)

  let inf_of fkind = dispatch_fkind fkind (F1.inf, F2.inf)
  let minus_inf_of fkind = dispatch_fkind fkind (F1.minus_inf, F2.minus_inf)
  let is_inf = dispatch (F1.is_inf, F2.is_inf)
  let is_neg_inf = dispatch (F1.is_minus_inf, F2.is_minus_inf)

  let get_fkind = function
    | F32 _ -> FFloat
    | F64 _ -> FDouble
    | FLong _ -> FLongDouble

  let leq = lift2_cmp (F1.leq, F2.leq)
  let join = lift2 (F1.join, F2.join)
  let meet = lift2 (F1.meet, F2.meet)
  let widen = lift2 (F1.widen, F2.widen)
  let narrow = lift2 (F1.narrow, F2.narrow)
  let is_exact = dispatch (F1.is_exact, F2.is_exact)

  let pretty = (fun () -> dispatch (F1.pretty (), F2.pretty ())) (* TODO add fkind to output *)

  let pretty_diff () (x, y) = lift2_cmp ((fun a b -> F1.pretty_diff () (a, b)), (fun a b -> F2.pretty_diff () (a, b))) x y(* TODO add fkind to output *)
  let printXml o = dispatch (F1.printXml o, F2.printXml o) (* TODO add fkind to output *)

  (* This is for debugging *)
  let name () = "FloatIntervalImplLifted"
  let to_yojson = dispatch (F1.to_yojson, F2.to_yojson)
  let tag = dispatch (F1.tag, F2.tag)
  let arbitrary fk = failwith @@ "Arbitrary not implement for " ^ (name ()) ^ "."

  let of_const fkind x = dispatch_fkind fkind ((fun () -> F1.of_const x), (fun () -> F2.of_const x))
  let of_string fkind str = dispatch_fkind fkind ((fun () -> F1.of_string str), (fun () -> F2.of_string str))
  let of_int fkind i = dispatch_fkind fkind ((fun () -> F1.of_int i), (fun () -> F2.of_int i))
  let of_interval fkind i = dispatch_fkind fkind ((fun () -> F1.of_interval i), (fun () -> F2.of_interval i))
  let starting fkind s = dispatch_fkind fkind ((fun () -> F1.starting s), (fun () -> F2.starting s))
  let starting_after fkind s = dispatch_fkind fkind ((fun () -> F1.starting_after s), (fun () -> F2.starting_after s))
  let ending fkind e = dispatch_fkind fkind ((fun () -> F1.ending e), (fun () -> F2.ending e))
  let ending_before fkind e = dispatch_fkind fkind ((fun () -> F1.ending_before e), (fun () -> F2.ending_before e))
  let minimal = dispatch (F1.minimal, F2.minimal)
  let maximal = dispatch (F1.maximal, F2.maximal)
  let to_int ikind = dispatch (F1.to_int ikind, F2.to_int ikind)
  let cast_to fkind v =
    if is_nan v then
      nan_of fkind
    else if is_inf v then
      inf_of fkind
    else if is_neg_inf v then
      minus_inf_of fkind
    else
      let create_interval fkind l h =
        match l, h with
        | Some l, Some h -> of_interval fkind (l,h)
        | Some l, None -> starting fkind l
        | None, Some h -> ending fkind h
        | _ -> top_of fkind
      in
      dispatch ((fun a -> create_interval fkind (F1.minimal a) (F1.maximal a)), (fun a -> create_interval fkind (F2.minimal a) (F2.maximal a))) v

  let invariant e (x:t) =
    let fk = get_fkind x in
    match minimal x, maximal x with
    | Some x1, Some x2 when x1 = x2 ->
      Invariant.of_exp Cil.(BinOp (Eq, e, Const (CReal (x1, fk, None)), intType))
    | Some x1, Some x2 ->
      let i1 = Invariant.of_exp Cil.(BinOp (Le, Const (CReal (x1, fk, None)), e, intType)) in
      let i2 = Invariant.of_exp Cil.(BinOp (Le, e, Const (CReal (x2, fk, None)), intType)) in
      Invariant.(&&) i1 i2
    | _ -> Invariant.none
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
  let ending_before fkind =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.ending_before fkind); }
  let starting_after fkind =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.starting_after fkind); }

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

  let nan_of =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.nan_of); }

  let inf_of =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.inf_of); }

  let minus_inf_of =
    create { fi= (fun (type a) (module F : FloatDomain with type t = a) -> F.minus_inf_of); }

  let is_bot =
    exists
    % mapp { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.is_bot); }
  let is_exact =
    exists
    % mapp { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.is_exact); }
  let is_top =
    for_all
    % mapp { fp= (fun (type a) (module F : FloatDomain with type t = a) -> F.is_top); }

  let get_fkind = Option.map_default F1.get_fkind FDouble

  let minimal x = Option.bind x F1.minimal
  let maximal x = Option.bind x F1.maximal
  let invariant e x = Option.map_default (F1.invariant e) (Invariant.none) x

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
  (* f1: unary functions *)
  let floor =
    map { f1= (fun (type a) (module F : FloatDomain with type t = a) -> F.floor); }
  let ceil =
    map { f1= (fun (type a) (module F : FloatDomain with type t = a) -> F.ceil); }
  let fabs =
    map { f1= (fun (type a) (module F : FloatDomain with type t = a) -> F.fabs); }
  let acos =
    map { f1= (fun (type a) (module F : FloatDomain with type t = a) -> F.acos); }
  let asin =
    map { f1= (fun (type a) (module F : FloatDomain with type t = a) -> F.asin); }
  let atan =
    map { f1= (fun (type a) (module F : FloatDomain with type t = a) -> F.atan); }
  let cos =
    map { f1= (fun (type a) (module F : FloatDomain with type t = a) -> F.cos); }
  let sin =
    map { f1= (fun (type a) (module F : FloatDomain with type t = a) -> F.sin); }
  let tan =
    map { f1= (fun (type a) (module F : FloatDomain with type t = a) -> F.tan); }

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
  let fmax =
    map2 { f2= (fun (type a) (module F : FloatDomain with type t = a) -> F.fmax); }
  let fmin =
    map2 { f2= (fun (type a) (module F : FloatDomain with type t = a) -> F.fmin); }

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
  let unordered =
    map2int { f2p= (fun (type a) (module F : FloatDomain with type t = a) -> F.unordered); }

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

  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end
