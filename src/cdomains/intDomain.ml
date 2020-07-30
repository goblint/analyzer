open GobConfig
open Pretty
open IntervalOps
open CircularInterval
open CircularIntOps

module GU = Goblintutil
module JB = Json
module M = Messages

module type S =
sig
  include Lattice.S
  val to_int: t -> int64 option
  val of_int: int64 -> t
  val is_int: t -> bool
  val equal_to: int64 -> t -> [`Eq | `Neq | `Top]

  val to_bool: t -> bool option
  val of_bool: bool -> t
  val is_bool: t -> bool
  val to_excl_list: t -> int64 list option
  val of_excl_list: Cil.ikind -> int64 list -> t
  val is_excl_list: t -> bool
  val of_interval: int64 * int64 -> t
  val starting   : int64 -> t
  val ending     : int64 -> t
  val maximal    : t -> int64 option
  val minimal    : t -> int64 option

  val neg: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  val rem: t -> t -> t

  val lt: t -> t -> t
  val gt: t -> t -> t
  val le: t -> t -> t
  val ge: t -> t -> t
  val eq: t -> t -> t
  val ne: t -> t -> t

  val bitnot: t -> t
  val bitand: t -> t -> t
  val bitor : t -> t -> t
  val bitxor: t -> t -> t
  val shift_left : t -> t -> t
  val shift_right: t -> t -> t

  val lognot: t -> t
  val logand: t -> t -> t
  val logor : t -> t -> t

  val cast_to : Cil.ikind -> t -> t
end

module Size = struct (* size in bits as int, range as int64 *)
  exception Not_in_int64
  open Cil open Int64 open Big_int
  let sign x = if x<0L then `Signed else `Unsigned
  let max = function
    | `Signed -> ILongLong
    | `Unsigned -> IULongLong
  let top_typ = TInt (ILongLong, [])
  let min_for x = intKindForValue (mkCilint (max (sign x)) x) (sign x = `Unsigned)
  let bit = function (* bits needed for representation *)
    | IBool -> 1
    | ik -> bytesSizeOfInt ik * 8
  let is_int64_big_int x = try let _ = int64_of_big_int x in true with _ -> false
  let card ik = (* cardinality *)
    let b = bit ik in
    shift_left_big_int unit_big_int b
  let bits ik = (* highest bits for neg/pos values *)
    let s = bit ik in
    if isSigned ik then s-1, s-1 else 0, s
  let bits_i64 ik = BatTuple.Tuple2.mapn of_int (bits ik)
  let range ik = (* min/max values as int64 (signed), anything bigger is cropped! *)
    let a,b = bits ik in
    if a>63 || b>63 then raise Not_in_int64 else
      let x = if isSigned ik then neg (shift_left 1L a) (* -2^a *) else 0L in
      let y = sub (shift_left 1L b) 1L in (* 2^b - 1 *)
      x,y
  let range_big_int ik =
    let a,b = bits ik in
    let x = if isSigned ik then minus_big_int (shift_left_big_int unit_big_int a) (* -2^a *) else zero_big_int in
    let y = sub_big_int (shift_left_big_int unit_big_int b) unit_big_int in (* 2^b - 1 *)
    x,y
  let cast t x = (* TODO: overflow is implementation-dependent! *)
    let a,b = range_big_int t in
    let c = card t in
    let x' = big_int_of_int64 x in
    (* let z = add (rem (sub x a) c) a in (* might lead to overflows itself... *)*)
    let y = mod_big_int x' c in
    let y = if gt_big_int y b then sub_big_int y c
      else if lt_big_int y a then add_big_int y c
      else y
    in
    M.tracel "cast_int" "Cast %Li to range [%s, %s] (%s) = %s (%s in int64)\n" x (string_of_big_int a) (string_of_big_int b) (string_of_big_int c) (string_of_big_int y) (if is_int64_big_int y then "fits" else "does not fit");
    try int64_of_big_int y with _ -> raise Not_in_int64
end

module Interval32 : S with type t = (int64 * int64) option = (* signed 32bit ints *)
struct
  include Printable.Std (* for default invariant, tag, ... *)

  open Int64

  type t = (int64 * int64) option [@@deriving to_yojson]

  let equal_to i = function
    | None -> failwith "unsupported: equal_to with bottom"
    | Some (a, b) -> if a = b && b = i then `Eq else if a <= i && i <= b then `Top else `Neq

  let min_int, max_int = Size.range Cil.IInt (* TODO this currently depends on the machine Cil was compiled on... *)

  let top () = Some (min_int, max_int)
  let bot () = None
  let is_top x = x=top ()
  let is_bot = function None -> true | _ -> false

  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) y = x=y
  let compare = Stdlib.compare
  let short _ = function None -> "bottom" | Some (x,y) -> "["^to_string x^","^to_string y^"]"
  let isSimple _ = true
  let name () = "32bit intervals"
  let pretty_f sh () x = text (sh 80 x)
  let pretty = pretty_f short
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML = toXML_f short
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)
  let pretty_diff () (x,y) = Pretty.dprintf "%a instead of %a" pretty x pretty y


  let norm = function None -> None | Some (x,y) ->
    if Int64.compare x y > 0 then None
    else if Int64.compare min_int x > 0 || Int64.compare max_int y < 0 then top ()
    else Some (x,y)

  let (@@) f x = f x

  let equal x y =
    match x, y with
    | None, None -> true
    | Some (x1,x2), Some (y1,y2) -> Int64.compare x1 y1 = 0 && Int64.compare x2 y2 = 0
    | _ -> false

  let leq (x:t) (y:t) =
    match x, y with
    | None, _ -> true
    | Some _, None -> false
    | Some (x1,x2), Some (y1,y2) -> Int64.compare x1 y1 >= 0 && Int64.compare x2 y2 <= 0

  let join (x:t) y =
    match x, y with
    | None, z | z, None -> z
    | Some (x1,x2), Some (y1,y2) -> norm @@ Some (min x1 y1, max x2 y2)

  let meet (x:t) y =
    match x, y with
    | None, z | z, None -> None
    | Some (x1,x2), Some (y1,y2) -> norm @@ Some (max x1 y1, min x2 y2)

  let is_int = function Some (x,y) when Int64.compare x y = 0 -> true | _ -> false
  let to_int = function Some (x,y) when Int64.compare x y = 0 -> Some x | _ -> None
  let of_interval (x,y) = norm @@ Some (x,y)
  let of_int x = of_interval (x,x)
  let zero = Some (0L, 0L)
  let one  = Some (1L, 1L)
  let top_bool = Some (0L, 1L)

  let of_bool = function true -> one | false -> zero
  let is_bool x = x <> None && not (leq zero x) || x = zero
  let to_bool = function
    | None -> None
    | Some (0L , 0L) -> Some false
    | x -> if leq zero x then None else Some true
  let to_bool_interval x = match x with
    | None | Some (0L, 0L) -> x
    | _ -> if leq zero x then top_bool else one

  let starting n = norm @@ Some (n,max_int)
  let ending   n = norm @@ Some (min_int,n)
  let maximal = function None -> None | Some (x,y) -> Some y
  let minimal = function None -> None | Some (x,y) -> Some x

  let to_excl_list _ = None
  let of_excl_list t _ = top ()
  let is_excl_list _ = false

  let cast_to t = function
    | None -> None
    | Some (x,y) ->
      try
        let a = Size.cast t x in
        let b = Size.cast t y in
        let a,b = if x<>a || y<>b then Size.range t else a,b in
        norm @@ Some (a, b)
      with Size.Not_in_int64 -> top () (* TODO top not safe b/c range too small *)

  let widen x y =
    match x, y with
    | None, z | z, None -> z
    | Some (l0,u0), Some (l1,u1) ->
      let l2 = if Int64.compare l0 l1 = 0 then l0 else min l1 min_int in
      let u2 = if Int64.compare u0 u1 = 0 then u0 else max u1 max_int in
      norm @@ Some (l2,u2)

  let narrow x y =
    match x, y with
    | _,None | None, _ -> None
    | Some (x1,x2), Some (y1,y2) ->
      let lr = if Int64.compare min_int x1 = 0 then y1 else x1 in
      let ur = if Int64.compare max_int x2 = 0 then y2 else x2 in
      norm @@ Some (lr,ur)

  let log f i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, _
    | _   , true -> bot ()
    | _ ->
      match to_bool i1, to_bool i2 with
      | Some x, Some y -> of_bool (f x y)
      | _              -> top ()

  let logor  = log (||)
  let logand = log (&&)

  let log1 f i1 =
    if is_bot i1 then
      bot ()
    else
      match to_bool i1 with
      | Some x -> of_bool (f x)
      | _      -> top ()

  let lognot = log1 not

  let bit f i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, _
    | _   , true -> bot ()
    | _ ->
      match to_int i1, to_int i2 with
      | Some x, Some y -> (try norm (of_int (f x y)) with Division_by_zero -> top ())
      | _              -> top ()

  let bitxor = bit Int64.logxor
  let bitand = bit Int64.logand
  let bitor  = bit Int64.logor

  let bit1 f i1 =
    if is_bot i1 then
      bot ()
    else
      match to_int i1 with
      | Some x -> of_int (f x)
      | _      -> top ()

  let bitnot = bit1 Int64.lognot
  let shift_right = bit (fun x y -> Int64.shift_right x (Int64.to_int y))
  let shift_left  = bit (fun x y -> Int64.shift_left  x (Int64.to_int y))

  let neg = function None -> None | Some (x,y) -> norm @@ Some (Int64.neg y, Int64.neg x)

  let add x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) -> norm @@ Some (Int64.add x1 y1, Int64.add x2 y2)

  let sub i1 i2 = add i1 (neg i2)

  let rem x y =
    let y' = sub y (of_int 1L) in
    meet (bit Int64.rem x y) (join y' (neg y'))

  let mul x y =
    match x, y with
    | None, _ | _, None -> bot ()
    | Some (x1,x2), Some (y1,y2) ->
      let x1y1 = (Int64.mul x1 y1) in let x1y2 = (Int64.mul x1 y2) in
      let x2y1 = (Int64.mul x2 y1) in let x2y2 = (Int64.mul x2 y2) in
      norm @@ Some ((min (min x1y1 x1y2) (min x2y1 x2y2)),
                    (max (max x1y1 x1y2) (max x2y1 x2y2)))

  let rec div x y =
    match x, y with
    | None, _ | _, None -> bot ()
    | Some (x1,x2), Some (y1,y2) ->
      begin match y1, y2 with
        | 0L, 0L       -> top () (* TODO warn about undefined behavior *)
        | 0L, _        -> div (Some (x1,x2)) (Some (1L,y2))
        | _      , 0L  -> div (Some (x1,x2)) (Some (y1,(-1L)))
        | _ when leq (of_int 0L) (Some (y1,y2)) -> top ()
        | _ ->
          let x1y1n = (Int64.div x1 y1) in let x1y2n = (Int64.div x1 y2) in
          let x2y1n = (Int64.div x2 y1) in let x2y2n = (Int64.div x2 y2) in
          let x1y1p = (Int64.div x1 y1) in let x1y2p = (Int64.div x1 y2) in
          let x2y1p = (Int64.div x2 y1) in let x2y2p = (Int64.div x2 y2) in
          norm @@ Some ((min (min x1y1n x1y2n) (min x2y1n x2y2n)),
                        (max (max x1y1p x1y2p) (max x2y1p x2y2p)))
      end
  let ne i1 i2 = to_bool_interval (sub i1 i2)

  let eq i1 i2 = to_bool_interval (lognot (sub i1 i2))

  let ge x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare y2 x1 <= 0 then of_bool true
      else if Int64.compare x2 y1 < 0 then of_bool false
      else top_bool

  let le x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare x2 y1 <= 0 then of_bool true
      else if Int64.compare  y2 x1 < 0 then of_bool false
      else top_bool

  let gt x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare  y2 x1 < 0 then of_bool true
      else if Int64.compare x2 y1 <= 0 then of_bool false
      else top_bool

  let lt x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare x2 y1 < 0 then of_bool true
      else if Int64.compare y2 x1 <= 0 then of_bool false
      else top_bool

  let invariant c = function
    | Some (x1, x2) when Int64.compare x1 x2 = 0 ->
      Invariant.of_string (c ^ " == " ^ Int64.to_string x1)
    | Some (x1, x2) ->
      let open Invariant in
      let i1 = if Int64.compare min_int x1 <> 0 then of_string (Int64.to_string x1 ^ " <= " ^ c) else none in
      let i2 = if Int64.compare x2 max_int <> 0 then of_string (c ^ " <= " ^ Int64.to_string x2) else none in
      i1 && i2
    | None -> None
end


exception Unknown
exception Error

module Integers = (* no top/bot, order is <= *)
struct
  include Printable.Std
  include Lattice.StdCousot
  let name () = "integers"
  type t = int64 [@@deriving to_yojson]
  let hash (x:t) = ((Int64.to_int x) - 787) * 17
  let equal (x:t) (y:t) = x=y
  let copy x = x
  let equal_to i x = if i > x then `Neq else `Top
  let top () = raise Unknown
  let is_top _ = false
  let bot () = raise Error
  let is_bot _ = false
  let isSimple _  = true
  let short _ x = if x = GU.inthack then "*" else Int64.to_string x
  let pretty_f _ _ x = text (Int64.to_string x)
  let toXML_f _ x = Xml.Element ("Leaf", [("text", Int64.to_string x)],[])
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let leq x y = x <= y
  let pretty_diff () (x,y) = Pretty.dprintf "%a instead of %a" pretty x pretty y
  let join x y = if Int64.compare x y > 0 then x else y
  let meet x y = if Int64.compare x y > 0 then y else x

  let of_bool x = if x then Int64.one else Int64.zero
  let to_bool' x = x <> Int64.zero
  let to_bool x = Some (to_bool' x)
  let is_bool _ = true
  let of_int  x = x
  let to_int  x = Some x
  let is_int  _ = true

  let to_excl_list x = None
  let of_excl_list t x = top ()
  let is_excl_list x = false
  let of_interval  x = top ()
  let starting     x = top ()
  let ending       x = top ()
  let maximal      x = None
  let minimal      x = None

  let neg  = Int64.neg
  let add  = Int64.add (* TODO: signed overflow is undefined behavior! *)
  let sub  = Int64.sub
  let mul  = Int64.mul
  let div  = Int64.div
  let rem  = Int64.rem
  let lt n1 n2 = of_bool (n1 <  n2)
  let gt n1 n2 = of_bool (n1 >  n2)
  let le n1 n2 = of_bool (n1 <= n2)
  let ge n1 n2 = of_bool (n1 >= n2)
  let eq n1 n2 = of_bool (n1 =  n2)
  let ne n1 n2 = of_bool (n1 <> n2)
  let bitnot = Int64.lognot
  let bitand = Int64.logand
  let bitor  = Int64.logor
  let bitxor = Int64.logxor
  let shift_left  n1 n2 = Int64.shift_left n1 (Int64.to_int n2)
  let shift_right n1 n2 = Int64.shift_right n1 (Int64.to_int n2)
  let lognot n1    = of_bool (not (to_bool' n1))
  let logand n1 n2 = of_bool ((to_bool' n1) && (to_bool' n2))
  let logor  n1 n2 = of_bool ((to_bool' n1) || (to_bool' n2))
  let pretty_diff () (x,y) = dprintf "%s: %a instead of %a" (name ()) pretty x pretty y
  let cast_to t x = Size.cast t x

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)
end

module FlatPureIntegers = (* Integers, but raises Unknown/Error on join/meet *)
struct
  include Integers

  let top () = raise Unknown
  let bot () = raise Error
  let leq = equal
  let pretty_diff () (x,y) = Pretty.dprintf "Integer %a instead of %a" pretty x pretty y
  let join x y = if equal x y then x else top ()
  let meet x y = if equal x y then x else bot ()
end

module Flat (Base: S) = (* identical to Lift, but goes to `Top/`Bot if Base raises Unknown/Error *)
struct
  include Lattice.Flat (Base) (struct
      let top_name = "Unknown int"
      let bot_name = "Error int"
    end)

  let name () = "flat integers"
  let cast_to t = function
    | `Lifted x -> `Lifted (Base.cast_to t x)
    | x -> x

  let equal_to i = function
    | `Bot -> failwith "unsupported: equal_to with bottom"
    | `Top -> `Top
    | `Lifted x -> Base.equal_to i x

  let of_int  x = `Lifted (Base.of_int x)
  let to_int  x = match x with
    | `Lifted x -> Base.to_int x
    | _ -> None
  let is_int  x = match x with
    | `Lifted x -> true
    | _ -> false

  let of_bool x = `Lifted (Base.of_bool x)
  let to_bool x = match x with
    | `Lifted x -> Base.to_bool x
    | _ -> None
  let is_bool = is_int

  let to_excl_list x = None
  let of_excl_list t x = top ()
  let is_excl_list x = false
  let of_interval  x = top ()
  let starting     x = top ()
  let ending       x = top ()
  let maximal      x = None
  let minimal      x = None

  let lift1 f x = match x with
    | `Lifted x ->
      (try `Lifted (f x) with Unknown -> `Top | Error -> `Bot)
    | x -> x
  let lift2 f x y = match x,y with
    | `Lifted x, `Lifted y ->
      (try `Lifted (f x y) with Unknown -> `Top | Error -> `Bot)
    | `Bot, `Bot -> `Bot
    | _ -> `Top

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
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)
end

module Lift (Base: S) = (* identical to Flat, but does not go to `Top/Bot` if Base raises Unknown/Error *)
struct
  include Lattice.LiftPO (Base) (struct
      let top_name = "MaxInt"
      let bot_name = "MinInt"
    end)

  let name () = "lifted integers"
  let cast_to t = function
    | `Lifted x -> `Lifted (Base.cast_to t x)
    | x -> x

  let equal_to i = function
    | `Bot -> failwith "unsupported: equal_to with bottom"
    | `Top -> `Top
    | `Lifted x -> Base.equal_to i x

  let of_int  x = `Lifted (Base.of_int x)
  let to_int  x = match x with
    | `Lifted x -> Base.to_int x
    | _ -> None
  let is_int  x = match x with
    | `Lifted x -> true
    | _ -> false

  let of_bool x = `Lifted (Base.of_bool x)
  let to_bool x = match x with
    | `Lifted x -> Base.to_bool x
    | _ -> None
  let is_bool = is_int

  let to_excl_list x = None
  let of_excl_list t x = top ()
  let is_excl_list x = false
  let of_interval  x = top ()
  let starting     x = top ()
  let ending       x = top ()
  let maximal      x = None
  let minimal      x = None

  let lift1 f x = match x with
    | `Lifted x -> `Lifted (f x)
    | x -> x
  let lift2 f x y = match x,y with
    | `Lifted x, `Lifted y -> `Lifted (f x y)
    | `Bot, `Bot -> `Bot
    | _ -> `Top

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
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)
end

module Flattened = Flat (Integers)
module Lifted    = Lift (Integers)

module Reverse (Base: S) =
struct
  include Base
  include (Lattice.Reverse (Base) : Lattice.S with type t := Base.t)
end

module DefExc = (* definite or set of excluded values *)
struct
  module S = SetDomain.Make (Integers)
  module R = Interval32 (* range for exclusion *)
  let size t = R.of_interval (let a,b = Size.bits_i64 t in Int64.neg a,b)
  include Printable.Std
  include Lattice.StdCousot
  type t = [
    | `Excluded of S.t * R.t
    | `Definite of Integers.t
    | `Bot
  ] [@@deriving to_yojson]
  let hash (x:t) =
    match x with
    | `Excluded (s,r) -> S.hash s + R.hash r
    | `Definite i -> 83*Integers.hash i
    | `Bot -> 61426164

  let equal_to i = function
    | `Bot -> failwith "unsupported: equal_to with bottom"
    | `Definite x -> if i = x then `Eq else `Neq
    | `Excluded (s,r) -> if S.mem i s then `Top else `Neq

  let equal x y =
    match x, y with
    | `Bot, `Bot -> true
    | `Definite x, `Definite y -> Integers.equal x y
    | `Excluded (xs,xw), `Excluded (ys,yw) -> S.equal xs ys && R.equal xw yw
    | _ -> false

  let name () = "def_exc"
  let top_of ik = `Excluded (S.empty (), size ik)
  let top_size = Size.max `Signed
  let top_range = size top_size
  let top () = top_of top_size
  let is_top x = x = top ()
  let bot () = `Bot
  let is_bot x = x = `Bot

  let bot_name = "Error int"
  let top_name = "Unknown int"

  let cast_to t = function
    | `Excluded (s,r) ->
      let r' = size t in (* target range *)
      `Excluded (if R.leq r r' then S.map (Integers.cast_to t) s, r else S.empty (), r') (* TODO can we do better here? *)
    | `Definite x -> (try `Definite (Integers.cast_to t x) with Size.Not_in_int64 -> top_of t)
    | `Bot -> `Bot

  let isSimple _ = true

  let short w x =
    let short_size x = "("^R.short 2 x^")" in
    match x with
    | `Bot -> bot_name
    | `Definite x -> Integers.short w x
    (* Print the empty exclusion as if it where a distinct top element: *)
    | `Excluded (s,l) when S.is_empty s -> top_name ^ short_size l
    (* Prepend the exclusion sets with something: *)
    | `Excluded (s,l) -> "Not " ^ S.short w s ^ short_size l

  let pretty_f sf () x = text (sf max_int x)
  let toXML_f sf x = Xml.Element ("Leaf", [("text", sf Goblintutil.summary_length x)],[])
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x

  let leq x y = match (x,y) with
    (* `Bot <= x is always true *)
    | `Bot, _ -> true
    (* Anything except bot <= bot is always false *)
    | _, `Bot -> false
    (* Two known values are leq whenever equal *)
    | `Definite x, `Definite y -> x = y
    (* A definite value is leq all exclusion sets that don't contain it *)
    | `Definite x, `Excluded (s,r) -> not (S.mem x s)
    (* No finite exclusion set can be leq than a definite value *)
    | `Excluded _, `Definite _ -> false
    (* Excluding X <= Excluding Y whenever Y <= X *)
    | `Excluded (x,xw), `Excluded (y,yw) -> S.subset y x && R.leq xw yw

  let pretty_diff () (x,y) = Pretty.dprintf "Integer %a instead of %a" pretty x pretty y

  let join x y =
    match (x,y) with
    (* The least upper bound with the bottom element: *)
    | `Bot, x -> x
    | x, `Bot -> x
    (* The case for two known values: *)
    | `Definite x, `Definite y ->
      (* If they're equal, it's just THAT value *)
      if x = y then `Definite x
      (* Unless one of them is zero, we can exclude it: *)
      else
        let a,b = Size.(min_for x, min_for y) in
        let r = R.join (size a) (size b) in
        `Excluded ((if x = 0L || y = 0L then S.empty () else S.singleton 0L), r)
    (* A known value and an exclusion set... the definite value should no
     * longer be excluded: *)
    | `Excluded (s,r), `Definite x
    | `Definite x, `Excluded (s,r) ->
      let a = size (Size.min_for x) in
      `Excluded (S.remove x s, R.join a r)
    (* For two exclusion sets, only their intersection can be excluded: *)
    | `Excluded (x,wx), `Excluded (y,wy) -> `Excluded (S.inter x y, R.join wx wy)

  let meet x y =
    match (x,y) with
    (* Greatest LOWER bound with the least element is trivial: *)
    | `Bot, _ -> `Bot
    | _, `Bot -> `Bot
    (* Definite elements are either equal or the glb is bottom *)
    | `Definite x, `Definite y -> if x = y then `Definite x else `Bot
    (* The glb of a definite element and an exclusion set is either bottom or
     * just the element itself, if it isn't in the exclusion set *)
    | `Excluded (s,r), `Definite x -> if S.mem x s then `Bot else `Definite x
    | `Definite x, `Excluded (s,r) -> if S.mem x s then `Bot else `Definite x
    (* The greatest lower bound of two exclusion sets is their union, this is
     * just DeMorgans Law *)
    | `Excluded (x,wx), `Excluded (y,wy) -> `Excluded (S.union x y, R.meet wx wy)

  let of_int  x = `Definite (Integers.of_int x)
  let to_int  x = match x with
    | `Definite x -> Integers.to_int x
    | _ -> None
  let is_int  x = match x with
    | `Definite x -> true
    | _ -> false

  let zero = of_int 0L
  let not_zero = `Excluded (S.singleton 0L, top_range)

  let of_bool x = if x then not_zero else zero
  let of_bool_cmp x = of_int (if x then 1L else 0L)
  let to_bool x =
    match x with
    | `Definite x -> Integers.to_bool x
    | `Excluded (s,r) when S.mem Int64.zero s -> Some true
    | _ -> None
  let is_bool x =
    match x with
    | `Definite x -> true
    | `Excluded (s,r) -> S.mem Int64.zero s
    | _ -> false

  let of_interval (x,y) = if Int64.compare x y == 0 then of_int x else top ()
  let starting x = if x > 0L then not_zero else top ()
  let ending x = if x < 0L then not_zero else top ()

  let max_of_range r = Option.map (fun i -> Int64.(pred @@ shift_left 1L (to_int i))) (R.maximal r)
  let min_of_range r = Option.map (fun i -> Int64.(neg @@ shift_left 1L (to_int (neg i)))) (R.minimal r)
  let maximal : t -> int64 option = function
    | `Definite x -> Integers.to_int x
    | `Excluded (s,r) -> max_of_range r
    | `Bot -> None

  let minimal = function
    | `Definite x -> Integers.to_int x
    | `Excluded (s,r) -> min_of_range r
    | `Bot -> None

  let of_excl_list t l = `Excluded (List.fold_right S.add l (S.empty ()), size t)
  let is_excl_list l = match l with `Excluded _ -> true | _ -> false
  let to_excl_list x = match x with
    | `Definite _ -> None
    | `Excluded (s,r) -> Some (S.elements s)
    | `Bot -> None

  (* Default behaviour for unary operators, simply maps the function to the
   * DefExc data structure. *)
  let lift1 f x = match x with
    | `Excluded (s,r) -> `Excluded (S.map f s, r)
    | `Definite x -> `Definite (f x)
    | `Bot -> `Bot

  let lift2 f x y = match x,y with
    (* We don't bother with exclusion sets: *)
    | `Excluded _, _ -> top ()
    | _, `Excluded _ -> top ()
    (* The good case: *)
    | `Definite x, `Definite y -> (try `Definite (f x y) with | Division_by_zero -> top ())
    (* If any one of them is bottom, we return top *)
    | _ -> top ()

  (* Default behaviour for binary operators that are injective in either
   * argument, so that Exclusion Sets can be used: *)
  let lift2_inj f x y = match x,y with
    (* If both are exclusion sets, there isn't anything we can do: *)
    | `Excluded _, `Excluded _ -> top ()
    (* A definite value should be applied to all members of the exclusion set *)
    | `Definite x, `Excluded (s,r) ->
      let min = Option.map (f x) (min_of_range r) in
      let max = Option.map (f x) (max_of_range r) in
      let r'  = match min, max with
      | Some min, Some max ->
        R.join (size (Size.min_for min)) (size (Size.min_for max))
      | _ , _ -> top_range in
      `Excluded (S.map (f x)  s, r')
    (* Same thing here, but we should flip the operator to map it properly *)
    | `Excluded (s,r), `Definite x -> let f x y = f y x in
      let min = Option.map (f x) (min_of_range r) in
      let max = Option.map (f x) (max_of_range r) in
      let r' = match min, max with
      | Some min, Some max -> R.join (size (Size.min_for min)) (size (Size.min_for max))
      | _ , _ -> top_range in
      `Excluded (S.map (f x) s, r')
    (* The good case: *)
    | `Definite x, `Definite y -> `Definite (f x y)
    (* If any one of them is bottom, we return bottom *)
    | _ -> `Bot

  (* The equality check: *)
  let eq x y = match x,y with
    (* Not much to do with two exclusion sets: *)
    | `Excluded _, `Excluded _ -> top ()
    (* Is x equal to an exclusion set, if it is a member then NO otherwise we
     * don't know: *)
    | `Definite x, `Excluded (s,r) -> if S.mem x s then of_bool false else top ()
    | `Excluded (s,r), `Definite x -> if S.mem x s then of_bool false else top ()
    (* The good case: *)
    | `Definite x, `Definite y -> of_bool_cmp (x=y)
    (* If either one of them is bottom, we return bottom *)
    | _ -> `Bot

  (* The inequality check: *)
  let ne x y = match x,y with
    (* Not much to do with two exclusion sets: *)
    | `Excluded _, `Excluded _ -> top ()
    (* Is x unequal to an exclusion set, if it is a member then Yes otherwise we
     * don't know: *)
    | `Definite x, `Excluded (s,r) -> if S.mem x s then of_bool true else top ()
    | `Excluded (s,r), `Definite x -> if S.mem x s then of_bool true else top ()
    (* The good case: *)
    | `Definite x, `Definite y -> of_bool_cmp (x<>y)
    (* If either one of them is bottom, we return bottom *)
    | _ -> `Bot

  let neg  = lift1 Integers.neg
  let add  = lift2_inj Integers.add
  let sub  = lift2_inj Integers.sub
  let mul x y = match x, y with
    | `Definite 0L, _
    | _, `Definite 0L -> `Definite 0L
    | _ -> lift2_inj Integers.mul x y
  let div  = lift2 Integers.div
  let rem  = lift2 Integers.rem
  let lt = lift2 Integers.lt
  let gt = lift2 Integers.gt
  let le = lift2 Integers.le
  let ge = lift2 Integers.ge
  let bitnot = lift1 Integers.bitnot
  let bitand = lift2 Integers.bitand
  let bitor  = lift2 Integers.bitor
  let bitxor = lift2 Integers.bitxor
  let shift_left  = lift2 Integers.shift_left
  let shift_right = lift2 Integers.shift_right
  (* TODO: lift does not treat Not {0} as true. *)
  let logand = lift2 Integers.logand
  let logor  = lift2 Integers.logor
  let lognot = eq (of_int 0L)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  let invariant c (x:t) = match x with
    | `Definite x -> Invariant.of_string (c ^ " == " ^ Int64.to_string x)
    | `Excluded (s, _) ->
      S.fold (fun x a ->
          let i = Invariant.of_string (c ^ " != " ^ Int64.to_string x) in
          Invariant.(a && i)
        ) s Invariant.none
    | `Bot -> Invariant.none
end

module OverflowInt64 = (* throws Overflow for add, sub, mul *)
struct
  exception Overflow of string

  include Int64

  let add (a:int64) (b:int64) =
    if logor (logxor a b) (logxor a (lognot (add a b))) < 0L  (* no kidding! *)
    then add a b
    else raise (Overflow (Printf.sprintf "%Ld + %Ld" a b))

  let sub (a:int64) (b:int64) =
    if b = min_int
    then
      if a >= 0L
      then raise (Overflow (Printf.sprintf "%Ld - %Ld" a b))
      else sub a b
    else
      let oppb = neg b in
      add a oppb

  let mul (a:int64) (b:int64) =
    if a = 0L then 0L
    else
      let x = mul a b in
      if b = div x a
      then x
      else raise (Overflow (Printf.sprintf "%Ld * %Ld" a b))

end

module CircInterval : S with type t = CBigInt.t interval =
struct
  include Printable.Std
  module I = CBigInt
  module C = CircularBigInt
  type t = I.t interval
  let to_yojson _ = failwith "TODO to_yojson"

  let max_width = 64
  let size t = Size.bit t

  let name () = "circular int intervals"
  let cast_to t x =
    match (I.bounds x) with
    | None -> Bot (size t)
    | Some(a,b) -> I.of_t (size t) a b

  let equal_to i x = failwith "equal_to not implemented"

  (* Int Conversion *)
  let to_int x =
    match x with
    | Int(w,a,b) when C.eq a b -> Some (C.to_int64 w a)
    | _ -> None
  let of_int x = I.of_int64 max_width x x
  let is_int x =
    match x with
    | Int(_,a,b) -> C.eq a b
    | _ -> false

  let of_interval (x,y) = I.of_int64 max_width x y

  (* Bool Conversion *)
  let to_bool x =
    match to_int x with
    | None -> None
    | Some 0L -> Some false
    | _ -> Some true
  let of_bool x =
    if x
    then Int(1, C.one, C.one)
    else Int(1, C.zero, C.zero)
  let is_bool x =
    match x with
    | Int(_,a,b) -> C.eq a b
    | _ -> false

  (* List Conversion *)
  let to_excl_list x = None
  let of_excl_list t x = Top max_width
  let is_excl_list x = false

  (* Starting/Ending *)
  let starting x =
    let r = I.of_t max_width (C.of_int64 max_width x) (C.max_value max_width)
    in
    print_endline ("starting: "^(I.to_string r)^" .. "^(Int64.to_string x));
    r
  let ending x =
    let r = I.of_t max_width C.zero (C.of_int64 max_width x)
    in
    print_endline ("ending: "^(I.to_string r)^" .. "^(Int64.to_string x));
    r
  let maximal x =
    print_endline ("maximal: "^(I.to_string x));
    match I.bounds x with
    | Some(_,m) -> Some (C.to_int64 (I.width x) m)
    | _ -> None
  let minimal x =
    print_endline ("minimal: "^(I.to_string x));
    match I.bounds x with
    | Some(m,_) -> Some (C.to_int64 (I.width x) m)
    | _ -> None

  (* Debug Helpers *)
  let wrap_debug1 n f =
    fun a ->
    let r = f a in
    if get_bool "ana.int.cdebug" then print_endline (n^": "^(I.to_string a)^" = "^(I.to_string r));
    r

  let wrap_debug2 n f =
    fun a b ->
    let r = f a b in
    if get_bool "ana.int.cdebug"
    then print_endline (n^": "^(I.to_string a)^" .. "^(I.to_string b)^" = "^(I.to_string r));
    r

  (* Arithmetic *)
  let neg = wrap_debug1 "neg" I.neg
  let add = wrap_debug2 "add" I.add
  let sub = wrap_debug2 "sub" I.sub
  let mul = wrap_debug2 "mul" I.mul
  let div = wrap_debug2 "div" I.div_s
  let rem = wrap_debug2 "rem" I.rem

  (* Comparison *)
  let comp_lt f a b =
    let w = I.width a in
    let np = I.north_pole_end w in
    if I.contains a (I.north_pole w) || I.contains b (I.north_pole w)
    then I.of_int 1 0 1
    else
      match I.bounds a, I.bounds b with
      | Some(l0,u0), Some(l1,u1) ->
        if (f w np u0 l1) then of_bool true
        else if (f w np l0 l1) then I.of_int 1 0 1
        else of_bool false
      | _ -> I.of_int 1 0 1

  let comp_gt f a b =
    let w = I.width a in
    let np = I.north_pole_end w in
    if I.contains a (I.north_pole w) || I.contains b (I.north_pole w)
    then I.of_int 1 0 1
    else
      match I.bounds a, I.bounds b with
      | Some(l0,u0), Some(l1,u1) ->
        if (f w np l0 u1) then of_bool true
        else if (f w np u0 u1) then I.of_int 1 0 1
        else of_bool false
      | _ -> I.of_int 1 0 1

  let lt = wrap_debug2 "lt" (comp_lt I.relative_lt)
  let le = wrap_debug2 "le" (comp_lt I.relative_leq)
  let gt = wrap_debug2 "gt" (comp_gt I.relative_gt)
  let ge = wrap_debug2 "ge" (comp_gt I.relative_geq)

  let eq' a b =
    match (I.meet a b) with
    | Bot _ -> of_bool false
    | _ ->
      match I.bounds a, I.bounds b with
      | Some(x,y), Some(u,v) ->
        if (C.eq x y) && (C.eq u v) && (C.eq x u)
        then of_bool true
        else I.of_int 1 0 1
      | _ -> I.of_int 1 0 1

  let ne' a b =
    match (I.meet a b) with
    | Bot _ -> of_bool true
    | _ ->
      match I.bounds a, I.bounds b with
      | Some(x,y), Some(u,v) ->
        if (C.eq x y) && (C.eq u v) && (C.eq x u)
        then of_bool false
        else I.of_int 1 0 1
      | _ -> I.of_int 1 0 1

  let eq = wrap_debug2 "eq" eq'
  let ne = wrap_debug2 "ne" ne'

  let leq a b = I.contains b a

  (* Bitwise *)
  let bitnot x =
    match x with
    | Bot _ -> x
    | Int(w,a,b) when C.eq a b ->
      let v = C.lognot w a in
      Int(w,v,v)
    | _ -> Top (I.width x)
  let bitand = wrap_debug2 "bitand" I.logand
  let bitor = wrap_debug2 "bitor" I.logor
  let bitxor = wrap_debug2 "bitxor" I.logxor
  let shift_left = wrap_debug2 "shift_left" I.shift_left
  let shift_right = wrap_debug2 "shift_right" I.shift_right

  (* Lattice *)
  let top () = Top max_width
  let bot () = Bot max_width
  let is_top x =
    match x with
    | Top _ -> true
    | _ -> false
  let is_bot x =
    match x with
    | Bot _ -> true
    | _ -> false

  (* Logical *)
  let log1 f i1 =
    if is_bot i1 then bot ()
    else
      match to_bool i1 with
      | Some x -> of_bool (f x)
      | _      -> top ()
  let log f i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, _
    | _   , true -> bot ()
    | _ ->
      match to_bool i1, to_bool i2 with
      | Some x, Some y -> of_bool (f x y)
      | _              -> top ()

  let lognot = log1 not
  let logor  = log (||)
  let logand = log (&&)

  (* Others *)
  let meet = wrap_debug2 "meet" I.meet
  let join = wrap_debug2 "join" I.join
  let equal = I.eql

  let hash x =
    match x with
    | Top w -> w
    | Bot _ -> 0
    | Int(w,a,b) -> w lxor (Hashtbl.hash b) lxor (Hashtbl.hash a)

  let isSimple x = true
  let short _ x = I.to_string x
  let pretty_f sh () x = text (sh 10 x)
  let pretty = pretty_f short
  let toXML_f sf x = Xml.Element ("Leaf", [("text", sf
                                              Goblintutil.summary_length x)],[])
  let toXML = toXML_f short
  let pretty_diff () (x,y) = dprintf "%s: %a instead of %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  (* Widen
   * Roughly double the interval size. *)
  let widen_double a b =
    let w = I.width b in
    let two = C.of_int w 2 and add = C.add w
    and sub = C.sub w and mul = C.mul w
    in
    if (I.contains a b) then b
    else if C.geq (I.count b) (I.north_pole_end w) then Top w
    else
      match I.bounds a,I.bounds b with
      | Some(u,v), Some(x,y) ->
        let j = I.join a b and uy = I.of_t w u y and xv = I.of_t w x y in
        if I.eql j uy then
          I.join uy (I.of_t w u (add (sub (mul v two) u) C.one))
        else if I.eql j xv then
          I.join xv (I.of_t w (sub (sub (mul u two) v) C.one) v)
        else if (I.contains_element b u) && (I.contains_element b v) then
          I.join b (I.of_t w x (add (sub (add x (mul v two)) (mul u two)) C.one))
        else Top w
      | _ -> Top w

  let widen_basic a b =
    if (I.eql a b) then b
    else Top (I.width b)

  let widen' a b =
    match get_string "ana.int.cwiden" with
    | "basic" -> widen_basic a b
    | "double" -> widen_double a b
    | _ -> b

  let widen = wrap_debug2 "widen" widen'

  (* Narrow
   * Take half of interval size. *)
  let narrow_half a b =
    let w = I.width b in
    let delta = C.of_int w 2 and add = C.add w
    and sub = C.sub w and div = C.div w
    in
    if I.eql a b then b
    else if C.leq (I.count b) (C.of_int w 2) then b
    else
      match I.bounds a, I.bounds b with
      | Some(u,v), Some(x,y) ->
        let m = I.meet a b and uy = I.of_t w u y and xv = I.of_t w x v in
        if I.eql m uy then
          I.meet uy (I.of_t w u (sub (add (div u delta) (div v delta)) C.one))
        else if I.eql m xv then
          I.meet xv (I.of_t w (add C.one (add (div u delta) (div v delta))) v)
        else
          I.meet b (I.of_t w (add (div u delta) (div v delta)) (add (div u delta) (div v delta)))
      | _ -> b

  let narrow_basic a b =
    if (I.eql a b) then b
    else
      match a with
      | Top _ -> b
      | _ -> a

  let narrow' a b =
    match get_string "ana.int.cnarrow" with
    | "basic" -> narrow_basic a b
    | "half"  -> narrow_half a b
    | _ -> b

  let narrow = wrap_debug2 "narrow" narrow'

end

(* BOOLEAN DOMAINS *)

module type BooleansNames =
sig
  val truename: string
  val falsename: string
end

module MakeBooleans (N: BooleansNames) =
struct
  include Printable.Std
  include Lattice.StdCousot
  type t = bool [@@deriving to_yojson]
  let hash = function true -> 51534333 | _ -> 561123444
  let equal_to i x = if x then `Top else failwith "unsupported: equal_to with bottom"
  let equal (x:t) (y:t) = x=y
  let name () = "booleans"
  let cast_to _ x = x (* ok since there's no smaller ikind to cast to *)
  let copy x = x
  let isSimple _ = true
  let short _ x = if x then N.truename else N.falsename
  let pretty_f sf _ x = Pretty.text (sf Goblintutil.summary_length x)
  let toXML_f sf x = Xml.Element ("Leaf", [("text", sf Goblintutil.summary_length x)],[])
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x

  let top () = true
  let is_top x = x
  let bot () = false
  let is_bot x = not x
  let leq x y = not x || y
  let join = (||)
  let meet = (&&)

  let of_bool x = x
  let to_bool x = Some x
  let is_bool x = not x
  let of_int x  = x = Int64.zero
  let to_int x  = if x then None else Some Int64.zero
  let is_int x  = not x

  let to_excl_list x = None
  let of_excl_list t x = top ()
  let is_excl_list x = false
  let of_interval  x = top ()
  let starting     x = top ()
  let ending       x = top ()
  let maximal      x = None
  let minimal      x = None

  let neg x = x
  let add x y = x || y
  let sub x y = x || y
  let mul x y = x && y
  let div x y = true
  let rem x y = true
  let lt n1 n2 = true
  let gt n1 n2 = true
  let le n1 n2 = true
  let ge n1 n2 = true
  let eq n1 n2 = true
  let ne n1 n2 = true
  let bitnot x = true
  let bitand x y = x && y
  let bitor  x y = x || y
  let bitxor x y = x && not y || not x && y
  let shift_left  n1 n2 = n1
  let shift_right n1 n2 = n1
  let lognot = (not)
  let logand = (&&)
  let logor  = (||)
  let pretty_diff () (x,y) = dprintf "%s: %a instead of %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)
end

module Booleans = MakeBooleans (
  struct
    let truename = "True"
    let falsename = "False"
  end)

(* Inclusion/Exclusion sets. Go to top on arithmetic operations after ana.int.enums_max values. Joins on widen, i.e. precise integers as long as not derived from arithmetic expressions. *)
module Enums : S = struct
  include Printable.Std (* for default invariant, tag, ... *)

  open Batteries
  module I = Integers
  module R = Interval32 (* range for exclusion *)
  let size t = R.of_interval (Size.bits_i64 t)
  type e = I.t (* element *)
  and t = Inc of e list | Exc of e list * R.t [@@deriving to_yojson] (* inclusion/exclusion set *)

  let name () = "enums"

  let equal_to i = function
    | Inc x ->
      if List.mem i x then
        if List.length x = 1 then `Eq
        else `Top
      else `Neq
    | Exc (x, r) ->
      if List.mem i x then `Neq
      else `Top

  let bot () = Inc []
  let top_of ik = Exc ([], size ik)
  let top () = top_of (Size.max `Signed)
  let short _ = function
    | Inc[] -> "bot" | Exc([],r) -> "top"
    | Inc xs -> "{" ^ (String.concat ", " (List.map (I.short 30) xs)) ^ "}"
    | Exc (xs,r) -> "not {" ^ (String.concat ", " (List.map (I.short 30) xs)) ^ "}"

  let of_int x = Inc [x]
  let cast_to t = function Inc xs -> (try Inc (List.map (I.cast_to t) xs |> List.sort_unique compare) with Size.Not_in_int64 -> top_of t) | Exc _ -> top_of t

  let of_interval (x,y) = (* TODO this implementation might lead to very big lists; also use ana.int.enums_max? *)
    let rec build_set set start_num end_num =
      if start_num > end_num then set
      else (build_set (set @ [start_num]) (Int64.add start_num (Int64.of_int 1)) end_num) in
    Inc (build_set [] x y)

  let rec merge_cup a b = match a,b with
    | [],x | x,[] -> x
    | x::xs, y::ys -> (match compare x y with
        | 0 -> x :: merge_cup xs ys
        | 1 -> y :: merge_cup a ys
        | _ -> x :: merge_cup xs b
      )
  let rec merge_cap a b = match a,b with
    | [],_ | _,[] -> []
    | x::xs, y::ys -> (match compare x y with
        | 0 -> x :: merge_cap xs ys
        | 1 -> merge_cap a ys
        | _ -> merge_cap xs b
      )
  let rec merge_sub a b = match a,b with
    | [],_ -> []
    | _,[] -> a
    | x::xs, y::ys -> (match compare x y with
        | 0 -> merge_sub xs ys
        | 1 -> merge_sub a ys
        | _ -> x :: merge_sub xs b
      )
  (* let merge_sub x y = Set.(diff (of_list x) (of_list y) |> to_list) *)
  let join = curry @@ function
    | Inc x, Inc y -> Inc (merge_cup x y)
    | Exc (x,r1), Exc (y,r2) -> Exc (merge_cap x y, R.join r1 r2)
    | Exc (x,r), Inc y
    | Inc y, Exc (x,r) -> Exc (merge_sub x y, if y = [] then r else R.join r (R.of_interval (List.hd y, List.last y)))
  let meet = curry @@ function
    | Inc x, Inc y -> Inc (merge_cap x y)
    | Exc (x,r1), Exc (y,r2) -> Exc (merge_cup x y, R.meet r1 r2)
    | Inc x, Exc (y,r)
    | Exc (y,r), Inc x -> Inc (merge_sub x y)
  (* let join x y = let r = join x y in print_endline @@ "join " ^ short 10 x ^ " " ^ short 10 y ^ " = " ^ short 10 r; r *)
  (* let meet x y = let r = meet x y in print_endline @@ "meet " ^ short 10 x ^ " " ^ short 10 y ^ " = " ^ short 10 r; r *)

  let widen x y = join x y
  let narrow x y = meet x y

  let leq x y = join x y = y

  let abstr_compare = curry @@ function
    | Exc _, Exc _ -> Inc[-1L; 0L ;1L]
    | Inc[],_ | _,Inc[] -> Inc[]
    | Inc x, Inc y ->
      let x_max = List.last x in
      let x_min = List.hd x in
      let y_max = List.last y in
      let y_min = List.hd y in
      if  x_max < y_min then Inc[-1L]
      else if y_max < x_min then Inc[1L]
      else if x_min = y_max then
        if  y_min = x_max then Inc[0L]
        else Inc[0L;1L]
      else if y_min = x_max then Inc[-1L;0L]
      else Inc[-1L;0L;1L]
    | Inc l, Exc (l',r) ->
      (match merge_sub l l' with
       | [] -> Inc[-1L;1L]
       | _ -> Inc[-1L;0L;1L]
      )
    | Exc (l,r), Inc l' ->
      (match merge_sub l' l with
       | [] -> Inc[-1L;1L]
       | _ -> Inc[-1L;0L;1L]
      )

  let max_elems () = get_int "ana.int.enums_max" (* maximum number of resulting elements before going to top *)
  let lift1 f = function
    | Inc[x] -> Inc[f x]
    | Inc xs when List.length xs <= max_elems () -> Inc (List.sort_unique compare @@ List.map f xs)
    | _ -> top ()
  let lift2 f = curry @@ function
    | Inc[],_| _,Inc[] -> Inc[]
    | Inc[x],Inc[y] -> Inc[f x y]
    | Inc xs,Inc ys ->
      let r = List.cartesian_product xs ys |> List.map (uncurry f) |> List.sort_unique compare in
      if List.length r <= max_elems () then Inc r else top ()
    | _,_ -> top ()
  let lift2 f a b =
    try lift2 f a b with Division_by_zero -> top ()

  let neg  = lift1 I.neg
  let add  = curry @@ function
    | Inc[0L],x | x,Inc[0L] -> x
    | x,y -> lift2 I.add x y
  let sub  = lift2 I.sub
  let mul  = curry @@ function
    | Inc[1L],x | x,Inc[1L] -> x
    | Inc[0L],_ | _,Inc[0L] -> Inc[0L]
    | x,y -> lift2 I.mul x y
  let div  = curry @@ function
    | Inc[1L],x | x,Inc[1L] -> x
    | Inc[0L],_ -> Inc[0L]
    | _,Inc[0L] -> top ()
    | x,y -> lift2 I.div x y
  let rem  = lift2 I.rem
  let lt = lift2 I.lt
  let gt = lift2 I.gt
  let le = lift2 I.le
  let ge = lift2 I.ge
  let eq = lift2 I.eq (* TODO: add more precise cases for Exc, like in DefExc? *)
  let ne = lift2 I.ne (* TODO: add more precise cases for Exc, like in DefExc? *)
  let bitnot = lift1 I.bitnot
  let bitand = lift2 I.bitand
  let bitor  = lift2 I.bitor
  let bitxor = lift2 I.bitxor
  let shift_left  = lift2 I.shift_left
  let shift_right = lift2 I.shift_right
  let lognot = lift1 I.lognot
  let logand = lift2 I.logand
  let logor  = lift2 I.logor

  let is_top x = x = top ()
  let is_bot x = x = bot ()
  let hash = Hashtbl.hash
  let equal = (=)
  let compare = compare
  let isSimple _  = true
  let pretty_list xs = text "(" ++ (try List.reduce (fun a b -> a ++ text "," ++ b) xs with _ -> nil) ++ text ")"
  let pretty_f _ _ = function
    | Inc [] -> text "bot"
    | Exc ([],r) -> text "top"
    | Inc xs -> text "Inc" ++ pretty_list (List.map (I.pretty ()) xs)
    | Exc (xs,r) -> text "Exc" ++ pretty_list (List.map (I.pretty ()) xs)
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let pretty_diff () (x,y) = Pretty.dprintf "%a instead of %a" pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  let of_bool x = Inc [if x then Int64.one else Int64.zero]
  let to_bool = function
    | Inc [] | Exc ([],_) -> None
    | Inc [0L] -> Some false
    | Inc xs when List.for_all ((<>) 0L) xs -> Some true
    | Exc (xs,_) when List.exists ((=) 0L) xs -> Some true
    | _ -> None
  let is_bool = Option.is_some % to_bool
  let of_int  x = Inc [x]
  let to_int = function Inc [x] -> Some x | _ -> None
  let is_int = Option.is_some % to_int

  let to_excl_list = function Exc (x,r) when x<>[] -> Some x | _ -> None
  let of_excl_list t x = Exc (x, size t)
  let is_excl_list = Option.is_some % to_excl_list
  let starting     x = top ()
  let ending       x = top ()
  let maximal = function Inc xs when xs<>[] -> Some (List.last xs) | _ -> None
  let minimal = function Inc (x::xs) -> Some x | _ -> None
  (* let of_incl_list xs = failwith "TODO" *)

  let invariant c = function
    | Inc ps ->
      List.fold_left (fun a x ->
          let i = Invariant.of_string (c ^ " == " ^ Int64.to_string x) in
          Invariant.(a || i)
        ) Invariant.none ps
    | Exc (ns, _) ->
      List.fold_left (fun a x ->
          let i = Invariant.of_string (c ^ " != " ^ Int64.to_string x) in
          Invariant.(a && i)
        ) Invariant.none ns
end

(* The above IntDomList has too much boilerplate since we have to edit every function in S when adding a new domain. With the following, we only have to edit the places where fn are applied, i.e., create, mapp, map, map2. *)
module IntDomTuple = struct
  include Printable.Std (* for default invariant, tag, ... *)

  open Batteries
  module I1 = DefExc
  module I2 = Interval32
  module I3 = CircInterval
  module I4 = Enums
  type t = I1.t option * I2.t option * I3.t option * I4.t option [@@deriving to_yojson]

  (* The Interval32 domain can lead to too many contexts for recursive functions (top is [min,max]), but we don't want to drop all ints as with `exp.no-int-context`. TODO better solution? *)
  let no_interval32 = Tuple4.map2 (const None)

  type 'a m = (module S with type t = 'a)
  (* only first-order polymorphism on functions -> use records to get around monomorphism restriction on arguments *)
  type 'b poly_in  = { fi  : 'a. 'a m -> 'b -> 'a } (* inject *)
  type 'b poly_pr  = { fp  : 'a. 'a m -> 'a -> 'b } (* project *)
  type 'b poly2_pr  = { f2p  : 'a. 'a m -> 'a -> 'a -> 'b }
  type poly1 = { f1 : 'a. 'a m -> 'a -> 'a } (* needed b/c above 'b must be different from 'a *)
  type poly2 = { f2 : 'a. 'a m -> 'a -> 'a -> 'a }
  let create r x = (* use where values are introduced *)
    let f n g = if get_bool ("ana.int."^n) then Some (g x) else None in
    f "def_exc" @@ r.fi (module I1), f "interval" @@ r.fi (module I2), f "cinterval" @@ r.fi (module I3), f "enums" @@ r.fi (module I4)
  let mapp r (a,b,c,d) = Option.(map (r.fp (module I1)) a, map (r.fp (module I2)) b, map (r.fp (module I3)) c, map (r.fp (module I4)) d)
  let map  r (a,b,c,d) = Option.(map (r.f1 (module I1)) a, map (r.f1 (module I2)) b, map (r.f1 (module I3)) c, map (r.f1 (module I4)) d)
  let opt_map2 f = curry @@ function | Some x, Some y -> Some (f x y) | _ -> None
  let map2  r (xa,xb,xc,xd) (ya,yb,yc,yd) = opt_map2 (r.f2  (module I1)) xa ya, opt_map2 (r.f2  (module I2)) xb yb, opt_map2 (r.f2  (module I3)) xc yc, opt_map2 (r.f2  (module I4)) xd yd
  let map2p r (xa,xb,xc,xd) (ya,yb,yc,yd) = opt_map2 (r.f2p (module I1)) xa ya, opt_map2 (r.f2p (module I2)) xb yb, opt_map2 (r.f2p (module I3)) xc yc, opt_map2 (r.f2p  (module I4)) xd yd
  let to_list x = Tuple4.enum x |> List.of_enum |> List.filter_map identity (* contains only the values of activated domains *)
  let to_list_some x = List.filter_map identity @@ to_list x (* contains only the Some-values of activated domains *)
  let exists, for_all = let f g = g identity % to_list in List.(f exists, f for_all)

  let name () = "intdomtuple"

  (* f0: constructors *)
  let top = create { fi = fun (type a) (module I:S with type t = a) -> I.top }
  let bot = create { fi = fun (type a) (module I:S with type t = a) -> I.bot }
  let of_bool = create { fi = fun (type a) (module I:S with type t = a) -> I.of_bool }
  let of_excl_list t = create { fi = fun (type a) (module I:S with type t = a) -> I.of_excl_list t }
  let of_int = create { fi = fun (type a) (module I:S with type t = a) -> I.of_int }
  let starting = create { fi = fun (type a) (module I:S with type t = a) -> I.starting }
  let ending = create { fi = fun (type a) (module I:S with type t = a) -> I.ending }
  let of_interval = create { fi = fun (type a) (module I:S with type t = a) -> I.of_interval }

  (* f1: unary ops *)
  let neg = map { f1 = fun (type a) (module I:S with type t = a) -> I.neg }
  let bitnot = map { f1 = fun (type a) (module I:S with type t = a) -> I.bitnot }
  let lognot = map { f1 = fun (type a) (module I:S with type t = a) -> I.lognot }
  let cast_to t = map { f1 = fun (type a) (module I:S with type t = a) -> I.cast_to t }

  (* fp: projections *)
  let equal_to i x =
    let xs = mapp { fp = fun (type a) (module I:S with type t = a) -> I.equal_to i } x |> Tuple4.enum |> List.of_enum |> List.filter_map identity in
    if List.mem `Eq xs then `Eq else
    if List.mem `Neq xs then `Neq else
      `Top
  let same show x = let xs = to_list_some x in let us = List.unique xs in let n = List.length us in
    if n = 1 then Some (List.hd xs)
    else (
      if n>1 then Messages.warn_all @@ "Inconsistent state! "^String.concat "," @@ List.map show us; (* do not want to abort, but we need some unsound category *)
      None
    )
  let flat f x = match to_list_some x with [] -> None | xs -> Some (f xs)
  let to_int = same Int64.to_string % mapp { fp = fun (type a) (module I:S with type t = a) -> I.to_int }
  let to_bool = same string_of_bool % mapp { fp = fun (type a) (module I:S with type t = a) -> I.to_bool }
  let to_excl_list x = mapp { fp = fun (type a) (module I:S with type t = a) -> I.to_excl_list } x |> flat List.concat
  let minimal = flat List.max % mapp { fp = fun (type a) (module I:S with type t = a) -> I.minimal }
  let maximal = flat List.min % mapp { fp = fun (type a) (module I:S with type t = a) -> I.maximal }
  (* exists/for_all *)
  let is_bot = for_all % mapp { fp = fun (type a) (module I:S with type t = a) -> I.is_bot }
  let is_top = for_all % mapp { fp = fun (type a) (module I:S with type t = a) -> I.is_top }
  let is_int = exists % mapp { fp = fun (type a) (module I:S with type t = a) -> I.is_int }
  let is_bool = exists % mapp { fp = fun (type a) (module I:S with type t = a) -> I.is_bool }
  let is_excl_list = exists % mapp { fp = fun (type a) (module I:S with type t = a) -> I.is_excl_list }
  (* others *)
  let short _ = String.concat "; " % to_list % mapp { fp = fun (type a) (module I:S with type t = a) -> I.short 30 }
  let hash = List.fold_left (lxor) 0 % to_list % mapp { fp = fun (type a) (module I:S with type t = a) -> I.hash }

  (* f2: binary ops *)
  let join = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.join }
  let meet = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.meet }
  let widen  = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.widen }
  let narrow = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.narrow }
  let add = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.add }
  let sub = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.sub }
  let mul = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.mul }
  let div = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.div }
  let rem = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.rem }
  let lt = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.lt }
  let gt = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.gt }
  let le = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.le }
  let ge = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.ge }
  let eq = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.eq }
  let ne = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.ne }
  let bitand = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.bitand }
  let bitor = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.bitor }
  let bitxor = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.bitxor }
  let shift_left = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.shift_left }
  let shift_right = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.shift_right }
  let logand = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.logand }
  let logor = map2 { f2 = fun (type a) (module I:S with type t = a) -> I.logor }

  (* f2p: binary projections *)
  let (%%) f g x = f % (g x) (* composition for binary function g *)
  let leq = for_all %% map2p { f2p = fun (type a) (module I:S with type t = a) -> I.leq }
  let equal = for_all %% map2p { f2p = fun (type a) (module I:S with type t = a) -> I.equal }
  let compare = List.fold_left (fun a x -> if x<>0 then x else a) 0 % to_list %% map2p { f2p = fun (type a) (module I:S with type t = a) -> I.compare } (* idea? same impl. as above... *)
  (* val pretty_f: (int -> t -> string) -> unit -> t -> doc *)
  let pretty_f sf () : t -> doc = (fun xs -> text "(" ++ (try List.reduce (fun a b -> a ++ text "," ++ b) xs with _ -> nil) ++ text ")") % to_list % mapp { fp = fun (type a) (module I:S with type t = a) -> (* assert sf==I.short; *) I.pretty_f I.short () } (* NOTE: the version above does something else. also, we ignore the sf-argument here. *)

  (* printing boilerplate *)
  let isSimple _ = true
  let toXML_f sf x =
    let esc = Goblintutil.escape in
    Xml.Element ("Leaf", [("text", esc (sf Goblintutil.summary_length x))], [])
  let toXML = toXML_f short
  let pretty = pretty_f short
  let pretty_diff () (x,y) = dprintf "%a instead of %a" pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  let invariant c x =
    let is = to_list (mapp { fp = fun (type a) (module I:S with type t = a) -> I.invariant c } x)
    in List.fold_left (fun a i ->
        Invariant.(a && i)
      ) Invariant.none is
end
