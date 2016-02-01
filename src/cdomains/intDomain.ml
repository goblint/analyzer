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

  val to_bool: t -> bool option
  val of_bool: bool -> t
  val is_bool: t -> bool
  val to_excl_list: t -> int64 list option
  val of_excl_list: int64 list -> t
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

  val cast_to_width : int -> t -> t
end

module Interval32 : S with type t = (int64 * int64) option =
struct
  open Int64

  type t = (int64 * int64) option

  let max_int_f b = Int64.sub (Int64.shift_left Int64.one (b-1)) Int64.one
  let min_int_f b = Int64.neg (Int64.shift_left Int64.one (b-1))

  let max_int = max_int_f 32
  let min_int = min_int_f 32

  let top () = Some (min_int, max_int)
  let bot () = None
  let is_top x = x=top ()
  let is_bot = function None -> true | _ -> false

  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) y = x=y
  let compare (x:t) y = Pervasives.compare x y
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
  let of_int x = norm @@ Some (x,x)
  let to_int = function Some (x,y) when Int64.compare x y = 0 -> Some x | _ -> None

  let of_interval (x,y) = norm @@ Some (x,y)

  let of_bool = function true -> Some (Int64.one,Int64.one) | false -> Some (Int64.zero,Int64.zero)
  let is_bool = function None -> false | Some (x,y) ->
    if Int64.compare x Int64.zero = 0 && Int64.compare y Int64.zero = 0 then true
    else not (leq (of_int Int64.zero) (Some (x,y)))
  let to_bool = function None -> None | Some (x,y) ->
    if Int64.compare x Int64.zero = 0 && Int64.compare y Int64.zero = 0 then Some false
    else if leq (of_int Int64.zero) (Some (x,y)) then None else Some true

  let starting n = norm @@ Some (n,max_int)
  let ending   n = norm @@ Some (min_int,n)
  let maximal = function None -> None | Some (x,y) -> Some y
  let minimal = function None -> None | Some (x,y) -> Some x

  let to_excl_list _ = None
  let of_excl_list _ = top ()
  let is_excl_list _ = false

  let cast_to_width b = function
    | None -> None
    | Some (x,y) -> norm @@ Some (max x (min_int_f b),min y (max_int_f b))

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
      | Some x, Some y -> (try of_int (f x y) with Division_by_zero -> top ())
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
  let rem  = bit Int64.rem

  let neg = function None -> None | Some (x,y) -> norm @@ Some (Int64.neg y, Int64.neg x)
  let add x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) -> norm @@ Some (Int64.add x1 y1, Int64.add x2 y2)

  let sub i1 i2 = add i1 (neg i2)

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
        | 0L, 0L       -> bot ()
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
  let ne i1 i2 = sub i1 i2

  let eq i1 i2 =
    match to_bool (sub i1 i2) with
    | Some x -> of_bool (not x)
    | None -> None

  let ge x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare y2 x1 <= 0 then of_bool true
      else if Int64.compare x2 y1 < 0 then of_bool false
      else top ()

  let le x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare x2 y1 <= 0 then of_bool true
      else if Int64.compare  y2 x1 < 0 then of_bool false
      else top ()

  let gt x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare  y2 x1 < 0 then of_bool true
      else if Int64.compare x2 y1 <= 0 then of_bool false
      else top ()

  let lt x y =
    match x, y with
    | None, _ | _, None -> None
    | Some (x1,x2), Some (y1,y2) ->
      if Int64.compare x2 y1 < 0 then of_bool true
      else if Int64.compare y2 x1 <= 0 then of_bool false
      else top ()
end


exception Unknown
exception Error

module Integers : S with type t = int64  = (* no top/bot, order is <= *)
struct
  include Printable.Std
  include Lattice.StdCousot
  let name () = "integers"
  type t = int64
  let hash (x:t) = ((Int64.to_int x) - 787) * 17
  let equal (x:t) (y:t) = x=y
  let copy x = x
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
  let of_excl_list x = top ()
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
  let div x y = (* TODO: exception is not very helpful here?! *)
    match y with
    | 0L -> raise Division_by_zero  (* -- this is for a bug (#253) where div throws *)
    | _  -> Int64.div x y           (*    sigfpe and ocaml has somehow forgotten how to deal with it*)
  let rem x y =
    match y with
    | 0L -> raise Division_by_zero  (* ditto *)
    | _  -> Int64.rem x y
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
  let cast_to_width w x =
    let y = BatInt64.pow 2L (Int64.of_int w) in
    if y=0L then
      x
    else
      Int64.rem x (BatInt64.pow 2L (Int64.of_int w)) (* TODO: this is implementation-dependent! *)

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
  let cast_to_width _ x = x

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
  let of_excl_list x = top ()
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

module Lift (Base: S) = (* identical to Flat, but does not go to `Top/`Bot if Base raises Unknown/Error *)
struct
  include Lattice.Lift (Base) (struct
      let top_name = "MaxInt"
      let bot_name = "MinInt"
    end)

  let name () = "lifted integers"
  let cast_to_width _ x = x

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
  let of_excl_list x = top ()
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

module Reverse (Base: S) = (* TODO: (almost) copy of Lattice.Reverse... *)
struct
  include Base
  let bot = Base.top
  let is_bot = Base.is_top
  let top = Base.bot
  let is_top = Base.is_bot
  let leq x y = Base.leq y x
  let join x y = Base.meet x y
  let meet x y = Base.join x y
  let name () = "Reversed (" ^ name () ^ ")"
  let pretty_diff () (x,y) =
    Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module Trier =
struct
  module S = SetDomain.Make (Integers)
  include Printable.Std
  include Lattice.StdCousot
  type t = [
    | `Excluded of S.t
    | `Definite of Integers.t
    | `Bot
  ]

  let cast_to_width w = function
    | `Excluded s -> `Excluded (S.empty ()) (* TODO can we do better here? *)
    | `Definite x -> `Definite (Integers.cast_to_width w x)
    | `Bot -> `Bot
  let hash (x:t) =
    match x with
    | `Excluded s -> S.hash s
    | `Definite i -> 83*Integers.hash i
    | `Bot -> 61426164

  let equal x y =
    match x, y with
    | `Bot, `Bot -> true
    | `Definite x, `Definite y -> Integers.equal x y
    | `Excluded xs, `Excluded ys -> S.equal xs ys
    | _ -> false

  let name () = "trier"
  let top () = `Excluded (S.empty ())
  let is_top x =
    match x with
    | `Excluded s -> S.is_empty s
    | _ -> false
  let bot () = `Bot
  let is_bot x = x = `Bot

  let bot_name = "Error int"
  let top_name = "Unknown int"

  let isSimple _ = true

  let short w x =
    match x with
    | `Bot -> bot_name
    | `Definite x -> Integers.short w x
    (* Print the empty exclusion as if it where a distinct top element: *)
    | `Excluded s when S.is_empty s -> top_name
    (* Prepend the exclusion sets with something: *)
    | `Excluded s -> "Not " ^ S.short w s

  let pretty_f sf () x = text (sf max_int x)
  let toXML_f sf x = Xml.Element ("Leaf", [("text", sf Goblintutil.summary_length x)],[])
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x

  let leq x y = match (x,y) with
    (* `Bot <= x is always true *)
    | `Bot, _ -> true
    (* Anything except bot <= bot is always false *)
    | _, `Bot -> false
    (* Two known values are leq whenver equal *)
    | `Definite x, `Definite y -> x = y
    (* A definite value is leq all exclusion sets that don't contain it *)
    | `Definite x, `Excluded s -> not (S.mem x s)
    (* No finite exclusion set can be leq than a definite value *)
    | `Excluded _, `Definite _ -> false
    (* Excluding X <= Excluding Y whenever Y <= X *)
    | `Excluded x, `Excluded y -> S.subset y x

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
      else if x = Int64.zero || y = Int64.zero then top ()
      else `Excluded (S.singleton Int64.zero)
    (* A known value and an exclusion set... the definite value should no
     * longer be excluded: *)
    | `Excluded s, `Definite x -> `Excluded (S.remove x s)
    | `Definite x, `Excluded s -> `Excluded (S.remove x s)
    (* For two exclusion sets, only their intersection can be excluded: *)
    | `Excluded x, `Excluded y -> `Excluded (S.inter x y)

  let meet x y =
    match (x,y) with
    (* Gretest LOWER bound with the least element is trivial: *)
    | `Bot, _ -> `Bot
    | _, `Bot -> `Bot
    (* Definite elements are either equal or the glb is bottom *)
    | `Definite x, `Definite y -> if x = y then `Definite x else `Bot
    (* The glb of a definite element and an exclusion set is either bottom or
     * just the element itself, if it isn't in the exclusion set *)
    | `Excluded s, `Definite x -> if S.mem x s then `Bot else `Definite x
    | `Definite x, `Excluded s -> if S.mem x s then `Bot else `Definite x
    (* The greatest lower bound of two exclusion sets is their union, this is
     * just DeMorgans Law *)
    | `Excluded x, `Excluded y -> `Excluded (S.union x y)

  let of_bool x = `Definite (Integers.of_bool x)
  let to_bool x =
    match x with
    | `Definite x -> Integers.to_bool x
    | `Excluded s when S.mem Int64.zero s -> Some true
    | _ -> None
  let is_bool x =
    match x with
    | `Definite x -> true
    | `Excluded s -> S.mem Int64.zero s
    | _ -> false

  let of_int  x = `Definite (Integers.of_int x)
  let to_int  x = match x with
    | `Definite x -> Integers.to_int x
    | _ -> None
  let is_int  x = match x with
    | `Definite x -> true
    | _ -> false

  let of_interval (x,y) = if Int64.compare x y == 0 then of_int x else top ()
  let ending   x = top ()
  let starting x = top ()
  let maximal _ = None
  let minimal _ = None

  let of_excl_list l = `Excluded (List.fold_right S.add l (S.empty ()))
  let is_excl_list l = match l with `Excluded _ -> true | _ -> false
  let to_excl_list x = match x with
    | `Definite _ -> None
    | `Excluded s -> Some (S.elements s)
    | `Bot -> None

  (* Default behaviour for unary operators, simply maps the function to the
   * Trier data structure. *)
  let lift1 f x = match x with
    | `Excluded s -> `Excluded (S.map f s)
    | `Definite x -> `Definite (f x)
    | `Bot -> `Bot

  let lift2 f x y = match x,y with
    (* We don't bother with exclusion sets: *)
    | `Excluded _, _ -> top ()
    | _, `Excluded _ -> top ()
    (* The good case: *)
    | `Definite x, `Definite y -> (try `Definite (f x y) with | Division_by_zero -> `Bot)
    (* If any one of them is bottom, we return bottom *)
    | _ -> `Bot

  (* Default behaviour for binary operators that are injective in either
   * argument, so that Exclusion Sets can be used: *)
  let lift2_inj f x y = match x,y with
    (* If both are exclusion sets, there isn't anything we can do: *)
    | `Excluded _, `Excluded _ -> top ()
    (* A definite value should be applied to all members of the exclusion set *)
    | `Definite x, `Excluded s -> `Excluded (S.map (f x)  s)
    (* Same thing here, but we should flip the operator to map it properly *)
    | `Excluded s, `Definite x -> let f x y = f y x in `Excluded (S.map (f x) s)
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
    | `Definite x, `Excluded s -> if S.mem x s then of_bool false else top ()
    | `Excluded s, `Definite x -> if S.mem x s then of_bool false else top ()
    (* The good case: *)
    | `Definite x, `Definite y -> of_bool (x=y)
    (* If either one of them is bottom, we return bottom *)
    | _ -> `Bot

  (* The inequality check: *)
  let ne x y = match x,y with
    (* Not much to do with two exclusion sets: *)
    | `Excluded _, `Excluded _ -> top ()
    (* Is x inequal to an exclusion set, if it is a member then Yes otherwise we
     * don't know: *)
    | `Definite x, `Excluded s -> if S.mem x s then of_bool true else top ()
    | `Excluded s, `Definite x -> if S.mem x s then of_bool true else top ()
    (* The good case: *)
    | `Definite x, `Definite y -> of_bool (x<>y)
    (* If either one of them is bottom, we return bottom *)
    | _ -> `Bot

  let neg  = lift1 Integers.neg
  let add  = lift2_inj Integers.add
  let sub  = lift2_inj Integers.sub
  let mul  = lift2_inj Integers.mul
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
end

module OverflowInt64 =
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

module InfInt =
struct
  (*module Int64 = OverflowInt64*)

  type t = NInf | Fin of int64 | PInf

  let equal x y =
    match x, y with
    | NInf, NInf -> true
    | PInf, PInf -> true
    | Fin x, Fin y when Int64.compare x y == 0 -> true
    | _ -> false

  let compare x y =
    match x, y with
    | NInf , NInf   ->  0
    | NInf , _      -> -1
    | Fin x, NInf   ->  1
    | Fin x, Fin y  -> Int64.compare x y
    | Fin x, _      -> -1
    | PInf , PInf   ->  0
    | PInf , _      ->  1

  let max x y =
    match x, y with
    | NInf, _      -> y
    | _   ,NInf    -> x
    | PInf, _      -> PInf
    | _   ,PInf    -> PInf
    | Fin x, Fin y -> if x < y then Fin y else Fin x

  let min x y =
    match x, y with
    | NInf, _      -> NInf
    | _   ,NInf    -> NInf
    | PInf, _      -> y
    | _   ,PInf    -> x
    | Fin x, Fin y -> if x < y then Fin x else Fin y

  let leq x y = compare x y <= 0
  let lt  x y = compare x y <  0

  let neg x =
    match x with
    | NInf -> PInf
    | PInf -> NInf
    | Fin x-> Fin (Int64.neg x)

  let addp d x y =
    match x, y with
    | NInf , NInf  -> NInf
    | NInf , Fin _ -> NInf
    | NInf , PInf  -> d
    | Fin _, NInf  -> NInf
    | Fin x, Fin y -> Fin (Int64.add x y)
    | Fin x, PInf  -> PInf
    | PInf , NInf  -> d
    | PInf , Fin x -> PInf
    | PInf , PInf  -> PInf

  let mul x y =
    let nf x = match Int64.compare x 0L with
      | 0          -> Fin 0L
      | x when x<0 -> PInf
      | _          -> NInf
    in
    let pf x = match Int64.compare x 0L with
      | 0          -> Fin 0L
      | x when x<0 -> NInf
      | _          -> PInf
    in
    match x, y with
    | NInf , NInf  -> PInf
    | NInf , Fin x -> nf x
    | NInf , PInf  -> NInf
    | Fin x, NInf  -> nf x
    | Fin x, Fin y -> Fin (Int64.mul x y)
    | Fin x, PInf  -> pf x
    | PInf , NInf  -> NInf
    | PInf , Fin x -> pf x
    | PInf , PInf  -> PInf

  let divp d x y =
    let nf x = match Int64.compare x 0L with
      | 0          -> d
      | x when x<0 -> PInf
      | _          -> NInf
    in
    let pf x = match Int64.compare x 0L with
      | 0          -> d
      | x when x<0 -> NInf
      | _          -> PInf
    in
    match x, y with
    | NInf , NInf  -> d
    | NInf , Fin x -> nf x
    | NInf , PInf  -> d
    | Fin x, NInf  -> nf x
    | Fin x, Fin y -> Fin (Int64.div x y)
    | Fin x, PInf  -> pf x
    | PInf , NInf  -> d
    | PInf , Fin x -> pf x
    | PInf , PInf  -> d

end

module CircInterval : S with type t = CBigInt.t interval =
struct
  include Printable.Std
  module I = CBigInt
  module C = CircularBigInt
  type t = I.t interval

  let max_width = 64

  let name () = "circular int intervals"
  let cast_to_width w x =
    match (I.bounds x) with
    | None -> Bot w
    | Some(a,b) -> I.of_t w a b

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
  let of_excl_list x = Top max_width
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

module Interval : S with type t = InfInt.t * InfInt.t =
struct
  include Printable.Std
  module I = InfInt
  type t = I.t * I.t
  let name () = "int intervals"

  let of_interval (x,y) = (I.Fin x, I.Fin y)
  let ending   x = (I.NInf , I.Fin x)
  let starting x = (I.Fin x, I.PInf)
  let maximal (_,y:t) =
    match y with
    | I.Fin x -> Some x
    | _ -> None
  let minimal (x,_:t) =
    match x with
    | I.Fin x -> Some x
    | _ -> None

  let equal (x1,x2:t) (y1,y2:t) =
    I.equal x1 y1 && I.equal x2 y2

  let hash (x:t) = Hashtbl.hash x
  let compare (x1,x2:t) (y1,y2:t) =
    let c = I.compare x1 y1 in
    if c == 0 then c else I.compare x2 y2

  let top () = (I.NInf,I.PInf)
  let is_top (x,y) =
    match x, y with
    | I.NInf, I.PInf -> true
    | _              -> false

  let bot () = (I.PInf,I.NInf)
  let is_bot (x,y) =
    I.compare x y > 0

  let isSimple _ = true
  let short _ (x,y) =
    let f p =
      match p with
      | I.NInf -> "-∞"
      | I.PInf -> "∞"
      | I.Fin x-> Int64.to_string x
    in
    if is_bot (x,y) then
      "⊥"
    else if (I.compare x y == 0) then
      "["^f x^"]"
    else
      "["^f x^".."^f y^"]"

  let pretty_f sh () x = text (sh 10 x)
  let pretty = pretty_f short
  let toXML_f sf x = Xml.Element ("Leaf", [("text", sf Goblintutil.summary_length x)],[])
  let toXML = toXML_f short
  let pretty_diff () (x,y) = dprintf "%s: %a instead of %a" (name ()) pretty x pretty y

  let leq  (x1,x2) (y1,y2) = I.leq y1 x1 && I.leq x2 y2
  let join (x1,x2) (y1,y2) = (I.min x1 y1, I.max x2 y2)
  let meet (x1,x2) (y1,y2) = (I.max x1 y1, I.min x2 y2)

  let widen (l0,u0 as i1) (l1,u1 as i2) =
    let res = (if I.lt l1 l0 then I.NInf else l0), (if I.lt u0 u1 then I.PInf else u0) in
    if M.tracing then M.tracel "widen" "Widening %a and %a yields %a\n" pretty i1 pretty i2 pretty res;
    res

  let narrow (l0,u0) (l1,u1) =
    let lr = match l0 with
      | I.NInf -> l1
      | _      -> l0 in
    let ur = match u0 with
      | I.PInf -> u1
      | _      -> u0 in
    (lr,ur)

  let of_int i = (I.Fin i, I.Fin i)
  let to_int (x,y:t) =
    match x, y with
    | I.Fin x, I.Fin y when Int64.compare x y == 0 -> Some x
    | _ -> None
  let is_int (x,y:t) =
    match x, y with
    | I.Fin x, I.Fin y when Int64.compare x y == 0 -> true
    | _ -> false

  let of_bool b = if b then (I.Fin 1L, I.PInf) else (I.Fin 0L, I.Fin 0L)
  let to_bool i =
    match i with
    | I.Fin 0L, I.Fin 0L -> Some false
    | _ when not (leq (of_int 0L) i) -> Some true
    | _ -> None
  let is_bool i =
    match to_bool i with
    | Some _ -> true
    | _      -> false

  let neg (x,y) = (I.neg y, I.neg x)
  let add (x1,x2) (y1,y2) = (I.addp I.NInf x1 y1, I.addp I.PInf x2 y2)
  let sub i1 i2 = add i1 (neg i2)
  let mul (x1,x2) (y1,y2) =
    if is_bot (x1, x2) || is_bot (y1, y2) then
      bot ()
    else begin
      let x1y1 = (I.mul x1 y1) in let x1y2 = (I.mul x1 y2) in
      let x2y1 = (I.mul x2 y1) in let x2y2 = (I.mul x2 y2) in
      (I.min (I.min x1y1 x1y2) (I.min x2y1 x2y2)),
      (I.max (I.max x1y1 x1y2) (I.max x2y1 x2y2))
    end

  let rec div (x1,x2:t) (y1,y2:t) =
    if is_bot (x1, x2) || is_bot (y1, y2) then
      bot ()
    else begin
      match y1, y2 with
      | I.Fin 0L, I.Fin 0L -> bot ()
      | I.Fin 0L, _        -> div (x1,x2) (I.Fin 1L,y2)
      | _      , I.Fin 0L  -> div (x1,x2) (y1, I.Fin (-1L))
      | _ when leq (of_int 0L) (y1,y2) -> top ()
      | _ ->
        let x1y1n = (I.divp I.NInf x1 y1) in let x1y2n = (I.divp I.NInf x1 y2) in
        let x2y1n = (I.divp I.NInf x2 y1) in let x2y2n = (I.divp I.NInf x2 y2) in
        let x1y1p = (I.divp I.PInf x1 y1) in let x1y2p = (I.divp I.PInf x1 y2) in
        let x2y1p = (I.divp I.PInf x2 y1) in let x2y2p = (I.divp I.PInf x2 y2) in
        (I.min (I.min x1y1n x1y2n) (I.min x2y1n x2y2n)),
        (I.max (I.max x1y1p x1y2p) (I.max x2y1p x2y2p))
    end

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
      | Some x, Some y -> (try of_int (f x y) with Division_by_zero -> top ())
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
  let rem  = bit Int64.rem

  let ne i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, _
    | _   , true -> bot ()
    | _ ->
      match sub i1 i2 with
      | (I.Fin 0L, I.Fin 0L) -> of_bool false
      | x when not (leq (I.Fin 0L, I.Fin 0L) x) -> of_bool true
      | _ -> top ()

  let eq i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, _
    | _   , true -> bot ()
    | _ ->
      match sub i1 i2 with
      | (I.Fin 0L, I.Fin 0L) -> of_bool true
      | x when not (leq (I.Fin 0L, I.Fin 0L) x) -> of_bool false
      | _ -> top ()

  let ge (x1,x2) (y1,y2) =
    if is_bot (x1, x2) || is_bot (y1, y2) then
      bot ()
    else begin
      if I.leq y2 x1 then of_bool true  else
      if I.lt  x2 y1 then of_bool false else
        top ()
    end

  let le (x1,x2) (y1,y2) =
    if is_bot (x1, x2) || is_bot (y1, y2) then
      bot ()
    else begin
      if I.leq x2 y1 then of_bool true  else
      if I.lt  y2 x1 then of_bool false else
        top ()
    end

  let gt (x1,x2) (y1,y2) =
    if is_bot (x1, x2) || is_bot (y1, y2) then
      bot ()
    else begin
      if I.lt  y2 x1 then of_bool true  else
      if I.leq x2 y1 then of_bool false else
        top ()
    end

  let lt (x1,x2) (y1,y2) =
    if is_bot (x1, x2) || is_bot (y1, y2) then
      bot ()
    else begin
      if I.lt  x2 y1 then of_bool true  else
      if I.leq y2 x1 then of_bool false else
        top ()
    end

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  let of_excl_list l = top ()
  let is_excl_list l = false
  let to_excl_list x = None
(*
  let add x y = try add x y with OverflowInt64.Overflow _ -> top ()
  let sub x y = try sub x y with OverflowInt64.Overflow _ -> top ()
  let mul x y = try mul x y with OverflowInt64.Overflow _ -> top ()
  *)
  let cast_to_width _ x = x
end

(*module IncExcInterval : S with type t = [ | `Excluded of Interval.t| `Included of Interval.t ] =
  struct
  include Printable.Std
  module I = Interval

  type t = [
    | `Excluded of I.t
    | `Included of I.t
  (*    | `Bot*)
    ]

  let name () = "Exclusive & Inclusive Integer Intervals"

  let equal (x:t) (y:t) =
    match x, y with
      | `Excluded x, `Excluded y
      | `Included x, `Included y -> I.equal x y
  (*      | `Bot, `Bot -> true*)
      | _ -> false

  let hash (x:t) =
    match x with
      | `Excluded x -> 2 * I.hash x
      | `Included x -> I.hash x
  (*      | `Bot -> 7*)

  let compare (x:t) (y:t) =
    match x, y with
  (*      | `Bot, `Bot -> 0
      |    _, `Bot -> 1
      | `Bot,    _ -> -1*)
      | `Excluded x, `Excluded y
      | `Included x, `Included y -> I.compare x y
      | `Included _, `Excluded _ -> -1
      | `Excluded _, `Included _ -> 1

  let short w (x:t) =
    match x with
  (*       | `Bot -> "⊥" *)
      | `Included x -> I.short w x
      | `Excluded x -> "Not " ^ I.short w x

  let isSimple _ = true

  let pretty_f sf () (x:t) = text (sf max_int x)
  let toXML_f sf (x:t) = Xml.Element ("Leaf", [("text", sf Goblintutil.summary_length x)],[])
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let pretty_diff () (x,y) = dprintf "%s: %a instead of %a" (name ()) pretty x pretty y

  let top () : t = `Included (I.top ())
  let bot () : t = `Included (I.bot ())

  let is_top (x:t) =
    match x with
      | `Included x -> I.is_top x
      | _ -> false

  let is_bot (x:t) =
    match x with
      | `Included x -> I.is_bot x
      | _ -> false

  let leq (x:t) (y:t) =
    match x, y with
  (*    | `Bot, _    -> true
    |    _, `Bot -> false*)
    | `Excluded x, `Excluded y -> I.leq y x
    | `Included x, `Included y -> I.leq x y
    | `Included (x1,x2), `Excluded (y1,y2) -> InfInt.lt x2 y1 || InfInt.lt y2 x1
    | `Excluded _, `Included _ -> false

  let norm (x:t) : t =
    match x with
      | `Excluded x when I.is_bot x -> `Included (I.top ())
      | `Excluded x when I.is_top x -> `Included (I.bot ())
      | `Excluded (InfInt.Fin x,InfInt.PInf ) -> `Included (InfInt.NInf, InfInt.Fin (Int64.sub x 1L))
      | `Excluded (InfInt.NInf ,InfInt.Fin x) -> `Included (InfInt.Fin (Int64.add x 1L), InfInt.PInf)
      | x -> x

  let join (x:t) (y:t) =
    match x, y with
      | `Excluded x, `Excluded y -> norm (`Excluded (I.meet x y))
      | `Included x, `Included y -> `Included (I.join x y)
      | `Included (y1, y2), `Excluded (x1, x2)
      | `Excluded (x1, x2), `Included (y1, y2) ->
        if InfInt.lt y2 x1 || InfInt.lt x2 y1 then
          `Excluded (x1, x2)
        else if InfInt.leq y1 x1 then
          norm (`Excluded (InfInt.addp InfInt.PInf y2 (InfInt.Fin 1L) , x2))
        else if InfInt.leq x2 y2 then
          norm (`Excluded (x1, InfInt.addp InfInt.NInf y2 (InfInt.Fin (-1L))))
        else
          top ()

  let meet (x:t) (y:t) =
    match x, y with
  (*      |    _, `Bot
      | `Bot,    _ -> `Bot*)
      | `Excluded x, `Excluded y -> norm (`Excluded (I.join x y))
      | `Included x, `Included y -> `Included (I.meet x y)
      | `Included (y1, y2), `Excluded (x1, x2)
      | `Excluded (x1, x2), `Included (y1, y2) ->
        if InfInt.lt y2 x1 || InfInt.lt x2 y1 then
          `Included (y1, y2)
        else if I.leq (y1,y2) (x1,x2) then
          bot ()
        else if InfInt.leq x1 y1 then
          `Included (InfInt.addp InfInt.NInf x2 (InfInt.Fin 1L),y2)
        else if InfInt.leq y2 x2 then
          `Included (y1, InfInt.addp InfInt.NInf x1 (InfInt.Fin (-1L)))
        else
          `Excluded (x1, x2) (* this is the bad case --- we just pick one of the arguments *)

  let widen (x:t) (y:t) =
    match x, y with
      | `Included x, `Included y -> `Included (I.widen x y)
      | `Excluded x, `Excluded y when I.equal x y -> `Excluded y
      | `Excluded x, `Excluded y -> top ()
      | x, y  -> y

  let narrow (x:t) (y:t) =
    match x, y with
      | `Included x, `Included y -> `Included (I.narrow x y)
      | x, y  -> x

  let minimal (x:t) =
    match x with
      | `Included x -> I.minimal x
      | _ -> None

  let maximal (x:t) =
    match x with
      | `Included x -> I.maximal x
      | _ -> None

  let starting x : t = `Included (I.starting x)
  let ending x : t = `Included (I.ending x)
  (*   let of_interval x y : t = `Included (I.of_interval x y) *)

  let to_excl_list (x:t)  =
    let rec els x y = if x == y then [x] else  x :: els (Int64.add x 1L) y in
    match x with
      | `Excluded (InfInt.Fin x, InfInt.Fin y) -> Some (els x y)
      | _ -> None

  let is_excl_list (x:t) =
    match x with
      | `Excluded (InfInt.Fin x, InfInt.Fin y) -> true
      | _ -> false

  let of_excl_list x =
    match x with
      | [] -> top ()
      | x::xs ->
        let f (min, max) x =
          if Int64.compare x min <= 0
          then (x,max)
          else if Int64.compare max x <= 0
          then (min,x)
          else (min,max)
        in
        let (x,y) = List.fold_left f (x,x) xs in
        `Excluded (InfInt.Fin x, InfInt.Fin y)


  let to_int (x:t) =
    match x with
      | `Included x -> I.to_int x
      | _ -> None

  let is_int (x:t) =
    match x with
      | `Included x -> I.is_int x
      | _ -> false

  let of_int x : t = `Included (I.of_int x)

  let to_bool (x:t) =
    match x with
      | `Included x -> I.to_bool x
      | `Excluded x when I.leq (I.of_int 0L) x -> Some true
      | _ -> None

  let is_bool (x:t) =
    match x with
      | `Included x -> I.is_bool x
      | `Excluded x -> I.leq (I.of_int 0L) x

  let of_bool x : t =
    match x with
      | true  -> `Excluded (I.of_int 0L)
      | false -> `Included (I.of_int 0L)

  let lognot x : t =
    match is_bot x with
      | true -> bot ()
      | _ ->
    match to_bool x with
      | Some x -> of_bool (not x)
      | _ -> top ()

  let log_f f x y : t =
    match is_bot x, is_bot y with
      | true, _
      | _   , true -> bot ()
      | _ ->
    match to_bool x, to_bool y with
      | Some x, Some y -> of_bool (f x y)
      | _ -> top ()

  let logor  = log_f (||)
  let logand = log_f (&&)

  let bitnot x : t =
    match is_bot x with
      | true -> bot ()
      | _ ->
    match to_int x with
      | Some x -> of_int (Int64.lognot x)
      | _ -> top ()

  let bit_f f x y : t =
    match is_bot x, is_bot y with
      | true, _
      | _   , true -> bot ()
      | _ ->
    match to_int x, to_int y with
      | Some x, Some y -> of_int (f x y)
      | _ -> top ()

  let bitxor = bit_f Int64.logxor
  let bitand = bit_f Int64.logand
  let bitor  = bit_f Int64.logor

  let shift_right = bit_f (fun x y -> Int64.shift_right x (Int64.to_int y))
  let shift_left  = bit_f (fun x y -> Int64.shift_left  x (Int64.to_int y))
  let rem  = bit_f Int64.rem

  let scheme2 f g h (x:t) (y:t) : t =
    match x, y with
      | `Included x, `Included y -> `Included (f x y)
      | `Excluded x, `Excluded y -> top ()
      | `Excluded x, `Included y -> (h x y)
      | `Included x, `Excluded y -> (g x y)

  let adde (e1,e2) (i1,i2) =
    norm (`Excluded (InfInt.addp InfInt.NInf e1 i2, InfInt.addp InfInt.PInf e2 i1))

  let add = scheme2 I.add adde adde

  let sub =
    let sube  i1 i2 = adde i1 (I.neg i2) in
    scheme2 I.sub sube sube

  let mul x y =
    match x, y with
      | `Excluded x, `Included (y1,y2) when InfInt.equal y1 y2
        -> `Excluded (I.mul x (y1,y1))
      | `Included (x1,x2), `Excluded y when InfInt.equal x1 x2
        -> `Excluded (I.mul (x1,x1) y)
      | _ -> scheme2 I.mul (fun _ _ -> top ()) (fun _ _ -> top ()) x y

  let div = scheme2 I.div (fun _ _ -> top ()) (fun _ _ -> top ())

  let neg (x:t) : t =
    match x with
    | `Included x -> `Included (I.neg x)
    | `Excluded x -> `Excluded (I.neg x)

  let le = scheme2 I.le (fun _ _ -> top ()) (fun _ _ -> top ())
  let lt = scheme2 I.lt (fun _ _ -> top ()) (fun _ _ -> top ())
  let ge = scheme2 I.ge (fun _ _ -> top ()) (fun _ _ -> top ())
  let gt = scheme2 I.gt (fun _ _ -> top ()) (fun _ _ -> top ())

  let eq (x:t) (y:t) : t =
    let eq i e = if I.leq i e then of_bool false else top () in
    match x, y with
      | `Included x, `Included y
        -> begin match I.to_bool (I.eq x y) with
            | Some x -> of_bool x
            | None -> top () end
      | `Excluded x, `Excluded y -> top ()
      | `Excluded x, `Included y -> eq x y
      | `Included x, `Excluded y -> eq y x

  let ne (x:t) (y:t) : t =
    let ne i e = if I.leq i e then of_bool true else top () in
    match x, y with
      | `Included x, `Included y
        -> begin match I.to_bool (I.ne x y) with
            | Some x -> of_bool x
            | None -> top () end
      | `Excluded x, `Excluded y -> top ()
      | `Excluded x, `Included y -> ne x y
      | `Included x, `Excluded y -> ne y x

  end
*)

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
  type t = bool
  let hash = function true -> 51534333 | _ -> 561123444
  let equal (x:t) (y:t) = x=y
  let name () = "booleans"
  let cast_to_width _ x = x
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
  let of_excl_list x = top ()
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

module None : S with type t = unit  =
struct
  include Printable.Std
  include Lattice.StdCousot
  let cast_to_width _ x = x
  let name () = "none"
  type t = unit
  let hash () = 101010
  let equal _ _ = true
  let copy x = ()
  let top () = ()
  let is_top _ = true
  let bot () = ()
  let is_bot _ = true
  let isSimple _  = true
  let short _ x = "?"
  let pretty_f _ _ x = text "?"
  let toXML_f _ x = Xml.Element ("Leaf", [("text", "?")],[])
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let leq x y = true
  let join () () = ()
  let meet x y = ()

  let of_bool _ = ()
  let to_bool _ = None
  let is_bool _ = false
  let of_int  _ = ()
  let to_int  _ = None
  let is_int  _ = false

  let is_excl_list _ = false
  let of_excl_list _ = top ()
  let to_excl_list _ = None
  let of_interval  x = top ()
  let starting     x = top ()
  let ending       x = top ()
  let maximal      x = None
  let minimal      x = None

  let neg x = ()
  let add _ _ = ()
  let sub _ _ = ()
  let mul _ _ = ()
  let div _ _ = ()
  let rem _ _ = ()
  let lt n1 n2 = ()
  let gt n1 n2 = ()
  let le n1 n2 = ()
  let ge n1 n2 = ()
  let eq n1 n2 = ()
  let ne n1 n2 = ()
  let bitnot n1 = ()
  let bitand n1 n2 = ()
  let bitor  n1 n2 = ()
  let bitxor n1 n2 = ()
  let shift_left  n1 n2 = ()
  let shift_right n1 n2 = ()
  let lognot n1    = ()
  let logand n1 n2 = ()
  let logor  n1 n2 = ()
  let pretty_diff () (x,y) = dprintf "%s: %a instead of %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)
end


module ManyInts : S =
struct
  module I1 = Trier
  module I2 = Interval

  include Lattice.Prod (I1) (I2)

  let cast_to_width _ x = x
  let name () = I1.name () ^ " * " ^ I2.name ()

  let equal (x1,x2) (y1,y2) =
    (I1.equal x1 y1 && I2.equal x2 y2)

  let logor (x1,x2) (y1,y2) =
    (I1.logor x1 y1
    ,I2.logor x2 y2)

  let logand (x1,x2) (y1,y2) =
    (I1.logand x1 y1
    ,I2.logand x2 y2)

  let lognot (x1,x2) =
    (I1.lognot x1
    ,I2.lognot x2)

  let shift_right (x1,x2) (y1,y2) =
    (I1.shift_right x1 y1
    ,I2.shift_right x2 y2)

  let shift_left (x1,x2) (y1,y2) =
    (I1.shift_left x1 y1
    ,I2.shift_left x2 y2)

  let bitxor (x1,x2) (y1,y2) =
    (I1.bitxor x1 y1
    ,I2.bitxor x2 y2)

  let bitor (x1,x2) (y1,y2) =
    (I1.bitor x1 y1
    ,I2.bitor x2 y2)

  let bitand (x1,x2) (y1,y2) =
    (I1.bitand x1 y1
    ,I2.bitand x2 y2)

  let bitnot (x1,x2) =
    (I1.bitnot x1
    ,I2.bitnot x2)

  let ne (x1,x2) (y1,y2) =
    (I1.ne x1 y1
    ,I2.ne x2 y2)

  let ne (x1,x2) (y1,y2) =
    (I1.ne x1 y1
    ,I2.ne x2 y2)

  let eq (x1,x2) (y1,y2) =
    (I1.eq x1 y1
    ,I2.eq x2 y2)

  let ge (x1,x2) (y1,y2) =
    (I1.ge x1 y1
    ,I2.ge x2 y2)

  let le (x1,x2) (y1,y2) =
    (I1.le x1 y1
    ,I2.le x2 y2)

  let gt (x1,x2) (y1,y2) =
    (I1.gt x1 y1
    ,I2.gt x2 y2)

  let lt (x1,x2) (y1,y2) =
    (I1.lt x1 y1
    ,I2.lt x2 y2)

  let rem (x1,x2) (y1,y2) =
    (I1.rem x1 y1
    ,I2.rem x2 y2)

  let div (x1,x2) (y1,y2) =
    (I1.div x1 y1
    ,I2.div x2 y2)

  let mul (x1,x2) (y1,y2) =
    (I1.mul x1 y1
    ,I2.mul x2 y2)

  let sub (x1,x2) (y1,y2) =
    (I1.sub x1 y1
    ,I2.sub x2 y2)

  let add (x1,x2) (y1,y2) =
    (I1.add x1 y1
    ,I2.add x2 y2)

  let neg (x1,x2) =
    (I1.neg x1
    ,I2.neg x2)

  let starting x =
    (I1.starting x
    ,I2.starting x)

  let ending x =
    (I1.ending x
    ,I2.ending x)

  let of_bool x =
    (I1.of_bool x
    ,I2.of_bool x)

  let of_excl_list x =
    (I1.of_excl_list x
    ,I2.of_excl_list x)

  let of_int x =
    (I1.of_int x
    ,I2.of_int x)

  let of_interval x = top ()

  let compare (x1,x2) (y1,y2) =
    match I1.compare x1 y1 with
    | 0 -> I2.compare x2 y2
    | x -> x

  let hash (x1,x2) = (I1.hash x1) lxor (I2.hash x2*33)

  let minimal (x1, x2) =
    match I1.minimal x1 with
    | None -> I2.minimal x2
    | Some x1 ->
      match I2.minimal x2 with
      | None -> Some x1
      | Some x2 -> Some (max x1 x2)

  let maximal (x1, x2) =
    match I1.maximal x1 with
    | None -> I2.minimal x2
    | Some x1 ->
      match I2.maximal x2 with
      | None -> Some x1
      | Some x2 -> Some (min x1 x2)

  let to_int (x1, x2) =
    match I1.to_int x1 with
    | None -> I2.to_int x2
    | Some x1 ->
      match I2.to_int x2 with
      | None -> Some x1
      | Some x2 when Int64.compare x1 x2 == 0 -> Some x1
      | Some x2 ->
        let msg = "Inconsistent state! "^(Int64.to_string x1)^" != "^(Int64.to_string x2) in
        Messages.warn_all msg; None

  let to_bool (x1, x2) =
    match I1.to_bool x1 with
    | None -> I2.to_bool x2
    | Some x1 ->
      match I2.to_bool x2 with
      | None -> Some x1
      | Some x2 when x1 == x2 -> Some x1
      | Some x2 ->
        let msg = "Inconsistent state! "^(string_of_bool x1)^" != "^(string_of_bool x2) in
        Messages.warn_all msg; None

  let to_excl_list (x1, x2) =
    match I1.to_excl_list x1 with
    | None -> I2.to_excl_list x2
    | Some x1 ->
      match I2.to_excl_list x2 with
      | None -> Some x1
      | Some x2 -> Some (x1 @ x2)

  let is_excl_list (x1,x2) = (I1.is_excl_list x1) || (I2.is_excl_list x2)
  let is_bool (x1,x2) = (I1.is_bool x1) || (I2.is_bool x2)
  let is_int (x1,x2) = (I1.is_int x1) || (I2.is_int x2)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

end

module IntDomList : S = (* deprecated, use IntDomTuple below *)
struct
  include Printable.Std
  exception IntDomListBroken

  module I1 = Trier
  module I2 = Interval32
  module I3 = CircInterval

  type e = Trier of I1.t
         | Interval of I2.t
         | CInterval of I3.t

  type t = e list

  (* constructors *)

  let name () = I1.name () (* why do we just use the first name? *)
  let cast_to_width' w x = (* why do we not call this on all?? *)
    match x with
    | CInterval a -> CInterval (I3.cast_to_width w a)
    | _ -> x

  let constr_scheme xs =
    let f (s,g) y : t =
      if get_bool ("ana.int."^s)
      then (g ()) :: y
      else y
    in
    List.fold_right f xs []

  let top () = constr_scheme
      [("trier"    ,fun () -> Trier    (I1.top ()))
      ;("interval" ,fun () -> Interval (I2.top ()))
      ;("cinterval",fun () -> CInterval (I3.top ()))]

  let bot () = constr_scheme
      [("trier"    ,fun () -> Trier    (I1.bot ()))
      ;("interval" ,fun () -> Interval (I2.bot ()))
      ;("cinterval",fun () -> CInterval (I3.bot ()))]

  let starting x = constr_scheme
      [("trier"    ,fun () -> Trier    (I1.starting x))
      ;("interval" ,fun () -> Interval (I2.starting x))
      ;("cinterval",fun () -> CInterval (I3.starting x))]

  let ending x = constr_scheme
      [("trier"   ,fun () -> Trier    (I1.ending x))
      ;("interval",fun () -> Interval (I2.ending x))
      ;("cinterval",fun () -> CInterval (I3.ending x))]

  let of_bool x = constr_scheme
      [("trier"   ,fun () -> Trier    (I1.of_bool x))
      ;("interval",fun () -> Interval (I2.of_bool x))
      ;("cinterval",fun () -> CInterval (I3.of_bool x))]

  let of_excl_list x = constr_scheme
      [("trier"   ,fun () -> Trier    (I1.of_excl_list x))
      ;("interval",fun () -> Interval (I2.of_excl_list x))
      ;("cinterval",fun () -> CInterval (I3.of_excl_list x))]

  let of_int x = constr_scheme
      [("trier"   ,fun () -> Trier    (I1.of_int x))
      ;("interval",fun () -> Interval (I2.of_int x))
      ;("cinterval",fun () -> CInterval (I3.of_int x))]

  let of_interval x = top ()

  (* element functions *)

  let narrow' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.narrow x y)
    | Interval x, Interval y -> Interval (I2.narrow x y)
    | CInterval x, CInterval y -> CInterval (I3.narrow x y)
    | _ -> raise IntDomListBroken

  let widen' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.widen x y)
    | Interval x, Interval y -> Interval (I2.widen x y)
    | CInterval x, CInterval y -> CInterval (I3.widen x y)
    | _ -> raise IntDomListBroken

  let is_top' x =
    match x with
    | Trier x -> I1.is_top x
    | Interval x -> I2.is_top x
    | CInterval x -> I3.is_top x
  (*      | _ -> raise IntDomListBroken*)

  let is_bot' x =
    match x with
    | Trier x -> I1.is_bot x
    | Interval x -> I2.is_bot x
    | CInterval x -> I3.is_bot x
  (*      | _ -> raise IntDomListBroken*)

  let meet' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.meet x y)
    | Interval x, Interval y -> Interval (I2.meet x y)
    | CInterval x, CInterval y -> CInterval (I3.meet x y)
    | _ -> raise IntDomListBroken

  let join' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.join x y)
    | Interval x, Interval y -> Interval (I2.join x y)
    | CInterval x, CInterval y -> CInterval (I3.join x y)
    | _ -> raise IntDomListBroken

  let leq' x y =
    match x, y with
    | Trier x, Trier y -> I1.leq x y
    | Interval x, Interval y -> I2.leq x y
    | CInterval x, CInterval y -> I3.leq x y
    | _ -> raise IntDomListBroken

  let short' w x =
    match x with
    | Trier x -> I1.short w x
    | Interval x -> I2.short w x
    | CInterval x -> I3.short w x
  (*      | _ -> raise IntDomListBroken*)

  let toXML_f' sf x =
    match x with
    | Trier x -> I1.toXML_f (fun w x -> sf w (Trier x)) x
    | Interval x -> I2.toXML_f (fun w x -> sf w (Interval x)) x
    | CInterval x -> I3.toXML_f (fun w x -> sf w (CInterval x)) x
  (*      | _ -> raise IntDomListBroken*)

  let pretty_f' sf () x =
    match x with
    | Trier x -> I1.pretty_f (fun w x -> sf w (Trier x)) () x
    | Interval x -> I2.pretty_f (fun w x -> sf w (Interval x)) () x
    | CInterval x -> I3.pretty_f (fun w x -> sf w (CInterval x)) () x
  (*      | _ -> raise IntDomListBroken*)

  let toXML' x = toXML_f' short' x

  let pretty' x = pretty_f' short' x

  let compare' x y =
    match x, y with
    | Trier x, Trier y -> I1.compare x y
    | Interval x, Interval y -> I2.compare x y
    | CInterval x, CInterval y -> I3.compare x y
    | _ -> raise IntDomListBroken

  let equal' x y =
    match x, y with
    | Trier x, Trier y -> I1.equal x y
    | Interval x, Interval y -> I2.equal x y
    | CInterval x, CInterval y -> I3.equal x y
    | _ -> raise IntDomListBroken

  let logor' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.logor x y)
    | Interval x, Interval y -> Interval (I2.logor x y)
    | CInterval x, CInterval y -> CInterval (I3.logor x y)
    | _ -> raise IntDomListBroken

  let logand' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.logand x y)
    | Interval x, Interval y -> Interval (I2.logand x y)
    | CInterval x, CInterval y -> CInterval (I3.logand x y)
    | _ -> raise IntDomListBroken

  let lognot' x =
    match x with
    | Trier x -> Trier (I1.lognot x )
    | Interval x -> Interval (I2.lognot x)
    | CInterval x -> CInterval (I3.lognot x)
  (*      | _ -> raise IntDomListBroken*)

  let shift_right' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.shift_right x y)
    | Interval x, Interval y -> Interval (I2.shift_right x y)
    | CInterval x, CInterval y -> CInterval (I3.shift_right x y)
    | _ -> raise IntDomListBroken

  let shift_left' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.shift_left x y)
    | Interval x, Interval y -> Interval (I2.shift_left x y)
    | CInterval x, CInterval y -> CInterval (I3.shift_left x y)
    | _ -> raise IntDomListBroken

  let bitxor' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.bitxor x y)
    | Interval x, Interval y -> Interval (I2.bitxor x y)
    | CInterval x, CInterval y -> CInterval (I3.bitxor x y)
    | _ -> raise IntDomListBroken

  let bitor' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.bitor x y)
    | Interval x, Interval y -> Interval (I2.bitor x y)
    | CInterval x, CInterval y -> CInterval (I3.bitor x y)
    | _ -> raise IntDomListBroken

  let bitand' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.bitand x y)
    | Interval x, Interval y -> Interval (I2.bitand x y)
    | CInterval x, CInterval y -> CInterval (I3.bitand x y)
    | _ -> raise IntDomListBroken

  let bitnot' x =
    match x with
    | Trier x -> Trier (I1.bitnot x)
    | Interval x -> Interval (I2.bitnot x)
    | CInterval x -> CInterval (I3.bitnot x)
  (*      | _ -> raise IntDomListBroken*)

  let ne' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.ne x y)
    | Interval x, Interval y -> Interval (I2.ne x y)
    | CInterval x, CInterval y -> CInterval (I3.ne x y)
    | _ -> raise IntDomListBroken

  let eq' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.eq x y)
    | Interval x, Interval y -> Interval (I2.eq x y)
    | CInterval x, CInterval y -> CInterval (I3.eq x y)
    | _ -> raise IntDomListBroken

  let ge' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.ge x y)
    | Interval x, Interval y -> Interval (I2.ge x y)
    | CInterval x, CInterval y -> CInterval (I3.ge x y)
    | _ -> raise IntDomListBroken

  let le' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.le x y)
    | Interval x, Interval y -> Interval (I2.le x y)
    | CInterval x, CInterval y -> CInterval (I3.le x y)
    | _ -> raise IntDomListBroken

  let gt' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.gt x y)
    | Interval x, Interval y -> Interval (I2.gt x y)
    | CInterval x, CInterval y -> CInterval (I3.gt x y)
    | _ -> raise IntDomListBroken

  let lt' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.lt x y)
    | Interval x, Interval y -> Interval (I2.lt x y)
    | CInterval x, CInterval y -> CInterval (I3.lt x y)
    | _ -> raise IntDomListBroken

  let rem' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.rem x y)
    | Interval x, Interval y -> Interval (I2.rem x y)
    | CInterval x, CInterval y -> CInterval (I3.rem x y)
    | _ -> raise IntDomListBroken

  let div' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.div x y)
    | Interval x, Interval y -> Interval (I2.div x y)
    | CInterval x, CInterval y -> CInterval (I3.div x y)
    | _ -> raise IntDomListBroken

  let mul' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.mul x y)
    | Interval x, Interval y -> Interval (I2.mul x y)
    | CInterval x, CInterval y -> CInterval (I3.mul x y)
    | _ -> raise IntDomListBroken

  let sub' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.sub x y)
    | Interval x, Interval y -> Interval (I2.sub x y)
    | CInterval x, CInterval y -> CInterval (I3.sub x y)
    | _ -> raise IntDomListBroken

  let add' x y =
    match x, y with
    | Trier x, Trier y -> Trier (I1.add x y)
    | Interval x, Interval y -> Interval (I2.add x y)
    | CInterval x, CInterval y -> CInterval (I3.add x y)
    | _ -> raise IntDomListBroken

  let neg' x =
    match x with
    | Trier x -> Trier (I1.neg x)
    | Interval x -> Interval (I2.neg x)
    | CInterval x -> CInterval (I3.neg x)
  (*      | _ -> raise IntDomListBroken*)

  let hash' x =
    match x with
    | Trier x-> I1.hash x
    | Interval x-> 17*I2.hash x
    | CInterval x-> 34*I3.hash x
  (*      | _ -> raise IntDomListBroken*)

  let minimal' x =
    match x with
    | Trier x -> I1.minimal x
    | Interval x -> I2.minimal x
    | CInterval x -> I3.minimal x
  (*      | _ -> raise IntDomListBroken*)

  let maximal' x =
    match x with
    | Trier x -> I1.maximal x
    | Interval x -> I2.maximal x
    | CInterval x -> I3.maximal x
  (*      | _ -> raise IntDomListBroken*)

  let to_int' x =
    match x with
    | Trier x -> I1.to_int x
    | Interval x -> I2.to_int x
    | CInterval x -> I3.to_int x
  (*      | _ -> raise IntDomListBroken*)

  let to_bool' x =
    match x with
    | Trier x -> I1.to_bool x
    | Interval x -> I2.to_bool x
    | CInterval x -> I3.to_bool x
  (*      | _ -> raise IntDomListBroken*)

  let to_excl_list' x =
    match x with
    | Trier x -> I1.to_excl_list x
    | Interval x -> I2.to_excl_list x
    | CInterval x -> I3.to_excl_list x
  (*      | _ -> raise IntDomListBroken*)

  let is_excl_list' x =
    match x with
    | Trier x -> I1.is_excl_list x
    | Interval x -> I2.is_excl_list x
    | CInterval x -> I3.is_excl_list x
  (*      | _ -> raise IntDomListBroken*)

  let is_bool' x =
    match x with
    | Trier x -> I1.is_bool x
    | Interval x -> I2.is_bool x
    | CInterval x -> I3.is_bool x
  (*      | _ -> raise IntDomListBroken*)

  let is_int' x =
    match x with
    | Trier x -> I1.is_int x
    | Interval x -> I2.is_int x
    | CInterval x -> I3.is_int x
  (*      | _ -> raise IntDomListBroken *)

  (* list functions *)

  let logor       = List.map2 logor'
  let logand      = List.map2 logand'
  let lognot      = List.map lognot'
  let shift_right = List.map2 shift_right'
  let shift_left  = List.map2 shift_left'
  let bitxor      = List.map2 bitxor'
  let bitor       = List.map2 bitor'
  let bitand      = List.map2 bitand'
  let bitnot      = List.map bitnot'
  let ne  = List.map2 ne'
  let eq  = List.map2 eq'
  let ge  = List.map2 ge'
  let le  = List.map2 le'
  let gt  = List.map2 gt'
  let lt  = List.map2 lt'
  let rem = List.map2 rem'
  let div = List.map2 div'
  let mul = List.map2 mul'
  let sub = List.map2 sub'
  let add = List.map2 add'
  let neg = List.map neg'
  let cast_to_width w = List.map (cast_to_width' w)

  let minimal x =
    let max x y =
      match x, y with
      | Some x, Some y -> Some (max x y)
      | x   , None -> x
      | None,    y -> y
    in
    match x with
    | (x::y) -> List.fold_left (fun x y -> max x (minimal' y)) (minimal' x) y
    | _ -> None

  let maximal x =
    let min x y =
      match x, y with
      | Some x, Some y -> Some (min x y)
      | x   , None -> x
      | None,    y -> y
    in
    match x with
    | (x::y) -> List.fold_left (fun x y -> min x (maximal' y)) (maximal' x) y
    | _ -> None

  let narrow = List.map2 narrow'
  let widen  = List.map2 widen'
  let meet   = List.map2 meet'
  let join= List.map2 join'

  let is_top = List.for_all is_top'
  let is_bot = List.for_all is_bot'
  let leq    = List.for_all2 leq'

  let short _ x =
    match x with
    | [] -> ""
    | [x] -> short' 30 x
    | x::xs ->  List.fold_left (fun p n -> p ^ ";" ^ short' 30 n) (short' 30 x) xs

  let pretty_f _ () x =
    match x with
    | [] -> text "()"
    | x :: [] -> pretty' () x
    | x :: y ->
      let first = pretty' () x in
      let rest  = List.fold_left (fun p n->p ++ text "," ++ pretty' () n) (text "") y in
      text "(" ++ first ++ rest ++ text ")"

  let pretty () x = pretty_f short () x

  let toXML_f sf x =
    let esc = Goblintutil.escape in
    Xml.Element ("Leaf", [("text", esc (sf Goblintutil.summary_length x))], [])

  let toXML = toXML_f short

  let compare = (* ?? doesn't use a! just returns the compare value of the last domain or 0 if the one before wasn't equal?! *)
    let f a x y =
      if a == 0
      then compare' x y
      else 0
    in
    List.fold_left2 f 0

  let isSimple _ = true
  let hash     = List.fold_left (fun x y -> x lxor (hash' y)) 0
  let equal    = List.for_all2 equal'

  let is_excl_list = List.exists  is_excl_list'
  let is_bool = List.exists  is_bool'
  let is_int = List.exists  is_int'

  let to_excl_list =
    let f x y =
      match x with
      | None -> to_excl_list' y
      | Some x ->
        match to_excl_list' y with
        | None -> Some x
        | Some y -> Some (x @ y)
    in
    List.fold_left f None

  exception Inconsistent

  let to_bool x =
    let f x y =
      match x with
      | None -> to_bool' y
      | Some x ->
        match to_bool' y with
        | None -> Some x
        | Some y when x == y -> Some x
        | Some y ->
          let msg = "Inconsistent state! "^(string_of_bool x)^" != "^(string_of_bool y) in
          Messages.warn_all msg;
          raise Inconsistent
    in
    try List.fold_left f None x
    with Inconsistent -> None

  let to_int x =
    let f x y =
      match x with
      | None -> to_int' y
      | Some x ->
        match to_int' y with
        | None -> Some x
        | Some y when Int64.compare x y == 0 -> Some x
        | Some y ->
          let msg = "Inconsistent state! "^(Int64.to_string x)^" != "^(Int64.to_string y) in
          Messages.warn_all msg;
          raise Inconsistent
    in
    try List.fold_left f None x
    with Inconsistent -> None

  let pretty_diff () (x,y) = dprintf "%a instead of %a" pretty x pretty y

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

end

module Enums : S = struct
  open Batteries
  module I = Integers
  type e = I.t
  type t = Neg of e list | Pos of e list

  let name () = "enums"

  let bot () = Pos []
  let top () = Neg []
  let short _ = function
    | Pos[] -> "bot" | Neg[] -> "top"
    | Pos xs -> "{" ^ (String.concat ", " (List.map (I.short 30) xs)) ^ "}"
    | Neg xs -> "not {" ^ (String.concat ", " (List.map (I.short 30) xs)) ^ "}"

  let of_int x = Pos [x]
  let cast_to_width w = function Pos xs -> Pos (List.map (I.cast_to_width w) xs) | Neg _ -> top ()

  let of_interval (x,y) =
    let rec build_set set start_num end_num =
      if start_num >= end_num then set
      else (build_set ([start_num] @ set) (Int64.add start_num (Int64.of_int 1)) end_num) in
    Pos (build_set [] x y)

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
    | Pos x, Pos y -> Pos (merge_cup x y)
    | Neg x, Neg y -> Neg (merge_cap x y)
    | Neg x, Pos y
    | Pos y, Neg x -> Neg (merge_sub x y)
  let meet = curry @@ function
    | Pos x, Pos y -> Pos (merge_cap x y)
    | Neg x, Neg y -> Neg (merge_cup x y)
    | Pos x, Neg y
    | Neg y, Pos x -> Pos (merge_sub x y)
  (* let join x y = let r = join x y in print_endline @@ "join " ^ short 10 x ^ " " ^ short 10 y ^ " = " ^ short 10 r; r *)
  (* let meet x y = let r = meet x y in print_endline @@ "meet " ^ short 10 x ^ " " ^ short 10 y ^ " = " ^ short 10 r; r *)

  let widen x y = join x y
  let narrow x y = meet x y

  let leq x y = join x y = y

  let abstr_compare = curry @@ function
    | Neg _, Neg _ -> Pos[-1L; 0L ;1L]
    | Pos[],_ | _,Pos[] -> Pos[]
    | Pos x, Pos y ->
      let x_max = List.last x in
      let x_min = List.hd x in
      let y_max = List.last y in
      let y_min = List.hd y in
      if  x_max < y_min then Pos[-1L]
      else if y_max < x_min then Pos[1L]
      else if x_min = y_max then
        if  y_min = x_max then Pos[0L]
        else Pos[0L;1L]
      else if y_min = x_max then Pos[-1L;0L]
      else Pos[-1L;0L;1L]
    | Pos l, Neg l' ->
      (match merge_sub l l' with
       | [] -> Pos[-1L;1L]
       | _ -> Pos[-1L;0L;1L]
      )
    | Neg l, Pos l' ->
      (match merge_sub l' l with
       | [] -> Pos[-1L;1L]
       | _ -> Pos[-1L;0L;1L]
      )

  let max_elems () = get_int "ana.int.enums_max" (* maximum number of resulting elements before going to top *)
  let lift1 f = function
    | Pos[x] -> Pos[f x]
    | Pos xs when List.length xs <= max_elems () -> Pos (List.sort_unique compare @@ List.map f xs)
    | _ -> Neg[]
  let lift2 f = curry @@ function
    | Pos[],_| _,Pos[] -> Pos[]
    | Pos[x],Pos[y] -> Pos[f x y]
    | Pos xs,Pos ys ->
      let r = List.cartesian_product xs ys |> List.map (uncurry f) |> List.sort_unique compare in
      if List.length r <= max_elems () then Pos r else Neg[]
    | _,_ -> Neg[]

  let neg  = lift1 I.neg
  let add  = curry @@ function
    | Pos[0L],x | x,Pos[0L] -> x
    | x,y -> lift2 I.add x y
  let sub  = lift2 I.sub
  let mul  = curry @@ function
    | Pos[1L],x | x,Pos[1L] -> x
    | Pos[0L],_ | _,Pos[0L] -> Pos[0L]
    | x,y -> lift2 I.mul x y
  let div  = curry @@ function
    | Pos[1L],x | x,Pos[1L] -> x
    | Pos[0L],_ -> Pos[0L]
    | _,Pos[0L] -> raise Division_by_zero
    | x,y -> lift2 I.div x y
  let rem  = lift2 I.rem
  let lt = lift2 I.lt
  let gt = lift2 I.gt
  let le = lift2 I.le
  let ge = lift2 I.ge
  let eq = lift2 I.eq
  let ne = lift2 I.ne
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
    | Pos [] -> text "bot"
    | Neg [] -> text "top"
    | Pos xs -> text "Pos" ++ pretty_list (List.map (I.pretty ()) xs)
    | Neg xs -> text "Neg" ++ pretty_list (List.map (I.pretty ()) xs)
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let pretty_diff () (x,y) = Pretty.dprintf "%a instead of %a" pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  let of_bool x = Pos [if x then Int64.one else Int64.zero]
  let to_bool = function
    | Pos [] | Neg [] -> None
    | Pos [0L] -> Some false
    | Pos xs when List.for_all ((<>) 0L) xs -> Some true
    | Neg xs when List.exists ((=) 0L) xs -> Some true
    | _ -> None
  let is_bool = Option.is_some % to_bool
  let of_int  x = Pos [x]
  let to_int = function Pos [x] -> Some x | _ -> None
  let is_int = Option.is_some % to_int

  let to_excl_list = function Neg x when x<>[] -> Some x | _ -> None
  let of_excl_list x = Neg x
  let is_excl_list = Option.is_some % to_excl_list
  let starting     x = top ()
  let ending       x = top ()
  let maximal = function Pos xs when xs<>[] -> Some (List.last xs) | _ -> None
  let minimal = function Pos (x::xs) -> Some x | _ -> None
end

module IntDomTuple : S = struct (* the above IntDomList has too much boilerplate. we have to touch every function in S if we want to add a new domain. here if we add a new option, we only have to edit the places where fn are applied, i.e. create, mapp, map, map2. *)
  open Batteries
  module I1 = Trier
  module I2 = Interval32
  module I3 = CircInterval
  module I4 = Enums
  type t = I1.t option * I2.t option * I3.t option * I4.t option

  type 'a m = (module S with type t = 'a)
  (* only first-order polymorphism on functions -> use records to get around monomorphism restriction on arguments *)
  type 'b poly_in  = { fi  : 'a. 'a m -> 'b -> 'a } (* inject *)
  type 'b poly_pr  = { fp  : 'a. 'a m -> 'a -> 'b } (* project *)
  type 'b poly2_pr  = { f2p  : 'a. 'a m -> 'a -> 'a -> 'b }
  type poly1 = { f1 : 'a. 'a m -> 'a -> 'a } (* needed b/c above 'b must be different from 'a *)
  type poly2 = { f2 : 'a. 'a m -> 'a -> 'a -> 'a }
  let create r x = (* use where values are introduced *)
    let f n g = if get_bool ("ana.int."^n) then Some (g x) else None in
    f "trier" @@ r.fi (module I1), f "interval" @@ r.fi (module I2), f "cinterval" @@ r.fi (module I3), f "enums" @@ r.fi (module I4)
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
  let of_excl_list = create { fi = fun (type a) (module I:S with type t = a) -> I.of_excl_list }
  let of_int = create { fi = fun (type a) (module I:S with type t = a) -> I.of_int }
  let starting = create { fi = fun (type a) (module I:S with type t = a) -> I.starting }
  let ending = create { fi = fun (type a) (module I:S with type t = a) -> I.ending }
  let of_interval = create { fi = fun (type a) (module I:S with type t = a) -> I.of_interval }

  (* f1: unary ops *)
  let neg = map { f1 = fun (type a) (module I:S with type t = a) -> I.neg }
  let bitnot = map { f1 = fun (type a) (module I:S with type t = a) -> I.bitnot }
  let lognot = map { f1 = fun (type a) (module I:S with type t = a) -> I.lognot }
  let cast_to_width w = map { f1 = fun (type a) (module I:S with type t = a) -> I.cast_to_width w } (* NOTE: we call it on all domains - above it is only called on I3=CircInterval, despite being also implemented in I2=Interval32. I1=Trier just ignores the cast. *)

  (* fp: projections *)
  let same show x = let xs = to_list_some x in let us = List.unique xs in let n = List.length us in
    if n = 1 then Some (List.hd xs)
    else (
      if n>1 then Messages.warn_all @@ "Inconsistent state! "^String.concat "," @@ List.map show us;
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
end
