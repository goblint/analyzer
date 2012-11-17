open GobConfig
open Pretty

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
(*  val of_interval: int64 -> int64 -> t*)
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
end

exception Unknown
exception Error

module Integers : S with type t = int64  =
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
  let add  = Int64.add
  let sub  = Int64.sub
  let mul  = Int64.mul
  let div x y =  
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
end

module FlatPureIntegers =
struct
  include Integers
  
  let top () = raise Unknown
  let bot () = raise Error
  let leq = equal
  let pretty_diff () (x,y) = Pretty.dprintf "Integer %a instead of %a" pretty x pretty y
  let join x y = if equal x y then x else top ()
  let meet x y = if equal x y then x else bot ()
end

module Flat (Base: S) =
struct
  include Lattice.Flat (Base) (struct
                                 let top_name = "Unknown int"
                                 let bot_name = "Error int"
                               end)

  let name () = "flat integers"

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
  let of_interval  x y = top ()
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
end

module Lift (Base: S) =
struct
  include Lattice.Lift (Base) (struct
                                 let top_name = "MaxInt"
                                 let bot_name = "MinInt"
                               end)

  let name () = "lifted integers"

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
  let of_interval  x y = top ()
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
end

module Flattened = Flat (Integers) 
module Lifted    = Lift (Integers) 

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
  
  let of_interval x y = if Int64.compare x y == 0 then of_int x else top ()
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
end


module InfInt =
struct
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

module Interval : S with type t = InfInt.t * InfInt.t =
struct 
  include Printable.Std
  module I = InfInt
  type t = I.t * I.t
  let name () = "int intervals"

  let of_interval x y = (I.Fin x, I.Fin y)
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
    | Some x, Some y -> of_int (f x y)
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

  let of_excl_list l = top ()
  let is_excl_list l = false
  let to_excl_list x = None
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
  let to_bool x = if x then None else Some false 
  let is_bool x = not x
  let of_int x  = x = Int64.zero
  let to_int x  = if x then None else Some Int64.zero
  let is_int x  = not x

  let to_excl_list x = None
  let of_excl_list x = top ()
  let is_excl_list x = false
  let of_interval  x y = top ()
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
  let of_interval  x y = top ()
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
end


module ManyInts : S =
struct
  module I1 = Trier
  module I2 = Interval
  
  include Lattice.Prod (I1) (I2)
  
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

end

module IntDomList : S =
struct
  include Printable.Std
  exception IntDomListBroken
  
  module I1 = Trier
  module I2 = Interval
  
  type e = Trier of Trier.t
         | Interval of Interval.t
         | B
  
  type t = e list
  
  (* constructors *)
  
  let name () = Trier.name ()
        
  let constr_scheme xs =
    let f (s,g) y : t = 
      if get_bool ("ana.int."^s)
      then (g ()) :: y
      else y
    in
    List.fold_right f xs []

  let top () = constr_scheme
    [("trier"   ,fun () -> Trier    (Trier.top ()))
    ;("interval",fun () -> Interval (Interval.top ()))]
      
  let bot () = constr_scheme
    [("trier"   ,fun () -> Trier    (Trier.bot ()))
    ;("interval",fun () -> Interval (Interval.bot ()))]
  
  let starting x = constr_scheme
    [("trier"   ,fun () -> Trier    (Trier.starting x))
    ;("interval",fun () -> Interval (Interval.starting x))]

  let ending x = constr_scheme
    [("trier"   ,fun () -> Trier    (Trier.ending x))
    ;("interval",fun () -> Interval (Interval.ending x))]
    
  let of_bool x = constr_scheme
    [("trier"   ,fun () -> Trier    (Trier.of_bool x))
    ;("interval",fun () -> Interval (Interval.of_bool x))]
  
  let of_excl_list x = constr_scheme
    [("trier"   ,fun () -> Trier    (Trier.of_excl_list x))
    ;("interval",fun () -> Interval (Interval.of_excl_list x))]

  let of_int x = constr_scheme
    [("trier"   ,fun () -> Trier    (Trier.of_int x))
    ;("interval",fun () -> Interval (Interval.of_int x))]
  
  (* element functions *)
  
  let narrow' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.narrow x y)
      | Interval x, Interval y -> Interval (Interval.narrow x y)
      | _ -> raise IntDomListBroken

  let widen' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.widen x y)
      | Interval x, Interval y -> Interval (Interval.widen x y)
      | _ -> raise IntDomListBroken

  let is_top' x =
    match x with
      | Trier x -> Trier.is_top x
      | Interval x -> Interval.is_top x
      | _ -> raise IntDomListBroken
  
  let is_bot' x =
    match x with
      | Trier x -> Trier.is_bot x
      | Interval x -> Interval.is_bot x
      | _ -> raise IntDomListBroken

  let meet' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.meet x y)
      | Interval x, Interval y -> Interval (Interval.meet x y)
      | _ -> raise IntDomListBroken

  let join' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.join x y)
      | Interval x, Interval y -> Interval (Interval.join x y)
      | _ -> raise IntDomListBroken

  let leq' x y =
    match x, y with
      | Trier x, Trier y -> Trier.leq x y
      | Interval x, Interval y -> Interval.leq x y
      | _ -> raise IntDomListBroken
      
  let short' w x =
    match x with
      | Trier x -> Trier.short w x
      | Interval x -> Interval.short w x
      | _ -> raise IntDomListBroken
      
  let toXML_f' sf x =
    match x with
      | Trier x -> Trier.toXML_f (fun w x -> sf w (Trier x)) x
      | Interval x -> Trier.toXML_f (fun w x -> sf w (Interval x)) x
      | _ -> raise IntDomListBroken
      
  let pretty_f' sf () x =
    match x with
      | Trier x -> Trier.pretty_f (fun w x -> sf w (Trier x)) () x
      | Interval x -> Interval.pretty_f (fun w x -> sf w (Interval x)) () x
      | _ -> raise IntDomListBroken
      
  let toXML' x = toXML_f' short' x
      
  let pretty' x = pretty_f' short' x
      
  let compare' x y =
    match x, y with
      | Trier x, Trier y -> Trier.compare x y
      | Interval x, Interval y -> Interval.compare x y
      | _ -> raise IntDomListBroken

  let equal' x y =
    match x, y with
      | Trier x, Trier y -> Trier.equal x y
      | Interval x, Interval y -> Interval.equal x y
      | _ -> raise IntDomListBroken

  let logor' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.logor x y)
      | Interval x, Interval y -> Interval (Interval.logor x y)
      | _ -> raise IntDomListBroken

  let logand' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.logand x y)
      | Interval x, Interval y -> Interval (Interval.logand x y)
      | _ -> raise IntDomListBroken

  let lognot' x =
    match x with
      | Trier x -> Trier (Trier.lognot x )
      | Interval x -> Interval (Interval.lognot x )
      | _ -> raise IntDomListBroken

  let shift_right' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.shift_right x y)
      | Interval x, Interval y -> Interval (Interval.shift_right x y)
      | _ -> raise IntDomListBroken

  let shift_left' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.shift_left x y)
      | Interval x, Interval y -> Interval (Interval.shift_left x y)
      | _ -> raise IntDomListBroken

  let bitxor' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.bitxor x y)
      | Interval x, Interval y -> Interval (Interval.bitxor x y)
      | _ -> raise IntDomListBroken

  let bitor' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.bitor x y)
      | Interval x, Interval y -> Interval (Interval.bitor x y)
      | _ -> raise IntDomListBroken

  let bitand' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.bitand x y)
      | Interval x, Interval y -> Interval (Interval.bitand x y)
      | _ -> raise IntDomListBroken

  let bitnot' x =
    match x with
      | Trier x -> Trier (Trier.bitnot x)
      | Interval x -> Interval (Interval.bitnot x)
      | _ -> raise IntDomListBroken

  let ne' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.ne x y)
      | Interval x, Interval y -> Interval (Interval.ne x y)
      | _ -> raise IntDomListBroken

  let eq' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.eq x y)
      | Interval x, Interval y -> Interval (Interval.eq x y)
      | _ -> raise IntDomListBroken

  let ge' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.ge x y)
      | Interval x, Interval y -> Interval (Interval.ge x y)
      | _ -> raise IntDomListBroken

  let le' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.le x y)
      | Interval x, Interval y -> Interval (Interval.le x y)
      | _ -> raise IntDomListBroken

  let gt' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.gt x y)
      | Interval x, Interval y -> Interval (Interval.gt x y)
      | _ -> raise IntDomListBroken

  let lt' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.lt x y)
      | Interval x, Interval y -> Interval (Interval.lt x y)
      | _ -> raise IntDomListBroken

  let rem' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.rem x y)
      | Interval x, Interval y -> Interval (Interval.rem x y)
      | _ -> raise IntDomListBroken

  let div' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.div x y)
      | Interval x, Interval y -> Interval (Interval.div x y)
      | _ -> raise IntDomListBroken

  let mul' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.mul x y)
      | Interval x, Interval y -> Interval (Interval.mul x y)
      | _ -> raise IntDomListBroken

  let sub' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.sub x y)
      | Interval x, Interval y -> Interval (Interval.sub x y)
      | _ -> raise IntDomListBroken

  let add' x y =
    match x, y with
      | Trier x, Trier y -> Trier (Trier.add x y)
      | Interval x, Interval y -> Interval (Interval.add x y)
      | _ -> raise IntDomListBroken

  let neg' x =
    match x with
      | Trier x -> Trier (Trier.neg x)
      | Interval x -> Interval (Interval.neg x)
      | _ -> raise IntDomListBroken

  let hash' x =
    match x with
      | Trier x-> Trier.hash x
      | Interval x-> 17*Interval.hash x
      | _ -> raise IntDomListBroken

  let minimal' x =
    match x with
      | Trier x -> Trier.minimal x
      | Interval x -> Interval.minimal x
      | _ -> raise IntDomListBroken

  let maximal' x =
    match x with
      | Trier x -> Trier.maximal x
      | Interval x -> Interval.maximal x
      | _ -> raise IntDomListBroken

  let to_int' x =
    match x with
      | Trier x -> Trier.to_int x
      | Interval x -> Interval.to_int x
      | _ -> raise IntDomListBroken

  let to_bool' x =
    match x with
      | Trier x -> Trier.to_bool x
      | Interval x -> Interval.to_bool x
      | _ -> raise IntDomListBroken

  let to_excl_list' x =
    match x with
      | Trier x -> Trier.to_excl_list x
      | Interval x -> Interval.to_excl_list x
      | _ -> raise IntDomListBroken

  let is_excl_list' x =
    match x with
      | Trier x -> Trier.is_excl_list x
      | Interval x -> Interval.is_excl_list x
      | _ -> raise IntDomListBroken

  let is_bool' x =
    match x with
      | Trier x -> Trier.is_bool x
      | Interval x -> Interval.is_bool x
      | _ -> raise IntDomListBroken

  let is_int' x =
    match x with
      | Trier x -> Trier.is_int x
      | Interval x -> Interval.is_int x
      | _ -> raise IntDomListBroken
  
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
  
  let compare =
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

end
