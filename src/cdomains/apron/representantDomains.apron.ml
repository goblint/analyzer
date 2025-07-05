open Batteries
open GoblintCil
open GobApron

module M = Messages

module Rhs = LinearTwoVarEqualityDomain.Rhs

module EConj = LinearTwoVarEqualityDomain.EqualitiesConjunction

module IntMap = EConj.IntMap

(*Modules for creating an unbounded interval arithmethic with the existing interval domain*)
module TopIntBase (Int_t : IntOps.IntOpsBase) =  
struct
  type sign = Pos | Neg [@@deriving eq, hash]
  type t = Int of Int_t.t 
         | Top of sign [@@deriving eq, hash]

  let compare a b = match a, b with
    | Int a, Int b -> Int_t.compare a b
    | Top Neg, Top Neg 
    | Top Pos, Top Pos -> 0
    | _ , Top Pos
    | Top Neg, _ -> -1
    | _ , Top Neg 
    | Top Pos, _ -> 1

  let max_val = Int_t.add Int_t.one @@ Int_t.of_bigint @@ snd @@ IntDomain0.Size.range IULongLong
  let min_val = Int_t.add (Int_t.of_int @@ -1) @@ Int_t.of_bigint @@ fst @@ IntDomain0.Size.range ILongLong

  let get_int_t = function
    | Int i -> i
    | Top Pos -> max_val (*needed so that we can call to_bigint on Top (e.g. for widening constants)*)
    | Top Neg -> min_val

  let neg_s = function
    | Pos -> Neg
    | Neg -> Pos


  let lift2 op t1 t2 = match t1, t2 with 
      Int t1, Int t2 -> Int (op t1 t2) 
    | Top Neg, Top Pos
    | Top Pos, Top Neg -> Top Neg
    | Top s, _
    | _, Top s -> Top s

  let lift2_1 op t1 t2 = match t1 with
    | Int t1 -> Int (op t1 t2) 
    | t -> t

  let name () = Int_t.name () ^ " with top" 

  let zero = Int (Int_t.zero)
  let one = Int (Int_t.one)

  let lower_bound = Some (Top Neg)
  let upper_bound = Some (Top Pos)

  let neg = function
    | Int i -> Int (Int_t.neg i)
    | Top Pos -> Top Neg
    | Top Neg -> Top Pos
  let abs = function
    | Int i -> Int (Int_t.abs i)
    | Top _ -> Top Pos

  let add a b = match a,b with
    | Int a, Int b -> Int (Int_t.add a b)
    | Top s, _ 
    | _, Top s -> Top s
  let sub a b = match a,b with
    | Int a, Int b -> Int (Int_t.sub a b)
    | Top s, _ -> Top s
    | Int _, Top Pos -> Top Neg
    | Int _, Top Neg -> Top Pos


  let mul a b = match a,b with
    | Int a, Int b -> Int (Int_t.mul a b)
    | Top s, Int x 
    | Int x, Top s ->
      let comp = Int_t.compare x Int_t.zero in
      if comp = 0 then Int (Int_t.zero)
      else if comp <  0 then Top (neg_s s)
      else Top s
    | Top _, Top _ -> Top Pos (*TODO: Does not make sense. Does it need to?*)
  let div a b = match a,b with
    | Int a, Int b -> Int (Int_t.div a b)
    | Top s, Int x ->
      let comp = Int_t.compare x Int_t.zero in
      if comp = 0 then Int (Int_t.zero)
      else if comp <  0 then Top (neg_s s)
      else Top s
    | Int x, Top s -> Int (Int_t.zero)
    | Top _, Top _ -> Top Pos (*TODO: Does not make sense. Does it need to?*)

  (*TODO will rem/gcd/shift/logical functions lead to problems??*)
  let rem = lift2 Int_t.rem
  let gcd = lift2 Int_t.gcd

  let shift_left = lift2_1 Int_t.shift_left
  let shift_right = lift2_1 Int_t.shift_right
  let logand = lift2 Int_t.logand
  let logor = lift2 Int_t.logor
  let logxor = lift2 Int_t.logxor
  let lognot = function
    | Int i ->  Int (Int_t.lognot i)
    | t -> t 

  (**TODO not clear what this should do*)
  let top_range _ _ = false
  let max a b =
    match a,b with
    | Top Neg, m
    | m, Top Neg -> m
    | Top Pos, _
    | _, Top Pos -> Top Pos
    | Int a, Int b -> Int (Int_t.max a b)
  let min a b =     
    match a,b with
    | Top Pos, m
    | m, Top Pos -> m
    | Top Neg, _
    | _, Top Neg -> Top Neg
    | Int a, Int b -> Int (Int_t.min a b)

  let of_int i = Int (Int_t.of_int i)
  let to_int t =  Int_t.to_int @@ get_int_t t

  let of_int64 i = Int (Int_t.of_int64 i)
  let to_int64 t = Int_t.to_int64 @@ get_int_t t

  let of_string s = if s = "+∞" then Top Pos else (if s = "-∞" then Top Pos else Int (Int_t.of_string s))
  let to_string = function
    | Int i ->  Int_t.to_string i
    | Top Pos -> "+∞"
    | Top Neg -> "-∞"

  (*Normalizes values outside the maximum range. Normalization is not done anywhere else 
    because we may temporarily have values outside that range e.g. when applying an equations*)
  let of_bigint i = let i = Int_t.of_bigint i in 
    if Int_t.compare i max_val >= 0 
    then Top Pos 
    else if Int_t.compare i min_val <= 0
    then Top Neg
    else Int i 
  let to_bigint t = Int_t.to_bigint @@ get_int_t t

  let arbitrary () = failwith "arbitrary not implemented"
end

(*TODO this is a copy of the IntOpsDecorator, but we keep the constructor of type t -> is there a better way??*)
module TopIntOps = struct

  include Printable.StdLeaf
  include TopIntBase(IntOps.BigIntOpsBase)
  let show = to_string
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = to_string
    end
    )
  let pred x = sub x one
  let of_bool x = if x then one else zero
  let to_bool x = x <> zero

  (* These are logical operations in the C sense! *)
  let log_op op a b = of_bool @@ op (to_bool a) (to_bool b)
  let c_lognot x = of_bool (x = zero)
  let c_logand = log_op (&&)
  let c_logor = log_op (||)
  let c_logxor = log_op (<>)

  let lt x y = of_bool (compare x y < 0)
  let gt x y = of_bool (compare x y > 0)
  let le x y = of_bool (compare x y <= 0)
  let ge x y = of_bool (compare x y >= 0)

end

module Unbounded : IntervalDomain.BoundedIntOps with type t = TopIntOps.t = struct
  include TopIntOps

  type t_interval = (t * t) option [@@deriving eq, ord, hash]

  let range _ = (Top Neg, Top Pos)
  let top_of ik = Some (range ik)
  let bot_of _ = None

  let norm ?(suppress_ovwarn=false) ?(cast=false) ik t = 
    let t = match t with 
      | Some (Top Pos, Top Neg) -> Some (Top Neg, Top Pos)
      | Some (l, Top Neg) ->  Some (l, Top Pos)
      | Some (Top Pos, u) ->  Some (Top Neg, u)
      | Some (Int a, Int b) when Z.compare a b > 0 -> None
      | _ -> t
    in (t,IntDomain0.{underflow=false; overflow=false})
end

(*Combining operations into one reduced product for values*)
module IntervalAndCongruence = struct
  module I = IntDomain0.SOverflowUnlifter(IntervalDomain.BoundedIntervalFunctor(Unbounded))
  module C = CongruenceDomain.CongruenceFunctor(CongruenceDomain.NoWrapping)

  type t = I.t * C.t [@@deriving eq, ord, hash]

  let show (i,c) = I.show i ^ "," ^ C.show c 

  let ik = IChar (*Placeholder for all functions that need one. Should not matter, but choosen small so that errors are detected with smaller numbers already*)

  let top = I.top_of ik, C.top_of ik

  let is_top = equal top

  let is_bot = function
    | None, None -> true
    | _,_ -> false

  let of_bigint x = (I.of_int ik (TopIntOps.of_bigint x), C.of_int ik x)

  let leq (i1,c1) (i2,c2) = I.leq i1 i2 && C.leq c1 c2

  let contains t v = leq (of_bigint v) t 

  let contains t v = 
    let res = contains t v in
    if M.tracing then M.tracel "contains" "is %s conained in %s -> %b" (Z.to_string v) (show t) (res);
    res

  let refine t = 
    let refine_step (i,c) = 
      let c' = match i with 
        | Some (TopIntOps.Int x, TopIntOps.Int y) -> C.refine_with_interval ik c (Some (x,y))
        | Some _ -> c (*No refinement possible if one side is infinite*)
        | _ -> None
      in 
      (I.refine_with_congruence ik i (BatOption.map (fun (x,y) -> (TopIntOps.Int x,TopIntOps.Int y)) c) ), c'
    in
    let t' = refine_step t in
    if t' = t then t else refine_step t' (*The second refinement is necessary if the refinement leads to a constant, otherwise not*)

  let add (i1,c1) (i2,c2) = refine (I.add ~no_ov:true ik i1 i2, C.add ~no_ov:true ik c1 c2)

  let sub (i1,c1) (i2,c2) = refine (I.sub ~no_ov:true ik i1 i2, C.sub ~no_ov:true ik c1 c2)

  let mul (i1,c1) (i2,c2) = refine (I.mul ~no_ov:true ik i1 i2, C.mul ~no_ov:true ik c1 c2)

  let div (i1,c1) (i2,c2) = refine (I.div ~no_ov:true ik i1 i2, C.div ~no_ov:true ik c1 c2)

  let rem (i1,c1) (i2,c2) = refine (I.rem ik i1 i2, C.rem ik c1 c2)

  let neg (i,c) = refine (I.neg ~no_ov:true ik i, C.neg ~no_ov:true ik c)

  let to_int (i,_) = I.to_int i 

  let meet (i1,c1) (i2,c2) = refine (I.meet ik i1 i2, C.meet ik c1 c2)

  let narrow (i1,c1) (i2,c2) = refine (I.narrow ik i1 i2, C.narrow ik c1 c2)

  let join (i1,c1) (i2,c2) = refine (I.join ik i1 i2, C.join ik c1 c2)
  let widen (i1,c1) (i2,c2) = refine (I.widen ik i1 i2, C.widen ik c1 c2)

  let of_congruence c = refine (I.top_of ik, C.of_congruence ik c)

  let must_be_pos (i,_) = I.leq i (I.starting ik (Int Z.one))

  let must_be_neg (i,_) = I.leq i (I.ending ik (Int (Z.neg Z.one)))

  let starting x = refine (I.starting ik (Int x), C.top_of ik)

  let ending x = refine (I.ending ik (Int x), C.top_of ik)

  let maximal (i,_) = I.maximal i
  let minimal (i,_) = I.minimal i

  let of_IntDomTuple tuple = 
    let interval = match IntDomain.IntDomTuple.minimal tuple, IntDomain.IntDomTuple.maximal tuple with 
      | Some min, Some max -> Some ( TopIntOps.Int min, TopIntOps.Int max)
      | _ -> None
    in refine (interval, IntDomain.IntDomTuple.to_congruence tuple) 

  let invariant env var ((i,c):t) acc =
    let with_i = 
      let lower l = 
        (*x - l >= 0*)
        let expr = Linexpr1.make env in
        Linexpr1.set_coeff expr (Environment.var_of_dim env var) (GobApron.Coeff.s_of_int 1);
        Linexpr1.set_cst expr (GobApron.Coeff.s_of_z @@ Z.neg l);
        Lincons1.make expr Lincons0.SUPEQ
      in
      let higher u = 
        (*-x + u >= 0*)
        let expr = Linexpr1.make env in
        Linexpr1.set_coeff expr (Environment.var_of_dim env var) (GobApron.Coeff.s_of_int (-1));
        Linexpr1.set_cst expr (GobApron.Coeff.s_of_z u);
        Lincons1.make expr Lincons0.SUPEQ
      in
      match i with
      | Some (TopIntOps.Int l, TopIntOps.Int u) -> lower l :: higher u :: acc
      | Some (TopIntOps.Int l, _) -> lower l :: acc
      | Some (_, TopIntOps.Int u) -> higher u :: acc
      | _ -> acc
    in match c with
    | Some (o, m) -> 
      (*x-o % m = 0*)
      let expr = Linexpr1.make env in
      Linexpr1.set_coeff expr (Environment.var_of_dim env var) (GobApron.Coeff.s_of_int 1);
      Linexpr1.set_cst expr (GobApron.Coeff.s_of_z @@ Z.neg o);
      Lincons1.make expr (Lincons0.EQMOD (Mpqf (Z_mlgmpidl.mpqf_of_q @@ Q.of_bigint m))) :: with_i
    | _ -> Lincons1.make_unsat env :: with_i
end 

module Value = IntervalAndCongruence

(*The type used for the simulated domain*)
module Relation = struct
  type cond = Lt | Gt
  type t = cond * Z.t

  let show_cond c = match c with 
    | Lt -> "<"
    | Gt -> ">"

  let show x (c,o) y = x ^ show_cond c ^ y ^ " + " ^ Z.to_string o 

  let swap_sides (cond, o) =
    let o' = Z.neg o in
    match cond with 
    | Lt -> Gt, o'
    | Gt -> Lt, o'

  (*Tries to combine two relations, with the variable on the rhs of the first condition being equal to the one at the lhs of the second*)
  let combine (c1, o1) (c2, o2) = match c1, c2 with
    | Lt, Lt -> Some ( Lt, Z.add o1 @@ Z.succ o2 )
    | Gt, Gt -> Some ( Gt, Z.add o1 @@ Z.succ o2 )
    | Lt, Gt 
    | Gt, Lt -> None


  let value_with_const_right (cond,o) const = 
    let open Z in
    match cond with
    | Lt -> Value.ending (o + const - one)
    | Gt -> Value.starting (o + const + one)

  let value_with_const_left t const =  value_with_const_right (swap_sides t) const

end

module Refinement = struct 
  type t = (int * (Value.t, Rhs.t) Either.t) list

  let show_formatted_single formatter (var, ref) = 
    let rhs = match ref with 
      | Either.Left x -> Value.show x
      | Right x -> Rhs.show_rhs_formatted formatter x 
    in
    Printf.sprintf "%s = %s" (formatter var) rhs

  let show_formatted formatter t = "[" ^ List.fold (fun acc r -> Printf.sprintf "%s, %s" (show_formatted_single formatter r) acc) "]" t

  let show = show_formatted (fun x -> "var_"^Int.to_string x)

  let of_value var v = (var, Either.Left v)
  let of_rhs var r = (var, Either.right r)

  let rhs_only t = List.filter (fun x -> BatEither.is_right (snd x)) t

end

module type TwoVarInequalities = sig
  type t

  (*returns the best lower and upper bound for the relation between variables with the given Rhs*)
  val get_relations :  ((Z.t * int * Z.t * Z.t) * Value.t) -> ((Z.t * int * Z.t * Z.t) * Value.t) -> t -> Relation.t list

  (*meet a single relation between two variables.*)
  val meet_relation : int -> int -> Relation.t -> (int -> Rhs.t) -> (int -> Value.t) -> t -> t * Refinement.t

  (*substitutes all occurences of a variable by a rhs*)
  val substitute : (int -> Value.t) -> t -> int -> Z.t * int * Z.t * Z.t -> t * Refinement.t

  (*called after every operation to limit the inequalities to the most relevant*)
  val limit : EConj.t -> t -> t

  val meet : (int -> Value.t) -> t -> t -> t * Refinement.t
  val narrow : (int -> Value.t) -> t -> t -> t * Refinement.t

  val leq : t -> (int -> Value.t) -> t -> bool

  val join : t -> (int -> Value.t) -> t -> (int -> Value.t) -> t
  val widen : t -> (int -> Value.t) -> t -> (int -> Value.t) -> t

  (*second loop of transform: e.g. a join can split groups of variables. This function copies the relevant inequalities to all new representants*)
  val copy_to_new_representants : EConj.t -> EConj.t -> (int -> Value.t) -> t -> t

  (*restore inequalities after an assignment that makes the assigned-to variable have a known relation to before the assignment*)
  val transfer : int -> Relation.t -> t -> (int -> Rhs.t) -> (int -> Value.t) -> t -> (int -> Rhs.t) -> (int -> Value.t) -> t * Refinement.t

  val show_formatted : (int -> string) -> t -> string
  val hash : t -> int
  val empty : t
  val is_empty : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val modify_variables_in_domain : t -> int array -> (int -> int -> int) -> t
  val forget_variable : t -> int -> t

  val interval_refinements : (int -> Value.t) -> t ->  Refinement.t
  val set_constant : int -> Z.t -> t -> t * Refinement.t

  val invariant : t -> Environment.t -> Lincons1.t list

end

module NoInequalties : TwoVarInequalities = struct
  type t = unit

  let get_relations _ _ _ = []
  let meet_relation _ _ _ _ _ _ = (), []

  let limit _ _ = ()

  let meet _ _ _ = () , []
  let narrow _ _ _ = () , []

  let leq _ _ _ = true
  let join _ _ _ _ = ()
  let widen _ _ _ _ = ()

  let show_formatted _ _ = "{}"
  let hash _ = 3
  let empty = ()
  let is_empty _ = true
  let equal _ _ = true
  let compare _ _ = 0
  let modify_variables_in_domain _ _ _ = ()
  let forget_variable _ _ = ()

  let substitute _ _ _ _ = (), []

  let copy_to_new_representants _ _ _ _ = ()

  let transfer _ _ _ _ _ _ _ _ = (), []

  let interval_refinements _ _ = []

  let set_constant _ _ _ = (), []

  let invariant _ _ = []

end

let qhash q = Z.hash (Q.num q) + 13 * Z.hash (Q.den q)
let round_up q = Z.cdiv (Q.num q) (Q.den q)
let round_down q = Z.fdiv (Q.num q) (Q.den q)

module LinearInequality = struct
  (*Normalised representation of an inequality through the origin 
    a/b x <= y (or >=) bzw. slope and direction. infinite slope represents 0 <= x / 0 >= x*)
  module OriginInequality = struct (*Separate module so we can use it as key in a map*)
    type t = LE of Q.t | GE of Q.t (*TODO rename into LE / GE*)

    (*make the representation of inequalities without y unique*)
    let norm = function
      | GE s when Q.equal s Q.minus_inf -> LE Q.inf
      | LE s when Q.equal s Q.minus_inf -> GE Q.inf
      | t -> t


    (*We want the inequalities to be ordered by angle (with arbitrary start point and direction), which is tan(slope) (+ pi for other direction) *)
    (*TODO: this is called very often -> performance optimisation?*)
    let compare t1 t2 =
      let classify t = 
        let a,b = match t with  
          | LE s when Q.equal s Q.inf -> (Q.one,Q.zero)
          | GE s when Q.equal s Q.inf -> (Q.minus_one, Q.zero)
          | LE s -> (s,Q.minus_one)
          | GE s -> (Q.neg s, Q.one)
        in let open Q in
        let c = if a < zero then 
            Int.(-) 7 (sign b)
          else if a = zero then 
            (if b <= zero then 1 else 5)
          else 
            Int.(+) 3 (sign b)
        in a, b, c
      in
      let a1, b1, class1 = classify t1 in
      let a2, b2, class2 = classify t2 in
      if class1 <> class2 then 
        class1 - class2
      else
        let open Q in compare  (a2*b1) (a1 * b2) (*different from paper , but otherwise wrong?*)

    let equal t1 t2 = 0 = compare t1 t2

    let hash = function LE q -> qhash q | GE q -> 7 * qhash q  

    let get_slope = function LE a -> a | GE a -> a

    let negate = function 
      | LE s -> GE s
      | GE s -> LE s

  end

  (*add an offset to the inequalities*)
  type t = OriginInequality.t * Q.t [@@deriving eq]

  let show x y (k,c) = 
    let show_var coeff var show_zero = 
      let open Z in 
      if coeff = zero then (if show_zero then "0 " else "")
      else if coeff = one then var ^ " "
      else to_string coeff ^ var ^ " "
    in
    let show_offset o = 
      let open Q in 
      if o = zero then ""
      else if o > zero then "+ " ^ to_string o
      else "- " ^ to_string (abs o)
    in
    let s = OriginInequality.get_slope k in
    Printf.sprintf "%s%s %s%s" 
      (show_var (Q.num s) x true) 
      (match k with LE _ -> "<=" | GE _ -> ">=") 
      (show_var (Q.den s) y (Q.equal c Q.zero)) 
      (show_offset @@ if Q.equal s Q.inf then c else Q.mul c @@ Q.of_bigint @@ Q.den s )

  (*Convert into coefficients of inequality ax + by <= c
    Useful because the TVLI paper (DOI: 10.1007/3-540-45013-0_7) uses this representation *)
  let to_coeffs = function
    | OriginInequality.LE s, c when Q.equal s Q.inf -> (Q.one,Q.zero,c)
    | GE s, c when Q.equal s Q.inf -> (Q.minus_one, Q.zero, Q.neg c)
    | LE s, c -> (s,Q.minus_one,c)
    | GE s, c -> (Q.neg s, Q.one, Q.neg c)

  let to_coeffs t = 
    let a,b,c as res = to_coeffs t in
    if M.tracing then M.trace "entails" "slope %s, whole: %s -> %s,%s,%s" (Q.to_string @@ OriginInequality.get_slope @@ fst t) (show "x" "y" t)  (Q.to_string a) (Q.to_string b) (Q.to_string c);
    res

  (*From TVLI: check if one or two inequalities imply an inequality*)
  let entails1 (s1,c1) (s2,c2) = OriginInequality.equal s1 s2 &&  match s1 with LE _ -> Q.leq c1 c2 | GE _ -> Q.geq c1 c2

  let entails1 t1 t2 = 
    let res = entails1 t1 t2 in
    if M.tracing then M.trace "entails" "%s |= %s ? %b" (show "x" "y" t1) (show "x" "y" t2) res;
    res

  let entails2 t1 t2 t = 
    let (a1,b1,c1) = to_coeffs t1 in
    let (a2,b2,c2) = to_coeffs t2 in
    let (a ,b ,c ) = to_coeffs t  in
    if M.tracing then M.trace "entails" "coeffs: %s,%s,%s  %s,%s,%s  %s,%s,%s" (Q.to_string a1) (Q.to_string b1) (Q.to_string c1) (Q.to_string a2) (Q.to_string b2) (Q.to_string c2) (Q.to_string a) (Q.to_string b) (Q.to_string c);
    let open Q in
    let d = a1 * b2 - a2 * b1 in
    if equal d zero then 
      entails1 t1 t || entails1 t2 t
    else
      let l1 = (a * b2 - a2 * b) / d in
      let l2 = (a1 * b - a * b1) / d in
      geq l1 zero && geq l2 zero && geq c @@ l1 * c1 + l2 * c2

  let entails2 t1 t2 t = 
    let res = entails2 t1 t2 t in
    if M.tracing then M.trace "entails" "%s , %s |= %s ? %b" (show "x" "y" t1) (show "x" "y" t2) (show "x" "y" t) res;
    res

  (*Calculate the best inequality with a fixed slope implied by two inequalities. Assumes the searched slope to be different to the known ones*)
  let best_entailed t1 t2 k = 
    let (a1,b1,c1) = to_coeffs t1 in
    let (a2,b2,c2) = to_coeffs t2 in
    let (a ,b ,_ ) = to_coeffs (k,Q.zero)  in
    let open Q in
    let d = a1 * b2 - a2 * b1 in
    if equal d zero then 
      None
    else
      let l1 = (a * b2 - a2 * b) / d in
      let l2 = (a1 * b - a * b1) / d in
      if not (geq l1 zero && geq l2 zero)
      then None
      else 
        let c = l1 * c1 + l2 * c2 in
        let c' = match k with LE _ -> c | GE _ -> neg c in
        Some c'

  (*convert interval information into inequalities*)
  let from_values x_val y_val =
    let open OriginInequality in
    let ineqs = match Value.maximal x_val with 
      | Some (Int z) -> (*x <= z *) [LE Q.inf, Q.of_bigint z] 
      | _ -> []
    in let ineqs = match Value.minimal x_val with 
        | Some (Int z) -> (*x >= z *) (GE Q.inf, Q.of_bigint z) :: ineqs
        | _ -> ineqs
    in let ineqs = match Value.maximal y_val with 
        | Some (Int z) -> (*y <= z *) (GE Q.zero, Q.neg @@ Q.of_bigint z ) :: ineqs
        | _ -> ineqs
    in let ineqs = match Value.minimal y_val with 
        | Some (Int z) -> (*y >= z *) (LE Q.zero, Q.neg @@ Q.of_bigint z ) :: ineqs
        | _ -> ineqs
    in ineqs

  (*Convert two righthandsides into an inequality isLessThan is the direction of the inequality and needed for making the inequality non-strict while we still know that all variables are integers*)
  let from_rhss (cx, ox, dx) (cy, oy, dy) isLessThan_opt = 
    let non_strict_offset = match isLessThan_opt with None -> Q.zero | Some isLessThan -> if isLessThan then Q.minus_one else Q.one in
    let a,b,c = (Q.make cx dx, Q.make cy dy, Q.add non_strict_offset @@ Q.sub (Q.make oy dy) (Q.make ox dx)) in (*subtracting one to convert it into a nonstrict inequality*)
    let s = Q.div a b in
    if Q.equal b Q.zero 
    then OriginInequality.norm (LE s), Q.div c a
    else if Q.gt b Q.zero 
    then LE s, Q.div c b 
    else GE s, Q.div c b


  (*apply the transformation to the variable on the left side*)
  let substitute_left (coeff, offs, divi) (k,o) =
    let open OriginInequality in
    let s = get_slope k in
    let s' = Q.mul s (Q.make coeff divi) in
    let o' = Q.sub o @@ Q.mul s @@ Q.make offs divi in
    match k with 
    | LE _ -> LE s', o'
    | GE _ -> GE s', o'

  (*apply the transformation to the variable on the right side*)
  let substitute_right (coeff, offs, divi) (k,o) = 
    let open OriginInequality in
    let s = get_slope k in
    let f = Q.make coeff divi in
    let s' = Q.div s f in
    let o' = Q.add (Q.div o f) @@ Q.make offs coeff in
    let k' = match k with 
      | LE _ -> LE s'
      | GE _ -> GE s'
    in if Q.lt f Q.zero 
    then (negate k', o')
    else k', o' 

  let swap_sides (k,o) =
    let open Q in
    let open OriginInequality in  
    match k with 
    | LE s when s < zero -> (LE (inv s), - (o / s))
    | LE s -> (GE (inv s), - (o / s))
    | GE s when s < zero -> (GE (inv s), - (o / s))
    | GE s -> (LE (inv s), - (o / s))

  (*combine an inequaliy x_old -> x_new with x_old -> y to x_new -> y*)
  let combine_left (k_rel, o_rel) (k, o) =
    let open OriginInequality in 
    (*factor we need to multiply rel with so that x_old has the same coefficient in both inequalities *)
    let f = Q.div (get_slope k) (get_slope k_rel) in
    let k_rel' = if Q.geq f Q.zero then k_rel else negate k_rel in
    match k_rel', k with 
    | LE _, LE _ 
    | GE _, GE _ -> None (*no useable inequality x_new -> y*)
    | GE _, LE _ -> Some (LE f, Q.sub o (Q.mul f o_rel))
    | LE _, GE _ -> Some (GE f, Q.sub o (Q.mul f o_rel))

  let combine_left rel t = 
    let res = combine_left rel t in
    if M.tracing then M.trace "combine" "combine_left %s with %s -> %s" (show "x_old" "x_new" rel) (show "x_old" "y" t) (BatOption.map_default (fun res -> show "x_new" "y" res) "Nothing" res);
    res

  (*combine an inequaliy y_old -> y_new with x -> y_old to x-> y_new*)
  let combine_right (k_rel, o_rel) (k, o) = 
    let open OriginInequality in 
    (*factor we need to multiply the inequality x -> y_old with so that y_old has the same coefficient in both inequalities *)
    let f = get_slope k_rel in
    let k' = if Q.geq f Q.zero then k else negate k in
    match k_rel, k' with 
    | LE _, GE _
    | GE _, LE _ -> None 
    | LE s_rel, LE s -> Some (LE (Q.mul s f), Q.add o_rel @@ Q.mul f o)
    | GE s_rel, GE s -> Some (GE (Q.mul s f), Q.add o_rel @@ Q.mul f o) 

  let combine_right rel t = 
    let res = combine_right rel t in
    if M.tracing then M.trace "combine" "combine_right %s with %s -> %s" (show "y_old" "y_new" rel) (show "x" "y_old" t) (BatOption.map_default (fun res -> show "x" "y_new" res) "Nothing" res);
    res

  let set_constant_lhs rhs_var const (k,o) ref_acc = 
    let open Q in 
    let s = OriginInequality.get_slope k in
    let bound = s * of_bigint const - o in
    let new_ref = match k with 
      | LE _ -> Refinement.of_value rhs_var @@ Value.starting @@ round_up bound
      | GE _ -> Refinement.of_value rhs_var @@ Value.ending @@ round_down bound
    in new_ref :: ref_acc 

  let set_constant_rhs lhs_var const (k,o) ref_acc = 
    let open Q in 
    let s = OriginInequality.get_slope k in
    let bound = (of_bigint const + o) / s in
    let is_leq = match k with 
      | LE _ -> s > zero
      | GE _ -> s < zero 
    in let new_ref = 
         if is_leq then
           Refinement.of_value lhs_var @@ Value.ending @@ round_down bound
         else
           Refinement.of_value lhs_var @@ Value.starting @@ round_up bound

    in new_ref :: ref_acc 

  let invariant env x y k o acc = 
    (*for LE, we need to swap signs of all coefficients*)
    let s, o = match k with
      | OriginInequality.LE s -> Q.neg s, Q.neg o 
      | GE s -> s, o
    in
    let o' = if Q.equal s Q.inf then Q.neg o else Q.neg @@ Q.mul o @@ Q.of_bigint @@ Q.den s in
    let coeffs = [
      GobApron.Coeff.s_of_z (Q.num s), Environment.var_of_dim env x; 
      GobApron.Coeff.s_of_z @@ Z.neg @@ Q.den s, Environment.var_of_dim env y
    ] in
    let expr = Linexpr1.make env in
    Linexpr1.set_list expr coeffs  (Some (GobApron.Coeff.s_of_mpq @@ Z_mlgmpidl.mpq_of_q o')); 
    Lincons1.make expr Lincons0.SUPEQ :: acc

end

(*very small wrapper to make the following code clearer to me*)
module MultiSet = struct
  module M = BatHashtbl

  type 'a t = ('a, int) M.t

  let create ?(initial_size = 5) () = M.create initial_size

  let change_member_count (ms : 'a t) (x : 'a) (count : int) = M.modify_def 0 x ((+) count) ms

  let iter = M.iter

end

let coeffs_from_econj (dim, map) =
  let m = BatHashtbl.create @@ IntMap.cardinal map in (*This is an overestimation*)
  let add_rhs _ = function
    | (Some (cy,y),oy,dy) ->               
      let s = Q.make cy dy in
      begin 
        match BatHashtbl.find_option m y with 
        | None -> 
          let set = MultiSet.create () in
          MultiSet.change_member_count set s 1;
          BatHashtbl.add m y set;
        | Some set -> MultiSet.change_member_count set s 1;
      end
    | _ -> () (*ignore constants*)
  in
  IntMap.iter add_rhs map;
  m

let coeffs_from_econj = Timing.wrap "coeffs" coeffs_from_econj

(*assumes x < y*)
let slopes_from_coeffs mapping (x,y) =
  let x_coeffs = BatHashtbl.find_default mapping x (MultiSet.create ~initial_size:1 ()) in 
  let y_coeffs = BatHashtbl.find_default mapping y (MultiSet.create ~initial_size:1 ()) in
  (*We do not explicetly store the representants coefficient -> add it here*)
  MultiSet.change_member_count x_coeffs Q.one 1;
  MultiSet.change_member_count y_coeffs Q.one 1;
  let slopes = MultiSet.create ~initial_size:(BatHashtbl.length x_coeffs * BatHashtbl.length y_coeffs) () in
  MultiSet.iter (fun cx cx_count -> MultiSet.iter (fun cy cy_count -> let s = Q.div cx cy in MultiSet.change_member_count slopes s (cx_count * cy_count)) y_coeffs) x_coeffs;
  slopes

let slopes_from_coeffs = Timing.wrap "slopes" slopes_from_coeffs

(*List of inequalities ax < by + c, mapping a and b to c*)
(*We need to make sure that x has lower index than y to keep this representation unique! *)
module TwoVarInequalitySet = struct
  module Key = LinearInequality.OriginInequality
  module CoeffMap = Map.Make(Key)

  type t = Q.t CoeffMap.t [@@deriving eq, ord]

  let hash t = CoeffMap.fold (fun k c acc -> qhash c + 17 * Key.hash k ) t 0

  let show_formatted x y t = 
    CoeffMap.fold (fun k c acc -> Printf.sprintf "%s , %s" (LinearInequality.show x y (k,c)) acc) t ""

  let empty = CoeffMap.empty


  let ignore_empty ls = 
    if CoeffMap.is_empty ls then None
    else Some ls

  (*limit how many inequalities we are saving: only keep inequalities with slopes that correspond to variables. 
    optionally, limit it further to the slopes that correspond to the most inequalities *)
  let limit slopes t = 
    let opt =  GobConfig.get_string "ana.lin2vareq_p.inequalities" in
    if opt = "unlimited" then ignore_empty t else
      let open LinearInequality.OriginInequality in
      let filtered = CoeffMap.filter (fun k c -> BatHashtbl.mem slopes (get_slope k) ) t in
      let keep = 
        if opt = "coeffs_count" then 
          GobConfig.get_int "ana.lin2vareq_p.coeffs_count" (*TODO for a fixed number, we do not need to sort but could instead use an O(n algorithm!)*)
        else
          let f = GobConfig.get_int "ana.lin2vareq_p.coeffs_threshold" in
          let total = BatHashtbl.fold (fun _ c acc -> c + acc) slopes 0 in
          max 4 @@ (total * f) / 100 (*allow a minimum of 4 inequality angles *)
      in
      let comp (k1,_) (k2,_) = 
        let v1 = BatHashtbl.find_default slopes (get_slope k1) 0 in
        let v2 = BatHashtbl.find_default slopes (get_slope k2) 0 in
        v2 - v1 (*list sorts ascending, we need descending -> inverted comparison*)
      in
      (*skip sorting if we keep all inequalities*)
      if keep >= CoeffMap.cardinal filtered then 
        ignore_empty filtered 
      else
        ignore_empty @@ CoeffMap.of_list @@ List.take keep @@ List.sort comp @@ CoeffMap.bindings filtered

  (*get the next key in anti-clockwise order*)
  let get_previous k t =
    match CoeffMap.find_first_opt (fun key -> Key.compare key k > 0) t with
    | None -> CoeffMap.min_binding_opt t (*there is no larger key -> take the first one*)
    | s -> s

  (*get the next key in clockwise order*)
  let get_next k t =
    match CoeffMap.find_last_opt (fun key -> Key.compare key k < 0) t with
    | None -> CoeffMap.max_binding_opt t (*there is no smaller key -> take the last one*)
    | s -> s

  (*adds the inequality while removing redundant ones. assumes that there is no inequality with this key already in the map*)
  let add_inequality k c t = 
    match get_previous k t, get_next k t with 
    | None, None -> CoeffMap.add k c t (* the map is empty *)
    | Some prev, Some next -> 
      if LinearInequality.entails2 prev next (k,c) then (
        if M.tracing then M.trace "add_ineq" "new %s entailed by prev: %s, next %s"  (LinearInequality.show "x" "y" (k,c))  (LinearInequality.show "x" "y" prev) (LinearInequality.show "x" "y" next);
        t) (*new inequality is already implied*)
      else (*check in both direction if the next inequality is now implied, and remove those that are. recursive because multiple may now be implied*)
        let rec remove_prev prev t = 
          match get_previous (fst prev) t with
          | None -> 
            if M.tracing then M.trace "add_ineq" "remove_prev?: none left to remove";
            t
          | Some prev_prev -> 
            if LinearInequality.equal prev prev_prev then begin
              if M.tracing then M.trace "add_ineq" "remove_prev?: only one left in %s -> no removal" (show_formatted "x" "y" t);
              t
            end else if LinearInequality.entails2 prev_prev (k,c) prev then begin
              if M.tracing then M.trace "add_ineq" "remove_prev?: new: %s and prev_prev: %s imply prev %s -> removing and continuing"  (LinearInequality.show "x" "y" (k,c))  (LinearInequality.show "x" "y" prev_prev) (LinearInequality.show "x" "y" prev);
              remove_prev prev_prev @@ CoeffMap.remove (fst prev) t
            end else begin
              if M.tracing then M.trace "add_ineq" "remove_prev?: new: %s and prev_prev: %s do not imply prev %s -> stop"  (LinearInequality.show "x" "y" (k,c))  (LinearInequality.show "x" "y" prev_prev) (LinearInequality.show "x" "y" prev);
              t
            end
        in let rec remove_next next t = 
             match get_next (fst next) t with
             | None -> 
               if M.tracing then M.trace "add_ineq" "remove_next?: none left to remove";
               t
             | Some next_next -> 
               if LinearInequality.equal next next_next then begin
                 if M.tracing then M.trace "add_ineq" "remove_next?: only one left in %s -> no removal" (show_formatted "x" "y" t);
                 t
               end else if LinearInequality.entails2 next_next (k,c) next then begin
                 if M.tracing then M.trace "add_ineq" "remove_next?: new: %s and next_next: %s imply next %s -> removing and continuing"  (LinearInequality.show "x" "y" (k,c))  (LinearInequality.show "x" "y" next_next) (LinearInequality.show "x" "y" next);
                 remove_next next_next @@ CoeffMap.remove (fst next) t
               end else begin
                 if M.tracing then M.trace "add_ineq" "remove_next?: new: %s and next_next: %s do not imply next %s -> stop"  (LinearInequality.show "x" "y" (k,c))  (LinearInequality.show "x" "y" next_next) (LinearInequality.show "x" "y" next);
                 t
               end
        in CoeffMap.add k c @@ remove_prev prev @@ remove_next next t
    | _,_ -> failwith "impossible state"

  let add_inequality k c t = 
    let res = add_inequality k c t in
    if M.tracing then M.trace "add_ineq" "adding %s to %s -> %s"  (LinearInequality.show "x" "y" (k,c))  (show_formatted "x" "y" t) (show_formatted "x" "y" res);
    res

  (*get the thightest offset for an inequality with a given slope that is implied by the current set of inequalities*)
  let get_best_offset k t = 
    match CoeffMap.find_opt k t with
    | Some c -> Some c
    | None -> 
      if CoeffMap.cardinal t < 2 then None 
      else LinearInequality.best_entailed (BatOption.get @@ get_next k t) (BatOption.get @@ get_previous k t) k

  let get_best_offset k t = 
    let res = get_best_offset k t in
    if M.tracing then M.trace "get_offset" "%s implies %s" (show_formatted "x" "y" t) (BatOption.map_default (fun c -> LinearInequality.show "x" "y" (k,c)) "Nothing for this slope" res);
    res

  (*lookup the best interval bounds from the inequalities!*)
  let interval_refinements (x,x_val) (y,y_val) t ref_acc =
    let ineqs = LinearInequality.from_values x_val y_val in
    let t = List.fold (fun t (k,c) -> add_inequality k c t) t ineqs in
    let ref_acc = match get_best_offset (LE Q.inf) t with 
      | Some upper -> (Refinement.of_value x @@ Value.ending @@ round_down upper ):: ref_acc 
      | _ -> ref_acc
    in
    let ref_acc = match get_best_offset (GE Q.inf) t with 
      | Some lower -> (Refinement.of_value x @@ Value.starting @@ round_up lower ):: ref_acc 
      | _ -> ref_acc
    in
    let ref_acc = match get_best_offset (GE Q.zero) t with 
      | Some upper -> (Refinement.of_value y @@ Value.ending @@ round_down @@ Q.neg upper ):: ref_acc 
      | _ -> ref_acc
    in
    match get_best_offset (LE Q.zero) t with 
    | Some lower -> (Refinement.of_value y @@ Value.starting @@ round_up @@ Q.neg lower ):: ref_acc 
    | _ -> ref_acc

  let meet_single_inequality (x,x_val) (y,y_val) k c (t,ref_acc) = 
    (*calculate value refinement. If one of the coefficients is zero, we should not add the inequality to the map*)
    let refinements, skip_adding = 
      let x_refine = 
        let upper_bound s = (*x <= y / s + c / s*)
          let max_y = match Value.maximal (Value.mul y_val (Value.of_bigint (round_down (Q.inv s)))) , Value.maximal @@ Value.mul y_val (Value.of_bigint (round_up (Q.inv s))) with
            | Some a, Some b -> TopIntOps.max a b
            | _,_ -> failwith "trying to refine bot in inequalities"
          in match max_y with 
          | Int max -> [Refinement.of_value x (Value.ending @@ Z.add max @@ round_down @@ Q.div c s)]
          | _ -> [] 
        in let lower_bound s = (*x >= y / s + c / s*)
             let min_y = match Value.minimal (Value.mul y_val (Value.of_bigint (round_down (Q.inv s)))) , Value.minimal @@ Value.mul y_val (Value.of_bigint (round_up (Q.inv s))) with
               | Some a, Some b -> TopIntOps.min a b
               | _,_ -> failwith "trying to refine bot in inequalities"
             in match min_y with 
             | Int min -> [Refinement.of_value x (Value.starting @@ Z.add min @@ round_up @@ Q.div c s)]
             | _ -> []
        in
        match k with 
        | LinearInequality.OriginInequality.LE s when Q.sign s > 0 -> upper_bound s
        | GE s when Q.sign s < 0 -> upper_bound s
        | LE s when Q.sign s < 0 -> lower_bound s
        | GE s when Q.sign s > 0 -> lower_bound s
        | _ -> [] (*Should never be used in this case*)
      in let y_refine =
           match k with 
           | LE s -> begin (*sx -c <= y*)
               let min_x = match Value.minimal (Value.mul x_val (Value.of_bigint (round_down s))) , Value.minimal @@ Value.mul x_val (Value.of_bigint (round_up s)) with
                 | Some a, Some b -> TopIntOps.min a b
                 | _,_ -> failwith "trying to refine bot in inequalities"
               in match min_x with 
               | Int min -> [Refinement.of_value y (Value.starting @@ Z.sub min @@ round_up c)]
               | _ -> [] 
             end
           | GE s ->  (*s x - c >= y*)
             let max_x = match Value.maximal (Value.mul x_val (Value.of_bigint (round_down s))) , Value.maximal @@ Value.mul x_val (Value.of_bigint (round_up s)) with
               | Some a, Some b -> TopIntOps.max a b
               | _,_ -> failwith "trying to refine bot in inequalities"
             in match max_x with 
             | Int max -> [Refinement.of_value y (Value.ending @@ Z.sub max @@ round_down c)]
             | _ -> [] 
      in match k with
      | LinearInequality.OriginInequality.LE s when Q.equal Q.zero s -> (* -c >= y *) [Refinement.of_value y @@ Value.ending @@ round_down @@ Q.neg c] , true
      | GE s when Q.equal Q.zero s -> (* -c <= y *) [Refinement.of_value y @@ Value.starting @@ round_up @@ Q.neg c] , true
      | LE s when Q.equal Q.inf s -> (*x >= c*) [Refinement.of_value x @@ Value.starting @@ round_up c ], true
      | GE s when Q.equal Q.minus_inf s -> (*x >= c*) [Refinement.of_value x @@ Value.starting @@ round_up c ], true 
      | LE s when Q.equal Q.minus_inf s -> (*x <= c*) [Refinement.of_value x @@ Value.ending @@ round_down c], true
      | GE s when Q.equal Q.inf s -> (*x <= c*) [Refinement.of_value x @@ Value.ending @@ round_down c], true
      | k -> (*an actual inequality *) x_refine @ y_refine, false 
    in 
    let ref_acc = refinements @ ref_acc in
    if skip_adding then t, ref_acc 
    else (*Look for contradicting inequality*)
      let contradicts c' = match k with 
        | LE _ -> Q.gt c' c
        | GE _ -> Q.lt c' c
      in
      match get_best_offset (Key.negate k) t with  
      | Some c' when contradicts c' -> 
        if M.tracing then M.trace "meet" "single_ineq new: %s contradicts existing information %s" (LinearInequality.show "x" "y" (k,c)) (show_formatted "x" "y" t);
        raise EConj.Contradiction
      (*we have an equality -> update the econj domain sn / sd x = y + cn/cd -> x = (sd y cd + sd cn) / sn cd *)
      | Some c' when c = c' -> begin
          let s = Key.get_slope k in
          let sn, sd = Q.num s, Q.den s in
          let cn, cd = Q.num c, Q.den c in
          let open Z in
          if M.tracing then begin 
            let show_var = (fun x -> "var_"^Int.to_string x) in
            M.trace "refinements" "single_ineq new: %s with %s results in equality" 
              (LinearInequality.show (show_var x) (show_var y) (k,c)) 
              (show_formatted (show_var x) (show_var y) t)
          end;
          t, (Refinement.of_rhs x @@ Rhs.canonicalize (Some (sd * cd, y), sd*cn, sn * cd)) :: ref_acc
        end
      | _ ->  
        (*add the inequality, while making sure that we do not save redundant inequalities*)
        let t' = match CoeffMap.find_opt k t with 
          | Some c_old when LinearInequality.entails1 (k,c_old) (k,c) -> t (*saved inequality is already thighter than new one*)
          | _ -> add_inequality k c @@ CoeffMap.remove k t (*we replace the current value with a new one *)
        in 
        let ref_acc = interval_refinements (x,x_val) (y,y_val) t' ref_acc in
        t', ref_acc

  let meet x_val y_val t1 t2 ref_acc = CoeffMap.fold (meet_single_inequality x_val y_val) t1 (t2,ref_acc)

  let narrow x_val y_val t1 t2 ref_acc = 
    let narrow_single_inequality k c ((t,_) as acc) = 
      if CoeffMap.mem k t then
        acc (*accelerated narowing: only restrict top bounds to only have finite number of narrowings*)
      else      
        fst @@ meet_single_inequality x_val y_val k c acc, []
    in
    CoeffMap.fold narrow_single_inequality t2 (t1,ref_acc)

  let narrow x_val y_val t1 t2 ref_acc = 
    let res = narrow x_val y_val t1 t2 ref_acc in 
    if M.tracing then M.trace "narrow" "narrow for coeffs a: %s b: %s -> %s" (show_formatted "x" "y" t1) (show_formatted "x" "y" t2) (show_formatted "x" "y" @@ fst res);
    res


  let implies x_val y_val t1_opt t2 = 
    let t1 = match t1_opt with 
      | None -> CoeffMap.empty
      | Some t -> t
    in let interval_ineqs = LinearInequality.from_values x_val y_val in
    let t1 = List.fold (fun t (k,c) -> add_inequality k c t) t1 interval_ineqs (*makes this O(n log n) instead of O(n)*)
    in if M.tracing then M.trace "implies" "after adding intervals: %s" (show_formatted "x" "y" t1);
    if CoeffMap.is_empty t2 then true
    else if CoeffMap.is_empty t1 then false
    else(*functional version of the entailment check from TVLI*)
      let ts1 = CoeffMap.bindings t1 in 
      let ts2 = CoeffMap.bindings t2 in
      let min_t1 = List.hd ts1 in
      let max_t1 = CoeffMap.max_binding t1 in
      let rec entails t1 t2 = match t1, t2 with 
        | _, [] -> true
        | [], _ -> false (*should never happen, but makes this matching complete*)
        | (tl::tu::t1s), ti::t2s when Key.compare (fst tu) (fst ti) < 0 -> entails (tu::t1s) (ti::t2s)
        | ((tl::tu::_) as t1s), ti::t2s -> LinearInequality.entails2 tl tu ti && entails t1s t2s
        | [tl], ti::t2s -> LinearInequality.entails2 tl min_t1 ti && entails [tl] t2s
      in entails (max_t1::ts1) ts2 

  let implies x_val y_val t1_opt t2 = 
    let res = implies x_val y_val t1_opt t2 in
    if M.tracing then M.trace "implies" "x = %s, y = %s, %s implies %s ? -> %b" (Value.show x_val) (Value.show y_val) (BatOption.map_default (show_formatted "x" "y") "{}" t1_opt) (show_formatted "x" "y" t2) res;
    res 

  let join x y get_val_t1 get_val_t2 t1 t2 =
    let t1 = match t1 with None -> CoeffMap.empty | Some t1 -> t1 in
    let t2 = match t2 with None -> CoeffMap.empty | Some t2 -> t2 in
    (*add interval inequalities to copies*)
    let t1_with_interval = 
      let ineqs = LinearInequality.from_values (get_val_t1 x) (get_val_t1 y) in
      List.fold (fun t (k,c) -> add_inequality k c t) t1 ineqs
    in let t2_with_interval = 
         let ineqs = LinearInequality.from_values (get_val_t2 x) (get_val_t2 y) in
         List.fold (fun t (k,c) -> add_inequality k c t) t2 ineqs
    in
    (*Keep slopes where the other element implies some inequality for the same slope *)
    let relax t k c = 
      let res = match get_best_offset k t with
        | None -> None (*drop if nothing is implied *)
        | Some c' -> (* in this case, we need to take the more relaxed bound*)
          if LinearInequality.entails1 (k, c') (k,c) then 
            Some c
          else 
            Some c'
      in
      if M.tracing then M.trace "relax" "%s with %s relaxed to %s" (show_formatted "x" "y" t) (LinearInequality.show "x" "y" (k,c)) (BatOption.map_default (fun c -> LinearInequality.show "x" "y" (k,c)) "Nothing" res);
      res
    in
    let t1_mapped = CoeffMap.filter_map (relax t2_with_interval) t1 in
    let t2_mapped = CoeffMap.filter_map (relax t1_with_interval) t2 in
    (* merge the two sets *)
    (*the existing add assumes there to be no inequality with this key. this happens only if both have the same offset now *)
    let add_inequality k c t = if CoeffMap.mem k t then t else add_inequality k c t in 
    let merged = CoeffMap.fold add_inequality t2_mapped t1_mapped
    in ignore_empty merged

  let widen _ _ _ _ t1 t2 = 
    let open GobOption.Syntax in
    (*only keep inequalities that the same are in both equations*)
    let keep_same k c1 c2 = if c1 = c2 then c1 else None in
    let* t1 = t1 in
    let* t2 = t2 in 
    ignore_empty @@ CoeffMap.merge (keep_same) t1 t2

  let substitute_left (coeff, offs, divi) t =
    let f k c t_acc = 
      let (k',c') = LinearInequality.substitute_left (coeff, offs, divi) (k,c) in
      CoeffMap.add k' c' t_acc (*affine transformation does not make a non redundant inequality redundant -> add directly*)
    in
    CoeffMap.fold f t CoeffMap.empty

  let substitute_right (coeff, offs, divi) t =
    let f k c t_acc = 
      let (k',c') = LinearInequality.substitute_right (coeff, offs, divi) (k,c) in
      CoeffMap.add k' c' t_acc
    in
    CoeffMap.fold f t CoeffMap.empty

  (*combine two inequalities into a single one if possible*)
  let combine_left rel t = 
    let fold_fun k c acc = 
      match LinearInequality.combine_left rel (k,c) with 
      | Some (k', c') -> CoeffMap.add k' c' acc
      | None -> acc
    in
    let t' = CoeffMap.fold fold_fun t CoeffMap.empty in
    if CoeffMap.is_empty t' then None else Some t'

  let combine_right rel t =     
    let fold_fun k c acc = 
      match LinearInequality.combine_right rel (k,c) with 
      | Some (k', c') -> CoeffMap.add k' c' acc
      | None -> acc
    in
    let t' = CoeffMap.fold fold_fun t CoeffMap.empty in
    if CoeffMap.is_empty t' then None else Some t'

  let swap_sides t = 
    let fold_fun k c acc = 
      let (k', c') = LinearInequality.swap_sides (k,c) in
      CoeffMap.add k' c' acc
    in
    CoeffMap.fold fold_fun t CoeffMap.empty 

  let set_constant_lhs rhs_var const t ref_acc = CoeffMap.fold (fun k c ref_acc -> LinearInequality.set_constant_lhs rhs_var const (k,c) ref_acc) t ref_acc
  let set_constant_rhs lhs_var const t ref_acc = CoeffMap.fold (fun k c ref_acc -> LinearInequality.set_constant_rhs lhs_var const (k,c) ref_acc) t ref_acc


  let invariant env x y t acc = CoeffMap.fold (LinearInequality.invariant env x y) t acc

  let to_set t = t

  let of_set _ _ t = ignore_empty t

end

module TVIS = TwoVarInequalitySet

module type Coeffs = sig
  type t

  val join : int -> int -> (int -> Value.t) -> (int -> Value.t) -> t option -> t option -> t option
  val widen : int -> int -> (int -> Value.t) -> (int -> Value.t) -> t option -> t option -> t option

  val limit : (Q.t, int) Hashtbl.t -> t -> t option

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val show_formatted : string -> string -> t -> string

  val to_set : t -> TwoVarInequalitySet.t

  val of_set : Value.t -> Value.t -> TwoVarInequalitySet.t -> t option

end


module InequalityFunctor (Coeffs : Coeffs): TwoVarInequalities = struct

  type t = Coeffs.t IntMap.t IntMap.t  [@@deriving eq, ord ]

  let empty = IntMap.empty
  let is_empty = IntMap.is_empty
  let hash t = IntMap.fold (fun _ ys acc -> IntMap.fold (fun _ coeff acc -> Coeffs.hash coeff + 3*acc) ys (5*acc)) t 0

  let ignore_empty ls = 
    if IntMap.is_empty ls then None
    else Some ls

  let show_map formatter elem_formatter t = 
    if IntMap.is_empty t then "{}" else
      let str = IntMap.fold (fun x ys acc -> IntMap.fold (fun y coeff acc -> Printf.sprintf "%s , %s" (elem_formatter (formatter x) (formatter y) coeff) acc) ys acc) t "" 
      in "{" ^ String.sub str 0 (String.length str - 3) ^ "}"

  let show_formatted formatter t = show_map formatter Coeffs.show_formatted t

  let show = show_formatted (Printf.sprintf "var_%d") 

  let forget_variable t v = 
    IntMap.filter_map (fun _ ys -> let ys' = IntMap.remove v ys in if IntMap.is_empty ys' then None else Some ys') (IntMap.remove v t)

  let modify_variables_in_domain map indexes op =
    let map_fun bump_var ys = IntMap.fold (fun y ->  IntMap.add (bump_var y) ) ys IntMap.empty in  
    EConj.modify_variables_in_domain_general map map_fun indexes op 

  let get_coeff x y t = BatOption.bind (IntMap.find_opt x t) (fun ys -> IntMap.find_opt y ys)

  let set_coeff x y coeff t = 
    IntMap.add x (IntMap.add y coeff @@ IntMap.find_default IntMap.empty x t ) t

  let remove_coeff x y t =
    let new_map = IntMap.remove y @@ IntMap.find_default IntMap.empty x t  in
    if IntMap.is_empty new_map then t
    else IntMap.add x new_map t

  let leq t1 get_value_t1 t2 = 
    let implies x y t2_coeff = 
      let t1_coeff = get_coeff x y t1 in 
      TVIS.implies (get_value_t1 x) (get_value_t1 y) (BatOption.map Coeffs.to_set t1_coeff)  (Coeffs.to_set t2_coeff)
    in
    IntMap.for_all (fun x ys -> IntMap.for_all (implies x) ys) t2

  let meet_one_coeff narrow get_value x y coeff (t,ref_acc) =
    let coeff_t = get_coeff x y t in
    let coeff_met, ref_acc' = match coeff_t with 
      | None -> coeff, ref_acc (*also fine for narrow if t is the one on the righthandside*)
      | Some coeff_t -> (if narrow then TVIS.narrow else TVIS.meet) (x, get_value x) (y, get_value y) (Coeffs.to_set coeff_t) coeff ref_acc
    in match (Coeffs.of_set (get_value x) (get_value y)) coeff_met with 
    | None -> remove_coeff x y t, ref_acc'
    | Some coeff_met -> set_coeff x y coeff_met t, ref_acc'

  let meet get_value t1 t2 = 
    IntMap.fold (fun x ys acc -> IntMap.fold (fun y coeff acc -> meet_one_coeff false get_value x y (Coeffs.to_set coeff)  acc) ys acc) t1 (t2,[])

  let narrow get_value t1 t2 = 
    IntMap.fold (fun x ys acc -> IntMap.fold (fun y coeff acc -> meet_one_coeff true get_value x y (Coeffs.to_set coeff) acc) ys acc) t1 (t2,[])

  let join t1 get_val_t1 t2 get_val_t2 = 
    let merge_y x y = Coeffs.join x y get_val_t1 get_val_t2 in 
    let merge_x x ys1 ys2 = 
      match ys1, ys2 with
      | Some ys1, None -> None
      | None, Some ys2 -> None
      | Some ys1, Some ys2 -> ignore_empty (IntMap.merge (merge_y x) ys1 ys2)
      | _, _ -> None
    in IntMap.merge (merge_x) t1 t2

  let join  t1 get_val_t1 t2 get_val_t2 = 
    let res = Timing.wrap "join_ineq" (join t1 get_val_t1 t2 ) get_val_t2 in
    if M.tracing then M.trace "join'" "a: %s b: %s -> %s" (show t1) (show t2) (show res);
    res

  let widen t1 get_val_t1 t2 get_val_t2 = 
    let merge_x x ys1 ys2 = 
      match ys1, ys2 with
      | Some ys1, None -> ignore_empty (IntMap.filter_map (fun y coeff1 -> Coeffs.widen x y get_val_t1 get_val_t2 (Some coeff1) None ) ys1)
      | None, Some ys2 -> ignore_empty (IntMap.filter_map (fun y coeff2 -> Coeffs.widen x y get_val_t1 get_val_t2 None (Some coeff2)) ys2)
      | Some ys1, Some ys2 -> ignore_empty (IntMap.merge (fun y cs1 cs2 -> Coeffs.widen x y get_val_t1 get_val_t2 cs1 cs2) ys1 ys2)
      | _, _ -> None
    in IntMap.merge (merge_x) t1 t2

  let widen a b c d =
    let res = widen a b c d in
    if M.tracing then M.trace "widen" "called for inequalities";
    res

  let interval_refinements get_value t = IntMap.fold (fun x ys acc -> 
      IntMap.fold (fun y cs acc -> 
          TVIS.interval_refinements 
            (x, get_value x) 
            (y, get_value y) 
            (Coeffs.to_set cs) 
            acc
        ) ys acc
    ) t []

  let set_constant var const t = IntMap.fold (fun x ys (t_acc, ref_acc) ->
      if x = var then 
        t_acc, IntMap.fold (fun y cs ref_acc -> TVIS.set_constant_lhs y const (Coeffs.to_set cs) ref_acc) ys ref_acc
      else 
        match IntMap.find_opt var ys with
        | None -> IntMap.add x ys t_acc, ref_acc
        | Some cs -> 
          let ys' = IntMap.remove var ys in
          let ref_acc = TVIS.set_constant_rhs x const (Coeffs.to_set cs) ref_acc in
          if IntMap.is_empty ys' then t_acc, ref_acc else (IntMap.add x ys' t_acc, ref_acc) 
    ) t (IntMap.empty,[])

  let invariant t env = IntMap.fold (fun x ys acc -> IntMap.fold (fun y cs acc -> TVIS.invariant env x y (Coeffs.to_set cs) acc) ys acc) t []

  let ignore_empty ls = 
    if IntMap.is_empty ls then None
    else Some ls

  let rec get_relations (((c_x, x,o_x,d_x), x_val) as x') (((c_y, y,o_y,d_y), y_val) as y') t = 
    if x > y then
      (*We save information only in one of the directions -> check the other one*)
      List.map Relation.swap_sides @@ get_relations y' x' t
    else begin
      if M.tracing then M.trace "get_relations" "checking x'=%s, y'=%s" (Rhs.show (Some (c_x, x), o_x, d_x)) (Rhs.show (Some (c_y, y), o_y, d_y));
      match get_coeff x y t with 
      | None -> begin if M.tracing then M.trace "get_relations" "no inequality for roots"; [] end (*No information*)
      | Some coeff -> 
        let coeff = Coeffs.to_set coeff in
        let interval_ineqs = LinearInequality.from_values x_val y_val in
        let coeff = List.fold (fun t (k,c) -> TVIS.add_inequality k c t) coeff interval_ineqs in
        let (k,c_rhs) = LinearInequality.from_rhss (c_x,o_x,d_x) (c_y,o_y,d_y) None in
        let factor = (*we need to muliply c' with this factor because LinearInequalities scales them down*)
          let a = Q.make c_x d_x in
          let b = Q.make c_y d_y in
          if Q.equal b Q.zero then a else b
        in
        let upper_bound = match TVIS.get_best_offset k coeff with
          | None -> []
          | Some c_ineq -> 
            let c' = Q.mul factor @@ Q.sub c_ineq c_rhs in 
            [Relation.Lt, Z.succ @@ Z.fdiv (Q.num c') (Q.den c')] (*add one to make it a strict inequality*)
        in match TVIS.get_best_offset (TVIS.Key.negate k) coeff with (*lower bound*)
        | None -> upper_bound
        | Some c_ineq -> 
          let c' = Q.mul factor ( Q.add c_ineq c_rhs) in
          (Gt, Z.pred @@ Z.cdiv (Q.num c') (Q.den c')) :: upper_bound
    end

  let get_relations x y t = 
    let res = get_relations x y t in
    if M.tracing then M.trace "get_relations" "result: %s" (BatList.fold (fun acc c -> acc ^ ", " ^ Relation.show "x'" c "y'") "" res);
    res

  let meet_relation x' y' cond get_rhs get_value t =
    let rec meet_relation_roots x y k c (t, ref_acc) = 
      if x > y then 
        let k', c' = LinearInequality.swap_sides (k,c) in
        meet_relation_roots y x k' c' (t, ref_acc)
      else begin
        if M.tracing then M.tracel "meet_relation" "meet_relation_roots: %s" @@ LinearInequality.show ("var_" ^ Int.to_string x) ("var_" ^ Int.to_string y) (k,c);
        if x = y then
          let s = TVIS.Key.get_slope k in
          if Q.equal Q.one s then (* x <= x + c (or >=) *)
            match k with 
            | LE _ -> if Q.lt c Q.zero then raise EConj.Contradiction else (t, []) (*trivially true*)
            | GE _ -> if Q.gt c Q.zero then raise EConj.Contradiction else (t, []) (*trivially true*)
          else (* sx <= x + c (or =>) -> refine the value in this case*)
            let s' = Q.sub s Q.one in
            let s', c' = match k with LE _ -> s',c | GE _ -> Q.neg s', Q.neg c in 
            (*s'x <= c' *) 
            if Q.gt s' Q.zero then 
              let max = Q.div c' s' in
              t, (x, Either.Left (Value.ending @@ Z.cdiv (Q.num max) (Q.den max))) :: ref_acc
            else
              let min = Q.div c' s' in
              t, (x, Left (Value.starting @@ Z.fdiv (Q.num min) (Q.den min))) :: ref_acc
        else 
          let coeffs = match get_coeff x y t with
            | None -> TVIS.empty
            | Some c -> Coeffs.to_set c
          in let coeffs', ref_acc = TVIS.meet_single_inequality (x,get_value x) (y,get_value y) k c (coeffs,ref_acc) 
          in match Coeffs.of_set (get_value x) (get_value y) coeffs' with 
          | None -> remove_coeff x y t , ref_acc
          | Some c ->  set_coeff x y c t, ref_acc
      end in
    let apply_transivity x y k c t = 
      if x = y then begin
        if M.tracing then M.tracel "transitivity" "transitivity between same variable %d -> skip" x;
        t, []
      end else begin
        if M.tracing then M.tracel "transitivity" "transitivity with %s and %s" (LinearInequality.show ("var_" ^ Int.to_string x) ("var_" ^ Int.to_string y) (k,c)) (show t);
        IntMap.fold (fun w zs acc -> 
            if w = x then 
              IntMap.fold (fun z cs acc ->   
                  match TVIS.combine_left (k,c) @@ Coeffs.to_set cs with
                  | None -> 
                    if M.tracing then M.tracel "transitivity" "case 1, combined with %s into Nothing " (Coeffs.show_formatted ("var_" ^ Int.to_string w) ("var_" ^ Int.to_string z) cs);
                    acc 
                  | Some cs' -> 
                    if M.tracing then M.tracel "transitivity" "case 1, combined with %s into %s " (Coeffs.show_formatted ("var_" ^ Int.to_string w) ("var_" ^ Int.to_string z) cs) (TVIS.show_formatted ("var_" ^ Int.to_string y) ("var_" ^ Int.to_string z) cs');
                    TVIS.CoeffMap.fold (fun k c acc -> meet_relation_roots y z k c acc) cs' acc
                ) zs acc
            else if w = y then 
              IntMap.fold (fun z cs acc ->
                  match TVIS.combine_left (LinearInequality.swap_sides (k,c)) @@ Coeffs.to_set cs with
                  | None -> 
                    if M.tracing then M.tracel "transitivity" "case 2, combined with %s into Nothing " (Coeffs.show_formatted ("var_" ^ Int.to_string w) ("var_" ^ Int.to_string z) cs);
                    acc 
                  | Some cs' -> 
                    if M.tracing then M.tracel "transitivity" "case 2, combined with %s into %s " (Coeffs.show_formatted ("var_" ^ Int.to_string w) ("var_" ^ Int.to_string z) cs) (TVIS.show_formatted ("var_" ^ Int.to_string x) ("var_" ^ Int.to_string z) cs');
                    TVIS.CoeffMap.fold (fun k c acc -> meet_relation_roots x z k c acc) cs' acc
                ) zs acc
            else  
              IntMap.fold (fun z cs acc -> 
                  if z = x then 
                    match TVIS.combine_right (k,c) @@ Coeffs.to_set cs with
                    | None -> 
                      if M.tracing then M.tracel "transitivity" "case 3, combined with %s into Nothing " (Coeffs.show_formatted ("var_" ^ Int.to_string w) ("var_" ^ Int.to_string z) cs);
                      acc 
                    | Some cs' -> 
                      if M.tracing then M.tracel "transitivity" "case 3, combined with %s into %s " (Coeffs.show_formatted ("var_" ^ Int.to_string w) ("var_" ^ Int.to_string z) cs) (TVIS.show_formatted ("var_" ^ Int.to_string w) ("var_" ^ Int.to_string y) cs');
                      TVIS.CoeffMap.fold (fun k c acc -> meet_relation_roots w y k c acc) cs' acc
                  else if z = y then 
                    match TVIS.combine_right (LinearInequality.swap_sides (k,c)) @@ Coeffs.to_set cs with
                    | None -> 
                      if M.tracing then M.tracel "transitivity" "case 4, combined with %s into Nothing " (Coeffs.show_formatted ("var_" ^ Int.to_string w) ("var_" ^ Int.to_string z) cs);
                      acc 
                    | Some cs' -> 
                      if M.tracing then M.tracel "transitivity" "case 4, combined with %s into %s " (Coeffs.show_formatted ("var_" ^ Int.to_string w) ("var_" ^ Int.to_string z) cs) (TVIS.show_formatted ("var_" ^ Int.to_string w) ("var_" ^ Int.to_string x) cs');
                      TVIS.CoeffMap.fold (fun k c acc -> meet_relation_roots w x k c acc) cs' acc
                  else
                    acc
                ) zs acc
          ) t (t, [])
      end in
    match get_rhs x', get_rhs y' with 
    | (Some (c_x, x),o_x,d_x), (Some (c_y, y),o_y,d_y) -> begin
        let (k,c) = LinearInequality.from_rhss (c_x,o_x,d_x) (c_y,o_y,d_y) (Some (match fst cond with Relation.Lt -> true | _ -> false))
        in let factor = (*we need to divide o by this factor because LinearInequalities normalises it.*)
             let a = Q.make c_x d_x in
             let b = Q.make c_y d_y in
             if Q.equal b Q.zero then a else b
        in match cond with 
        | Relation.Lt, o -> 
          let c' = (Q.add c @@ Q.div (Q.of_bigint o) factor) in
          let t, ref_acc = apply_transivity x y k c' t in
          meet_relation_roots x y k c' (t, ref_acc)
        | Gt, o -> 
          let k = TVIS.Key.negate k in
          let c' = (Q.add c @@ Q.div (Q.of_bigint o) factor) in
          let t, ref_acc = apply_transivity x y k c' t in
          meet_relation_roots x y k c' (t,ref_acc)
      end
    (*Cases where one of the variables is a constant -> refine value*)
    | (None, o_x, _), (Some _,_,_) -> t, [Refinement.of_value y' @@ Relation.value_with_const_left cond o_x]
    | (Some _,_,_), (None, o_y, _) -> t, [Refinement.of_value x' @@ Relation.value_with_const_right cond o_y]
    | (None, o_x, _), (None, o_y, _) -> 
      let v = Relation.value_with_const_right cond o_y in
      if Value.contains v o_x then
        t, []
      else 
        raise EConj.Contradiction


  let meet_relation x y c r v t = 
    if M.tracing then M.tracel "meet_relation" "meeting %s with %s" (show t) (Relation.show ("var_"^Int.to_string x) c ("var_"^Int.to_string y));  
    let res, refine_acc = meet_relation x y c r v t in
    if M.tracing then M.tracel "meet_relation" "result: %s, refinements: %s " (show res) (Refinement.show refine_acc);  
    res, refine_acc

  (*We want to be lazy and only convert to TVIS if necessary. To be able to reuse this function in transfer,
    we also need to be able to work with an already converted Map -> add functions for conversion*)
  let substitute' t i (coeff, j, offs, divi) to_set of_set = 
    (*if both sides refer to the same variable: check for contradictions  or refine the value*)
    let constraints_same_variable cs  = 
      let check_single k c value = 
        let s = TVIS.Key.get_slope k in
        if Q.equal Q.one s then (* x <= x + c (or >=) *)
          match k with 
          | LE _ -> if Q.lt c Q.zero then raise EConj.Contradiction else value (*trivially true*)
          | GE _ -> if Q.gt c Q.zero then raise EConj.Contradiction else value (*trivially true*)
        else (* sx <= x + c (or =>) -> refine the value in this case*)
          let s' = Q.sub s Q.one in
          let s', c' = match k with LE _ -> s',c | GE _ -> Q.neg s', Q.neg c in 
          (*s'x <= c' *) 
          if Q.gt s' Q.zero then 
            let max = Q.div c' s' in
            Value.meet value @@ Value.ending @@ Z.cdiv (Q.num max) (Q.den max)
          else
            let min = Q.div c' s' in
            Value.meet value @@ Value.starting @@ Z.fdiv (Q.num min) (Q.den min)
      in TVIS.CoeffMap.fold check_single cs Value.top
    in
    let without_i = forget_variable t i in
    (*add to bindings, meeting with existing if necessary*)
    let merge_single x y cs_new (t,ref_acc) = 
      let cs_curr = BatOption.default TVIS.empty @@ BatOption.map to_set @@ get_coeff x y t in
      let cs_combined, ref_acc = TVIS.meet (x,Value.top) (y,Value.top) cs_new cs_curr ref_acc in
      match of_set x y cs_combined with 
      | None -> t,ref_acc 
      | Some cs_combined -> set_coeff x y cs_combined t, ref_acc
    in
    let fold_x x ys acc = 
      if x < i then 
        match IntMap.find_opt i ys with
        | None -> acc
        | Some cs ->
          let cs = to_set cs in          
          let cs' = TVIS.substitute_right (coeff, offs, divi) cs in
          if x = j then (*We now have inequalities with the same variable on both sides *)
            let t,ref_acc = acc in 
            let v = constraints_same_variable cs' in
            t, Refinement.of_value x v :: ref_acc
          else if x < j then 
            merge_single x j cs' acc
          else (*x > j -> swap sides*)
            let cs'' = TVIS.swap_sides cs' in
            merge_single j x cs'' acc
      else if x = i then  
        let fold_y y cs acc = 
          let cs' = TVIS.substitute_left (coeff, offs, divi) cs in
          if j < y then 
            merge_single j y cs' acc
          else if j = y then begin
            let t,ref_acc = acc in 
            let v = constraints_same_variable cs' in
            t, Refinement.of_value x v :: ref_acc
          end else
            let cs'' = TVIS.swap_sides cs' in
            merge_single y j cs'' acc
        in
        IntMap.fold fold_y (IntMap.map to_set ys) acc
      else
        acc
    in IntMap.fold fold_x t (without_i, [])

  let substitute get_value t i (c,j,o,d) = 
    if M.tracing then M.trace "substitute" "substituting var_%d in %s with %s" i (show t) (Rhs.show (Some (c,j), o, d));
    let t, ref_acc = substitute' t i (c,j,o,d) Coeffs.to_set (fun x y s -> Coeffs.of_set (get_value x) (get_value y) s) in
    if M.tracing then M.trace "substitute" "resulting in %s, refinements: %s" (show t) (Refinement.show ref_acc);
    t, ref_acc

  let transfer x cond t_old get_rhs_old get_value_old t get_rhs get_value = 
    match get_rhs_old x, get_rhs x with 
    | (Some (coeff_old,x_root_old), off_old, divi_old), ((Some (coeff,x_root), off, divi) as rhs) -> 
      (*convert the relation to a linear inequality refering to the old root *)
      let (k,c) = LinearInequality.from_rhss (coeff_old, off_old, divi_old) (coeff_old, off_old, divi_old) (Some (match fst cond with Relation.Lt -> true | _ -> false))
      in let factor = Q.make coeff_old divi_old (*we need to divide o by this factor because LinearInequalities normalizes*)
      in let ineq_from_cond = match cond with 
          | Relation.Lt, o -> k, (Q.add c @@ Q. div (Q.of_bigint o) factor)
          | Gt, o -> (TVIS.Key.negate k), (Q.add c @@ Q. div (Q.of_bigint o) factor)
      in
      (*combine the inequality from cond with all inequalities*)
      (*throw out all inequalities that do not contain the representative of x*)
      let combine_1 v1 v2s = 
        if v1 = x_root_old then ignore_empty @@ IntMap.filter_map (fun _ c -> TVIS.combine_left ineq_from_cond @@ Coeffs.to_set c) v2s
        else
          let combine_2 v2 c = if v2 = x_root_old then TVIS.combine_right ineq_from_cond @@ Coeffs.to_set c else None in
          ignore_empty @@ IntMap.filter_map combine_2 v2s
      in
      let filtered = IntMap.filter_map combine_1 t_old in
      if M.tracing then M.tracel "transfer" "filtered + combined %s" (show_map (Printf.sprintf "var_%d") TVIS.show_formatted filtered);  
      (*transform all inequalities to refer to new root of x*)
      (*invert old rhs, then substitute the new rhs for x*)
      let (m, o, d) = Rhs.subst rhs x @@ snd @@ EConj.inverse x (coeff_old,x_root_old, off_old, divi_old) in
      let c, v = BatOption.get m in
      let transformed, ref_acc = substitute' filtered x_root (c, v, o, d) identity (fun _ _ c -> TVIS.ignore_empty c) in
      if M.tracing then M.tracel "transfer" "transformed: %s, refinements: %s" (show_map (Printf.sprintf "var_%d") TVIS.show_formatted transformed) (Refinement.show ref_acc);  
      (*meet with this set of equations*)
      let meet' t1 t2 = IntMap.fold (fun x ys acc -> IntMap.fold (fun y coeff acc -> meet_one_coeff true get_value x y coeff acc) ys acc) t1 (t2,ref_acc) in
      meet' transformed t
    | _,_ -> t, [] (*ignore constants*)

  let transfer x cond t_old get_rhs_old get_value_old t get_rhs get_value =
    let cond = Relation.swap_sides cond in (*The interface changed, but I do not want to reimplement this function*)
    if M.tracing then M.tracel "transfer" "transfering  with %s from %s into %s" (Relation.show ("var_" ^ Int.to_string x ^ "_old") cond ("var_" ^ Int.to_string x ^ "_new") ) (show t_old) (show t);  
    let res, ref_acc = transfer x cond t_old get_rhs_old get_value_old t get_rhs get_value in
    if M.tracing then M.tracel "transfer" "result: %s, refinements: %s" (show res) (Refinement.show ref_acc);  
    res, ref_acc

  let limit econj t = 
    let conf = GobConfig.get_string "ana.lin2vareq_p.inequalities" in
    if  conf <> "coeffs_count" && conf <> "coeffs_threshold" then t else 
      let coeffs = coeffs_from_econj econj in
      let limit_single x y cs = 
        Coeffs.limit ( slopes_from_coeffs coeffs (min x y, max x y)) cs
      in IntMap.filter_map (fun x ys -> ignore_empty @@ IntMap.filter_map (fun y cs -> limit_single x y cs) ys) t

  let limit e t = Timing.wrap "limit" (limit e) t

  let copy_to_new_representants econj_old econj_new get_value t = 
    let coeffs = coeffs_from_econj econj_new in
    (*a var is representant if it does not show up in the sparse map*)
    let all_representants_in_new = 
      let rec aux acc n =
        if n > (fst econj_new) then acc
        else if IntMap.mem n (snd econj_new) then aux acc (n + 1)
        else aux (n :: acc) (n + 1)
      in aux [] 0
    in let new_representants_in_new = List.filter (fun v -> IntMap.mem v (snd econj_old)) all_representants_in_new
    in if M.tracing then M.trace "new_reps" "all_in_new: %s, new_in_new: %s" (List.fold (fun a i -> Int.to_string i ^ ", " ^ a) "" all_representants_in_new) (List.fold (fun a i -> Int.to_string i ^ ", " ^ a) "" new_representants_in_new);
    let add_new v_new t_acc other_var = 
      if v_new = other_var then t_acc else
        (*get the old rhs*)
        match IntMap.find v_new (snd econj_old) with 
        | None,_,_ -> t_acc (*skip constants*)
        | (Some (c,old_rep), o, d) ->
          let allowed_slopes = Hashtbl.keys @@ slopes_from_coeffs coeffs (min v_new other_var, max v_new other_var) in
          (*inverse rhs so that we can translate the inequalities of the old representant to slopes corresponding to the new representant*)
          let (_, (mi,oi,di)) = EConj.inverse v_new (c,old_rep,o,d) in
          let ci,_ = BatOption.get mi in 
          (*convert the slope from new representant to old*)
          let convert_to_old ineq = 
            if v_new < other_var then 
              LinearInequality.substitute_left (c,o,d) ineq
            else 
              let ineq' = LinearInequality.substitute_right (c,o,d) ineq in
              if old_rep < other_var then 
                LinearInequality.swap_sides ineq'
              else ineq'
              (*convert back*)
          in let convert_to_new ineq = 
               if v_new < other_var then 
                 LinearInequality.substitute_left (ci,oi,di) ineq
               else 
                 let ineq' =  if old_rep < other_var then LinearInequality.swap_sides ineq else ineq
                 in LinearInequality.substitute_right (ci,oi,di) ineq'
                 (*relations between the old representant and the other variable*)
          in let coeffs_old = BatOption.default TVIS.empty @@ BatOption.map Coeffs.to_set @@ get_coeff (min old_rep other_var) (max old_rep other_var) t in
          if M.tracing then M.trace "new_reps" "coeffs_old for old %d other %d new %d: %s" old_rep other_var v_new (TVIS.show_formatted "min" "max" coeffs_old);
          if TVIS.CoeffMap.is_empty coeffs_old then t_acc else (*skip the rest if there is no need *)
            let add_single_slope c_acc s = 
              let ineqs = [LinearInequality.OriginInequality.LE s, Q.zero; GE s, Q.zero;]
              in let copy_single_ineq c_acc ineq = 
                   let k_old = fst @@ convert_to_old ineq in
                   (*TODO maybe this introduces too many new inequalities -> only take explicit stored ones?*)
                   match TVIS.get_best_offset k_old coeffs_old with 
                   | None -> c_acc
                   | Some o -> 
                     let k_neq, o_new = convert_to_new (k_old, o) in
                     TVIS.add_inequality k_neq o_new c_acc
              in List.fold copy_single_ineq c_acc ineqs 
            in let coeffs_new = Enum.fold add_single_slope TVIS.empty allowed_slopes
            in let x, y = min v_new other_var , max v_new other_var
            in match Coeffs.of_set (get_value x) (get_value y) coeffs_new with 
            | Some coeffs_new ->  set_coeff x y coeffs_new t_acc
            | _ -> t_acc
    in List.fold (fun acc v_new -> List.fold (add_new v_new ) acc all_representants_in_new ) t new_representants_in_new 

  let copy_to_new_representants econj_old econj_new t = Timing.wrap "new_reps" (copy_to_new_representants econj_old econj_new) t

end

module PentagonCoeffs : Coeffs = struct
  (*true  -> x < y *)
  (*false -> x > y *)
  type t = bool [@@deriving eq, hash, ord]

  let join x y get_val_t1 get_val_t2 t1 t2 = 
    match t1, t2 with 
    | None, None -> None 
    | Some d1, Some d2 -> if d1 = d2 then Some d1 else None
    | Some true, None ->  if Value.must_be_neg @@ Value.sub (get_val_t2 x) (get_val_t2 y) then Some true else None 
    | Some false, None ->  if Value.must_be_pos @@ Value.sub (get_val_t2 x) (get_val_t2 y) then Some false else None 
    | None, Some true ->  if Value.must_be_neg @@ Value.sub (get_val_t1 x) (get_val_t1 y) then Some true else None 
    | None, Some false ->  if Value.must_be_pos @@ Value.sub (get_val_t1 x) (get_val_t1 y) then Some false else None 

  let widen x y get_val_t1 get_val_t2 t1 t2 = t2

  let  limit _ t = Some t

  let show_formatted x y t = x ^ (if t then "<" else ">") ^ y

  let to_set t = 
    let open LinearInequality.OriginInequality in
    let i = if t then LE Q.one else GE Q.one in
    (*x < y  <==> x <= y - 1*)
    let o = if t then Q.minus_one else Q.one in
    TVIS.CoeffMap.singleton i o

  let to_set = Timing.wrap "to_set" to_set

  let of_set x_val y_val s = 
    let open LinearInequality.OriginInequality in
    let s' = List.fold (fun t (k,c) -> TVIS.add_inequality k c t) s @@ LinearInequality.from_values x_val y_val in
    match TVIS.get_best_offset (LE Q.one) s' with 
    | Some c when Q.(<=) c Q.minus_one -> Some true 
    | _ -> match TVIS.get_best_offset (GE Q.one) s' with 
      | Some c when Q.(>=) c Q.one -> Some false 
      | _ -> None

  let of_set x_val y_val = Timing.wrap "of_set" (of_set x_val y_val)

end

module PentagonOffsetCoeffs : Coeffs = struct
  (*x <= y + c, x >= y + c, None = Top  *)
  type t = Z.t option * Z.t option [@@deriving eq, hash, ord]

  let flatten (u,l) = if u = None && l = None then None else Some (u,l)

  let join x y get_val_t1 get_val_t2 t1 t2 = 
    let l_of_values get_values = 
      let diff = Value.sub (get_values x) (get_values y) in
      match Value.minimal diff with 
      | Some (Int i) -> Some i
      | _ -> None 
    in
    let u_of_values get_values =
      let diff = Value.sub (get_values x) (get_values y) in
      match Value.maximal diff with 
      | Some (Int i) -> Some i
      | _ -> None 
    in
    let open BatOption in
    let lift2 f a b = bind a (fun a -> bind b (fun b -> Some (f a b))) in
    match t1, t2 with
    | None, None -> None 
    | None, Some (u,l) -> 
      let u = lift2 max u @@ u_of_values get_val_t1 in
      let l = lift2 min l @@ l_of_values get_val_t1 in
      flatten (u,l)
    | Some (u,l), None -> 
      let u = lift2 max u @@ u_of_values get_val_t2 in
      let l = lift2 min l @@ l_of_values get_val_t2 in
      flatten (u,l)
    | Some (u1,l1), Some (u2,l2) ->
      let u1 = lift2 min u1 @@ u_of_values get_val_t1 in
      let u2 = lift2 min u2 @@ u_of_values get_val_t2 in
      let u = lift2 max u1 u2 in
      let l1 = lift2 max l1 @@ l_of_values get_val_t1 in
      let l2 = lift2 max l2 @@ l_of_values get_val_t2 in 
      let l = lift2 min l1 l2 in
      flatten (u,l)

  let show_formatted x y (u,l) = 
    let u = match u with 
      | None -> ""
      | Some u -> x ^ " <= " ^ y ^ " + " ^ Z.to_string u
    in let l = match l with 
        | None -> ""
        | Some l -> x ^ " >= " ^ y ^ " + " ^ Z.to_string l
    in u ^ " , " ^ l

  let join x y get_val_t1 get_val_t2 t1 t2 =
    let res = join x y get_val_t1 get_val_t2 t1 t2 in
    if M.tracing then M.trace "joinc" "\na: %s, x=%s,y=%s\nb: %s, x=%s,y=%s\n -> %s" 
        (BatOption.map_default (show_formatted "x" "y") "None" t1) 
        (Value.show @@ get_val_t1 x)
        (Value.show @@ get_val_t1 y)
        (BatOption.map_default (show_formatted "x" "y") "None" t2) 
        (Value.show @@ get_val_t2 x)
        (Value.show @@ get_val_t2 y)
        (BatOption.map_default (show_formatted "x" "y") "None" res);
    res 

  let widen x y get_val_t1 get_val_t2 t1 t2 =
    match t1, t2 with
    | None, None 
    | None, Some _ 
    | Some _, None -> None
    | Some (u1,l1), Some (u2,l2) ->
      let l = if l1 = l2 then l2 else None in  
      let u = if u1 = u2 then u2 else None in
      flatten (u,l)

  let limit _ t = Some t

  let to_set (u,l) = 
    let open LinearInequality.OriginInequality in
    let s = TVIS.empty in
    let s = match u with 
      | None -> s
      | Some u -> TVIS.add_inequality (LE Q.one) (Q.of_bigint u) s
    in  
    match l with 
    | None -> s
    | Some l -> TVIS.add_inequality (GE Q.one) (Q.of_bigint l) s

  let to_set = Timing.wrap "to_set" to_set

  let of_set x_val y_val s = 
    let open LinearInequality.OriginInequality in
    let s' = List.fold (fun t (k,c) -> TVIS.add_inequality k c t) s @@ LinearInequality.from_values x_val y_val in
    let u = BatOption.map round_down @@ TVIS.get_best_offset (LE Q.one) s' in 
    let l = BatOption.map round_up @@ TVIS.get_best_offset (GE Q.one) s' in 
    flatten (u,l)

  let of_set x_val y_val = Timing.wrap "of_set" (of_set x_val y_val)

end

(*TODOs:*)

(*?? limit: use linear time algorithm for coeffs_count instead of sorting??*)

(*!! fix cohencu tests*)

(*+ look at complexities. I expect for all: (n² log n) 
    not leq because of interval fixpoint!!!*)
(*+ How to do a useful narrow?*)

(*  widening thresholds: from offsets of rhs?*)
(*  store information about representants to avoid recalculating them: congruence information, group size/ coefficients ??*)

(*- copy_to_new: introduces too many inequlities?*)
(*- ineq refine_with_tcons: normalisation*)
(*- better to_inequalities? with query?*)

(*-- assign expr restore ineqs based on value *)
(*--memo_bumbvar created 3 times*)
(*--eval_int: answer nonlinear*)

(*! general renaming*)