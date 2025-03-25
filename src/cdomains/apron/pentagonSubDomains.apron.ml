open Batteries
open GoblintCil
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

  (*TODO*)
  let arbitrary () = failwith "arbitrary not implemented yet"
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

end 

module Value = IntervalAndCongruence

module Relation = struct
  type cond = Lt |  Eq | Gt
  type t = cond * Z.t

  let show_cond c = match c with 
    | Lt -> "<"
    | Eq -> "="
    | Gt -> ">"

  let show x (c,o) y = x ^ show_cond c ^ y ^ " + " ^ Z.to_string o 

  let invert (cond, o) =
    let o' = Z.neg o in
    match cond with 
    | Lt -> Gt, o'
    | Gt -> Lt, o'
    | Eq -> Eq, o'

  (*Tries to combine two relations, with the variable on the rhs of the first condition being equal to the one at the lhs of the second*)
  let combine (c1, o1) (c2, o2) = match c1, c2 with
    | Lt, Lt -> Some ( Lt, Z.add o1 @@ Z.add o2 Z.one )
    | Lt, Eq 
    | Eq, Lt -> Some ( Lt, Z.add o1 o2 )
    | Eq, Eq -> Some ( Eq, Z.add o1 o2 )
    | Gt, Gt -> Some ( Gt, Z.add o1 @@ Z.add o2 Z.one )
    | Gt, Eq 
    | Eq, Gt -> Some ( Gt, Z.add o1 o2 )
    | Lt, Gt 
    | Gt, Lt -> None
end


module type TwoVarInequalities = sig
  type t

  val get_relations :  (Rhs.t * Value.t) -> (Rhs.t * Value.t) -> t -> Relation.t list

  (*meet x' < y' + c (or with = / > *)
  val meet_relation : int -> int -> Relation.t -> (int -> Rhs.t) -> (int -> Value.t) -> t -> t * (int * Value.t) list

  val meet : (int -> Value.t) -> t -> t -> t
  val narrow : (int -> Value.t) -> t -> t -> t

  val leq : t -> (int -> Value.t) -> t -> bool

  val join : t -> (int -> Value.t) -> t -> (int -> Value.t) -> t
  val widen : t -> (int -> Value.t) -> t -> (int -> Value.t) -> t

  (*copy all constraints for some variable to a different t if they still hold for a new x' with x' (cond) x *)
  val transfer : int -> int -> Relation.t -> t -> (int -> Rhs.t) -> (int -> Value.t) -> t -> (int -> Rhs.t) -> (int -> Value.t) -> t

  val show_formatted : (int -> string) -> t -> string
  val hash : t -> int
  val empty : t
  val is_empty : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val modify_variables_in_domain : t -> int array -> (int -> int -> int) -> t
  val forget_variable : t -> int -> t

end

module NoInequalties : TwoVarInequalities = struct
  type t = unit

  let get_relations _ _ _ = []
  let meet_relation _ _ _ _ _ _ = (), []

  let meet _ _ _ = ()
  let narrow _ _ _ = ()

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

  let transfer _ _ _ _ _ _ _ _ _ = ()
end

module type Coeffs = sig
  type t
  val implies : Value.t -> Value.t -> t option -> t -> bool
  val meet : Value.t -> Value.t -> t -> t -> t 
  val narrow : Value.t -> Value.t -> t -> t -> t

  val join : int -> int -> (int -> Value.t) -> (int -> Value.t) -> t option -> t option -> t option
  val widen : int -> int -> (int -> Value.t) -> (int -> Value.t) -> t option -> t option -> t option

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val show_formatted : string -> string -> t -> string
end

module CommonActions (Coeffs : Coeffs) = struct

  type t = Coeffs.t IntMap.t IntMap.t  [@@deriving eq, ord ]

  let empty = IntMap.empty
  let is_empty = IntMap.is_empty
  let hash t = IntMap.fold (fun _ ys acc -> IntMap.fold (fun _ coeff acc -> Coeffs.hash coeff + 3*acc) ys (5*acc)) t 0

  let show_formatted formatter t = 
    if IntMap.is_empty t then "{}" else
    if IntMap.exists (fun _ -> IntMap.is_empty) t then failwith "Map not sparse" else
      let str = IntMap.fold (fun x ys acc -> IntMap.fold (fun y coeff acc -> Printf.sprintf "%s , %s" (Coeffs.show_formatted (formatter x) (formatter y) coeff) acc) ys acc) t "" 
      in "{" ^ String.sub str 0 (String.length str - 3) ^ "}"

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
      Coeffs.implies (get_value_t1 x) (get_value_t1 y) t1_coeff  t2_coeff
    in
    IntMap.for_all (fun x ys -> IntMap.for_all (implies x) ys) t2

  let meet_one_coeff narrow get_value x y coeff t =
    let coeff_t = get_coeff x y t in
    let coeff_met = match coeff_t with 
      | None -> coeff
      | Some coeff_t -> (if narrow then Coeffs.narrow else Coeffs.meet) (get_value x) (get_value y) coeff coeff_t
    in set_coeff x y coeff_met t

  let meet get_value t1 t2 = 
    IntMap.fold (fun x ys t -> IntMap.fold (meet_one_coeff false get_value x) ys t) t2 t1

  let narrow get_value t1 t2 = 
    IntMap.fold (fun x ys t -> IntMap.fold (meet_one_coeff true get_value x) ys t) t2 t1

  let join' widen t1 get_val_t1 t2 get_val_t2 = 
    let merge_y x y = (if widen then Coeffs.widen else Coeffs.join) x y get_val_t1 get_val_t2
    in let merge_x x ys1 ys2 = 
         let ignore_empty ls = 
           if IntMap.is_empty ls then None
           else Some ls
         in match ys1, ys2 with
         | Some ys1, None -> ignore_empty (IntMap.filter (fun y coeff -> Coeffs.implies (get_val_t2 x) (get_val_t2 y) None coeff ) ys1)
         | None, Some ys2 -> ignore_empty (IntMap.filter (fun y coeff -> Coeffs.implies (get_val_t1 x) (get_val_t1 y) None coeff ) ys2)
         | Some ys1, Some ys2 -> ignore_empty (IntMap.merge (merge_y x) ys1 ys2)
         | _, _ -> None in 
    IntMap.merge (merge_x) t1 t2

  let join = join' false
  let widen = join' true

end

(*Equations of the type x < y*)
module NoCoeffs = struct
  type t = unit [@@deriving eq, ord, hash ]

  let implies x y t1_opt _ = match t1_opt with 
    | Some _ -> true
    | None -> match Value.maximal x, Value.minimal y with 
      | Some x, Some y -> TopIntOps.compare x y < 0
      | _, _ -> false

  let meet x y _ _ = ()

  let narrow = meet

  let join x y get_val_t1 get_val_t2 t1 t2 =      
    let of_bool b = if b then Some () else None in  
    match t1 with 
    | Some t1 -> of_bool (implies (get_val_t2 x) (get_val_t2 y) t2 t1)
    | None -> match t2 with 
      | Some t2 -> of_bool (implies (get_val_t1 x) (get_val_t1 y) t1 t2)
      | None -> None

  let widen = join

  let show_formatted x y t = x ^ " < " ^ y

  let add_constraints x y x_val y_val acc = 
    let acc = match Value.maximal y_val with
      | Some (Int v) -> (x, Value.ending @@ Z.sub v Z.one) :: acc
      | _ -> acc
    in match Value.minimal x_val with
    | Some (Int v) -> (y, Value.starting @@ Z.add v Z.one) :: acc
    | _ -> acc

end

(*Semantics: x -> y -> () => x < y*)
module SimpleInequalities : TwoVarInequalities = struct
  module Coeffs = NoCoeffs
  include CommonActions(Coeffs)

  let get_relations x y t = 
    let open Relation in
    let check_inequality ((var_x,o_x,d_x), val_x) ((var_y,o_y,d_y), val_y) =
      if M.tracing then M.trace "get_relations" "checking x: %s, y: %s" (Rhs.show (var_x,o_x,d_x)) (Rhs.show (var_y,o_y,d_y));
      match var_x, var_y with
      | Some (c_x, x), Some (c_y, y) -> begin
          match get_coeff x y t with 
          | None -> 
            if M.tracing then M.trace "get_relations" "no inequality for roots";
            [] (*No information*)
          | Some _ -> (*we know x < y -> check if this translates to x' < y' or x' > y'*) 
            let d_c = Z.sub (Z.mul d_x c_y) (Z.mul d_y c_x) in
            let d_o = Z.sub (Z.mul o_x d_y) (Z.mul o_y d_x) in
            let x_d_c = Value.mul val_x (Value.of_bigint d_c) in
            if Z.lt c_y Z.zero && Value.leq x_d_c (Value.ending d_o) (* c_y < 0, x * d_c <= d_o*) 
            then[ (Gt, Z.zero)] (*x' > y '*)
            else if Z.gt c_y Z.zero && Value.leq x_d_c (Value.starting d_o) (* c_y > 0, x * d_c >= d_o*) 
            then [(Lt, Z.zero)] (*x' < y '*)
            else  
              let d_c' = Z.neg d_c in
              let d_o' = Z.neg d_o in
              let y_d_c = Value.mul val_y (Value.of_bigint d_c') in
              if Z.lt c_x Z.zero && Value.leq y_d_c (Value.starting d_o') (* c_x < 0, y * d_c >= d_o*) 
              then [(Gt, Z.zero) ](*x' > y '*)
              else if  Z.gt c_x Z.zero && Value.leq y_d_c (Value.ending d_o') (* c_x > 0, y * d_c <= d_o*) 
              then [Lt, Z.zero] (*x' < y '*)
              else []
        end
      | _, _ -> failwith "Inequalities.get_relations does not take constants directly" (*TODO should we take the coefficients directly to enforce this*)
    in 
    let res = check_inequality x y in
    if res = [] then List.map invert @@ check_inequality y x
    else res

  let get_relations x y t = 
    let res = get_relations x y t in
    if M.tracing then M.trace "get_relations" "result: %s" (BatList.fold (fun acc c -> acc ^ ", " ^ Relation.show "x'" c "y'") "" res);
    res

  let meet_relation x' y' cond get_rhs get_value t =
    let open Relation in
    (*strict: if the inequality is strict *)
    let meet_less_root x y strict t = 
      if M.tracing then M.tracel "meet_relation" "meet_less_root x: %d y: %d strict: %b " x y strict;  
      let union = IntMap.union (fun _ _ _ -> Some ()) (IntMap.find_default IntMap.empty x t) (IntMap.find_default IntMap.empty y t) 
      in let union' = if strict then IntMap.add y () union else union 
      in if IntMap.mem x union' then raise EConj.Contradiction
      else if IntMap.is_empty union' then t, []
      else IntMap.add x union' t, IntMap.fold (fun z _ acc -> Coeffs.add_constraints x z (get_value x) (get_value z) acc) union' []
    in
    let meet_less x' y' strict t = 
      if M.tracing then M.tracel "meet_relation" "meet_less x': %d y': %d strict: %b" x' y' strict;  
      let get_rhs' lhs = 
        match get_rhs lhs with 
        | (Some (c,v),o,d) -> c,v,o,d
        | (None, o, d) -> Z.one, lhs, Z.zero, Z.one
      in let (c_x, x, o_x, d_x) = get_rhs' x'
      in let (c_y, y, o_y, d_y) = get_rhs' y'
      in if M.tracing then M.tracel "meet_relation" "x' = %s, y' = %s " (Rhs.show (Some (c_x, x),o_x,d_x)) (Rhs.show (Some (c_y,y),o_y,d_y));  
      let val_x = get_value x
      in let val_y = get_value y in
      let d_c = Z.sub (Z.mul d_x c_y) (Z.mul d_y c_x) in
      let d_o = Z.sub (Z.mul o_x d_y) (Z.mul o_y d_x) in
      let x_d_c = Value.mul val_x (Value.of_bigint d_c) in
      if Value.leq x_d_c (Value.ending d_o) then (*x * d_c <= d_o*)
        (*We are strict iff we have been strict before or this bound is strict*)
        if Z.lt c_y Z.zero then meet_less_root y x (strict || Value.leq x_d_c (Value.ending (Z.sub d_o Z.one))) t
        else meet_less_root x y (strict || Value.leq x_d_c (Value.ending (Z.sub d_o Z.one))) t
      else
        let d_c' = Z.neg d_c in
        let d_o' = Z.neg d_o in
        let y_d_c = Value.mul val_y (Value.of_bigint d_c') in
        if Value.leq y_d_c (Value.starting d_o') then (*x * d_c >= d_o*)
          (*We are strict iff we have been strict before or this bound is strict*)
          if Z.gt c_y Z.zero then meet_less_root x y (strict || Value.leq y_d_c (Value.ending (Z.add d_o' Z.one))) t
          else meet_less_root y x (strict || Value.leq y_d_c (Value.ending (Z.add d_o' Z.one))) t
        else t, []
    in
    match cond with 
    | Gt, z when Z.geq z Z.zero -> meet_less y' x' true t 
    | Gt, z when Z.equal z Z.minus_one -> meet_less y' x' false t 
    | Eq, z when Z.equal z Z.zero -> 
      let rhs_x = get_rhs x' in
      let rhs_y = get_rhs y' in
      if M.tracing then M.tracel "meet_relation" "in equality: x' (var_%d) = %s, y' (var_%d)= %s " x' (Rhs.show rhs_x) y' (Rhs.show rhs_y);  
      if Rhs.equal rhs_x rhs_y then begin
        if M.tracing then M.tracel "meet_relation" "equality with same rhs";  
        let x,y = match rhs_x, rhs_y with 
          | (Some (_,x), _,_), (Some (_,y), _,_) -> (x,y)
          | (None,_,_), (None, _,_) -> x',y'
          | _,_ -> failwith "Should never happen"
        in
        let union = IntMap.union (fun _ _ _ -> Some ()) (IntMap.find_default IntMap.empty x t) (IntMap.find_default IntMap.empty y t) in
        if IntMap.mem x union || IntMap.mem y union then raise EConj.Contradiction
        else if IntMap.is_empty union then t, []
        else IntMap.add x union @@ IntMap.add y union t, [] (*TODO more is possible for refinement, but is it worth it?*)
      end else 
        let (t', acc) = meet_less y' x' false t in
        let (t'', acc2) = meet_less x' y' false t' in
        t'', acc @ acc2 
    | Eq, z when Z.gt z Z.zero -> meet_less y' x' true t
    | Eq, z when Z.lt z Z.zero -> meet_less x' y' true t
    | Lt, z when Z.equal z Z.one -> meet_less x' y' false t 
    | Lt, z when Z.leq z Z.zero-> meet_less x' y' true t 
    | _ -> t, [] (*TODO adapt the equations to take care of offsets!*)

  let meet_relation x y c r v t = 
    if M.tracing then M.tracel "meet_relation" "meeting %s with %s" (show t) (Relation.show ("var_"^Int.to_string x) c ("var_"^Int.to_string y));  
    let res, refinements = meet_relation x y c r v t in
    if M.tracing then M.tracel "meet_relation" "result: %s " (show res);  
    res, refinements

  let transfer x x_new cond t_old get_rhs_old get_value_old t get_rhs get_value = 
    let get_old_condition x y = 
      let get_information lhs =
        let rhs = get_rhs_old lhs in
        match rhs with 
        | (Some (_,var), _ ,_) -> (rhs, get_value_old var)
        (*We need to know which root a constant is referring to, so we use the trivial equation to carry that information*)
        | (_,o,_) -> (Rhs.var_zero lhs, Value.of_bigint o) 
      in
      get_relations (get_information x) (get_information y) t_old 
    in let vars_to_check = 
         let root = match get_rhs_old x with
           | (Some (_,var), _ ,_) -> var
           | (_,o,_) -> x
           (*we need to check all y with root -> y -> coeff  or y -> root -> coeff*)
         in BatEnum.append (IntMap.keys @@ IntMap.find_default IntMap.empty root t_old) (List.enum @@ IntMap.fold (fun k ys acc -> if IntMap.mem root ys then k :: acc else acc) t_old [])   
    in let keep_less = match cond with 
        | Relation.Eq | Lt -> true 
        | _ -> false
    in let keep_greater = match cond with 
        | Eq | Gt -> true 
        | _ -> false
    in let transfer_single_var t' y = 
         match get_old_condition x y with 
         |[ (Lt, o)] -> (*transfering the variables does not lead to new information -> drop the refinements*)
           if keep_less then fst @@ meet_relation x_new y (Lt, o) get_rhs get_value t' else t'
         | [(Gt, o)] ->
           if keep_greater then fst @@ meet_relation x_new y (Gt, o) get_rhs get_value t' else t'
         | _ -> t'
    in BatEnum.fold (transfer_single_var) t vars_to_check

  (*TODO we currently just strip the offset, but could take advantage of the offset*)
  let transfer x x_new cond t_old get_rhs_old get_value_old t get_rhs get_value = 
    match cond with 
    | Relation.Eq, o when Z.equal o Z.zero -> transfer x x_new Eq t_old get_rhs_old get_value_old t get_rhs get_value 
    | Relation.Lt, o when Z.leq o Z.zero -> transfer x x_new Lt t_old get_rhs_old get_value_old t get_rhs get_value 
    | Relation.Gt, o when Z.geq o Z.zero -> transfer x x_new Gt t_old get_rhs_old get_value_old t get_rhs get_value 
    | _ -> t 

  let transfer x x_new cond t_old get_rhs_old get_value_old t get_rhs get_value = 
    let res = transfer x x_new cond t_old get_rhs_old get_value_old t get_rhs get_value in
    if M.tracing then M.tracel "transfer" "transfering  with %s from %s into %s -> %s" (Relation.show (Int.to_string x) cond (Int.to_string x_new) ) (show t_old) (show t) (show res);  
    res

end 

(*List of inequalities ax < by + c, mapping a and b to c*)
(*We need to make sure that x has lower index than y to keep this representation unique! *)
module ArbitraryCoeffsSet = struct
  module Key = struct
    type t = Q.t * Q.t [@@deriving ord]
  end 
  module CoeffMap = Map.Make(Key)

  type t = Q.t CoeffMap.t [@@deriving eq, ord]

  let hash t = CoeffMap.fold (fun (a,b) c acc -> let open Q in Z.hash @@ Q.to_bigint @@ a + b + b + c+c+ c) t 0

  let show_single_inequality x y a b c = Printf.sprintf "%s %s < %s %s + %s" (Q.to_string a) x (Q.to_string b) y (Q.to_string c)

  let show_formatted x y t = 
    CoeffMap.fold (fun (a,b) c acc -> Printf.sprintf "%s , %s" (show_single_inequality x y a b c ) acc) t ""

  let empty = CoeffMap.empty

  (*TODO this function should limit how many inequalities we are saving. What information does this need?
    likely: values, coefficients of Rhs relating to x and y*)
  (* Throw away inequalities that are least useful:
     implied by the current values? -> need to adapt implies to check all inequalities !!, otherwise join is not valid
     least rhs with fitting coefficients *)
  let limit = identity

  let meet_single_inequality refine_data narrow x_val y_val (a,b) c t = 
    (*calculate value refine. If one of the coefficients is zero, we should not add it to the map*)
    let refinements, skip_adding = match refine_data with 
      | None -> [], (Q.equal a Q.zero) || Q.equal b Q.zero  
      | Some (x,y) -> 
        let round_up q = Z.cdiv (Q.num q) (Q.den q) in
        let round_down q = Z.fdiv (Q.num q) (Q.den q) in
        let x_refine a_sign = 
          let ba = Q.div b a in
          if a_sign = 1 then (*x < b/a y + c/a*)
            let max_y = match Value.maximal (Value.mul y_val(Value.of_bigint (round_down ba))) , Value.maximal @@ Value.mul y_val (Value.of_bigint (round_up ba)) with
              | Some a, Some b -> TopIntOps.max a b
              | _,_ -> failwith "trying to refine bot in inequalities"
            in match max_y with 
            | Int max -> [x, Value.ending @@ Z.add Z.minus_one @@ Z.add max @@ round_up @@ Q.div c a]
            | _ -> [] 
          else (*x > b/a y + c/a*)
            let min_y = match Value.minimal (Value.mul y_val (Value.of_bigint (round_down ba))) , Value.minimal @@ Value.mul y_val (Value.of_bigint (round_up ba)) with
              | Some a, Some b -> TopIntOps.min a b
              | _,_ -> failwith "trying to refine bot in inequalities"
            in match min_y with 
            | Int min -> [x, Value.starting @@ Z.add Z.one @@ Z.add min @@ round_down @@ Q.div c a]
            | _ -> [] 
        in let y_refine b_sign =
             let ba = Q.div a b in
             if b_sign = 1 then (*a/b x - c/b < y*)
               let min_x = match Value.minimal (Value.mul x_val (Value.of_bigint (round_down ba))) , Value.minimal @@ Value.mul x_val (Value.of_bigint (round_up ba)) with
                 | Some a, Some b -> TopIntOps.min a b
                 | _,_ -> failwith "trying to refine bot in inequalities"
               in match min_x with 
               | Int min -> [y, Value.starting @@ Z.add Z.one @@ Z.sub min @@ round_up @@ Q.div c a]
               | _ -> [] 
             else (*a/b x - c/b > y*)
               let max_x = match Value.maximal (Value.mul x_val (Value.of_bigint (round_down ba))) , Value.maximal @@ Value.mul x_val (Value.of_bigint (round_up ba)) with
                 | Some a, Some b -> TopIntOps.max a b
                 | _,_ -> failwith "trying to refine bot in inequalities"
               in match max_x with 
               | Int max -> [y, Value.ending @@ Z.add Z.minus_one @@ Z.sub max @@ round_down @@ Q.div c a]
               | _ -> [] 
        in match Q.compare a Q.zero, Q.compare b Q.zero with
        | 0, 0 -> (*0 < c*) if Q.gt c Q.zero then [], true else raise EConj.Contradiction
        | 0, -1 -> (* -c / b > y*) [y, Value.ending @@ Z.add Z.minus_one @@ round_up @@ Q.neg @@ Q.div c b] , true
        | 0, 1 ->  (* -c / b < y*) [y, Value.starting @@ Z.add Z.one @@ round_down @@ Q.neg @@ Q.div c b] , true
        | 1, 0 -> (*x < c / a*) [x, Value.ending @@ Z.add Z.minus_one @@ round_up @@ Q.div c a ], true
        | -1,0 -> (*x > c / a*) [x, Value.starting @@ Z.add Z.one @@ round_down @@ Q.div c a ], true
        | a_sign, b_sign -> x_refine a_sign @ y_refine b_sign, false
    in if skip_adding then t, refinements 
    else match CoeffMap.find_opt (Q.neg a, Q.neg b) t with  (*Look for contradicting inequality*)
      | Some c' when Q.geq (Q.add Q.one @@ Q.neg c') c -> raise EConj.Contradiction
      (*TODO if c = - c' + 2 , then we have an equality -> maybe we can update the econj domain *) 
      | _ ->  match CoeffMap.find_opt (a,b) t with
        | Some c_old -> (*TODO if narrow then undefined "narrow not implemented" else *)CoeffMap.add (a,b) (Q.min c c_old) t, refinements
        | None -> CoeffMap.add (a,b) c t , refinements

  (*when meeting, the values should already ben refined before -> ignore the refinement data*)
  let meet' narrow x_val y_val t1 t2 = CoeffMap.fold (fun k c t -> fst @@ meet_single_inequality None narrow x_val y_val k c t) t1 t2

  (*TODO: We could check all inequalities if they imply this for the specific intervals, but it might be too inefficient! leq O(|t|^2) instead of O(|t|) ?*)
  let implies_single_inequality x_val y_val t_opt (a,b) c = 
    let implied_by_value () = (*TODO will rounding lead to problems?*)
      let ax = Value.div (Value.mul x_val @@ Value.of_bigint @@ Q.num a) @@ Value.of_bigint @@ Q.den a in
      let by = Value.div (Value.mul y_val @@ Value.of_bigint @@ Q.num b) @@ Value.of_bigint @@ Q.den b in
      let c' = Value.maximal @@ Value.sub ax by in
      match c' with 
      | Some (TopIntOps.Int c') -> Q.lt (Q.of_bigint c') c
      | _ -> false
    in let implied_by_value () = 
         let res = implied_by_value () in
         if M.tracing then M.trace "implied" "checking %s returned %b" (show_single_inequality (Value.show x_val) (Value.show y_val) a b c) res ;
         res
    in match t_opt with 
    | Some t -> begin match CoeffMap.find_opt (a,b) t with 
        | Some c' -> Q.leq c' c
        | None -> implied_by_value ()
      end 
    | None -> implied_by_value ()
  let implies x_val y_val t_opt t = CoeffMap.for_all (implies_single_inequality x_val y_val t_opt) t

  let join' widen x y get_val_t1 get_val_t2 t1 t2 =
    let join_single_inequality (a,b) c1 c2 = 
      match c1, c2 with
      | None, None -> None 
      | Some c1, Some c2 -> if widen && c2 > c1 then None else Some (Q.max c1 c2) (*TODO widening thresholds?*)
      | Some c1, None -> if implies_single_inequality (get_val_t2 x) (get_val_t2 y) None (a,b) c1 then Some c1 else None 
      | None, Some c2 -> if implies_single_inequality (get_val_t1 x) (get_val_t1 y) None (a,b) c2 then Some c2 else None 
    in
    let ignore_empty ls = 
      if CoeffMap.is_empty ls then None
      else Some ls
    in
    match t1, t2 with 
    | None, None -> None
    | Some t1, None -> ignore_empty @@ limit @@ CoeffMap.filter (implies_single_inequality (get_val_t2 x) (get_val_t2 y) None) t1
    | None, Some t2 -> ignore_empty @@ limit @@ CoeffMap.filter (implies_single_inequality (get_val_t1 x) (get_val_t1 y) None) t2
    | Some t1, Some t2 -> ignore_empty @@ limit @@ CoeffMap.merge join_single_inequality t1 t2

  let join = join' false
  let widen = join' true
  let meet = meet' false
  let narrow = meet' true

  (*Convert two righthandsides into coefficients to an inequality*)
  let coeffs_from_rhss (cx, ox, dx) (cy, oy, dy)= (Q.make cx dx, Q.make cy dy, Q.sub (Q.make oy dy) (Q.make ox dx))

end


module LinearInequalities: TwoVarInequalities = struct
  module Coeffs = ArbitraryCoeffsSet
  include CommonActions(Coeffs)

  (*Is it woth it in here to check all inequalities inside the intervals?*)
  let rec get_relations (((var_x,o_x,d_x), val_x) as x') (((var_y,o_y,d_y), val_y) as y') t = 
    match var_x, var_y with
    | Some (c_x, x), Some (c_y, y) -> 
      if x > y then
        (*We save information only in one of the directions -> check the other one*)
        List.map Relation.invert @@ get_relations y' x' t
      else begin
        if M.tracing then M.trace "is_less_than" "checking x': %s, y': %s" (Rhs.show @@ fst x') (Rhs.show @@ fst y');
        match get_coeff x y t with 
        | None -> begin if M.tracing then M.trace "is_less_than" "no inequality for roots"; [] end (*No information*)
        | Some coeff -> (*TODO should we check all inequalities here? how could we do that*)
          let (a,b,c_rhs) = Coeffs.coeffs_from_rhss (c_x,o_x,d_x) (c_y,o_y,d_y) in
          let upper_bound = match Coeffs.CoeffMap.find_opt (a,b) coeff with
            | None -> []
            | Some c_ineq -> 
              let c' = Q.sub c_ineq c_rhs in 
              [Relation.Lt, Z.fdiv (Q.num c') (Q.den c')]
          in match Coeffs.CoeffMap.find_opt (Q.neg a, Q.neg b) coeff with (*lower bound*)
          | None -> upper_bound
          | Some c_ineq -> 
            let c' =  Q.neg ( Q.add c_ineq c_rhs) in
            (Gt, Z.cdiv (Q.num c') (Q.den c')) :: upper_bound
      end
    | _, _ -> failwith "Inequalities.is_less_than does not take constants directly" (*TODO should we take the coefficients directly to enforce this*)

  let get_relations x y t = 
    let res = get_relations x y t in
    if M.tracing then M.trace "get_relations" "result: %s" (BatList.fold (fun acc c -> acc ^ ", " ^ Relation.show "x'" c "y'") "" res);
    res

  let rec meet_relation x' y' cond get_rhs get_value t = 
    let get_rhs' lhs =
      let rhs = get_rhs lhs in
      match rhs with 
      | (Some (c,var), o ,d) -> (c,o,d), var
      | (None, o ,d)-> (Z.one,Z.zero,Z.one), lhs (*TODO I think we should not save relations to constants here, as that information will be saved in the intervals, but am not sure if this is always done*)
    in let (rhs_x, x) = get_rhs' x'
    in let (rhs_y, y) = get_rhs' y'
    in if x > y then
      (*We save information only in one of the directions*)
      meet_relation y' x' (Relation.invert cond) get_rhs get_value t
    else
      let coeffs = match get_coeff x y t with
        | None -> Coeffs.empty
        | Some c -> c
      in let (a,b,c_rhs) = Coeffs.coeffs_from_rhss rhs_x rhs_y
      in let meet_relation_roots (a,b) c t = 
           if M.tracing then M.tracel "meet_relation" "meet_relation_roots: %s var_%d < %s var_%d + %s" (Q.to_string a) x (Q.to_string b) y (Q.to_string c);
           (*do not save inequalities refering to the same variable*) 
           if x = y then
             if a = b then
               if Q.leq c Q.zero then raise EConj.Contradiction
               else t, [] (*trivially true*)
             else (*refine the value in this case*)
               let ab = Q.sub a b in
               if Q.gt ab Q.zero then 
                 let max = Q.sub (Q.div c ab) Q.one in
                 t, [x, Value.ending @@ Z.cdiv (Q.num max) (Q.den max)]
               else
                 let min = Q.add (Q.div c ab) Q.one in
                 t, [x, Value.starting @@ Z.fdiv (Q.num min) (Q.den min)]
           else Coeffs.meet_single_inequality (Some (x,y)) false (get_value x) (get_value y) (a,b) c t
      in let (new_coeffs, refine_acc) = match cond with 
          | Relation.Lt, o -> meet_relation_roots (a,b) (Q.add c_rhs @@ Q.of_bigint o) coeffs
          | Gt, o -> meet_relation_roots (Q.neg a ,Q.neg b) (Q.neg @@ (Q.add c_rhs @@ Q.of_bigint o)) coeffs
          | Eq, o -> coeffs, []
          (*TODO: I think this should always be stored by the lin2vareq domain (at least the way we are generating this information)
            (*meet with < +1 und > -1*)
            if M.tracing then M.tracel "meet_relation" "meeting equality!";
            meet_relation_roots (a,b) (Q.add c_rhs @@ Q.of_bigint @@ Z.add Z.one o) @@
            meet_relation_roots (Q.neg a ,Q.neg b) (Q.neg @@ (Q.add c_rhs @@ Q.of_bigint @@ Z.add Z.minus_one o)) coeffs*)
      in if Coeffs.CoeffMap.is_empty new_coeffs 
      then remove_coeff x y t , refine_acc
      else set_coeff x y new_coeffs t, refine_acc

  let meet_relation x y c r v t = 
    if M.tracing then M.tracel "meet_relation" "meeting %s with %s" (show t) (Relation.show ("var_"^Int.to_string x) c ("var_"^Int.to_string y));  
    let res, refine_acc = meet_relation x y c r v t in
    if M.tracing then M.tracel "meet_relation" "result: %s, refinements: %s " (show res) (List.fold (fun acc (var,value) -> Printf.sprintf "var_%d: %s, %s" var (Value.show value) acc) "" refine_acc);  
    res, refine_acc

  (*TODO very similar to simple equalities -> generalise?*)
  let transfer x x_new cond t_old get_rhs_old get_value_old t get_rhs get_value = 
    let get_old_condition x y = 
      let get_information lhs =
        let rhs = get_rhs_old lhs in
        match rhs with 
        | (Some (_,var), _ ,_) -> (rhs, get_value_old var)
        (*We need to know which root a constant is referring to, so we use the trivial equation to carry that information*)
        | (_,o,_) -> (Rhs.var_zero lhs, Value.of_bigint o) 
      in
      get_relations (get_information x) (get_information y) t_old 
    in let vars_to_check = 
         let root = match get_rhs_old x with
           | (Some (_,var), _ ,_) -> var
           | (_,o,_) -> x
           (*we need to check all y with root -> y -> coeff  or y -> root -> coeff*)
           (*TODO we know all vars greater than y can not contain y *)
         in BatEnum.append (IntMap.keys @@ IntMap.find_default IntMap.empty root t_old) (List.enum @@ IntMap.fold (fun k ys acc -> if IntMap.mem root ys then k :: acc else acc) t_old [])   
    in let transfer_single_condition y t' old_cond =
         match Relation.combine cond old_cond with 
         | Some new_cond -> fst @@ meet_relation x_new y new_cond get_rhs get_value t' 
         | None -> t'
    in let transfer_single_var t' y = List.fold (transfer_single_condition y ) t' (get_old_condition x y) 
    in BatEnum.fold (transfer_single_var) t vars_to_check

  let transfer x x_new cond t_old get_rhs_old get_value_old t get_rhs get_value = 
    if M.tracing then M.tracel "transfer" "transfering  with %s from %s into %s" (Relation.show (Int.to_string x) cond (Int.to_string x_new) ) (show t_old) (show t);  
    let res = transfer x x_new cond t_old get_rhs_old get_value_old t get_rhs get_value in
    if M.tracing then M.tracel "transfer" "result: %s" (show res);  
    res

end

(*TODOs:*)
(*adapt simple equalities to take advantage of the offset!*)
(*domain inbetween these two: with offset between roots? -> should be trivial to implement*)
(*what is required of narrow?*)
(*limit in ArbitraryCoeaffsList*)
(*store information about representants to avoid recalculating them: congruence information, group size/ coefficients ??*)
(*widening thresholds: from offsets of rhs?*)
(*rebase to main branch*)