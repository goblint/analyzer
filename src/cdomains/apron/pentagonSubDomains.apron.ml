open Batteries
open GoblintCil
open VectorMatrix

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

  (*returns the best lower and upper bound for the relation between variables with the given Rhs*)
  val get_relations :  (Rhs.t * Value.t) -> (Rhs.t * Value.t) -> t -> Relation.t list

  (*meet relation between two variables. also returns a list of value refinements *)
  val meet_relation : int -> int -> Relation.t -> (int -> Rhs.t) -> (int -> Value.t) -> t -> t * (int * Value.t) list

  (*substitutes all occurences of a variable by a rhs*)
  val substitute : t -> int -> Z.t * int * Z.t * Z.t -> t

  (*called after every operation to limit the inequalities to the most relevant*)
  val limit : EConj.t -> t -> t

  val meet : (int -> Value.t) -> t -> t -> t
  val narrow : (int -> Value.t) -> t -> t -> t

  val leq : t -> (int -> Value.t) -> t -> bool

  val join : t -> (int -> Value.t) -> t -> (int -> Value.t) -> t
  val widen : t -> (int -> Value.t) -> t -> (int -> Value.t) -> t

  (*a join can split groups of variables. This function copies the relevant inequalities to all new representants*)
  val copy_to_new_representants : EConj.t -> EConj.t -> t -> t

  (*restore inequalities after an assignment that makes the assigned to variable have a known relation to before the assignment *)
  val transfer : int -> Relation.t -> t -> (int -> Rhs.t) -> (int -> Value.t) -> t -> (int -> Rhs.t) -> (int -> Value.t) -> t

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

  let limit _ _ = ()

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

  let substitute _ _ _ = ()

  let copy_to_new_representants _ _ _ = ()

  let transfer _ _ _ _ _ _ _ _ = ()
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

  let ignore_empty ls = 
    if IntMap.is_empty ls then None
    else Some ls

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
         match ys1, ys2 with
         | Some ys1, None -> ignore_empty (IntMap.filter (fun y coeff -> Coeffs.implies (get_val_t2 x) (get_val_t2 y) None coeff ) ys1)
         | None, Some ys2 -> ignore_empty (IntMap.filter (fun y coeff -> Coeffs.implies (get_val_t1 x) (get_val_t1 y) None coeff ) ys2)
         | Some ys1, Some ys2 -> ignore_empty (IntMap.merge (merge_y x) ys1 ys2)
         | _, _ -> None in 
    IntMap.merge (merge_x) t1 t2

  let join = join' false
  let widen = join' true

end

(* TODO Redo this


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

*)


let qhash q = Z.hash (Q.num q) + 13 * Z.hash (Q.den q)

module LinearInequality = struct
  (*Normalised representation of an inequality through the origin 
    a/b x <= y (or >=) bzw. slope and direction. infinite slope represents 0 <= x / 0 >= x*)
  module OriginInequality = struct (*Separate module so we can use it as key in a map*)
    type t = LT of Q.t | GT of Q.t

    (*make the representation of inequalities without y unique*)
    let norm = function
      | GT s when Q.equal s Q.minus_inf -> LT Q.inf
      | LT s when Q.equal s Q.minus_inf -> GT Q.inf
      | t -> t

    (*We want the inequalities to be ordered by angle (with arbitrary start point and direction), which is tan(slope) (+ pi for other direction) *)
    (*because tan is monotone, we can simply sort by slope: LT < GT, LT ordered by a, GT ordered by -a*)
    let compare t1 t2 = match t1, t2 with  
      | LT _, GT _ -> -1
      | GT _, LT _ -> 1
      | LT a1, LT a2 -> Q.compare a1 a2
      | GT a1, GT a2 -> -(Q.compare a1 a2)

    let equal t1 t2 = 0 = compare t1 t2

    let hash = function LT q -> qhash q | GT q -> 7 * qhash q  

    let get_slope = function LT a -> a | GT a -> a

    let negate = function 
      | LT s -> GT s
      | GT s -> LT s

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
      (match k with LT _ -> "<=" | GT _ -> ">=") 
      (show_var (Q.den s) y (Q.equal c Q.zero)) 
      (show_offset @@ if Q.equal s Q.inf then c else Q.mul c @@ Q.of_bigint @@ Q.den s )

  (*Convert into coefficients of inequality ax + by <= c
    Useful because the TVLI paper (DOI: 10.1007/3-540-45013-0_7) uses this representation *)
  let to_coeffs = function
    | OriginInequality.LT s, c when Q.equal s Q.inf -> (Q.one,Q.zero,c)
    | GT s, c when Q.equal s Q.inf -> (Q.minus_one, Q.zero, Q.neg c)
    | LT s, c -> (s,Q.minus_one,c)
    | GT s, c -> (Q.neg s, Q.one, Q.neg c)

  let to_coeffs t = 
    let a,b,c as res = to_coeffs t in
    if M.tracing then M.trace "entails" "slope %s, whole: %s -> %s,%s,%s" (Q.to_string @@ OriginInequality.get_slope @@ fst t) (show "x" "y" t)  (Q.to_string a) (Q.to_string b) (Q.to_string c);
    res

  (*From TVLI: check if one or two inequalities imply an inequality*)
  let entails1 (s1,c1) (s2,c2) = OriginInequality.equal s1 s2 &&  match s1 with LT _ -> Q.leq c1 c2 | GT _ -> Q.geq c1 c2

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
        let c' = match k with LT _ -> c | GT _ -> neg c in
        Some c'

  (*convert interval information into inequalities*)
  let from_values x_val y_val =
    let open OriginInequality in
    let ineqs = match Value.maximal x_val with 
      | Some (Int z) -> (*x <= z *) [LT Q.inf, Q.of_bigint z] 
      | _ -> []
    in let ineqs = match Value.minimal x_val with 
        | Some (Int z) -> (*x >= z *) (GT Q.inf, Q.of_bigint z) :: ineqs
        | _ -> ineqs
    in let ineqs = match Value.maximal y_val with 
        | Some (Int z) -> (*y <= z *) (GT Q.zero, Q.neg @@ Q.of_bigint z ) :: ineqs
        | _ -> ineqs
    in let ineqs = match Value.minimal y_val with 
        | Some (Int z) -> (*y >= z *) (LT Q.zero, Q.neg @@ Q.of_bigint z ) :: ineqs
        | _ -> ineqs
    in ineqs

  (*Convert two righthandsides into an inequality isLessThan is the direction of the inequality and needed for making the inequality non-strict while we still know that all variables are integers*)
  let from_rhss (cx, ox, dx) (cy, oy, dy) isLessThan_opt = 
    let non_strict_offset = match isLessThan_opt with None -> Q.zero | Some isLessThan -> if isLessThan then Q.minus_one else Q.one in
    let a,b,c = (Q.make cx dx, Q.make cy dy, Q.add non_strict_offset @@ Q.sub (Q.make oy dy) (Q.make ox dx)) in (*subtracting one to convert it into a nonstrict inequality*)
    let s = Q.div a b in
    if Q.equal b Q.zero 
    then OriginInequality.norm (LT s), Q.div c a
    else if Q.gt b Q.zero 
    then LT s, Q.div c b 
    else GT s, Q.div c b


  (*apply the transformation to the variable on the left side*)
  let substitute_left (coeff, offs, divi) (k,o) =
    let open OriginInequality in
    let s = get_slope k in
    let s' = Q.mul s (Q.make coeff divi) in
    let o' = Q.sub o @@ Q.mul s @@ Q.make offs divi in
    match k with 
    | LT _ -> LT s', o'
    | GT _ -> GT s', o'

  (*apply the transformation to the variable on the right side*)
  let substitute_right (coeff, offs, divi) (k,o) = 
    let open OriginInequality in
    let s = get_slope k in
    let f = Q.make coeff divi in
    let s' = Q.div s f in
    let o' = Q.add (Q.div o f) @@ Q.make offs coeff in
    let k' = match k with 
      | LT _ -> LT s'
      | GT _ -> GT s'
    in if Q.lt f Q.zero 
    then (negate k', Q.neg o')
    else k', o' 

  let swap_sides (k,o) =
    let open Q in
    let open OriginInequality in  
    match k with 
    | LT s when s < zero -> (GT (inv s), - (o / s))
    | LT s -> (LT (inv s), - (o / s))
    | GT s when s < zero -> (LT (inv s), - (o / s))
    | GT s -> (GT (inv s), - (o / s))

  (*combine an inequaliy x_old -> x_new with x_old -> y to x_new -> y*)
  let combine_left (k_rel, o_rel) (k, o) =
    let open OriginInequality in 
    (*factor we need to multiply rel with so that x_old has the same coefficient in both inequalities *)
    let f = Q.div (get_slope k) (get_slope k_rel) in
    let k_rel' = if Q.geq f Q.zero then k_rel else negate k_rel in
    match k_rel', k with 
    | LT _, LT _ 
    | GT _, GT _ -> None (*no useable inequality x_new -> y*)
    | GT _, LT _ -> Some (LT f, Q.sub o (Q.mul f o_rel))
    | LT _, GT _ -> Some (GT f, Q.sub o (Q.mul f o_rel))

  (*combine an inequaliy y_old -> y_new with x -> y_old to x-> y_new*)
  let combine_right (k_rel, o_rel) (k, o) = 
    let open OriginInequality in 
    (*factor we need to multiply the inequality x -> y_old with so that y_old has the same coefficient in both inequalities *)
    let f = (get_slope k) in
    let k' = if Q.geq f Q.zero then k else negate k in
    match k_rel, k' with 
    | LT _, GT _
    | GT _, LT _ -> None 
    | LT s_rel, LT s -> Some (LT (Q.mul s s_rel), Q.add o_rel @@ Q.mul s o)
    | GT s_rel, GT s -> Some (GT (Q.mul s s_rel), Q.add o_rel @@ Q.mul s o) 

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
      BatHashtbl.modify_def (MultiSet.create ()) y (fun set -> MultiSet.change_member_count set s 1; set) m (*TODO unneccessary readding!*)
    | _ -> () (*ignore constants*)
  in
  IntMap.iter add_rhs map;
  m

let coeffs_from_econj = timing_wrap "coeffs" coeffs_from_econj

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

let slopes_from_coeffs = timing_wrap "slopes" slopes_from_coeffs


(*List of inequalities ax < by + c, mapping a and b to c*)
(*We need to make sure that x has lower index than y to keep this representation unique! *)
module ArbitraryCoeffsSet = struct
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
    let open LinearInequality.OriginInequality in
    let filtered = CoeffMap.filter (fun k c -> BatHashtbl.mem slopes (get_slope k) ) t in
    if true then (*TODO add option to configure this*)
      filtered
    else 
      let keep = 10 in  (*TODO add option to configure this. there are possibly 2 inequalities per slope, so should be adjusted accordingly*)
      let comp (k1,_) (k2,_) = 
        let v1 = BatHashtbl.find_default slopes (get_slope k1) 0 in
        let v2 = BatHashtbl.find_default slopes (get_slope k2) 0 in
        v2 - v1 (*list sorts ascending, we need descending -> inverted comparison*)
      in
      CoeffMap.of_list @@ List.take keep @@ List.sort comp @@ CoeffMap.bindings filtered

  (*get the next key in anti-clockwise order*)
  let get_previous k t =
    match CoeffMap.find_first_opt (fun key -> Key.compare key k >= 0) t with
    | None -> CoeffMap.min_binding_opt t (*there is no larger key -> take the first one*)
    | s -> s

  (*get the next key in clockwise order*)
  let get_next k t =
    match CoeffMap.find_last_opt (fun key -> Key.compare key k <= 0) t with
    | None -> CoeffMap.max_binding_opt t (*there is no smaller key -> take the last one*)
    | s -> s

  (*adds the inequality while removing redundant ones. assumes that there is no inequality with this key already in the map*)
  let add_inequality k c t = 
    match get_previous k t, get_next k t with 
    | None, None -> CoeffMap.add k c t (* the map is empty *)
    | Some prev, Some next -> 
      if LinearInequality.entails2 prev next (k,c) then t (*new inequality is already implied*)
      else (*check in both direction if the next inequality is now implied, and remove those that are. recursive because multiple may now be implied*)
        let rec remove_prev prev t = 
          match get_previous (fst prev) t with
          | None -> t
          | Some prev_prev -> 
            if not (LinearInequality.equal prev prev_prev) && LinearInequality.entails2 prev_prev (k,c) prev then
              remove_prev prev_prev @@ CoeffMap.remove (fst prev) t
            else t
        in let rec remove_next next t = 
             match get_next (fst next) t with
             | None -> t
             | Some next_next -> 
               if not (LinearInequality.equal next next_next) && LinearInequality.entails2 next_next (k,c) next then
                 remove_next next_next @@ CoeffMap.remove (fst next) t
               else t
        in CoeffMap.add k c @@ remove_prev prev @@ remove_next next t
    | _,_ -> failwith "impossible state"

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

  let meet_single_inequality refine_data narrow x_val y_val k c t = 
    (*calculate value refine. If one of the coefficients is zero, we should not add it to the map*)
    let refinements, skip_adding = match refine_data with 
      | None -> [], (Q.equal Q.zero @@ Key.get_slope k) || not @@ Q.is_real @@ Key.get_slope k
      | Some (x,y) -> 
        let round_up q = Z.cdiv (Q.num q) (Q.den q) in
        let round_down q = Z.fdiv (Q.num q) (Q.den q) in
        let x_refine = 
          let upper_bound s = (*x <= y / s + c / s*)
            let max_y = match Value.maximal (Value.mul y_val (Value.of_bigint (round_down (Q.inv s)))) , Value.maximal @@ Value.mul y_val (Value.of_bigint (round_up (Q.inv s))) with
              | Some a, Some b -> TopIntOps.max a b
              | _,_ -> failwith "trying to refine bot in inequalities"
            in match max_y with 
            | Int max -> [x, Value.ending @@ Z.add max @@ round_up @@ Q.div c s] (*TODO I'm not sure anymore: why are we rounding up?*)
            | _ -> [] 
          in let lower_bound s = (*x >= y / s + c / s*)
               let min_y = match Value.minimal (Value.mul y_val (Value.of_bigint (round_down (Q.inv s)))) , Value.minimal @@ Value.mul y_val (Value.of_bigint (round_up (Q.inv s))) with
                 | Some a, Some b -> TopIntOps.min a b
                 | _,_ -> failwith "trying to refine bot in inequalities"
               in match min_y with 
               | Int min -> [x, Value.starting @@ Z.add min @@ round_down @@ Q.div c s]
               | _ -> []
          in
          match k with 
          | LT s when Q.sign s > 0 -> upper_bound s
          | GT s when Q.sign s < 0 -> upper_bound s
          | LT s when Q.sign s < 0 -> lower_bound s
          | GT s when Q.sign s > 0 -> lower_bound s
          | _ -> [] (*Should never be used in this case*)
        in let y_refine =
             match k with 
             | LT s -> begin (*sx -c <= y*)
                 let min_x = match Value.minimal (Value.mul x_val (Value.of_bigint (round_down s))) , Value.minimal @@ Value.mul x_val (Value.of_bigint (round_up s)) with
                   | Some a, Some b -> TopIntOps.min a b
                   | _,_ -> failwith "trying to refine bot in inequalities"
                 in match min_x with 
                 | Int min -> [y, Value.starting @@ Z.sub min @@ round_up c]
                 | _ -> [] 
               end
             | GT s ->  (*s x - c >= y*)
               let max_x = match Value.maximal (Value.mul x_val (Value.of_bigint (round_down s))) , Value.maximal @@ Value.mul x_val (Value.of_bigint (round_up s)) with
                 | Some a, Some b -> TopIntOps.max a b
                 | _,_ -> failwith "trying to refine bot in inequalities"
               in match max_x with 
               | Int max -> [y, Value.ending @@ Z.sub max @@ round_down c]
               | _ -> [] 
        in match k with
        | LT s when Q.equal Q.zero s -> (* -c >= y *) [y, Value.ending @@ round_up @@ Q.neg c] , true
        | GT s when Q.equal Q.zero s -> (* -c <= y *) [y, Value.starting @@ round_down @@ Q.neg c] , true
        | LT s when Q.equal Q.inf s -> (*x >= c*) [x, Value.starting @@ round_down c ], true
        | GT s when Q.equal Q.minus_inf s -> (*x >= c*) [x, Value.starting @@ round_down c ], true 
        | LT s when Q.equal Q.minus_inf s -> (*x <= c*) [x, Value.ending @@ round_up c], true
        | GT s when Q.equal Q.inf s -> (*x <= c*) [x, Value.ending @@ round_up c], true
        | k -> (*an actual inequality *) x_refine @ y_refine, false 
    in if skip_adding then t, refinements 
    else (*Look for contradicting inequality*)
      let contradicts c' = match k with 
        | LT _ -> Q.gt c' c
        | GT _ -> Q.lt c' c
      in
      match get_best_offset (Key.negate k) t with  
      | Some c' when contradicts c' -> raise EConj.Contradiction
      (*TODO if c = c', then we have an equality -> maybe we can update the econj domain *) 
      | _ ->  
        (*add the inequality, while making sure that we do not save redundant inequalities*)
        (*TODO make this consider the intervals! -> adapt get_next and get_previous?*)
        let t' = match CoeffMap.find_opt k t with 
          | Some c_old when LinearInequality.entails1 (k,c_old) (k,c) -> t (*saved inequality is already thighter than new one*) (*TODO narrow?*) 
          | _ -> add_inequality k c @@ CoeffMap.remove k t (*we replace the current value with a new one *)
        in t', refinements (*TODO: lookup the best interval information from the inequalities!*)

  (*when meeting, the values should already been refined before -> ignore the refinement data*) (*TODO is this actually true?*)
  let meet' narrow x_val y_val t1 t2 = CoeffMap.fold (fun k c t -> fst @@ meet_single_inequality None narrow x_val y_val k c t) t1 t2

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

  let join' widen x y get_val_t1 get_val_t2 t1 t2 =
    let implies_single_equality t k c = 
      let res = match get_best_offset k t with None -> false | Some c' -> LinearInequality.entails1 (k, c') (k,c)
      in if M.tracing then M.trace "implies" "single ineq: %s implies %s ? -> %b" (show_formatted "x" "y" t) (LinearInequality.show "x" "y" (k,c)) res;
      res
    in
    let t1 = match t1 with None -> CoeffMap.empty | Some t1 -> t1 in
    let t2 = match t2 with None -> CoeffMap.empty | Some t2 -> t2 in
    (*add interval inequalities to copies, because doing it at every filter step would be more work*)
    let t1_with_interval = 
      let ineqs = LinearInequality.from_values (get_val_t1 x) (get_val_t1 y) in
      List.fold (fun t (k,c) -> add_inequality k c t) t1 ineqs
    in let t2_with_interval = 
         let ineqs = LinearInequality.from_values (get_val_t2 x) (get_val_t2 y) in
         List.fold (fun t (k,c) -> add_inequality k c t) t2 ineqs
    in
    (*we want to keep inequalities that are in one of the elements and implied by the other (maxbe by also being in there) *)
    let t1_filtered = CoeffMap.filter (implies_single_equality t2_with_interval) t1_with_interval in
    let t2_filtered = CoeffMap.filter (implies_single_equality t1_with_interval) t2_with_interval in
    (* merge the two sets. if one inequality is in both, take the less tight bound *)
    (* we make two passes over the list: first the relaxation, then adding all other inequalities*)
    (* this prevents an inequality from being deemed redundant by an inequality that is later relaxed*)
    (* TODO: test if this increased precision is worth the time?*)
    let relax k c2 (t1_f, t2_f) = (*we need to modify t2 because in the case of widening, the key might not be in both after this first step *)
      match CoeffMap.find_opt k t1 with (*look up in original t1 so that we can take care of widening for inequalities that get filtered*)
      | None -> (t1_f, t2_f)
      | Some c1 when Q.equal c1 c2 -> (t1_f, CoeffMap.remove k t2_f)
      | Some c1 when LinearInequality.entails1 (k,c1) (k,c2) && widen-> (CoeffMap.remove k t1_f, CoeffMap.remove k t2_f) (*t2 has more relaxed bound -> do widening*)
      | Some c1 when LinearInequality.entails1 (k,c1) (k,c2) -> (CoeffMap.add k c2 t1_f, CoeffMap.remove k t2_f) (*t2 has more relaxed bound*)
      | Some c1 -> (t1_f, CoeffMap.remove k t2_f) (*last remaining case: t1 has more relaxed bound*)
    in let t1_filtered', t2_filtered' = CoeffMap.fold relax t2_filtered (t1_filtered, t2_filtered)
    in let merged = CoeffMap.fold add_inequality t2_filtered' t1_filtered' 
    (*remove the explicetly stored interval inequalities*)
    in ignore_empty @@ CoeffMap.remove (LT Q.zero) @@ CoeffMap.remove (GT Q.zero) @@ CoeffMap.remove (LT Q.inf) @@ CoeffMap.remove (GT Q.inf) merged

  let join = join' false
  let widen = join' true
  let meet = meet' false
  let narrow = meet' true

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

end


module LinearInequalities: TwoVarInequalities = struct
  module Coeffs = ArbitraryCoeffsSet
  include CommonActions(Coeffs)

  let ignore_empty ls = 
    if IntMap.is_empty ls then None
    else Some ls

  let rec get_relations (((var_x,o_x,d_x), x_val) as x') (((var_y,o_y,d_y), y_val) as y') t = 
    match var_x, var_y with
    | Some (c_x, x), Some (c_y, y) -> 
      if x > y then
        (*We save information only in one of the directions -> check the other one*)
        List.map Relation.invert @@ get_relations y' x' t
      else begin
        if M.tracing then M.trace "get_relations" "checking x': %s, y': %s" (Rhs.show @@ fst x') (Rhs.show @@ fst y');
        match get_coeff x y t with 
        | None -> begin if M.tracing then M.trace "get_relations" "no inequality for roots"; [] end (*No information*)
        | Some coeff -> 
          let interval_ineqs = LinearInequality.from_values x_val y_val in
          let coeff = List.fold (fun t (k,c) -> Coeffs.add_inequality k c t) coeff interval_ineqs in
          let (k,c_rhs) = LinearInequality.from_rhss (c_x,o_x,d_x) (c_y,o_y,d_y) None in
          let factor = (*we need to muliply c' with this factor because LinearInequalities scales them down*)
            let a = Q.make c_x d_x in
            let b = Q.make c_y d_y in
            if Q.equal b Q.zero then a else b
          in
          let upper_bound = match Coeffs.get_best_offset k coeff with
            | None -> []
            | Some c_ineq -> 
              let c' = Q.mul factor @@ Q.sub c_ineq c_rhs in 
              [Relation.Lt, Z.add Z.one @@ Z.fdiv (Q.num c') (Q.den c')] (*add one to make it a strict inequality*)
          in match Coeffs.get_best_offset (Coeffs.Key.negate k) coeff with (*lower bound*)
          | None -> upper_bound
          | Some c_ineq -> 
            let c' = Q.mul factor ( Q.add c_ineq c_rhs) in
            (Gt, Z.add Z.minus_one @@ Z.cdiv (Q.num c') (Q.den c')) :: upper_bound
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
      in let (k,c) = LinearInequality.from_rhss rhs_x rhs_y (Some (match fst cond with Relation.Lt -> true | _ -> false))
      in let meet_relation_roots k c t = 
           if M.tracing then M.tracel "meet_relation" "meet_relation_roots: %s" @@ LinearInequality.show ("var_" ^ Int.to_string x) ("var_" ^ Int.to_string y) (k,c);
           (*do not save inequalities refering to the same variable*) 
           if x = y then
             let s = Coeffs.Key.get_slope k in
             if Q.equal Q.one s then (* x <= x + c (or >=) *)
               match k with 
               | LT _ -> if Q.lt c Q.zero then raise EConj.Contradiction else (t, []) (*trivially true*)
               | GT _ -> if Q.gt c Q.zero then raise EConj.Contradiction else (t, []) (*trivially true*)
             else (* sx <= x + c (or =>) -> refine the value in this case*)
               let s' = Q.sub s Q.one in
               let s', c' = match k with LT _ -> s',c | GT _ -> Q.neg s', Q.neg c in 
               (*s'x <= c' *) 
               if Q.gt s' Q.zero then 
                 let max = Q.div c' s' in
                 t, [x, Value.ending @@ Z.cdiv (Q.num max) (Q.den max)]
               else
                 let min = Q.div c' s' in
                 t, [x, Value.starting @@ Z.fdiv (Q.num min) (Q.den min)]
           else Coeffs.meet_single_inequality (Some (x,y)) false (get_value x) (get_value y) k c t
      in let factor = (*we need to divide o by this factor because LinearInequalities scales everything down. TODO is there a better way?*)
           let a = Q.make (Tuple3.first rhs_x) (Tuple3.third rhs_x) in
           let b = Q.make (Tuple3.first rhs_y) (Tuple3.third rhs_y) in
           if Q.equal b Q.zero then a else b
           (*TODO: transfer some transitivity, similar to the simple inequalities*)
      in let (new_coeffs, refine_acc) = match cond with 
          | Relation.Lt, o -> meet_relation_roots k (Q.add c @@ Q.div (Q.of_bigint o) factor) coeffs
          | Gt, o -> meet_relation_roots (Coeffs.Key.negate k) ((Q.add c @@ Q.div (Q.of_bigint o) factor) ) coeffs
          | Eq, o -> coeffs, [] (*This should always be stored by the lin2vareq domain (at least the way we are generating this information)*)
      in if Coeffs.CoeffMap.is_empty new_coeffs 
      then remove_coeff x y t , refine_acc
      else set_coeff x y new_coeffs t, refine_acc

  let meet_relation x y c r v t = 
    if M.tracing then M.tracel "meet_relation" "meeting %s with %s" (show t) (Relation.show ("var_"^Int.to_string x) c ("var_"^Int.to_string y));  
    let res, refine_acc = meet_relation x y c r v t in
    if M.tracing then M.tracel "meet_relation" "result: %s, refinements: %s " (show res) (List.fold (fun acc (var,value) -> Printf.sprintf "var_%d: %s, %s" var (Value.show value) acc) "" refine_acc);  
    res, refine_acc

  (*TODO switching sides ov variables because of substitution??*)
  let substitute t i (coeff, j, offs, divi) = 
    let fold_x x ys acc = 
      let check_for_contradiction cs = 
        let check_single k c = 
          match k with 
          | Coeffs.Key.LT s when Q.equal s Q.one -> if Q.lt c Q.zero then raise EConj.Contradiction 
          | GT s when Q.equal s Q.one -> if Q.gt c Q.zero then raise EConj.Contradiction
          | _ -> () (*TODO value refinement?*)
        in Coeffs.CoeffMap.iter check_single cs
      in
      if x < i then 
        let ys' = match IntMap.find_opt i ys with
          | None -> Some ys
          | Some cs -> 
            let cs' = Coeffs.substitute_right (coeff, offs, divi) cs in
            if x = j then (*We now have inequalities with the same variable on both sides -> check for contradictions*)
              (check_for_contradiction cs'; None)
            else
              let combine = function
                | None -> Some cs'
                | Some cs_j -> Some (Coeffs.meet Value.top Value.top cs' cs_j)
              in Some (IntMap.update_stdlib j combine (IntMap.remove i ys))
        in match ys' with 
        | Some ys' -> IntMap.add x ys' acc
        | _ -> acc
      else if x = i then  
        let convert y cs = 
          let tranformed = Coeffs.substitute_left (coeff, offs, divi) cs in
          if y = j 
          then (check_for_contradiction tranformed; None) 
          else Some tranformed
        in let ys' = IntMap.filter_map convert ys in
        if IntMap.is_empty ys' then 
          acc
        else 
          let combine = function 
            | None -> Some ys'
            | Some js -> Some (IntMap.union (fun y c1 c2 -> Some (Coeffs.meet Value.top Value.top c1 c2)) ys' js) 
          in IntMap.update_stdlib j combine acc
      else
        acc
    in IntMap.fold fold_x t IntMap.empty 

  let substitute t i (c,j,o,d) = 
    let res = substitute t i (c,j,o,d) in
    if M.tracing then M.trace "substitute" "substituting var_%d in %s with %s -> %s" i (show t) (Rhs.show (Some (c,j), o, d)) (show res);
    res

  let transfer x cond t_old get_rhs_old get_value_old t get_rhs get_value = 
    match get_rhs_old x, get_rhs x with 
    | (Some (coeff_old,x_root_old), off_old, divi_old), ((Some (coeff,x_root), off, divi) as rhs) -> 
      (*convert the relation to a linear inequality refering to the old root *)
      let (k,c) = LinearInequality.from_rhss (coeff_old, off_old, divi_old) (coeff_old, off_old, divi_old) (Some (match fst cond with Relation.Lt -> true | _ -> false))
      in let factor = Q.make coeff_old divi_old (*we need to divide o by this factor because LinearInequalities scales everything down. TODO is there a better way?*)
      in let ineq_from_cond = match cond with 
          | Relation.Lt, o -> k, (Q.add c @@ Q. div (Q.of_bigint o) factor)
          | Gt, o -> (Coeffs.Key.negate k), (Q.add c @@ Q. div (Q.of_bigint o) factor)
          | Eq, o -> undefined "TODO" (*Should we exclude EQ from relation?*)
      in
      (*combine the inequality from cond with all inequalities*)
      (*throw out all inequalities that do not contain the representative of x*)
      let combine_1 v1 v2s = 
        if v1 = x_root_old then ignore_empty @@ IntMap.filter_map (fun _ c -> Coeffs.combine_left ineq_from_cond c) v2s
        else
          let combine_2 v2 c = if v2 = x_root_old then Coeffs.combine_right ineq_from_cond c else None in
          ignore_empty @@ IntMap.filter_map combine_2 v2s
      in
      let filtered = IntMap.filter_map combine_1 t_old in
      if M.tracing then M.tracel "transfer" "filtered: %s" (show filtered);  

      (*transform all inequalities to refer to new root of x*)
      (*invert old rhs, then substitute the new rhs for x*)
      let (m, o, d) = Rhs.subst rhs x @@ snd @@ EConj.inverse x (coeff_old,x_root_old, off_old, divi_old) in
      let c, v = BatOption.get m in
      let transformed = substitute filtered x_root (c, v, o, d) in
      if M.tracing then M.tracel "transfer" "transformed: %s" (show transformed);  
      (*meet with this set of equations*)
      meet get_value t transformed
    | _,_ -> t (*ignore constants*)

  let transfer x cond t_old get_rhs_old get_value_old t get_rhs get_value = 
    if M.tracing then M.tracel "transfer" "transfering  with %s from %s into %s" (Relation.show ("var_" ^ Int.to_string x ^ "_old") cond ("var_" ^ Int.to_string x ^ "_new") ) (show t_old) (show t);  
    let res = transfer x cond t_old get_rhs_old get_value_old t get_rhs get_value in
    if M.tracing then M.tracel "transfer" "result: %s" (show res);  
    res

  let limit econj t = 
    let coeffs = coeffs_from_econj econj in
    let limit_single x y cs = 
      Coeffs.limit ( slopes_from_coeffs coeffs (min x y, max x y)) cs
    in IntMap.filter_map (fun x ys -> ignore_empty @@ IntMap.filter_map (fun y cs -> Coeffs.ignore_empty @@ limit_single x y cs) ys) t

  let limit e t = timing_wrap "limit" (limit e) t

  let copy_to_new_representants econj_old econj_new t = 
    let coeffs = coeffs_from_econj econj_new in
    (*a var is representant if it does not show up in the sparse map*)
    let all_representants_in_new = 
      let rec aux acc n =
        if n > (fst econj_new) then acc
        else if IntMap.mem n (snd econj_new) then aux acc (n + 1)
        else aux (n :: acc) (n + 1)
      in aux [] 0
    in let new_representants_in_new = List.filter (fun v -> IntMap.mem v (snd econj_old)) all_representants_in_new
    in let add_new v_new t_acc other_var = 
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
           in let coeffs_old = BatOption.default Coeffs.empty @@ get_coeff (min old_rep other_var) (max old_rep other_var) t in 
           let add_single_slope c_acc s = 
             let ineqs = [LinearInequality.OriginInequality.LT s, Q.zero; GT s, Q.zero;]
             in let copy_single_ineq c_acc ineq = 
                  let k_old = fst @@ convert_to_old ineq in
                  (*TODO maybe this introduces too many new inequalities -> only take explicit stored ones?*)
                  match Coeffs.get_best_offset k_old coeffs_old with 
                  | None -> c_acc
                  | Some o -> 
                    let k_neq, o_new = convert_to_new (k_old, o) in
                    Coeffs.add_inequality k_neq o_new c_acc
             in List.fold copy_single_ineq c_acc ineqs 
           in let coeffs_new = Enum.fold add_single_slope Coeffs.empty allowed_slopes
           in if Coeffs.CoeffMap.is_empty coeffs_new then t_acc else set_coeff (min v_new other_var) (max v_new other_var) coeffs_new t_acc
    in List.fold (fun acc v_new -> List.fold (add_new v_new ) acc all_representants_in_new ) t new_representants_in_new 

  let copy_to_new_representants econj_old econj_new t = timing_wrap "new_reps" (copy_to_new_representants econj_old econj_new) t

end

(*TODOs:*)
(*
  ArbitraryCoeaffsList.meet + affine_transform -> refinement
  refinement of equalities must be limited to have acceptable runtimes! 
*)
(*look at complexities. I expect for all: (n² log n) *)

(*rework relation to offset domain -> remove Eq? *)
(*store information about representants to avoid recalculating them: congruence information, group size/ coefficients ??*)
(*redo simple equalities (take advantage of the offset!, affine transform)*)
(*domain inbetween these two: with offset between roots? -> should be trivial to implement*)
(*what is required of narrow?*)
(*widening thresholds: from offsets of rhs?*)
(*general renaming*)
(*rebase to main branch*)