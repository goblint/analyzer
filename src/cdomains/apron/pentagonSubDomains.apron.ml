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

  let get_int_t = function
    | Int i -> i
    | _ -> failwith "get_int_t on top value"

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

  let of_bigint i = Int (Int_t.of_bigint i)
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

module type TwoVarInequalities = sig
  type t
  type cond = Lt | Le | Eq | Gt | Ge

  val show_cond : cond -> string

  val is_less_than :  (Rhs.t * Value.t) -> (Rhs.t * Value.t) -> t -> bool option

  (*meet x' < y' (or with = / <= *)
  val meet_condition : int -> int -> cond -> (int -> Rhs.t) -> (int -> Value.t) -> t -> t

  val meet : (int -> Rhs.t) -> (int -> Value.t) -> t -> t -> t

  val leq : t -> (int -> Value.t) -> t -> bool

  val join : t -> (int -> Value.t) -> t -> (int -> Value.t) -> t

  (*copy all constraints for some variable to a different t if they still hold for a new x' with x' (cond) x *)
  val transfer : int -> cond -> t -> (int -> Rhs.t) -> (int -> Value.t) -> t -> (int -> Rhs.t) -> (int -> Value.t) -> t

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
  type cond = Lt | Le | Eq | Gt | Ge

  let show_cond c = match c with 
    | Le -> "<="
    | Lt -> "<"
    | Eq -> "="
    | Gt -> ">"
    | Ge -> ">="

  let is_less_than _ _ _ = None
  let meet_condition _ _ _ _ _ _ = ()

  let meet _ _ _ _ = ()

  let leq _ _ _ = true
  let join _ _ _ _ = ()

  let show_formatted _ _ = "{}"
  let hash _ = 3
  let empty = ()
  let is_empty _ = true
  let equal _ _ = true
  let compare _ _ = 0
  let modify_variables_in_domain _ _ _ = ()
  let forget_variable _ _ = ()

  let transfer _ _ _ _ _ _ _ _ = ()
end

module type Coeffs = sig
  type t
  val implies : Value.t -> Value.t -> t option -> t -> bool
  val meet : Value.t -> Value.t -> t -> t -> t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val show_formatted : string -> string -> t -> string

end

module CommonActions (Coeffs : Coeffs) = struct
  type cond = Lt | Le | Eq | Gt | Ge

  type t = Coeffs.t IntMap.t IntMap.t  [@@deriving eq, ord ]

  let show_cond c = match c with 
    | Le -> "<="
    | Lt -> "<"
    | Eq -> "="
    | Gt -> ">"
    | Ge -> ">="

  let empty = IntMap.empty
  let is_empty = IntMap.is_empty
  let hash t = IntMap.fold (fun _ ys acc -> IntMap.fold (fun _ coeff acc -> Coeffs.hash coeff + 3*acc) ys (5*acc)) t 0

  let show_formatted formatter t = 
    if IntMap.is_empty t then "{}" else
      let str = IntMap.fold (fun x ys acc -> IntMap.fold (fun y coeff acc -> Printf.sprintf "%s , %s" (Coeffs.show_formatted (formatter x) (formatter y) coeff) acc) ys acc) t "" 
      in "{" ^ (*String.sub str 0 (String.length str - 3) *) str ^ "}" (*TODO why does this not work when removing the things?*)

  let show = show_formatted (Printf.sprintf "var_%d")

  let forget_variable t v = 
    IntMap.map (fun ys -> IntMap.remove v ys) (IntMap.remove v t)

  let modify_variables_in_domain map indexes op =
    let map_fun bump_var ys = IntMap.fold (fun y ->  IntMap.add (bump_var y) ) ys IntMap.empty in  
    EConj.modify_variables_in_domain_general map map_fun indexes op 

  let get_coeff x y t = BatOption.bind (IntMap.find_opt x t) (fun ys -> IntMap.find_opt y ys)

  let set_coeff x y coeff t = 
    IntMap.add x (IntMap.add y coeff @@ IntMap.find_default IntMap.empty x t ) t

  let remove_coeff x y t = 
    IntMap.add x (IntMap.remove y @@ IntMap.find_default IntMap.empty x t ) t

  let leq t1 get_value_t1 t2 = 
    let implies x y t2_coeff = 
      let t1_coeff = get_coeff x y t1 in 
      Coeffs.implies (get_value_t1 x) (get_value_t1 y) t1_coeff  t2_coeff
    in
    IntMap.for_all (fun x ys -> IntMap.for_all (implies x) ys) t2

  let meet_one_coeff get_value x y coeff t =
    let coeff_t = get_coeff x y t in
    let coeff_met = match coeff_t with 
      | None -> coeff
      | Some coeff_t -> Coeffs.meet (get_value x) (get_value y) coeff coeff_t
    in set_coeff x y coeff_met t

  (*TODO I do not see an obvious way that an inequalitiy between roots could contradict a rhs. Is there one?*)
  let meet get_rhs get_value t1 t2 = 
    IntMap.fold (fun x ys t -> IntMap.fold (meet_one_coeff get_value x) ys t) t2 t1

end

(*Equations of the type x < y*)
module NoCoeffs = struct
  type t = unit [@@deriving eq, ord, hash ]

  let implies x y t1_opt _ = match t1_opt with 
    | Some _ -> true
    | None -> match Value.maximal x, Value.minimal y with 
      | Some x, Some y -> TopIntOps.compare x y < 0
      | _, _ -> false

  let meet x y _ _ = 
    match Value.minimal x, Value.maximal y with 
    | Some x, Some y when  TopIntOps.compare x y > 0 -> raise EConj.Contradiction
    | _, _ -> ()

  let show_formatted x y t = x ^ " < " ^ y

end

(*Semantics: x -> y -> () => x < y*)
module SimpleInequalities : TwoVarInequalities = struct
  module Coeffs = NoCoeffs
  include CommonActions(Coeffs)

  let join t1 get_val_t1 t2 get_val_t2 = 
    let merge_y x y c1 c2 =
      let of_bool b = if b then Some () else None in  
      match c1 with 
      | Some c1 -> of_bool (Coeffs.implies (get_val_t2 x) (get_val_t2 y) c2 c1)
      | None -> match c2 with 
        | Some c2 -> of_bool (Coeffs.implies (get_val_t1 x) (get_val_t1 y) c1 c2)
        | None -> None
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

  let is_less_than x y t = 
    let check_inequality ((var_x,o_x,d_x), val_x) ((var_y,o_y,d_y), val_y) =
      if M.tracing then M.trace "is_less_than" "checking x: %s, y: %s" (Rhs.show (var_x,o_x,d_x)) (Rhs.show (var_y,o_y,d_y));
      match var_x, var_y with
      | Some (c_x, x), Some (c_y, y) -> begin
          match get_coeff x y t with 
          | None -> 
            if M.tracing then M.trace "is_less_than" "no inequality for roots";
            None (*No information*)
          | Some _ -> (*we know x < y -> check if this translates to x' < y' or x' > y'*) 
            let d_c = Z.sub (Z.mul d_x c_y) (Z.mul d_y c_x) in
            let d_o = Z.sub (Z.mul o_x d_y) (Z.mul o_y d_x) in
            let x_d_c = Value.mul val_x (Value.of_bigint d_c) in
            if Z.lt c_y Z.zero && Value.leq x_d_c (Value.ending d_o) (* c_y < 0, x * d_c <= d_o*) 
            then Some false (*x' > y '*)
            else if Z.gt c_y Z.zero && Value.leq x_d_c (Value.starting d_o) (* c_y > 0, x * d_c >= d_o*) 
            then Some true (*x' < y '*)
            else  
              let d_c' = Z.neg d_c in
              let d_o' = Z.neg d_o in
              let y_d_c = Value.mul val_y (Value.of_bigint d_c') in
              if Z.lt c_x Z.zero && Value.leq y_d_c (Value.starting d_o') (* c_x < 0, y * d_c >= d_o*) 
              then Some false (*x' > y '*)
              else if  Z.gt c_x Z.zero && Value.leq y_d_c (Value.ending d_o') (* c_x > 0, y * d_c <= d_o*) 
              then Some true (*x' < y '*)
              else None
        end
      | _, _ -> failwith "Inequalities.is_less_than does not take constants directly" (*TODO should we take the coefficients directly to enforce this*)
    in 
    let res = check_inequality x y in
    if res = None then BatOption.map not @@ check_inequality y x
    else res

  let is_less_than x y t = 
    let res = is_less_than x y t in
    if M.tracing then M.trace "is_less_than" "result: %s" (BatOption.map_default (Bool.to_string) "unknown" res);
    res

  (**)
  let meet_condition x' y' cond get_rhs get_value t =
    (*TODO should we check values for contradictions?*)
    (*strict: if the inequality is strict *)
    let meet_less_root x y strict t = 
      if M.tracing then M.tracel "meet_condition" "meet_less_root x: %d y: %d strict: %b " x y strict;  
      let union = IntMap.union (fun _ _ _ -> Some ()) (IntMap.find_default IntMap.empty x t) (IntMap.find_default IntMap.empty y t) 
      in let union' = if strict then IntMap.add y () union else union 
      in if IntMap.mem x union' then raise EConj.Contradiction
      else IntMap.add x union' t
    in
    let meet_less x' y' strict t = 
      if M.tracing then M.tracel "meet_condition" "meet_less x': %d y': %d strict: %b" x' y' strict;  
      let get_rhs' lhs = 
        match get_rhs lhs with 
        | (Some (c,v),o,d) -> c,v,o,d
        | (None, o, d) -> Z.one, lhs, Z.zero, Z.one
      in let (c_x, x, o_x, d_x) = get_rhs' x'
      in let (c_y, y, o_y, d_y) = get_rhs' y'
      in if M.tracing then M.tracel "meet_condition" "x' = %s, y' = %s " (Rhs.show (Some (c_x, x),o_x,d_x)) (Rhs.show (Some (c_y,y),o_y,d_y));  
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
        else t
    in
    match cond with 
    | Gt -> meet_less y' x' true t 
    | Ge -> meet_less y' x' false t 
    | Eq -> 
      let rhs_x = get_rhs x' in
      let rhs_y = get_rhs y' in
      if M.tracing then M.tracel "meet_condition" "in equality: x' (var_%d) = %s, y' (var_%d)= %s " x' (Rhs.show rhs_x) y' (Rhs.show rhs_y);  
      if Rhs.equal rhs_x rhs_y then begin
        if M.tracing then M.tracel "meet_condition" "equality with same rhs";  
        let x,y = match rhs_x, rhs_y with 
          | (Some (_,x), _,_), (Some (_,y), _,_) -> (x,y)
          | (None,_,_), (None, _,_) -> x',y'
          | _,_ -> failwith "Should never happen"
        in
        let union = IntMap.union (fun _ _ _ -> Some ()) (IntMap.find_default IntMap.empty x t) (IntMap.find_default IntMap.empty y t) in
        if IntMap.mem x union || IntMap.mem y union then raise EConj.Contradiction
        else IntMap.add x union @@ IntMap.add y union t
      end else
        meet_less x' y' false @@ meet_less y' x' false t (*TODO skip repeat calculations?*)
    | Le -> meet_less x' y' false t 
    | Lt -> meet_less x' y' true t 

  let meet_condition x y c r v t = 
    if M.tracing then M.tracel "meet_condition" "meeting %s with x': %d y': %d cond %s" (show t) x y (show_cond c);  
    let res = meet_condition x y c r v t in
    if M.tracing then M.tracel "meet_condition" "result: %s " (show res);  
    res

  (*TODO I think this will be the same (or at least almost) for all the domain, but depends on is_less_than / meet_condition -> make it general?*)
  let transfer x cond t_old get_rhs_old get_value_old t get_rhs get_value = 
    let was_less_than x y = 
      let get_information lhs =
        let rhs = get_rhs_old lhs in
        match rhs with 
        | (Some (_,var), _ ,_) -> (rhs, get_value_old var)
        (*We need to know which root a constant is referring to, so we use this the trivial equation to carry that information*)
        | (_,o,_) -> (Rhs.var_zero lhs, Value.of_bigint o) 
      in
      is_less_than (get_information x) (get_information y) t_old 
    in let vars_to_check = 
         let root = match get_rhs_old x with
           | (Some (_,var), _ ,_) -> var
           | (_,o,_) -> x
           (*we need to check all y with root -> y -> coeff  or y -> root -> coeff*)
         in BatEnum.append (IntMap.keys @@ IntMap.find_default IntMap.empty root t_old) (List.enum @@ IntMap.fold (fun k ys acc -> if IntMap.mem root ys then k :: acc else acc) t_old [])   
    in let keep_less = match cond with 
        | Eq | Lt | Le -> true 
        | _ -> false
    in let keep_greater = match cond with 
        | Eq | Gt | Ge -> true 
        | _ -> false
    in let transfer_single_var t' y = 
         match was_less_than x y with 
         | Some true -> 
           if keep_less then meet_condition x y Lt get_rhs get_value t' else t'
         | Some false ->
           if keep_greater then meet_condition x y Gt get_rhs get_value t' else t'
         | _ -> t'
    in BatEnum.fold (transfer_single_var) t vars_to_check

  let transfer x cond t_old get_rhs_old get_value_old t get_rhs get_value = 
    let res = transfer x cond t_old get_rhs_old get_value_old t get_rhs get_value in
    if M.tracing then M.tracel "transfer" "transfering for var_%d with cond: %s from %s into %s -> %s" x (show_cond cond) (show t_old) (show t) (show res);  
    res

end 