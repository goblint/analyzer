(**Extending the LinearTwoVarDomain with Intervals for the representative interval *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open GobApron
open VectorMatrix

module Mpqf = SharedFunctions.Mpqf

module Rhs = LinearTwoVarEqualityDomain.Rhs

module EConj = LinearTwoVarEqualityDomain.EqualitiesConjunction

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
    | Top s, Int x 
    | Int x, Top s ->
      let comp = Int_t.compare x Int_t.zero in
      if comp = 0 then Int (Int_t.zero)
      else if comp <  0 then Top (neg_s s)
      else Top s
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

  let of_string s = if s = "+⊤" then Top Pos else (if s = "-⊤" then Top Pos else Int (Int_t.of_string s))
  let to_string = function
    | Int i ->  Int_t.to_string i
    | Top Pos -> "+⊤"
    | Top Neg -> "-⊤"

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

module Unbounded : IntervalDomainWithBounds.BoundedIntOps with type t = TopIntOps.t = struct
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

(*TODO add wrapper to remove ikind parameter or not? *)
module IntervalAndCongruence = struct
  module I = IntDomain0.SOverflowUnlifter(IntervalDomainWithBounds.IntervalFunctor(Unbounded))
  module C = CongruenceDomainNormFunctor.Congruence(CongruenceDomainNormFunctor.NoWrapping)

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
  let join (i1,c1) (i2,c2) = refine (I.join ik i1 i2, C.join ik c1 c2)
  let widen (i1,c1) (i2,c2) = refine (I.widen ik i1 i2, C.widen ik c1 c2)

  let of_congruence c = refine (I.top_of ik, C.of_congruence ik c)


end 

module Value = IntervalAndCongruence

module EqualitiesConjunctionWithIntervals =
struct
  module IntMap = EConj.IntMap
  type t = EConj.t *  (Value.t IntMap.t) [@@deriving eq, ord]

  let hash (econj, x) = EConj.hash econj + 13* IntMap.fold (fun k value acc -> 13 * 13 * acc + 31 * k + Value.hash value) x 0

  let show_intervals formatter is =     
    if IntMap.is_empty is then "{}"
    else
      let str = IntMap.fold (fun k v acc -> Printf.sprintf "%s=%s , %s" (formatter k) (Value.show v) acc) is "" in
      "{" ^ String.sub str 0 (String.length str - 3) ^ "}" 

  let show_formatted formatter ((dim, econj), is) = Printf.sprintf "(%s, %s)" (EConj.show_formatted formatter econj) (show_intervals formatter is)

  let show = show_formatted (Printf.sprintf "var_%d") 

  let copy = identity
  let empty () = (EConj.empty (), IntMap.empty)

  let is_empty (e,is) = EConj.is_empty e && IntMap.is_empty is

  let is_top_con (e, is) = EConj.is_top_con e && IntMap.is_empty is

  let modify_variables_in_domain_intervals map indexes op =
    if Array.length indexes = 0 then map else
      let rec bumpentry k v = function 
        | (tbl,delta,head::rest) when k>=head -> bumpentry k v (tbl,delta+1,rest) (* rec call even when =, in order to correctly interpret double bumps *)
        | (tbl,delta,lyst) (* k<head or lyst=[] *) -> (IntMap.add (op k delta) v tbl, delta, lyst)
      in
      let (a,_,_) = IntMap.fold bumpentry map (IntMap.empty,0,Array.to_list indexes) in (* Build new map during fold with bumped keys *)
      a

  let modify_variables_in_domain_intervals map indexes op =
    let res = modify_variables_in_domain_intervals map indexes op in if M.tracing then
      M.tracel "modify_dims" "dimarray bumping with (fun x -> x + %d) at positions [%s] in { %s } -> { %s }"
        (op 0 1)
        (Array.fold_right (fun i str -> (string_of_int i) ^ ", " ^ str) indexes "")
        (show_intervals (Printf.sprintf "var_%d") map)
        (show_intervals (Printf.sprintf "var_%d") res);
    res


  let make_empty_with_size size = (EConj.make_empty_conj size, IntMap.empty)

  let dim_add (ch: Apron.Dim.change) (econj, i) =
    (EConj.dim_add ch econj, modify_variables_in_domain_intervals i ch.dim (+))

  let forget_variable (econj, is) var = (EConj.forget_variable econj var, IntMap.remove var is)


  let dim_remove (ch: Apron.Dim.change) (econj, i) ~del =
    if Array.length ch.dim = 0 || EConj.is_empty econj then
      (econj, i)
    else (
      let cpy = Array.copy ch.dim in
      Array.modifyi (+) cpy; (* this is a hack to restore the original https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html remove_dimensions semantics for dim_remove *)
      let (econj', i') = Array.fold_lefti (fun y i x -> forget_variable y (x)) (econj, i) cpy in  (* clear m' from relations concerning ch.dim *)
      let econj'' = EConj.modify_variables_in_domain econj' cpy (-) in
      let i'' = modify_variables_in_domain_intervals i' cpy (-) in
      (econj'', i''))


  let get_rhs t = EConj.get_rhs (fst t)

  let get_interval (econ, is) lhs = 
    match IntMap.find_opt lhs is with
      Some i -> i
    | None -> (*If there is no interval saved, we have calculate it*)
      let (v,o,d) = get_rhs (econ, is) lhs in
      if (v,o,d) = Rhs.var_zero lhs then Value.top  (*no relation -> Top*) 
      else match v with 
          None -> Value.div (Value.of_bigint o) (Value.of_bigint d)(*constant*) (*TODO is divisor always 1?*) 
        | Some (coeff,v) -> match IntMap.find_opt v is with
            None -> Value.top (*uninitialised*) 
          | Some i -> Value.div (Value.add (Value.of_bigint o) @@ Value.mul (Value.of_bigint coeff) i) (Value.of_bigint d)

  let get_interval t lhs = 
    let res = get_interval t lhs in
    if M.tracing then M.tracel "get_interval" "reading var_%d from %s -> %s" lhs (show t) (Value.show res);
    res

  let set_interval ((econ, is):t) lhs i =
    let refine econ lhs i =(**TODO do not recalculate this every time?*)
      (*calculate the congruence constraint for x from a single equation (cx + o) / d  *)
      let congruence_of_rhs (c, o, d) =
        (*adapted euclids extended algorithm for calculating the modular multiplicative inverse*)
        let rec inverse t r t_old r_old = 
          if Z.equal r Z.zero 
          then t_old
          else 
            let q = Z.div r_old r in
            inverse (Z.sub t_old (Z.mul q t)) (Z.sub r_old (Z.mul q r)) t r
        in let inverse a n = inverse Z.one a Z.zero n
        (*  x = -o c^-1 (mod d)   *)
        in Value.of_congruence @@ (Z.mul (Z.neg o) (inverse c d), d)
      in
      let meet_with_rhs _ rhs i = match rhs with
        | (Some (c, v), o, d) when v = lhs -> Value.meet i (congruence_of_rhs (c, o, d))
        | _ -> i
      in
      IntMap.fold meet_with_rhs (snd econ) i 
    in 
    let set_interval_for_root lhs i =
      let i = refine econ lhs i in
      if M.tracing then M.tracel "modify_pentagon" "set_interval_for_root var_%d=%s" lhs (Value.show i);
      if i = Value.top then (econ, IntMap.remove lhs is) (*stay sparse*)
      else match Value.to_int i with
        | Some (Int x) ->  (*If we have a constant, update all equations refering to this root*)
          let update_references = function
            | (Some (coeff, v), o, d) when v = lhs -> (None, Z.div (Z.add o @@ Z.mul x coeff) d, Z.one)
            | t -> t
          in
          ((fst econ, IntMap.add lhs (None, x, Z.one) @@ IntMap.map update_references (snd econ)), IntMap.remove lhs is)
        | _ -> (econ, IntMap.add lhs i is) (*Not a constant*)
    in let (v,o,d) = get_rhs (econ, is) lhs in
    if (v,o,d) = Rhs.var_zero lhs then
      set_interval_for_root lhs i
    else
      match v with
      | None -> (econ, is) (*For a constant, we do not need to save an interval*) (*TODO should we check for equality?*)
      | Some (coeff, v) ->
        let i1 = Value.mul (Value.of_bigint d) i in
        let i2 = Value.sub i1 (Value.of_bigint o) in
        let i3 = Value.div i2 (Value.of_bigint coeff) in
        let i_transformed = i3 in 
        if M.tracing then M.tracel "modify_pentagon" "transforming with %s i: %s i1: %s i2: %s i3: %s" (Rhs.show ((Some (coeff, v)),o,d)) (Value.show i) (Value.show i1) (Value.show i2) (Value.show i3);
        set_interval_for_root v i_transformed

  let set_interval t lhs i =
    let res = set_interval t lhs i in
    if M.tracing then M.tracel "modify_pentagon" "set_interval before: %s eq: var_%d=%s  ->  %s " (show t) lhs (Value.show i) (show res);
    res

  (*TODO: If we are uptdating a variable, we will overwrite the interval again -> maybe skip setting it here, because of performance?*)
  let set_rhs (econ, is) lhs rhs =
    let econ' = EConj.set_rhs econ lhs rhs in
    match rhs with 
    | (None, _, _) -> econ', IntMap.remove lhs is (*when setting as a constant, we do not need a separate interval *)
    | _ -> 
      let new_constraint = get_interval (econ', is) lhs in
      let old_constraint = get_interval (econ, is) lhs in
      let new_interval = Value.meet new_constraint old_constraint in
      set_interval (econ', is) lhs new_interval

  let set_rhs t lhs rhs = 
    let res = set_rhs t lhs rhs  in
    if M.tracing then M.tracel "modify_pentagon" "set_rhs before: %s eq: var_%d=%s  ->  %s " (show t) lhs (Rhs.show rhs) (show res);
    res

  let meet_with_one_conj ((ts, is):t) i (var, offs, divi) =
    let (var,offs,divi) = Rhs.canonicalize (var,offs,divi) in (* make sure that the one new conj is properly canonicalized *)
    let res : t =
      let subst_var ((dim,econj), is) x (vary, o, d) =
        let (vary, o, d) = Rhs.canonicalize (vary, o, d) in
        (* [[x substby (cy+o)/d ]] ((c'x+o')/d')             *)
        (* =====>   (c'cy + c'o+o'd)/(dd')                   *)
        let adjust = function
          | (Some (c',varx), o',d') when varx = x ->
            let open Z in Rhs.canonicalize (BatOption.map (fun (c, y)-> (c * c', y)) vary, c'*o + o'*d, d'*d)
          | e -> e
        in
        (match vary with 
         | None when d <> Z.one -> (if M.tracing then M.tracel "meet_with_one_conj" "meet_with_one_conj substituting var_%d with constant %s, which is not an integer" i (Rhs.show (var,offs,divi));
                                    raise EConj.Contradiction)
         | _ -> ()
        );
        let interval = get_interval (ts, is) x in
        if not @@ Value.contains interval (Z.div offs divi) then 
          (if M.tracing then M.tracel "meet_with_one_conj" "meet_with_one_conj substituting var_%d with constant %s, Contradicts %s" (i) (Rhs.show (var,offs,divi)) (Value.show interval);
           raise EConj.Contradiction)
        else 
          let is' = IntMap.remove x is in (*if x was the representative, it might not be anymore -> remove interval and add it back afterwards*) 
          set_interval ( (dim, IntMap.add x (vary, o, d) @@ IntMap.map adjust econj), is' ) x interval (* in case of sparse representation, make sure that the equality is now included in the conjunction *)
      in
      (match var, (EConj.get_rhs ts i) with
       (*| new conj      , old conj          *)
       | None          , (None            , o1, divi1) -> if not @@ (Z.equal offs o1 && Z.equal divi divi1) then raise EConj.Contradiction else ts, is
       (*  o/d         =  x_i  = (c1*x_h1+o1)/d1            *)
       (*  ======> x_h1 = (o*d1-o1*d)/(d*c1) /\  x_i = o/d  *)
       | None          , (Some (coeff1,h1), o1, divi1) -> 
         subst_var (ts, is) h1 (None, Z.(offs*divi1 - o1*divi),Z.(divi*coeff1))
       (* (c*x_j+o)/d  =  x_i  =  o1/d1                     *)
       (*  ======> x_j = (o1*d-o*d1)/(d1*c) /\  x_i = o1/d1 *)
       | Some (coeff,j), (None            , o1, divi1) -> subst_var (ts, is) j  (None, Z.(o1*divi - offs*divi1),Z.(divi1*coeff))
       (* (c*x_j+o)/d  =  x_i  = (c1*x_h1+o1)/d1            *)
       (*  ======>   x_j needs normalization wrt. ts        *)
       | Some (coeff,j), ((Some (coeff1,h1), o1, divi1) as oldi)->
         (match EConj.get_rhs ts j with
          (* ts[x_j]=o2/d2             ========>  ... *)
          | (None            , o2, divi2) ->
            let newxi  = Rhs.subst (None,o2,divi2) j (Some (coeff,j),offs,divi) in
            let newxh1 = snd @@ EConj.inverse i (coeff1,h1,o1,divi1) in
            let newxh1 = Rhs.subst newxi i newxh1 in
            subst_var (ts, is) h1 newxh1
          (* ts[x_j]=(c2*x_h2+o2)/d2   ========>   ...  *)
          | (Some (coeff2,h2), o2, divi2) as normalizedj ->
            if h1 = h2 then (* this is the case where x_i and x_j already where in the same equivalence class; let's see whether the new equality contradicts the old one *)
              let normalizedi= Rhs.subst normalizedj j (Some(coeff,j),offs,divi) in
              if not @@ Rhs.equal normalizedi oldi then raise EConj.Contradiction else (ts, is)
            else if h1 < h2 (* good, we now unite the two equvalence classes; let's decide upon the representative *)
            then (* express h2 in terms of h1: *)
              let (_,newh2)= EConj.inverse j (coeff2,h2,o2,divi2) in
              let newh2 = Rhs.subst oldi i (Rhs.subst (snd @@ EConj.inverse i (coeff,j,offs,divi)) j newh2) in
              subst_var (ts, is) h2 newh2
            else (* express h1 in terms of h2: *)
              let (_,newh1)= EConj.inverse i (coeff1,h1,o1,divi1) in
              let newh1 = Rhs.subst normalizedj j (Rhs.subst (Some(coeff,j),offs,divi) i newh1) in
              subst_var (ts, is) h1 newh1)) in
    if M.tracing then M.tracel "meet_with_one_conj" "meet_with_one_conj conj: %s eq: var_%d=%s  ->  %s " (show (ts,is)) i (Rhs.show (var,offs,divi)) (show res)
  ; res

  let meet_with_one_interval var interval t =
    let refined_interval = Value.refine interval in
    let new_interval = Value.meet refined_interval (get_interval t var)
    in if Value.is_bot new_interval then raise EConj.Contradiction else 
      let res =  set_interval t var new_interval
      in if M.tracing then M.tracel "meet_interval" "meet var_%d: before: %s meeting: %s -> %s, total: %s-> %s" (var) (Value.show @@ get_interval t var) (Value.show interval) (Value.show new_interval) (show t) (show res);
      res
end

(** [VarManagement] defines the type t of the affine equality domain (a record that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by [RelationDomain.D2]) such as [add_vars], [remove_vars].
    Furthermore, it provides the function [simplified_monomials_from_texp] that converts an apron expression into a list of monomials of reference variables and a constant offset *)
module VarManagement =
struct
  module EConjI = EqualitiesConjunctionWithIntervals
  include SharedFunctions.VarManagementOps (EConjI)

  let dim_add = EConjI.dim_add
  let size t = BatOption.map_default (fun ((d,_),_) -> d) 0 t.d

  (** Parses a Texpr to obtain a (coefficient, variable) pair list to repr. a sum of a variables that have a coefficient. If variable is None, the coefficient represents a constant offset. *)
  let monomials_from_texp (t: t) texp =
    let open Apron.Texpr1 in
    let exception NotLinearExpr in
    let exception ScalarIsInfinity in
    let negate coeff_var_list =
      List.map (fun (monom, offs, divi) -> Z.(BatOption.map (fun (coeff,i) -> (neg coeff, i)) monom, neg offs, divi)) coeff_var_list in
    let multiply_with_Q dividend divisor coeff_var_list =
      List.map (fun (monom, offs, divi) -> Rhs.canonicalize Z.(BatOption.map (fun (coeff,i) -> (dividend*coeff,i)) monom, dividend*offs, divi*divisor) ) coeff_var_list in
    let multiply a b =
      (* if one of them is a constant, then multiply. Otherwise, the expression is not linear *)
      match a, b with
      | [(None,coeff, divi)], c
      | c, [(None,coeff, divi)] -> multiply_with_Q coeff divi c
      | _ -> raise NotLinearExpr
    in
    let rec convert_texpr texp =
      begin match texp with
        | Cst (Interval _) -> failwith "constant was an interval; this is not supported"
        | Cst (Scalar x) ->
          begin match SharedFunctions.int_of_scalar ?round:None x with
            | Some x -> [(None,x,Z.one)]
            | None -> raise ScalarIsInfinity end
        | Var x ->
          let var_dim = Environment.dim_of_var t.env x in
          begin match t.d with
            | None -> [(Some (Z.one,var_dim),Z.zero,Z.one)]
            | Some d ->
              (match (EConjI.get_rhs d var_dim) with
               | (Some (coeff,i), k,divi) -> [(Some (coeff,i),Z.zero,divi); (None,k,divi)]
               | (None,           k,divi) -> [                              (None,k,divi)])
          end
        | Unop  (Neg,  e, _, _) -> negate (convert_texpr e)
        | Unop  (Cast, e, _, _) -> convert_texpr e (* Ignore since casts in apron are used for floating point nums and rounding in contrast to CIL casts *)
        | Unop  (Sqrt, e, _, _) -> raise NotLinearExpr
        | Binop (Add, e1, e2, _, _) -> convert_texpr e1 @ convert_texpr e2
        | Binop (Sub, e1, e2, _, _) -> convert_texpr e1 @ negate (convert_texpr e2)
        | Binop (Mul, e1, e2, _, _) -> multiply (convert_texpr e1) (convert_texpr e2)
        | Binop _  -> raise NotLinearExpr end
    in match convert_texpr texp with
    | exception NotLinearExpr -> None
    | exception ScalarIsInfinity -> None
    | x -> Some(x)

  (** convert and simplify (wrt. reference variables) a texpr into a tuple of a list of monomials (coeff,varidx,divi) and a (constant/divi) *)
  let simplified_monomials_from_texp (t: t) texp =
    BatOption.bind (monomials_from_texp t texp)
      (fun monomiallist ->
         let d = Option.get t.d in
         let module IMap = EConjI.IntMap in
         let accumulate_constants (exprcache,(aconst,adiv)) (v,offs,divi) = match v with
           | None -> let gcdee = Z.gcd adiv divi in exprcache,(Z.(aconst*divi/gcdee + offs*adiv/gcdee),Z.lcm adiv divi)
           | Some (coeff,idx) -> let (somevar,someoffs,somedivi)=Rhs.subst (EConjI.get_rhs d idx) idx (v,offs,divi) in (* normalize! *)
             let newcache = Option.map_default (fun (coef,ter) -> IMap.add ter Q.((IMap.find_default zero ter exprcache) + make coef somedivi) exprcache) exprcache somevar in
             let gcdee = Z.gcd adiv divi in
             (newcache,(Z.(aconst*divi/gcdee + offs*adiv/gcdee),Z.lcm adiv divi))
         in
         let (expr,constant) = List.fold_left accumulate_constants (IMap.empty,(Z.zero,Z.one)) monomiallist in (* abstract simplification of the guard wrt. reference variables *)
         Some (IMap.fold (fun v c acc -> if Q.equal c Q.zero then acc else (Q.num c,v,Q.den c)::acc) expr [], constant) )

  let simplified_monomials_from_texp (t: t) texp =
    let res = simplified_monomials_from_texp t texp in
    if M.tracing then M.tracel "from_texp" "%s %a -> %s" (EConjI.show @@ BatOption.get t.d) Texpr1.Expr.pretty texp
        (BatOption.map_default (fun (l,(o,d)) -> List.fold_right (fun (a,x,b) acc -> Printf.sprintf "%s*var_%d/%s + %s" (Z.to_string a) x (Z.to_string b) acc) l ((Z.to_string o)^"/"^(Z.to_string d))) "" res);
    res

  let simplify_to_ref_and_offset (t: t) texp =
    BatOption.bind (simplified_monomials_from_texp t texp )
      (fun (sum_of_terms, (constant,divisor)) ->
         (match sum_of_terms with
          | [] -> Some (None, constant,divisor)
          | [(coeff,var,divi)] -> Some (Rhs.canonicalize (Some (Z.mul divisor coeff,var), Z.mul constant divi,Z.mul divisor divi))
          |_ -> None))

  let simplify_to_ref_and_offset t texp = timing_wrap "coeff_vec" (simplify_to_ref_and_offset t) texp

  (*TODO texpr has rather few constructors. Would we be more precise if we evaluated the CIL expression instead??*)
  let eval_texpr (t:t) texp = 
    let open Apron.Texpr1 in
    let binop_function = function
      | Add -> Value.add 
      | Sub -> Value.sub
      | Mul -> Value.mul
      | Div -> Value.div
      | Mod -> Value.rem
      | Pow -> failwith "power is not supported" (*TODO should this be supported*)
    in let unop_function = function
        | Neg -> Value.neg
        | Cast -> identity
        | Sqrt -> failwith "sqrt is not supported" (*TODO should this be supported*)
    in let rec eval = function
        | Cst (Scalar x) -> 
          begin match SharedFunctions.int_of_scalar ?round:None x with
            | Some x -> Value.of_bigint x
            | None -> Value.top
          end
        | Cst (Interval _) -> failwith "constant was an interval; this is not supported" (*TODO monomials_from_texp does not support this as well, but maybe we should*)
        | Var x -> 
          let var_dim = Environment.dim_of_var t.env x in
          begin match t.d with
            | None -> Value.top
            | Some d -> EConjI.get_interval d var_dim
          end
        | Binop (op, a, b, Int, _) -> (binop_function op) (eval a) (eval b)
        | Unop (op, a, Int, _) -> (unop_function op) (eval a)
        | _ -> Value.top (*not integers*)
    in 
    let res = eval texp in
    if M.tracing then M.tracel "eval_texp" "%s %a -> %s" (EConjI.show @@ BatOption.get t.d) Texpr1.Expr.pretty texp (Value.show res);
    res


  let assign_const t var const divi = match t.d with
    | None -> t
    | Some t_d -> {d = Some (EConjI.set_rhs t_d var (None, const, divi)); env = t.env}

end


module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement

  let bound_texpr t texpr =
    if t.d = None then None, None
    else
      match simplify_to_ref_and_offset t (Texpr1.to_expr texpr) with
      | Some (None, offset, divisor) when Z.equal (Z.rem offset divisor) Z.zero -> let res = Z.div offset divisor in
        (if M.tracing then M.tracel "bounds" "min: %a max: %a" GobZ.pretty res GobZ.pretty res;
         Some res, Some res)
      | _ -> None, None

  let bound_texpr d texpr1 = timing_wrap "bounds calculation" (bound_texpr d) texpr1
end

module D =
struct
  include Printable.Std
  include ConvenienceOps (Mpqf)
  include VarManagement

  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end
  module Convert = SharedFunctions.Convert (V) (Bounds) (Arg) (SharedFunctions.Tracked)

  type var = V.t

  let name () = "lin2vareq_pentagon"

  let to_yojson _ = failwith "ToDo Implement in future"

  (** t.d is some empty array and env is empty *)
  let is_bot t = equal t (bot ())

  (** forall x_i in env, x_i:=X_i+0 *)
  let top_of env = {d = Some (EConjI.make_empty_with_size (Environment.size env)); env = env}

  (** env = \emptyset, d = Some([||]) *)
  let top () = {d = Some (EConjI.empty()); env = empty_env}

  (** is_top returns true for top_of array and empty array; precondition: t.env and t.d are of same size *)
  let is_top t = GobOption.exists EConjI.is_top_con t.d

  let to_subscript i =
    let transl = [|"₀";"₁";"₂";"₃";"₄";"₅";"₆";"₇";"₈";"₉"|] in
    let rec subscr i =
      if i = 0 then ""
      else (subscr (i/10)) ^ transl.(i mod 10) in
    subscr i

  let show_var env i =
    let res = Var.to_string (Environment.var_of_dim env i) in
    match String.split_on_char '#' res with
    | varname::rest::[] -> varname ^ (try to_subscript @@ int_of_string rest with _ -> "#" ^ rest)
    | _ -> res

  (** prints the current variable equalities with resolved variable names *)
  let show varM =
    match varM.d with
    | None -> "⊥\n"
    | Some arr ->
      if is_bot varM then
        "Bot \n"
      else
        EConjI.show_formatted (show_var varM.env) arr ^ (to_subscript @@ fst @@ fst arr)

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nequalities\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%a</value>\n</map>\n</value>\n" (XmlUtil.escape (show x)) Environment.printXml x.env
  let eval_interval ask t texpr = 
    let from_top = function
      | TopIntOps.Int x -> Some x
      | _ -> None 
    in let i = eval_texpr t (Texpr1.to_expr texpr)
    in if M.tracing then M.tracel "eval_interval" "evaluating %a in %s to %s" Texpr1.pretty texpr (show t) (Value.show i);
    match fst i with
    | None -> (None, None)
    | Some (l, u) -> (from_top l, from_top u)

  let meet_with_one_conj t i (var, o, divi) =
    match t.d with
    | None -> t
    | Some d ->
      try
        { d = Some (EConjI.meet_with_one_conj d i (var, o, divi)); env = t.env}
      with EConj.Contradiction ->
        if M.tracing then M.trace "meet" " -> Contradiction with conj\n";
        { d = None; env = t.env}

  let meet_with_one_conj t i e =
    let res = meet_with_one_conj t i e in
    if M.tracing then M.tracel "meet" "%s with single eq %s=%s -> %s" (show t) (Z.(to_string @@ Tuple3.third e)^ show_var t.env i) (Rhs.show_rhs_formatted (show_var t.env) e) (show res);
    res

  let meet_with_one_interval i interval t =
    let res = match t.d with
      | None -> t
      | Some d ->
        try
          { d = Some (EConjI.meet_with_one_interval i interval d ); env = t.env}
        with EConj.Contradiction ->
          if M.tracing then M.trace "meet" " -> Contradiction with interval\n";
          { d = None; env = t.env}
    in
    if M.tracing then M.tracel "meet" "%s with single interval %s=%s -> %s" (show t) (show_var t.env i) (Value.show interval) (show res);
    res

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = change_d t1 sup_env ~add:true ~del:false in
    let t2 = change_d t2 sup_env ~add:true ~del:false in
    match t1.d, t2.d with
    | Some d1', Some d2' ->
      let conj_met = EConjI.IntMap.fold (fun lhs rhs map -> meet_with_one_conj map lhs rhs) (snd @@ fst d2') t1 (* even on sparse d2, this will chose the relevant conjs to meet with*)
      in EConjI.IntMap.fold meet_with_one_interval (snd d2') conj_met 
    | _ -> {d = None; env = sup_env}

  let meet t1 t2 =
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s\n U  \n b: %s \n -> %s" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = timing_wrap "meet" (meet t1) t2

  let leq t1 t2 =
    let env_comp = Environment.cmp t1.env t2.env in (* Apron's Environment.cmp has defined return values. *)
    let implies ts i (var, offs, divi) =
      let tuple_cmp = Tuple3.eq (Option.eq ~eq:(Tuple2.eq (Z.equal) (Int.equal))) (Z.equal) (Z.equal) in
      match var with
      (* directly compare in case of constant value *)
      | None -> tuple_cmp (var, offs, divi) (EConj.get_rhs ts i)
      (* normalize in case of a full blown equality *)
      | Some (coeffj,j) -> tuple_cmp (EConj.get_rhs ts i) @@ Rhs.subst (EConj.get_rhs ts j) j (var, offs, divi)
    in
    let implies_interval v i interval = Value.leq (EConjI.get_interval v i) interval
    in
    if env_comp = -2 || env_comp > 0 then false else
    if is_bot_env t1 || is_top t2 then true
    else if is_bot_env t2 || is_top t1 then false else
      let m1, m2 = Option.get t1.d, Option.get t2.d in
      let m1' = if env_comp = 0 then m1 else VarManagement.dim_add (Environment.dimchange t1.env t2.env) m1 in
      EConj.IntMap.for_all (implies @@ fst m1') (snd @@ fst m2) (* even on sparse m2, it suffices to check the non-trivial equalities, still present in sparse m2 *)
      && EConj.IntMap.for_all (implies_interval m1') (snd m2)

  let leq a b = timing_wrap "leq" (leq a) b

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b" (show t1) (show t2) res ;
    res

  (*The first parameter is the function used to join two intervals. Different uses for join / widen*)
  let join' join_function a b  = 
    let join_econj ad bd env = (LinearTwoVarEqualityDomain.D.join {d = Some ad; env} {d = Some bd; env}).d
    in
    (*Check all variables (up to index vars) if we need to save an interval for them*)
    let rec collect_intervals x y econj_joined vars is =
      if vars < 0 then is
      else if EConj.nontrivial econj_joined vars then collect_intervals x y econj_joined (vars-1) is (*we only need intervals for roots of the connected components*)
      else let joined_interval = join_function (EConjI.get_interval x vars) (EConjI.get_interval y vars) in (*TODO: if we tighten the interval in set_interval, we also should do that here.*)
        if Value.is_top joined_interval 
        then collect_intervals x y econj_joined (vars-1) is (*DO not add top intervals*)
        else collect_intervals x y econj_joined (vars-1) (EConjI.IntMap.add vars joined_interval is)
    in
    let join_d x y env = 
      let econj' = join_econj (fst x) (fst y) env in
      match econj' with 
        None ->  None 
      | Some econj'' ->
        let is' = collect_intervals x y econj'' ((Environment.size env)-1) (EConjI.IntMap.empty) in
        Some (econj'', is')
    in
    (*Normalize the two domains a and b such that both talk about the same variables*)
    match a.d, b.d with
    | None, _ -> b
    | _, None -> a
    | Some x, Some y when is_top a || is_top b ->
      let new_env = Environment.lce a.env b.env in
      top_of new_env
    | Some x, Some y when (Environment.cmp a.env b.env <> 0) ->
      let sup_env = Environment.lce a.env b.env in
      let mod_x = dim_add (Environment.dimchange a.env sup_env) x in
      let mod_y = dim_add (Environment.dimchange b.env sup_env) y in
      {d = join_d mod_x mod_y sup_env; env = sup_env}
    | Some x, Some y when EConjI.equal x y -> {d = Some x; env = a.env}
    | Some x, Some y  -> {d = join_d x y a.env; env = a.env} 


  let join = join' (Value.join) 
  let join a b = timing_wrap "join" (join a) b

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s" (show a) (show b) (show res) ;
    assert(leq a res);
    assert(leq b res);
    res

  let widen = join' (Value.widen) 

  let widen a b =
    let res = widen a b in
    if M.tracing then M.tracel "widen" "widen a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let narrow a b = meet a b (*TODO use narrow for intervals!*)

  let narrow a b =
    let res = narrow a b in
    if M.tracing then M.tracel "narrow" "narrow a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let forget_var t var =
    if is_bot_env t || is_top t then t
    else
      {d = Some (EConjI.forget_variable (Option.get t.d) (Environment.dim_of_var t.env var)); env = t.env}

  let forget_vars t vars =
    if is_bot_env t || is_top t || List.is_empty vars then
      t
    else
      let newm = List.fold (fun map i -> EConjI.forget_variable map (Environment.dim_of_var t.env i)) (Option.get t.d) vars in
      {d = Some newm; env = t.env}

  let forget_vars t vars =
    let res = forget_vars t vars in
    if M.tracing then M.tracel "ops" "forget_vars %s -> %s" (show t) (show res);
    res

  let forget_vars t vars = timing_wrap "forget_vars" (forget_vars t) vars

  (** implemented as described on page 10 in the paper about Fast Interprocedural Linear Two-Variable Equalities in the Section "Abstract Effect of Statements"
      This makes a copy of the data structure, it doesn't change it in-place. *)
  let assign_texpr (t: VarManagement.t) var texp =
    match t.d with
    | Some d ->
      let var_i = Environment.dim_of_var t.env var (* this is the variable we are assigning to *) in
      let t' = match simplify_to_ref_and_offset t texp with
        | None ->
          (* Statement "assigned_var = ?" (non-linear assignment) *)
          forget_var t var
        | Some (None, off, divi) ->
          (* Statement "assigned_var = off" (constant assignment) *)
          assign_const (forget_var t var) var_i off divi
        | Some (Some (coeff_var,exp_var), off, divi) when var_i = exp_var ->
          (* Statement "assigned_var = (coeff_var*assigned_var + off) / divi" *)
          {d=Some (EConj.affine_transform (fst d) var_i (coeff_var, var_i, off, divi), snd d); env=t.env }
        | Some (Some monomial, off, divi) ->
          (* Statement "assigned_var = (monomial) + off / divi" (assigned_var is not the same as exp_var) *)
          meet_with_one_conj (forget_var t var) var_i (Some (monomial), off, divi)
      in begin match t'.d with None -> bot_env
                             | Some d' -> {d = Some (EConjI.set_interval d' var_i (VarManagement.eval_texpr t' texp)); env = t'.env} 
      end
    | None -> bot_env

  let assign_texpr t var texp = timing_wrap "assign_texpr" (assign_texpr t var) texp

  (* no_ov -> no overflow
     if it's true then there is no overflow
      -> Convert.texpr1_expr_of_cil_exp handles overflow *)
  let assign_exp ask (t: VarManagement.t) var exp (no_ov: bool Lazy.t) =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp ask t t.env exp no_ov with
    | texp -> assign_texpr t var texp
    | exception Convert.Unsupported_CilExp _ -> forget_var t var

  let assign_exp ask t var exp no_ov =
    let res = assign_exp ask t var exp no_ov in
    if M.tracing then M.tracel "ops" "assign_exp t:\n %s \n var: %a \n exp: %a\n no_ov: %b -> \n %s"
        (show t) Var.pretty var d_exp exp (Lazy.force no_ov) (show res);
    res

  let assign_var (t: VarManagement.t) v v' =
    let t = add_vars t [v; v'] in
    assign_texpr t v (Var v')

  let assign_var t v v' =
    let res = assign_var t v v' in
    if M.tracing then M.tracel "ops" "assign_var t:\n %s \n v: %a \n v': %a\n -> %s" (show t) Var.pretty v Var.pretty v' (show res);
    res

  (** Parallel assignment of variables.
      First apply the assignments to temporary variables x' and y' to keep the old dependencies of x and y
      and in a second round assign x' to x and y' to y
  *)
  let assign_var_parallel t vv's =
    let assigned_vars = List.map fst vv's in
    let t = add_vars t assigned_vars in
    let primed_vars = List.init (List.length assigned_vars) (fun i -> Var.of_string (Int.to_string i  ^"'")) in (* TODO: we use primed vars in analysis, conflict? *)
    let t_primed = add_vars t primed_vars in
    let multi_t = List.fold_left2 (fun t' v_prime (_,v') -> assign_var t' v_prime v') t_primed primed_vars vv's in
    match multi_t.d with
    | Some arr when not @@ is_top multi_t ->
      let switched_arr = List.fold_left2 (fun multi_t assigned_var primed_var-> assign_var multi_t assigned_var primed_var) multi_t assigned_vars primed_vars in
      drop_vars switched_arr primed_vars ~del:true
    | _ -> t

  let assign_var_parallel t vv's =
    let res = assign_var_parallel t vv's in
    if M.tracing then M.tracel "ops" "assign_var parallel: %s -> %s" (show t) (show res);
    res

  let assign_var_parallel t vv's = timing_wrap "var_parallel" (assign_var_parallel t) vv's

  let assign_var_parallel_with t vv's =
    (* TODO: If we are angling for more performance, this might be a good place ot try. `assign_var_parallel_with` is used whenever a function is entered (body),
       in unlock, at sync edges, and when entering multi-threaded mode. *)
    let t' = assign_var_parallel t vv's in
    t.d <- t'.d;
    t.env <- t'.env

  let assign_var_parallel_with t vv's =
    if M.tracing then M.tracel "var_parallel" "assign_var parallel'";
    assign_var_parallel_with t vv's

  let assign_var_parallel' t vs1 vs2 =
    let vv's = List.combine vs1 vs2 in
    assign_var_parallel t vv's

  let assign_var_parallel' t vv's =
    let res = assign_var_parallel' t vv's in
    if M.tracing then M.tracel "ops" "assign_var parallel'";
    res

  let substitute_exp ask t var exp no_ov =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    let res = assign_exp ask t var exp no_ov in
    forget_var res var

  let substitute_exp ask t var exp no_ov =
    let res = substitute_exp ask t var exp no_ov in
    if M.tracing then M.tracel "ops" "Substitute_expr t: \n %s \n var: %a \n exp: %a \n -> \n %s" (show t) Var.pretty var d_exp exp (show res);
    res

  let substitute_exp ask t var exp no_ov = timing_wrap "substitution" (substitute_exp ask t var exp) no_ov

  (** Assert a constraint expression.
      The overflow is completely handled by the flag "no_ov",
      which is set in relationAnalysis.ml via the function no_overflow.
      In case of a potential overflow, "no_ov" is set to false
      and Convert.tcons1_of_cil_exp will raise the exception Unsupported_CilExp Overflow

      meet_tcons -> meet with guard in if statement
      texpr -> tree expr (right hand side of equality)
      -> expression used to derive tcons -> used to check for overflow
      tcons -> tree constraint (expression < 0)
      -> does not have types (overflow is type dependent)
  *)
  let meet_tcons ask t tcons original_expr no_ov =
    match t.d with
    | None -> t
    | Some d ->
      let expr = (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons) in
      (* meet EConj*)
      let t' = match simplified_monomials_from_texp t expr with
        | None -> t
        | Some (sum_of_terms, (constant,divisor)) ->(
            match sum_of_terms with
            | [] -> (* no reference variables in the guard, so check constant for zero *)
              begin match Tcons1.get_typ tcons with
                | EQ when Z.equal constant Z.zero -> t
                | SUPEQ when Z.geq constant Z.zero -> t
                | SUP when Z.gt constant Z.zero -> t
                | DISEQ when not @@ Z.equal constant Z.zero -> t
                | EQMOD _ -> t
                | _ -> bot_env (* all other results are violating the guard *)
              end
            | [(coeff, index, divi)] -> (* guard has a single reference variable only *)
              if Tcons1.get_typ tcons = EQ then
                meet_with_one_conj t index (Rhs.canonicalize (None, Z.neg @@ Z.(divi*constant),Z.(coeff*divisor)))
              else
                t (* only EQ is supported in equality based domains *)
            | [(c1,var1,d1); (c2,var2,d2)] -> (* two variables in relation needs a little sorting out *)
              begin match Tcons1.get_typ tcons with
                | EQ -> (* c1*var1/d1 + c2*var2/d2 +constant/divisor = 0*)
                  (* ======>  c1*divisor*d2 * var1 = -c2*divisor*d1 * var2 +constant*-d1*d2*)
                  (*   \/     c2*divisor*d1 * var2 = -c1*divisor*d2 * var1 +constant*-d1*d2*)
                  let open Z in
                  if var1 < var2 then
                    meet_with_one_conj t var2 (Rhs.canonicalize (Some (neg @@ c1*divisor,var1),neg @@ constant*d2*d1,c2*divisor*d1))
                  else
                    meet_with_one_conj t var1 (Rhs.canonicalize (Some (neg @@ c2*divisor,var2),neg @@ constant*d2*d1,c1*divisor*d2))
                | _-> t (* Not supported in equality based 2vars without coeffiients *)
              end
            | _ -> t (* For equalities of more then 2 vars we just return t *))
      in if t'.d = None then (if M.tracing then M.tracel "meet_tcons" "meet_conj resulted in None (expr: %s)" (Tcons1.show tcons); t') else begin
        (*meet interval*) (*TODO this could be extended much further, maybe reuse some code from base -> meet with CIL expression instead?*)
        (* currently only supports simple assertions x > c (x - c > 0)*)
        if M.tracing then M.tracel "meet_tcons" "after conj: %s (expr: %s)" (show t') (Tcons1.show tcons);
        match expr with
        | Binop (Sub,Var v,Cst (Scalar c),_,_) -> 
          begin match SharedFunctions.int_of_scalar ?round:None c with
            | None -> t'
            | Some c -> begin match Tcons1.get_typ tcons with
                | SUP -> 
                  meet_with_one_interval (Environment.dim_of_var t'.env v) (Some (Int (Z.add Z.one c), Top Pos), Value.C.top_of Value.ik) t'
                | SUPEQ -> meet_with_one_interval (Environment.dim_of_var t'.env v) (Some (Int c, Top Pos), Value.C.top_of Value.ik) t'
                | EQ -> 
                  if M.tracing then M.tracel "meet_tcons" "meet_tcons interval matching eq" ;
                  meet_with_one_interval (Environment.dim_of_var t'.env v) (Some (Int c, Int c), Value.C.top_of Value.ik) t' (*Should already be matched by the conjuction above?*)
                | _ ->
                  if M.tracing then M.tracel "meet_tcons" "meet_tcons interval not matching comparison op";
                  t' (*NEQ and EQMOD do not have any usefull interval representations*)
                  (*TODO If we have e.g.  y = 5x + 2 and condition y == 14 (or y != 14), we know this can't (must) be correct*)
              end
          end
        | Binop (Sub,Cst (Scalar c), Var v,_,_) -> 
          if M.tracing then M.tracel "meet_tcons" "meet_tcons interval matching structure 1";
          begin match SharedFunctions.int_of_scalar ?round:None c with
            | None -> t'
            | Some c -> begin match Tcons1.get_typ tcons with
                | SUP -> 
                  meet_with_one_interval (Environment.dim_of_var t'.env v) (Some (Top Neg, Int (Z.sub c Z.one)), Value.C.top_of Value.ik) t'
                | SUPEQ -> meet_with_one_interval (Environment.dim_of_var t'.env v) (Some (Top Neg, Int c), Value.C.top_of Value.ik) t'
                | EQ -> 
                  if M.tracing then M.tracel "meet_tcons" "meet_tcons interval matching eq (expr %s)" (Tcons1.show tcons);
                  meet_with_one_interval (Environment.dim_of_var t'.env v) (Some (Int c, Int c), Value.C.top_of Value.ik) t' (*Should already be matched by the conjuction above?*)
                | _ ->
                  if M.tracing then M.tracel "meet_tcons" "meet_tcons interval not matching comparison op (expr %s)" (Tcons1.show tcons);
                  t' (*NEQ and EQMOD do not have any usefull interval representations*)
              end
          end
        | _ ->
          if M.tracing then M.tracel "meet_tcons" "meet_tcons interval not matching structure";
          t'
      end


  let meet_tcons ask t tcons original_expr no_ov  =
    let res = meet_tcons ask t tcons original_expr no_ov
    in if M.tracing then M.tracel "meet_tcons" "meet_tcons with expr: %a no_ov:%b : %s -> %s" d_exp original_expr (Lazy.force no_ov) (show t) (show res);
    res


  let meet_tcons t tcons expr = timing_wrap "meet_tcons" (meet_tcons t tcons) expr

  let unify a b =
    meet a b

  let unify a b =
    let res = unify a b in
    if M.tracing then M.tracel "ops" "unify: %s\n    U\n %s -> %s" (show a) (show b) (show res);
    res

  (** Assert a constraint expression. Defined in apronDomain.apron.ml

      If the constraint is never fulfilled, then return bottom.
      Else the domain can be modified with the new information given by the constraint.

      It basically just calls the function meet_tcons.

      It is called by eval (defined in sharedFunctions), but also when a guard in
      e.g. an if statement is encountered in the C code.

  *)
  let assert_constraint ask d e negate (no_ov: bool Lazy.t) =
    match Convert.tcons1_of_cil_exp ask d d.env e negate no_ov with
    | tcons1 -> meet_tcons ask d tcons1 e no_ov
    | exception Convert.Unsupported_CilExp _ -> d

  let assert_constraint ask d e negate no_ov = timing_wrap "assert_constraint" (assert_constraint ask d e negate) no_ov

  let relift t = t

  (*TODO add value information to invariants?*)
  (** representation as C expression

      This function returns all the equalities that are saved in our datastructure t.

      Lincons -> linear constraint *)
  let invariant t =
    let of_coeff xi coeffs o =
      let typ = (Option.get @@ V.to_cil_varinfo xi).vtype in
      let ikind = Cilfacade.get_ikind typ in
      let cst = Coeff.s_of_z (IntDomain.Size.cast ikind o) in
      let lincons = Lincons1.make (Linexpr1.make t.env) Lincons1.EQ in
      Lincons1.set_list lincons coeffs (Some cst);
      lincons
    in
    let get_const acc i = function
      | (None, o, d) ->
        let xi = Environment.var_of_dim t.env i in
        of_coeff xi [(GobApron.Coeff.s_of_z @@ Z.neg d, xi)] o :: acc
      | (Some (c,r), _,_) when r = i -> acc
      | (Some (c,r), o, d) ->
        let xi = Environment.var_of_dim t.env i in
        let ri = Environment.var_of_dim t.env r in
        of_coeff xi [(GobApron.Coeff.s_of_z @@ Z.neg d, xi); (GobApron.Coeff.s_of_z c, ri)] o :: acc
    in
    BatOption.get t.d |> fun ((_,map),_) -> EConj.IntMap.fold (fun lhs rhs list -> get_const list lhs rhs) map []

  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

  let env t = t.env

  type marshal = t
  (* marshal is not compatible with apron, therefore we don't have to implement it *)
  let marshal t = t

  let unmarshal t = t

end

module D2: RelationDomain.RD with type var = Var.t =
struct
  module D = D
  module ConvArg = struct
    let allow_global = false
  end
  include SharedFunctions.AssertionModule (D.V) (D) (ConvArg)
  include D
end