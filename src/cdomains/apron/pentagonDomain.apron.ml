(** Implementation of the pentagon domain (pntg)
    @see <https://doi.org/10.1016/j.scico.2009.04.004> 
    "Pentagons: A weakly relational abstract domain for the efficient validation of array accesses"
    -- Francesco Logozzo, Manuel Fähndrich (2010) *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open GobApron
open IntDomain0

module Inequalities = struct
  module VarMap = BatMap.Make(Int)
  module VarSet = BatSet.Make(Int)

  type t =  (VarSet.t VarMap.t) [@@deriving eq, ord]
  let hash : (t -> int)  = fun _ -> failwith "TODO"
  let copy (x: t) = x
  let empty () = (VarMap.empty: t)
  let is_empty ineq  = VarMap.is_empty ineq
  let dim_add : (Apron.Dim.change -> t -> t)  = fun _ -> failwith "TODO"
  let dim_remove : (Apron.Dim.change -> t -> del:bool-> t)  = fun _ -> failwith "TODO"

end


module VariableManagement =
struct
  include SharedFunctions.VarManagementOps (Inequalities)

  let get_map_opt t = t.d

  let get_env t = t.env

end

module SUB =
struct
  module VarMan = VariableManagement
  module VarMap = Inequalities.VarMap
  module VarSet = Inequalities.VarSet

  type t = VarMan.t

  let unify_from_env (sub1:t) (sub2:t) lce = 
    match sub1.d, sub2.d with
    | Some(sub_map_1), Some(sub_map_2) -> ( 
        let ext_sub1 = Inequalities.dim_add (Environment.dimchange sub1.env lce) sub_map_1 in
        let ext_sub2 = Inequalities.dim_add (Environment.dimchange sub2.env lce) sub_map_2 in
        (ext_sub1, ext_sub2)
      )
    | _ -> failwith "There is no need to unify the environments here"



  let equal sub1 sub2 = VarMap.equal (VarSet.equal) sub1 sub2

  let bot_of env = ({ d = None; env = env}: t)

  let bot () = ({ d = None; env= VarMan.empty_env } :t )

  (** 
      When possible, we use `d = None` to signal a bot value.
      However, we cannot be sure if a value differs from bot, when
      `d != None`, as there might still be contradictions in our map,
      which indicates a bot value. This search is expensive and
      we try to shortcut it as best as possible.
  *)
  let is_bot (sub:t) = 
    match sub.d with
    | None -> true
    (** 
       TODO: Implement contradiction search -- transitive closure in the worst-case.
    *)
    | Some(sub_map) ->
      let key_is_equal_for_all k1 = (fun k2 -> VarSet.exists (fun k -> k == k1) (VarMap.find k2 sub_map)) in
      let iterate_greater_variables = (fun k1 v -> VarSet.exists (key_is_equal_for_all k1) v) in
      VarMap.exists iterate_greater_variables sub_map 

  (** The environment (env) manages the number of variables stored in our datatype and 
      therefore the dimensions of our value space. The inequalities should
      return empty on variables not found in the underlying map.*)
  let top_of env = ({ d = Some(Inequalities.empty ()); env = env }: t)

  (** This is the top value for the null-space i.e. the space with dimension 0 or no stored variables.
      In the case where there are no variables, it holds that bot == top. *)
  let top () = ({ d = Some(Inequalities.empty ()); env = VarMan.empty_env }: t)
  let is_top (sub: t) = 
    match VarMan.get_map_opt sub with
    | None -> false
    | Some(sub_map) -> VarMap.for_all (fun _ set -> VarSet.is_empty set) sub_map 

  let subseteq set1 set2 = VarSet.subset set1 set2 || VarSet.equal set1 set2 (** helper, missing in batteries *)

  (**
     The inequalities map s1 is less than or equal to s2 iff
      forall x in s2.
      s2(x) subseteq s1(x)
  *)
  let leq (sub1: t) (sub2: t) =
    match sub1.d, sub2.d with
    | None, _ -> true
    | _, None -> false
    | Some(sub_map_1), Some(sub_map_2) -> 
      let subseteq_s1 var_key_2 greater_vars_2 = 
        let greater_vars_1 = VarMap.find var_key_2 sub_map_1 in
        subseteq greater_vars_2 greater_vars_1
      in
      VarMap.for_all subseteq_s1 sub_map_2

  let join (sub1: t) (sub2: t) =
    let lce = Environment.lce sub1.env sub2.env in
    match sub1.d, sub2.d with
    | None, None -> bot ()
    | None, _ -> sub2
    | _, None -> sub1
    | Some(sub_map_1), Some(sub_map_2) when is_top sub1 || is_top sub2 ->
      top_of lce
    | Some(sub_map_1), Some(sub_map_2) ->
      (** Make sure that the maps contain keys for all variables before comparing. *)
      let ext_sub1 = Inequalities.dim_add (Environment.dimchange sub1.env lce) sub_map_1 in
      let ext_sub2 = Inequalities.dim_add (Environment.dimchange sub2.env lce) sub_map_2 in
      let intersect_sets_of_key var_key var_set1_opt var_set2_opt =
        match var_set1_opt, var_set2_opt with
        | Some(var_set1), Some(var_set2) -> Some(VarSet.inter var_set1 var_set2)
        | None, None -> failwith "This should never happen :)"
        | _ -> failwith "dim_add should take care of that :)"
      in
      { d = Some(VarMap.merge (intersect_sets_of_key) ext_sub1 ext_sub2); env = lce }

  let meet (sub1: t) (sub2: t) =
    let lce = Environment.lce sub1.env sub2.env in
    match sub1.d, sub2.d with
    | Some(sub_map_1), Some(sub_map_2) -> (
        let ext_sub_map_1 = Inequalities.dim_add (Environment.dimchange sub1.env lce) sub_map_1 in
        let ext_sub_map_2 = Inequalities.dim_add (Environment.dimchange sub2.env lce) sub_map_2 in
        let union_sets_of_key var_key var_set1_opt var_set2_opt =
          match var_set1_opt, var_set2_opt with
          | Some(var_set1), Some(var_set2) -> Some(VarSet.union var_set1 var_set2)
          | None, None -> failwith "This should never happen :)"
          | _ -> failwith "dim_add should take care of that :)"
        in
        ({ d = Some(VarMap.merge union_sets_of_key ext_sub_map_1 ext_sub_map_2); env=lce }: t)
      )
    | _ -> bot_of lce

  let widen (sub1: t) (sub2: t) =
    let lce = Environment.lce sub1.env sub2.env in
    match sub1.d, sub2.d with
    | Some(sub_map_1), Some(sub_map_2) -> (
        let ext_sub_map_1 = Inequalities.dim_add (Environment.dimchange sub1.env lce) sub_map_1 in
        let ext_sub_map_2 = Inequalities.dim_add (Environment.dimchange sub2.env lce) sub_map_2 in
        let widen_sets_of_key var_key var_set1_opt var_set2_opt =
          match var_set1_opt, var_set2_opt with
          | Some(var_set1), Some(var_set2) ->
            if subseteq var_set1 var_set2 then Some (var_set2) else Some (VarSet.empty)
          | None, None -> failwith "This should never happen :)"
          | _ -> failwith "dim_add should take care of that :)"
        in
        ({ d = Some(VarMap.merge widen_sets_of_key ext_sub_map_1 ext_sub_map_2); env = lce }:t)
      )
    | _ -> top_of lce (** Naively extrapolate to top. We are unsure if this is intented by the papers definitions. *)

  (** No narrowing mentioned in the paper. *)
  let narrow sub1 sub2 = meet sub1 sub2


end

module INTERVALS  = 
struct
  module VarMap = BatMap.Make(Int)

  type interval = Z.t * Z.t
  type t = interval VarMap.t
  module IArith = IntervalArith(IntOps.BigIntOps)

  let equal intv1 intv2 =
    let tuple_equal (a1, b1) (a2, b2) = Z.equal a1 a2 && Z.equal b1 b2 in
    VarMap.equal tuple_equal intv1 intv2

  let leq_single (i1: interval) (i2: interval) =
    fst i1 >= fst i2 && snd i1 <= snd i2

  let join_single (i1: interval) (i2: interval) =
    (Z.min (fst i1) (fst i2), Z.max (snd i1) (snd i2))

  let meet_single (i1: interval) (i2: interval) =
    let l = Z.max (fst i1) (fst i2) in
    let u = Z.min (snd i1) (snd i2) in
    if l <= u then Some (l, u) else None

  let top_single () = (Z.of_int min_int, Z.of_int max_int)

  let is_top_single (i: interval) =
    fst i = Z.of_int min_int && snd i = Z.of_int max_int

  let widen_single (i1: interval) (i2: interval) =
    let l = if fst i1 <= fst i2 then fst i1 else Z.of_int min_int in
    let u = if snd i1 >= snd i2 then snd i1 else Z.of_int max_int in
    (l, u)

  let narrow_single (i1: interval) (i2: interval) = meet_single i1 i2

  let is_bot_single (i: interval) =
    fst i > snd i

  let leq (i1: t) (i2: t) =
    VarMap.for_all (fun var iv2 ->
        match VarMap.find_opt var i1 with
        | Some iv1 -> leq_single iv1 iv2
        | None -> false
      ) i2

  let join (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
        match iv1, iv2 with
        | Some iv1, Some iv2 -> Some (join_single iv1 iv2)
        | Some iv, None | None, Some iv -> Some iv
        | None, None -> None
      ) i1 i2

  let meet (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
        match iv1, iv2 with
        | Some iv1, Some iv2 -> meet_single iv1 iv2
        | _ -> None
      ) i1 i2

  let top () =
    VarMap.empty |> VarMap.map (fun _ -> top_single ())

  let is_top (i: t) =
    VarMap.for_all (fun _ iv -> is_top_single iv) i

  let widen (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
        match iv1, iv2 with
        | Some iv1, Some iv2 -> Some (widen_single iv1 iv2)
        | Some iv, None | None, Some iv -> Some iv
        | None, None -> None
      ) i1 i2

  let narrow (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
        match iv1, iv2 with
        | Some iv1, Some iv2 -> narrow_single iv1 iv2
        | _ -> None
      ) i1 i2

  let is_bot (i: t) =
    VarMap.exists (fun _ iv -> is_bot_single iv) i

  let bot () = VarMap.empty |> VarMap.add 0 ( Z.of_int max_int, Z.of_int min_int)

  let sup (x: interval) = snd x

  let inf (x: interval) = fst x
end

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module D =
struct
  include Printable.Std
  module SUB = SUB

  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end

  (**
     TODO: module Tracked
  *)
  module Tracked = struct let varinfo_tracked _ = failwith "TODO";; let type_tracked _ = failwith "TODO";; end

  type var = V.t
  type t = { intv: INTERVALS.t; sub: SUB.t }
  type marshal

  let name () = "pentagon"

  let to_yojson _ = failwith "TODO"

  let bot () = { intv = INTERVALS.bot (); sub = SUB.bot () }

  let is_bot t = INTERVALS.is_bot t.intv || SUB.is_bot t.sub

  let top () = { intv = INTERVALS.top (); sub = SUB.top () }

  let is_top t = INTERVALS.is_top t.intv && SUB.is_top t.sub

  let show varM = failwith "TODO"
  let pretty () (x:t) = failwith "TODO"
  let printXml f x = failwith "TODO"

  let meet t1 t2 =
    { intv = INTERVALS.meet t1.intv t2.intv;
      sub = SUB.meet t1.sub t2.sub }

  let meet t1 t2 =
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s\n U  \n b: %s \n -> %s" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = Timing.wrap "meet" (meet t1) t2

  let leq t1 t2 =
    let b1, b2 = t1.intv, t2.intv in
    let s_map_opt_1, s_map_opt_2 = t1.sub.d, t2.sub.d in
    INTERVALS.leq b1 b2
    &&
    match s_map_opt_1, s_map_opt_2 with
    | None, _ -> true (** might be wrong? *)
    | _, None -> false (** might be wrong? *)
    | Some(s_map_1), Some(s_map_2) -> 
      (
        (* Boilerplate *)     
        let lce = Environment.lce t1.sub.env t2.sub.env in
        let sub_map_1, sub_map_2 = SUB.unify_from_env t1.sub t2.sub lce in

        SUB.VarMap.for_all (
          fun x s2x -> 
            SUB.VarSet.for_all (
              fun y -> (
                  let s1x = SUB.VarMap.find x sub_map_1 in
                  let b1x = INTERVALS.VarMap.find x b1 in
                  SUB.VarSet.exists (Int.equal y) s1x ||
                  INTERVALS.sup b1x < INTERVALS.inf b1x
                )          
            ) s2x) sub_map_2
      )

  let leq a b = Timing.wrap "leq" (leq a) b

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b" (show t1) (show t2) res ;
    res

  let join a b = failwith "TODO"

  let join a b = Timing.wrap "join" (join a) b

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let widen t1 t2 = 
    { intv = INTERVALS.widen t1.intv t2.intv;
    sub = SUB.widen t1.sub t2.sub }

  let widen a b =
    let res = widen a b in
    if M.tracing then M.tracel "widen" "widen a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let narrow t1 t2 = 
    { intv = INTERVALS.narrow t1.intv t2.intv;
      sub = SUB.narrow t1.sub t2.sub }

  let narrow a b =
    let res = narrow a b in
    if M.tracing then M.tracel "narrow" "narrow a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let pretty_diff () (x, y) = failwith "TODO"


  (* S2 Specific functions of RelationDomain *)
  let is_bot_env _ = failwith "TODO"
  let vars _ = failwith "TODO"
  let add_vars _ = failwith "TODO"
  let remove_vars _ = failwith "TODO"

  let remove_vars_with _ = failwith "TODO"

  let remove_filter _ = failwith "TODO"
  let remove_filter_with _ = failwith "TODO"

  let copy _ = failwith "TODO"
  let keep_vars _ = failwith "TODO"
  let keep_filter _ = failwith "TODO"
  let forget_vars _ = failwith "TODO"


  let assign_exp _ = failwith "TODO"
  let assign_var _ = failwith "TODO"

  let assign_var_parallel_with _ = failwith "TODO"

  let assign_var_parallel' _ = failwith "TODO"
  let substitute_exp _ = failwith "TODO"
  let unify _ = failwith "TODO"
  let marshal _ = failwith "TODO"
  let unmarshal _ = failwith "TODO"
  let mem_var _ = failwith "TODO"
  let assert_inv _ = failwith "TODO"
  let elet_int _ = failwith "TODO"
  let cil_exp_of_lincons1 _ = failwith "TODO"
  let invariant _ = failwith "TODO"
  let equal _ = failwith "TODO"
  let hash _ = failwith "TODO"
  let compare _ = failwith "TODO"
  let relift _ = failwith "TODO"
  let eval_int _ = failwith "TODO"
end

