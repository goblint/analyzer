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

  let get_map_default t = BatOption.default (Inequalities.empty ()) t.d
end

module SUB =
struct
  module VarMan = VariableManagement
  module VarMap = Inequalities.VarMap
  module VarSet = Inequalities.VarSet

  type t = VarMan.t

  let get_map_default_2 s1 s2 = 
    VarMan.get_map_default s1, VarMan.get_map_default s2

  (** Verify that this is actually top *)
  let bot () = VarMan.bot ()
  let is_bot sub = VarMan.equal sub (bot ())

  (** Verify that this is actually top *)
  let top () = ({ d = Some(Inequalities.empty ()); env = VarMan.empty_env }: t)
  let is_top (sub: t) = 
    let sub_map = VarMan.get_map_default sub in    
    VarMap.for_all (fun _ set -> VarSet.is_empty set) sub_map 

  let subseteq set1 set2 = VarSet.subset set1 set2 || VarSet.equal set1 set2 (** helper, missing in batteries *)

  (**
     The inequalities map s1 is less than or equal to s2 iff
      forall x in s2.
      s2(x) subseteq s1(x)
  *)

  let leq (sub1: t) (sub2: t) =
    let sub_map_1, sub_map_2 = get_map_default_2 sub1 sub2 in
    let subseteq_s1 var_key_2 greater_vars_2 = 
      let greater_vars_1 = VarMap.find var_key_2 sub_map_1 in
      subseteq greater_vars_2 greater_vars_1
    in
    VarMap.for_all subseteq_s1 sub_map_2

  let join (sub1: t) (sub2: t) = 
    let sub_map_1, sub_map_2 = get_map_default_2 sub1 sub2 in
    let intersect_values var_key var_set1_opt var_set2_opt = 
      match var_set1_opt, var_set2_opt with
      | Some(var_set1), Some(var_set2) -> Some(VarSet.inter var_set1 var_set2)
      | None, None -> failwith "This should never happen :)"  
      | None, s -> s
      | s, None -> s
    in
    VarMap.merge intersect_values sub_map_1 sub_map_2


  let meet (sub1: t) (sub2: t) =
    let sub_map_1, sub_map_2 = get_map_default_2 sub1 sub2 in
    let union_values var_key var_set1_opt var_set2_opt =
      match var_set1_opt, var_set2_opt with
      | Some(var_set1), Some(var_set2) -> Some(VarSet.union var_set1 var_set2)
      | None, None -> failwith "This should never happen :)"
      | _ -> None
    in
    VarMap.merge union_values sub_map_1 sub_map_2

  let widen (sub1: t) (sub2: t) =
    let sub_map_1, sub_map_2 = get_map_default_2 sub1 sub2 in
    let widen_set var_key var_set1_opt var_set2_opt =
      match var_set1_opt, var_set2_opt with
      | Some(var_set1), Some(var_set2) ->
        if subseteq var_set1 var_set2 then Some (var_set2) else Some (VarSet.empty)
      | None, Some(var_set2) -> Some(var_set2)
      | None, None -> failwith "This should never happen :)"
      | _ -> Some (VarSet.empty)
    in
    VarMap.merge widen_set sub_map_1 sub_map_2

  (** TODO: No narrowing mentioned in the paper. Can we improve on this? *)
  let narrow sub1 sub2 = meet sub1 sub2


end

module Intervals = 
struct
  module VarMap = BatMap.Make(Int)

  type interval = Z.t * Z.t
  type t = interval VarMap.t
  module IArith = IntervalArith(IntOps.BigIntOps)

  let leq (i1: t) (i2: t) =
    VarMap.for_all (fun var (l2, u2) ->
      match VarMap.find_opt var i1 with
      | Some (l1, u1) -> l1 >= l2 && u1 <= u2
      | None -> false
    ) i2

  let join (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
      match iv1, iv2 with
      | Some (l1, u1), Some (l2, u2) -> Some (Z.min l1 l2, Z.max u1 u2)
      | Some iv, None | None, Some iv -> Some iv
      | None, None -> None
    ) i1 i2

  let meet (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
      match iv1, iv2 with
      | Some (l1, u1), Some (l2, u2) ->
        let l = Z.max l1 l2 in
        let u = Z.min u1 u2 in
        if l <= u then Some (l, u) else None
      | _ -> None
    ) i1 i2

  let top () =
    VarMap.empty |> VarMap.map (fun _ -> (Z.of_int min_int, Z.of_int max_int))

  let is_top (i: t) =
    VarMap.for_all (fun _ (l, u) -> l = Z.of_int min_int && u = Z.of_int max_int) i

  let widen (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
      match iv1, iv2 with
      | Some (l1, u1), Some (l2, u2) ->
        let l = if l1 <= l2 then l1 else Z.of_int min_int in
        let u = if u1 >= u2 then u1 else Z.of_int max_int in
        Some (l, u)
      | Some iv, None | None, Some iv -> Some iv
      | None, None -> None
    ) i1 i2

  let narrow (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
      match iv1, iv2 with
      | Some (l1, u1), Some (l2, u2) ->
        let l = Z.max l1 l2 in
        let u = Z.min u1 u2 in
        if l <= u then Some (l, u) else None
      | _ -> None
    ) i1 i2

  let bot () = VarMap.empty

  let is_bot (i: t) =
    VarMap.exists (fun _ (l, u) -> l > u) i

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

  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end

  module Tracked = struct let varinfo_tracked _ = failwith "TODO";; let type_tracked _ = failwith "TODO";; end

  type var = V.t
  type t
  type marshal

  let name () = "pentagon"

  let to_yojson _ = failwith "TODO"

  let bot _ = failwith "TODO"

  let is_bot t = failwith "TODO"

  let top () = failwith "TODO"

  let is_top t = failwith "TODO"

  let show varM = failwith "TODO"
  let pretty () (x:t) = failwith "TODO"
  let printXml f x = failwith "TODO"
  let meet t1 t2 = failwith "TODO"

  let meet t1 t2 =
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s\n U  \n b: %s \n -> %s" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = Timing.wrap "meet" (meet t1) t2

  let leq t1 t2 = failwith "TODO"

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

  let widen a b = failwith "TODO"

  let widen a b =
    let res = widen a b in
    if M.tracing then M.tracel "widen" "widen a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let narrow a b = failwith "TODO"

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

