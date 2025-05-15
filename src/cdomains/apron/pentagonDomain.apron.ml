(** Implementation of the pentagon domain (pntg)
    @see <https://doi.org/10.1016/j.scico.2009.04.004> 
    "Pentagons: A weakly relational abstract domain for the efficient validation of array accesses"
    -- Francesco Logozzo, Manuel Fähndrich (2010) *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open GobApron

module Mpqf = SharedFunctions.Mpqf


module Inequalities = struct
  module VarMap = BatMap.Make(Int)
  module VarSet = BatSet.Make(Int)

  type t =  (VarSet.t VarMap.t) [@@deriving eq, ord]
  let hash : (t -> int)  = fun _ -> failwith "TODO"
  let copy (x: t) = x
  let empty () = (VarMap.empty: t)
  let is_empty : (t -> bool)  = VarMap.is_empty 
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

  let (bot: unit -> t) = VarMan.bot
  let is_bot sub = VarMan.equal sub (bot ())
  let top (): (unit -> t) = fun _ -> failwith "TODO" (** Philip *)
  let is_top t: (t -> bool) = fun _ -> failwith "TODO" (** Philip *)

  (**
     The inequalities map s1 is less than or equal to s2 iff

      forall x in s2.
      s1(x) subseteq s2(x)

      or equivalently

      forall x in s2
      !(s2(x) subset s1(x))
  *)

  let leq (sub1: t) (sub2: t) =
    let sub_map_1, sub_map_2 = get_map_default_2 sub1 sub2 in
    let subseteq sub1 var_key greater_vars_2 = 
      let greater_vars_1 = VarMap.find var_key sub1 in
      not (VarSet.subset greater_vars_1 greater_vars_2) 
    in
    VarMap.for_all (subseteq sub_map_1) sub_map_2

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


  let meet: (t -> t -> t) = fun _ -> failwith "TODO" (** Alex *)
  let widen: (t -> t -> t) = fun _ -> failwith "TODO" (** Alex *)
  let narrow: (t -> t -> t) = fun _ -> failwith "TODO" (** Philip *)


end

module Intervals = 
struct
  type t = T (*change*)
  let leq: (t -> t -> bool)  = fun _ -> failwith "TODO"
  let join: (t -> t -> t) = fun _ -> failwith "TODO"
  let meet: (t -> t -> t) = fun _ -> failwith "TODO"
  let widen: (t -> t -> t) = fun _ -> failwith "TODO"
  let narrow: (t -> t -> t) = fun _ -> failwith "TODO"
  let bot (): (unit -> t) = fun _ -> failwith "TODO"
  let is_bot t: (t -> bool) = fun _ -> failwith "TODO"
  let top (): (unit -> t) = fun _ -> failwith "TODO"
  let is_top t: (t -> bool) = fun _ -> failwith "TODO"
end

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module D =
struct
  include Printable.Std
  include RatOps.ConvenienceOps (Mpqf)

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

