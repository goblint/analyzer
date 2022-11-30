open Prelude.Ana
open Analyses
open Node0

(*
  An implementation of the alarm repositioning analysis
  from the article "Repositioning of Static Analysis Alarms"
  by Tukaram Muske, Rohith Talluri and Alexander Serebrenik
*)

module Idx = PreValueDomain.IndexDomain

module Cond = Printable.Prod (Basetype.CilExp) (Idx)

module Alarm = Printable.Prod (Printable.Strings) (Node)

module NH = Hashtbl.Make (Node)

let alarmsNH = NH.create 100

module CondSet = SetDomain.ToppedSet(Cond) (struct let topname = "top" end)
module AlarmSet = SetDomain.ToppedSet(Alarm) (struct let topname = "top" end)

module DAnt =
struct
  include SetDomain.ToppedSet(Printable.Prod (Cond) (Alarm)) (struct let topname = "top" end)

  let conds_in s = 
    match s with
    | `Lifted _ -> fold (fun (cond, alarm) conds -> CondSet.add cond conds) s (CondSet.empty ())
    | _ -> CondSet.empty ()

  let tuples_of c s = filter (fun (cond, alarm) -> Cond.equal c cond) s

  let meet x y =
    let conds = CondSet.inter (conds_in x) (conds_in y) in
    match x, y with
    | `Top, _ -> y
    | _, `Top -> x
    | `Lifted _, `Lifted _ -> CondSet.fold (fun c acc -> union acc (union (tuples_of c x) (tuples_of c y))) conds (empty ())

end

module DAv =
struct
  include SetDomain.ToppedSet(Printable.Prod3 (Cond) (Alarm) (Alarm)) (struct let topname = "top" end)

  let conds_in s = 
    match s with
    | `Lifted _ -> fold (fun (cond, orig_alarm, rel_alarm) conds -> CondSet.add cond conds) s (CondSet.empty ())
    | _ -> CondSet.empty ()

  let tuples_of c s = filter (fun (cond, orig_alarm, rel_alarm) -> Cond.equal c cond) s

  let meet x y =
    let conds = CondSet.inter (conds_in x) (conds_in y) in
    match x, y with
    | `Top, _ -> y
    | _, `Top -> x
    | `Lifted _, `Lifted _ -> CondSet.fold (fun c acc -> union acc (union (tuples_of c x) (tuples_of c y))) conds (empty ())

end

let ask : (Node0.t -> Queries.ask) ref = ref (fun a -> assert false)

module VarNode =
struct
  include Analyses.Var

  let is_write_only _ = false
  let node n = n

  let pretty_trace = Node.pretty_trace

end

module VarNodeL =
struct
  include VarNode

  let pretty_trace () x = Pretty.dprintf "In %a" pretty_trace x
end

module VarNodeG =
struct
  include VarNode

  let pretty_trace () x = Pretty.dprintf "Out %a" pretty_trace x
end

module V =
struct
  include Printable.Std
  include Constraints.Var2 (VarNodeL) (VarNodeG)

  let pretty = pretty_trace

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module HM = Hashtbl.Make (V)

let hoistHM = HM.create 10

let antSolHM = ref (HM.create 10)

let sinkHM = HM.create 10

let avSolHM = ref (HM.create 10)

module LocSet = SetDomain.ToppedSet(V) (struct let topname = "top" end)

(* Anticipable Alarm Conditions Analysis *)
module Ant =
struct

  module Var = V
  type v = V.t

  module Dom = DAnt
  type d = Dom.t

  let box _ _ _ = failwith "TODO"

  let conds_in s = Dom.fold (fun (cond,alarm) conds -> CondSet.add cond conds) s (CondSet.empty ())

  let rel_alarms c s = Dom.fold (fun (cond,alarm) alarms -> AlarmSet.add alarm alarms) s (AlarmSet.empty ())

  let dep_gen node x = Dom.empty ()

  (* Find the set of those condition and alarm pairs, where an operand of the condition is changed in the given node *)
  let kill node x =
    match node with
    | Statement stmt ->
      begin match stmt.skind with
        | Instr [] -> Dom.empty ()
        | Instr xs ->
          let assigned_vars = List.fold (fun acc instr ->
              match instr with
              | Set ((Var varinfo, NoOffset), _, _, _) -> varinfo :: acc
              (* TODO: other lval cases *)
              | _ -> acc
            ) [] xs in
          Dom.filter (fun ((exp, _), alarm) -> List.fold (fun acc var -> Basetype.CilExp.occurs var exp || acc) false assigned_vars) x
        | _ -> Dom.empty ()
      end
    | _ -> Dom.empty ()

  let process (alarm : (Cond.t * Alarm.t)) y = alarm

  let gen' node x =
    let module CFG = (val !MyCFG.current_cfg) in
    match NH.find_option alarmsNH node with
    | Some alarm -> Dom.singleton (process alarm (conds_in x))
    | None -> Dom.empty ()

  let gen node x = Dom.union (gen' node x) (dep_gen node (kill node x))

  (* Compute the anticipable conditions at the entry (`L) and exit (`G) of a node *)
  let system = function
    (* AntIn *)
    | `L node ->
      let f get _ =
        let ant_out = get (`G node) in
        Dom.union (gen' node ant_out) (Dom.diff ant_out (kill node ant_out))
      in
      Some f
    (* AntOut *)
    | `G node ->
      let f get _ =
        match node with
        | Node.Function _ -> Dom.empty ()
        | _ ->
          let module CFG = (val !MyCFG.current_cfg) in
          let next_nodes = List.map snd (CFG.next node) in
          let next_values = List.map (fun node -> get (`L node)) next_nodes in
          List.fold_left Dom.meet (Dom.top ()) next_values
      in
      Some f

  let increment = Analyses.empty_increment_data ()

  let iter_vars _ _ _ = ()
end

(* Available Alarm Conditions Analysis *)
module Av = 
struct

  module Var = V
  type v = V.t

  module Dom = DAv
  type d = Dom.t

  let box _ _ _ = failwith "TODO"

  let conds_in s = Dom.fold (fun (cond, orig_alarm, rel_alarm) conds -> CondSet.add cond conds) s (CondSet.empty ())

  let dep_gen node x = Dom.empty ()

  let kill node x =
    match node with
    | Statement stmt ->
      begin match stmt.skind with
        | Instr [] -> Dom.empty ()
        | Instr xs ->
          let assigned_vars = List.fold (fun acc instr ->
              match instr with
              | Set ((Var varinfo, NoOffset), _, _, _) -> varinfo :: acc
              (* TODO: other lval cases *)
              | _ -> acc
            ) [] xs in
          Dom.filter (fun ((exp, _), _, _) -> List.fold (fun acc var -> Basetype.CilExp.occurs var exp || acc) false assigned_vars) x
        | _ -> Dom.empty ()
      end
    | _ -> Dom.empty ()

  let gen node x = CondSet.fold (fun cond acc ->
      let ant = HM.find !antSolHM node in
      let rel_alarms = Ant.rel_alarms cond ant in
      AlarmSet.fold (fun (msg, n) acc -> Dom.add (
          cond, (msg, n), match node with
          | `L n -> (msg, n)
          | `G n -> (msg, n)) 
          acc) rel_alarms acc
    ) x (Dom.empty ())

  let gen_entry node x =
    match HM.find_option hoistHM (`L node) with
    | None -> Dom.empty ()
    | Some set ->
      match CondSet.is_empty set with
      | true -> Dom.empty ()
      | false -> gen (`L node) set

  let gen_exit node =
    match HM.find_option hoistHM (`G node) with
    | None -> Dom.empty ()
    | Some set ->
      match CondSet.is_empty set with
      | true -> Dom.empty ()
      | false -> gen (`G node) set

  let system = function
    (* AvIn *)
    | `L node ->
      let f get _ =
        match node with
        | Node.FunctionEntry _ -> Dom.empty ()
        | _ ->
          let module CFG = (val !MyCFG.current_cfg) in
          let prev_nodes = List.map snd (CFG.prev node) in
          let prev_values = List.map (fun node -> get (`G node)) prev_nodes in
          List.fold_left Dom.meet (Dom.top ()) prev_values
      in
      Some f
    (* AvOut *)
    | `G node ->
      let f get _ =
        let av_in' n =
          let av_in = get (`L n) in
          Dom.union av_in @@ gen_entry n av_in in
        let av_out' n =
          let av_in' = av_in' n in
          Dom.union (Dom.diff av_in' (kill n av_in')) (dep_gen n av_in') in
        let av_out n = Dom.union (gen_exit n) (av_out' n) in
        av_out node
      in
      Some f

  let increment = Analyses.empty_increment_data ()

  let iter_vars _ _ _ = ()
end

module MLocSet = Set.Make (M.Location)

let array_oob_warn idx_before_end idx_after_start node var_node =
  (* For an explanation of the warning types check the Pull Request #255 *)
  let (orig_locs, rel_locs) = DAv.fold (fun (cond, (_, orig_node), (_, rel_node)) (orig_locs, rel_locs) -> 
      (MLocSet.add (M.Location.Node orig_node) orig_locs, MLocSet.add (M.Location.Node rel_node) rel_locs)) 
      (HM.find !avSolHM var_node) (MLocSet.empty, MLocSet.empty) in
  let locs : Messages.Locs.t = {original=(MLocSet.to_list orig_locs); related=MLocSet.to_list rel_locs} in
  let loc = MLocSet.any rel_locs in
  match(idx_after_start, idx_before_end) with
  | Some true, Some true -> (* Certainly in bounds on both sides.*)
    ()
  | Some true, Some false -> (* The following matching differentiates the must and may cases*)
    M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "Must access array past end" ~loc:loc ~locs:locs
  | Some true, None ->
    M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.past_end "May access array past end" ~loc:loc ~locs:locs
  | Some false, Some true ->
    M.error ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.before_start "Must access array before start" ~loc:loc ~locs:locs
  | None, Some true ->
    M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.before_start "May access array before start" ~loc:loc ~locs:locs
  | _ ->
    M.warn ~category:M.Category.Behavior.Undefined.ArrayOutOfBounds.unknown "May access array out of bounds" ~loc:loc ~locs:locs


let init _ =
  NH.clear alarmsNH;
  HM.clear hoistHM;
  HM.clear sinkHM

let finalize _ =
  let fd = Cilfacade.find_name_fundec "main" in
  let start_node = `L (Node.FunctionEntry fd) in
  let module IncrSolverArg =
  struct
    let should_prune = false
    let should_verify = false
    let should_warn = false
    let should_save_run = false
  end
  in

  (* NH.iter (fun k (cond, a) -> ignore (Pretty.printf "%a->%a, %a\n" Node.pretty_trace k Cond.pretty cond Alarm.pretty a)) alarmsNH; *)

  let module Solver = Td3.WP (IncrSolverArg) (Ant) (HM) in
  let (solution, _) = Solver.solve Ant.box [] [start_node] in

  antSolHM := solution;

  (* HM.iter (fun k v -> ignore (Pretty.printf "%a->%a\n" Ant.Var.pretty_trace k Ant.Dom.pretty v)) solution; *)

  let filter_always_true node cs = CondSet.filter (fun (exp, l) ->
      let q = (!ask node).f (EvalInt exp) in
      let v = Idx.of_interval (Cilfacade.ptrdiff_ikind ()) (Option.get @@ Queries.ID.minimal q, Option.get @@ Queries.ID.maximal q) in
      (* ignore (Pretty.printf "eval %a%a" Node.pretty node Idx.pretty v); *)
      let idx_before_end = Idx.to_bool (Idx.lt v l) (* check whether index is before the end of the array *)
      and idx_after_start = Idx.to_bool (Idx.ge v (Idx.of_int Cil.ILong Z.zero)) in (* check whether the index is non-negative *)
      match (idx_before_end, idx_after_start) with
      | Some true, Some true -> false (* Certainly in bounds on both sides.*)
      | _ -> true
    ) cs in

  let hoist_entry node =
    let module CFG = (val !MyCFG.current_cfg) in
    let prev_nodes = List.map snd (CFG.prev node) in
    let prev_conds = List.map (fun prev_node -> Ant.conds_in (HM.find solution (`G prev_node))) prev_nodes in
    let cs = List.fold_left CondSet.inter (CondSet.top ()) prev_conds in
    filter_always_true node @@ CondSet.diff (Ant.conds_in (HM.find solution (`L node))) cs in

  let hoist_exit node =
    let module CFG = (val !MyCFG.current_cfg) in
    let next_nodes = List.map snd (CFG.next node) in
    let node' = List.hd next_nodes in
    filter_always_true node' @@ Ant.conds_in @@ Ant.Dom.filter
      (fun c -> Ant.Dom.is_empty @@ Ant.dep_gen node (Ant.Dom.singleton c))
      (Ant.kill node @@ HM.find solution (`G node)) in

  (* Update hashtable of hoisted conditions *)
  HM.iter (fun k v ->
      match k with
      | `L Function _ -> ()
      | `G Function _ -> ()
      | `L node -> HM.replace hoistHM k @@ hoist_entry node
      | `G node -> HM.replace hoistHM k @@ hoist_exit node
    ) solution;

  (* HM.iter (fun k v -> ignore (Pretty.printf "%a->%a\n" Ant.Var.pretty_trace k CondSet.pretty v)) hoistHM; *)

  let end_node = `G (Node.Function fd) in
  let module SolverAv = Td3.WP (IncrSolverArg) (Av) (HM) in
  let (solution_av, _) = SolverAv.solve Av.box [] [end_node] in

  avSolHM := solution_av;

  (* HM.iter (fun k v -> ignore (Pretty.printf "%a->%a\n" Av.Var.pretty_trace k Av.Dom.pretty v)) solution_av; *)

  let conds_entry node =
    let av_in = HM.find solution_av (`L node) in
    let av_in' = Av.Dom.union av_in @@ Av.gen_entry node av_in in
    Av.conds_in (Av.kill node av_in') in

  let conds_exit node =
    let module CFG = (val !MyCFG.current_cfg) in
    let next_nodes = List.map snd (CFG.next node) in
    let av_in_conds = List.fold (fun acc node -> CondSet.inter acc (Av.conds_in @@ HM.find solution_av (`L node))) (CondSet.top ()) next_nodes in
    let av_out_conds = Av.conds_in @@ HM.find solution_av (`G node) in
    CondSet.diff av_out_conds av_in_conds in

  (* 
    As a special case, the algorithm utilizes every condition from 
    the entry of the last node for repositioning,
    because a few avconds can reach the program end point, 
    but not get computed by equations conds_entry and conds_exit.
  *)
  let conds_end node =
    let module CFG = (val !MyCFG.current_cfg) in
    let prev_nodes = List.map snd (CFG.prev node) in
    let prev_conds = List.map (fun prev_node -> Av.conds_in (HM.find solution_av (`G prev_node))) prev_nodes in
    let conds = List.fold_left CondSet.inter (CondSet.top ()) prev_conds in
    List.fold (fun acc node -> HM.replace sinkHM (`G node) conds) () prev_nodes in

  (* Update hashtable of sinked conditions *)
  HM.iter (fun k v ->
      match k with
      | `L (Function n) -> conds_end (Function n)
      | `L node -> HM.replace sinkHM k @@ conds_entry node
      | `G node -> HM.replace sinkHM k @@ conds_exit node
    ) solution_av;

  (* HM.iter (fun k v -> ignore (Pretty.printf "%a->%a\n" Av.Var.pretty_trace k CondSet.pretty v)) sinkHM; *)

  (* Print repositioned warnings *)
  HM.iter (fun k s -> CondSet.iter (fun (exp, l) ->
      let warn q_node loc_node =
        let q = (!ask q_node).f (EvalInt exp) in
        let v = Idx.of_interval (Cilfacade.ptrdiff_ikind ()) (Option.get @@ Queries.ID.minimal q, Option.get @@ Queries.ID.maximal q) in
        let idx_before_end = Idx.to_bool (Idx.lt v l) (* check whether index is before the end of the array *)
        and idx_after_start = Idx.to_bool (Idx.ge v (Idx.of_int Cil.ILong Z.zero)) in (* check whether the index is non-negative *)
        array_oob_warn idx_before_end idx_after_start (M.Location.Node loc_node) k in
      match k with
      | `L n -> warn n n
      | `G n ->
        let module CFG = (val !MyCFG.current_cfg) in
        let next_nodes = List.map snd (CFG.next n) in
        List.iter (fun node ->
            match node with
            | Function _ -> warn n n
            | _ -> warn node n) next_nodes
    ) s) sinkHM;
