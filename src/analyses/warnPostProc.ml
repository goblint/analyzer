open Prelude.Ana
open Analyses
open Node0

(*
  An implementation of the alarm repositioning analysis
  from the article "Repositioning of Static Analysis Alarms"
  by Tukaram Muske, Rohith Talluri and Alexander Serebrenik
*)

module Idx = PreValueDomain.IndexDomain

module Alarm = RepositionMessages.ReposMessage

module RM = RepositionMessages

module CondSet = SetDomain.ToppedSet(RM.Cond) (struct let topname = "top" end)
module AlarmSet = SetDomain.ToppedSet(Alarm) (struct let topname = "top" end)

module D =
struct
  include SetDomain.ToppedSet(Alarm) (struct let topname = "top" end)

  let conds_in s = 
    match s with
    | `Lifted _ -> fold (fun alarm conds -> CondSet.add alarm.cond conds) s (CondSet.empty ())
    | _ -> CondSet.empty ()

  let tuples_of c s = filter (fun alarm -> RM.Cond.equal c alarm.cond) s

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

let set_loc (message : Alarm.t) loc =
  match message.multipiece with
  | Single piece -> let piece = {piece with loc=Some loc} in
    {message with multipiece=Single piece}
  | _ -> message


let get_alarm_loc (mp : RM.MultiPiece.t) : M.Location.t option =
  match mp with
  | Single piece -> piece.loc
  | Group g -> failwith "TODO"

(* Anticipable Alarm Conditions Analysis *)
module Ant =
struct

  module Var = V
  type v = V.t

  module Dom = D
  type d = Dom.t

  let box _ _ _ = failwith "TODO"

  let sys_change _ = {Analyses.obsolete = []; delete = []; reluctant = []; restart = []}

  let conds_in s = Dom.fold (fun alarm conds -> CondSet.add alarm.cond conds) s (CondSet.empty ())

  let rel_alarms c s = Dom.fold (fun alarm alarms -> AlarmSet.add alarm alarms) s (AlarmSet.empty ())

  let dep_gen node x = Dom.empty ()

  (* Find the set of those condition and alarm pairs, where an operand of the condition is changed in the given node *)
  let changed_in_node var (cond : RM.Cond.t) = 
    match cond with
    | Aob (exp, _) -> Basetype.CilExp.occurs var exp

  let var_changed_in_node stmt x =
    begin match stmt.skind with
      | Instr [] -> Dom.empty ()
      | Instr xs ->
        let assigned_vars = List.fold (fun acc instr ->
            match instr with
            | Set ((Var varinfo, NoOffset), _, _, _) -> varinfo :: acc
            (* TODO: other lval cases *)
            | _ -> acc
          ) [] xs in
        Dom.filter (fun alarm -> List.fold (fun acc var -> changed_in_node var alarm.cond || acc) false assigned_vars) x
      | _ -> Dom.empty ()
    end

  let kill node x =
    match node with
    | Statement stmt -> var_changed_in_node stmt x
    | _ -> Dom.empty ()

  let process (alarm : Alarm.t) y = alarm

  let gen' node x =
    match RM.NH.find_opt RM.messagesNH node with
    | Some alarm -> Dom.singleton (process alarm (conds_in x))
    | None -> Dom.empty ()

  let gen node x = Dom.union (gen' node x) (dep_gen node (kill node x))

  (* Compute the anticipable conditions at the entry (`L) and exit (`G) of a node *)
  let system = function
    (* AntIn *)
    | `L node ->
      let f get _ =
        let ant_out = get (`G node) in
        let alarms = Dom.union (gen node ant_out) (Dom.diff ant_out (kill node ant_out)) in
        Dom.map (fun alarm -> set_loc alarm @@ M.Location.Node node) alarms
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
          let alarms = List.fold_left Dom.meet (Dom.top ()) next_values in
          Dom.map (fun alarm -> set_loc alarm @@ M.Location.Node node) alarms
      in
      Some f

  let increment = None

  let iter_vars _ _ _ = ()
end

module RMLocSet = Set.Make (RM.Location)

(* Available Alarm Conditions Analysis *)
module Av = 
struct

  module Var = V
  type v = V.t

  module Dom = D
  type d = Dom.t

  let box _ _ _ = failwith "TODO"

  let sys_change _ = {Analyses.obsolete = []; delete = []; reluctant = []; restart = []}

  let conds_in s = Dom.fold (fun alarm conds -> CondSet.add alarm.cond conds) s (CondSet.empty ())

  let dep_gen node x = Dom.empty ()

  let changed_in_node var (cond : RM.Cond.t) = 
    match cond with
    | Aob (exp, _) -> Basetype.CilExp.occurs var exp

  let var_changed_in_node stmt x =
    begin match stmt.skind with
      | Instr [] -> Dom.empty ()
      | Instr xs ->
        let assigned_vars = List.fold (fun acc instr ->
            match instr with
            | Set ((Var varinfo, NoOffset), _, _, _) -> varinfo :: acc
            (* TODO: other lval cases *)
            | _ -> acc
          ) [] xs in
        Dom.filter (fun alarm -> List.fold (fun acc var -> changed_in_node var alarm.cond || acc) false assigned_vars) x
      | _ -> Dom.empty ()
    end

  let kill node x =
    match node with
    | Statement stmt -> var_changed_in_node stmt x
    | _ -> Dom.empty ()

  let set_locs alarm node rel_alarms = 
    let alarm = set_loc alarm (M.Location.Node node) in (* TODO: does this work? *)
    let orig_alarms = AlarmSet.fold (fun alarm acc ->
        let origs = List.to_seq alarm.locs.original in
        RMLocSet.add_seq origs acc)
        rel_alarms RMLocSet.empty in
    let rel_alarms = AlarmSet.fold (fun alarm acc ->
        match (get_alarm_loc alarm.multipiece) with
        | Some loc -> RMLocSet.add loc acc
        | _ -> acc) rel_alarms RMLocSet.empty in
    {alarm with locs={original=RMLocSet.to_list orig_alarms; related = RMLocSet.to_list rel_alarms}}

  let gen node x = CondSet.fold (fun cond dom ->
      let ant = HM.find !antSolHM node in
      let rel_alarms = Ant.rel_alarms cond ant in
      (* Update the message locations to correspond to the sinked location and add original and related alarms *)
      AlarmSet.fold (fun alarm dom_updated -> 
          Dom.add
            (match node with
             | `L n -> set_locs alarm n rel_alarms
             | `G n -> set_locs alarm n rel_alarms)
            dom_updated)
        rel_alarms dom
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

  let increment = None

  let iter_vars _ _ _ = ()
end

let init _ =
  RM.NH.clear RM.messagesNH; (* TODO: does not work with incremental *)
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

  (* RM.NH.iter (fun n alarm -> ignore (Pretty.printf "%a->%a\n" Node.pretty_trace n RM.ReposMessage.pretty alarm)) RM.messagesNH; *)

  let module Solver = Td3.Basic (IncrSolverArg) (Ant) (HM) in
  let (solution, _) = Solver.solve Ant.box [] [start_node] None in

  antSolHM := solution;

  (* HM.iter (fun k v -> ignore (Pretty.printf "%a->%a\n" Ant.Var.pretty_trace k Ant.Dom.pretty v)) solution; *)

  let cond_holds node (cond : RM.Cond.t) =
    match cond with
    | Aob (exp, l) ->
      let q = (!ask node).f (EvalInt exp) in
      let v = Idx.of_interval (Cilfacade.ptrdiff_ikind ()) (Option.get @@ Queries.ID.minimal q, Option.get @@ Queries.ID.maximal q) in
      (* ignore (Pretty.printf "eval %a%a" Node.pretty node Idx.pretty v); *)
      let idx_before_end = Idx.to_bool (Idx.lt v l) (* check whether index is before the end of the array *)
      and idx_after_start = Idx.to_bool (Idx.ge v (Idx.of_int Cil.ILong Z.zero)) in (* check whether the index is non-negative *)
      match (idx_before_end, idx_after_start) with
      | Some true, Some true -> false (* Certainly in bounds on both sides.*)
      | _ -> true
  in

  let filter_always_true node cs = CondSet.filter (cond_holds node) cs in

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

  (* 
    Similarly to handling the conditions in the end node during sinking, 
    as a special case, the algorithm utilizes every condition from 
    the exit of the first node for repositioning,
    because a few antconds can reach the program starting point, 
    but not get computed by equations hoist_entry and hoist_exit.
  *)
  let conds_start node =
    let sol = HM.find solution (`G node) in
    let conds = Av.conds_in sol in
    let og_node = match (D.choose sol).locs.original with
      | (x::xs) -> begin match x with
          | Node l -> l
          | _ -> node end
      | _ -> node in 
    HM.replace hoistHM (`L og_node) conds in

  (* Update hashtable of hoisted conditions *)
  HM.iter (fun k v ->
      match k with
      | `G (FunctionEntry n) -> conds_start (FunctionEntry n)
      | `G Function _ -> () (* cannot find next nodes for end node *)
      | `L node -> HM.replace hoistHM k @@ hoist_entry node
      | `G node -> HM.replace hoistHM k @@ hoist_exit node
    ) solution;

  (* HM.iter (fun k v -> ignore (Pretty.printf "%a->%a\n" Ant.Var.pretty_trace k CondSet.pretty v)) hoistHM; *)

  let end_node = `G (Node.Function fd) in
  let module SolverAv = Td3.Basic (IncrSolverArg) (Av) (HM) in
  let (solution_av, _) = SolverAv.solve Av.box [] [end_node] None in

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
  let reposmessage_to_message (rm : Alarm.t) =
    M.add {tags = rm.tags; severity = rm.severity; multipiece = rm.multipiece; locs = rm.locs}
  in

  (* Merge original and related alarms to one message and
     update the message location to be one of the hoisted
     locations instead of sinked location, if available
  *)
  let warn alarms =
    let (orig_locs, rel_locs) = D.fold (fun alarm (orig_locs, rel_locs)->
        let origs = List.to_seq alarm.locs.original in
        let reltd = List.to_seq alarm.locs.related in
        RMLocSet.add_seq origs orig_locs, RMLocSet.add_seq reltd rel_locs)
        alarms (RMLocSet.empty, RMLocSet.empty) 
    in
    let rel_alarm = D.choose alarms in
    let alarm =
      match rel_alarm.locs.related with
      | rel_loc :: _ -> set_loc rel_alarm rel_loc
      | _ -> rel_alarm
    in
    reposmessage_to_message {alarm with locs={original=RMLocSet.to_list orig_locs; related=RMLocSet.to_list rel_locs}}
  in

  (* Find the corresponding messages for the sinked alarms from solution_av and print them out *)
  HM.iter (fun n s -> CondSet.iter (fun _ -> warn @@ HM.find !avSolHM n) s) sinkHM;
