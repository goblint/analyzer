open Prelude.Ana
open Analyses
open Node0

(*
  An implementation of the alarm repositioning analysis
  from the article "Repositioning of Static Analysis Alarms"
  by Tukaram Muske, Rohith Talluri and Alexander Serebrenik.
*)

module Alarm = RepositionMessages.ReposMessage
module Idx = PreValueDomain.IndexDomain
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

let antSolHM = HM.create 10

let sinkHM = HM.create 10

let avSolHM = HM.create 10

let ask : (Node0.t -> Queries.ask) ref = ref (fun a -> assert false)
let ask' : (Node0.t -> (CilType.Location.t * Edge.t) -> Node0.t -> Queries.ask) ref = ref (fun n e n' -> assert false)

let set_loc (message : Alarm.t) loc =
  match message.multipiece with
  | Single piece -> {message with multipiece = Single {piece with loc = Some loc}}
  | Group group -> {message with multipiece = Group {group with loc = Some loc}}

(* Do not add related alarm when an alarm with that location already exists in the pieces list
   because otherwise the original alarm is set as related and duplicated *)
let contains_alarm_with_loc node (pieces : M.Piece.t list) =
  let node_equals loc node =
    match loc with
    | Some M.Location.Node n -> Node.equal n node
    | _ -> false
  in
  List.fold (fun acc (piece : M.Piece.t) -> node_equals piece.loc node || acc) false pieces


let set_related (message : Alarm.t) node =
  match message.multipiece with
  | Group group ->
    if contains_alarm_with_loc node group.pieces then message else
      let piece = RM.Piece.{text = "Possible cause"; loc = Some (RM.Location.Node node); context = None} in
      let group = {group with pieces = (piece::group.pieces)} in
      {message with multipiece = Group group}
  | _ -> failwith "TODO"

let filter_eq_locs loc1 loc2 =
  match loc1, loc2 with
  | Some (M.Location.Node n1), Some (M.Location.Node n2) -> not @@ Node.equal n1 n2
  | _, _ -> true

let rem_related (message : Alarm.t) node =
  match message.multipiece with
  | Group group ->
    let pieces = List.filter (fun (piece : M.Piece.t) -> filter_eq_locs (Some (RM.Location.Node node)) piece.loc || piece.text != "Possible cause") group.pieces in (* TODO: remove the text check *)
    let group = {group with pieces = pieces} in
    {message with multipiece = Group group}
  | _ -> failwith "TODO"

let rec append_unique l1 l2 =
  match l2 with
  | [] -> l1
  | (x::xs) -> if List.mem x l1
    then append_unique l1 xs
    else append_unique (x::l1) xs

(* Find the set of those messages, where an operand of the condition is changed in the given node *)
let var_changed_in_node stmt exp =
  begin match stmt.skind with
    | Instr [] -> false
    | Instr xs ->
      let assigned_vars = List.fold (fun acc instr ->
          match instr with
          | Set ((Var varinfo, NoOffset), _, _, _) -> varinfo :: acc
          (* TODO: other lval cases *)
          | _ -> acc
        ) [] xs in
      List.fold (fun acc var -> Basetype.CilExp.occurs var exp || acc) false assigned_vars
    | _ -> false
  end

let no_race_or_lock stmt node acc = 
  if (!ask node).f (MayRace (Obj.repr acc)) then
    begin match stmt.skind with
      | Instr [] -> false
      | Instr xs ->
        let varinfos = List.fold (fun acc instr ->
            match instr with
            | Call (_, Lval (Var varinfo, NoOffset), exp_list, _, _) -> (varinfo, exp_list) :: acc
            | _ -> acc
          ) [] xs in
        let is_lock library_desc = 
          match library_desc with
          | LibraryDesc.Lock _ -> true
          | _ -> false
        in
        List.fold (fun acc (var, exp_list) -> is_lock ((LibraryFunctions.find var).special exp_list) || acc) false varinfos
      | _ -> false
    end
  else true

(* Anticipable Alarm Conditions Analysis *)
module Ant =
struct

  module Var = V
  type v = V.t

  module Dom = D
  type d = Dom.t

  let sys_change _ = {Analyses.obsolete = []; delete = []; reluctant = []; restart = []}

  let conds_in s = Dom.fold (fun alarm conds -> CondSet.add alarm.cond conds) s (CondSet.empty ())

  let rel_alarms c s = Dom.fold (fun alarm alarms -> 
      if (RM.Cond.equal alarm.cond c) then AlarmSet.add alarm alarms else alarms) s (AlarmSet.empty ())

  let dep_gen node x = Dom.empty ()

  let filter_killed stmt node (alarm : Alarm.t) = 
    match alarm.cond with 
    | Aob (exp, _) -> var_changed_in_node stmt exp
    | Acc (a, accs, _) -> Access.AS.fold (fun (_,_,_,_,a) acc -> no_race_or_lock stmt node a && acc) accs true

  let kill node x =
    match node with
    | Statement stmt -> Dom.filter (filter_killed stmt node) x
    | _ -> Dom.empty ()

  let process (alarm : Alarm.t) y = alarm

  let gen' node x =
    match RM.NH.find_option RM.messagesNH node with
    | Some alarms -> RM.RMSet.fold (fun alarm acc -> Dom.add alarm acc) alarms (Dom.empty ()) 
    | None -> Dom.empty ()

  let gen node x = Dom.union (gen' node x) (dep_gen node (kill node x))

  (* Compute the anticipable conditions at the entry (`L) and exit (`G) of a node.
     We set the message location to the hoisted location in the end,
     which is different from the original algorithm
     because we would like to show the message in the related hoisted position
     rahter than a random sinked position *)
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


(* Available Alarm Conditions Analysis *)
module Av =
struct

  module Var = V
  type v = V.t

  module Dom = D
  type d = Dom.t

  let sys_change _ = {Analyses.obsolete = []; delete = []; reluctant = []; restart = []}

  let conds_in s = Dom.fold (fun alarm conds -> CondSet.add alarm.cond conds) s (CondSet.empty ())

  let dep_gen node x = Dom.empty ()

  let filter_killed stmt node (alarm : Alarm.t) =
    match alarm.cond with
    | Aob (exp, _) -> var_changed_in_node stmt exp
    | Acc (a, accs, _) -> Access.AS.fold (fun (_,_,_,_,a) acc -> no_race_or_lock stmt node a && acc) accs true

  let kill node x =
    match node with
    | Statement stmt -> Dom.filter (filter_killed stmt node) x
    | _ -> Dom.empty ()

  let set_locs alarm node rel_alarms =
    let alarm = set_loc alarm (M.Location.Node node) in (* TODO: does this work? *)
    let rel_pieces = AlarmSet.fold (fun alarm acc ->
        match alarm.multipiece with
        | Group group -> append_unique group.pieces acc
        | _ -> acc) rel_alarms []
    in
    match alarm.multipiece with
    | Group group ->
      let rel_alarms = append_unique group.pieces rel_pieces in
      let new_group = {group with pieces=rel_alarms} in
      {alarm with multipiece=Group new_group}
    | _ -> alarm

  let gen node x = CondSet.fold (fun cond dom ->
      let ant = HM.find antSolHM node in
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


module IncrSolverArg =
struct
  let should_prune = false
  let should_verify = false
  let should_warn = false
  let should_save_run = false
end

module Solver = Td3.Basic (IncrSolverArg) (Ant) (HM)

let warn_postprocess fd =
  (* RM.NH.iter (fun n alarms -> ignore (Pretty.printf "%a->%a\n" Node.pretty_trace n RM.RMSet.pretty alarms)) RM.messagesNH; *)

  let start_node = `L (Node.FunctionEntry fd) in
  let (solution, _) = Solver.solve [] [start_node] None in

  (* HM.iter (fun k v -> ignore (Pretty.printf "%a->%a\n" Ant.Var.pretty_trace k Ant.Dom.pretty v)) solution; *)

  (* The function to check if a condition holds in the given node.
     This function can be extended with other variant types when
     the message repositioning is implemented for other analyses. *)
  let cond_holds (ask : Queries.ask) (cond : RM.Cond.t) =
    match cond with
    | Aob (exp, l) ->
      let q = ask.f (EvalInt exp) in
      let v = Idx.of_interval (Cilfacade.ptrdiff_ikind ()) (Option.get @@ Queries.ID.minimal q, Option.get @@ Queries.ID.maximal q) in
      (* ignore (Pretty.printf "eval %a%a" Node.pretty node Idx.pretty v); *)
      let idx_before_end = Idx.to_bool (Idx.lt v l) (* check whether index is before the end of the array *)
      and idx_after_start = Idx.to_bool (Idx.ge v (Idx.of_int Cil.ILong Z.zero)) in (* check whether the index is non-negative *)
      begin match (idx_before_end, idx_after_start) with
        | Some true, Some true -> false (* Certainly in bounds on both sides.*)
        | _ -> true
      end
    | Acc (a, accs, _) -> Access.AS.fold (fun (_,_,_,_,a) acc -> ask.f (MayRace (Obj.repr a)) || acc) accs false (* does not work due to join *)
  in

  let filter_always_true ask cs = CondSet.filter (cond_holds ask) cs in

  let hoist_entry node =
    let module CFG = (val !MyCFG.current_cfg) in
    let prev_nodes = List.map snd (CFG.prev node) in
    let prev_conds = List.map (fun prev_node -> Ant.conds_in (HM.find solution (`G prev_node))) prev_nodes in
    let cs = List.fold_left CondSet.inter (CondSet.top ()) prev_conds in
    (* TODO: there is something fishy going on here somewhere. *)
    filter_always_true (!ask node) @@ CondSet.diff (Ant.conds_in (HM.find solution (`L node))) cs in

  let hoist_exit node =
    let module CFG = (val !MyCFG.current_cfg) in
    let next_nodes = CFG.next node in
    (* TODO: Evaluating at the post-state of a transfer function before the join. 
       If no unique successor assume only refinements possible -> use current state;
       if there is a unique successor, we use the state at that node. 
       Also, if target node has another incoming edge, less precise but safe.  *)
    let ask = if List.compare_length_with next_nodes 1 = 0 then
        let (edges, next_node) = List.hd next_nodes in
        !ask' node (List.hd edges) next_node
      else !ask node in
    filter_always_true ask @@ Ant.conds_in @@ Ant.Dom.filter
      (fun c -> Ant.Dom.is_empty @@ Ant.dep_gen node (Ant.Dom.singleton c))
      (Ant.kill node @@ HM.find solution (`G node)) in

  (* Update hashtable of hoisted conditions *)
  HM.iter (fun k v ->
      match k with
      | `G (FunctionEntry _) | `G Function _ -> () (* handling entry nodes separately later; cannot find next nodes for end node *)
      | `L node ->
        HM.replace hoistHM k @@ hoist_entry node;
        HM.replace solution k @@ Ant.Dom.map (fun alarm -> set_related alarm node) v (* Add hoisted location as a related piece in each message. *)
      | `G node ->
        HM.replace hoistHM k @@ hoist_exit node;
        HM.replace solution k @@ Ant.Dom.map (fun alarm -> set_related alarm node) v (* Add hoisted location as a related piece in each message. *)
    ) solution;

  (* Similarly to handling the conditions in the end node during sinking,
     as a special case, the algorithm utilizes every condition from
     the exit of the first node for repositioning,
     because a few antconds can reach the program starting point,
     but not get computed by equations hoist_entry and hoist_exit.
     This is an addition that was not described in the original algorithm. *)
  let conds_start node =
    let sol = HM.find solution (`G node) in
    let conds = Av.conds_in sol in
    if (D.is_empty sol) then ()
    else
      let og_node = match (D.choose sol).multipiece with
        | Group group ->
          begin match (List.hd group.pieces).loc with
            | Some Node n -> n
            | _ -> node
          end
        | _ -> node
      in
      let merge = D.fold (fun alarm acc -> D.add (set_loc alarm (RM.Location.Node og_node)) acc) sol in
      let filter = D.map (fun alarm -> rem_related alarm og_node) in
      HM.modify_def (D.empty ()) (`L og_node) (fun alarm -> filter (merge alarm)) solution;
      HM.modify_def (CondSet.empty ()) (`L og_node) (CondSet.union conds) hoistHM;
  in

  conds_start (Node.FunctionEntry fd);

  HM.iter (HM.add antSolHM) solution;

  (* HM.iter (fun k v -> ignore (Pretty.printf "%a->%a\n" Ant.Var.pretty_trace k CondSet.pretty v)) hoistHM; *)

  let end_node = `G (Node.Function fd) in
  let module SolverAv = Td3.Basic (IncrSolverArg) (Av) (HM) in
  let (solution_av, _) = SolverAv.solve [] [end_node] None in

  HM.iter (HM.add avSolHM) solution_av;

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

  (* Update hashtable of sinked conditions *)
  HM.iter (fun k v ->
      match k with
      | `L node -> HM.replace sinkHM k @@ conds_entry node
      | `G node -> HM.replace sinkHM k @@ conds_exit node
    ) solution_av;

  (* As a special case, the algorithm utilizes every condition from
     the entry of the last node for repositioning,
     because a few avconds can reach the program end point,
     but not get computed by equations conds_entry and conds_exit. *)
  let conds_end node =
    let module CFG = (val !MyCFG.current_cfg) in
    let prev_nodes = List.map snd (CFG.prev node) in
    let prev_conds = List.map (fun prev_node -> Av.conds_in (HM.find solution_av (`G prev_node))) prev_nodes in
    let conds = List.fold_left CondSet.inter (CondSet.top ()) prev_conds in
    List.fold (fun acc node -> HM.replace sinkHM (`G node) conds) () prev_nodes in

  conds_end (Function fd);

  (* HM.iter (fun k v -> ignore (Pretty.printf "%a->%a\n" Av.Var.pretty_trace k CondSet.pretty v)) sinkHM; *)

module HA = Hashtbl.Make (Access.AS)
let accGroupsHM = HA.create 10

let init _ =
  RM.NH.clear RM.messagesNH; (* TODO: does not work with incremental *)
  HM.clear antSolHM;
  HM.clear avSolHM;
  HM.clear hoistHM;
  HM.clear sinkHM

let finalize _ = Cil.iterGlobals !Cilfacade.current_file (function
    | GFun (fd, _) -> warn_postprocess fd
    | _ -> ()
  );

  (* Add the repositioned messages as regular messages. *)
  let reposmessage_to_message (rm : Alarm.t) =
    M.add {tags = rm.tags; severity = rm.severity; multipiece = rm.multipiece}
  in

  let convert_to_single (alarm : Alarm.t) merged_pieces = 
    match alarm.multipiece with
    | Group group -> 
      if (List.length merged_pieces == 1) 
      then {alarm with multipiece = Single (List.hd merged_pieces)}
      else {alarm with multipiece = Group {group with pieces = merged_pieces}}
    | _ -> alarm
  in

  let merge_group_warnings alarms = D.fold (fun alarm acc ->
      (* TODO: how to merge different categories of the message groups? *)
      match alarm.multipiece with
      | Group group -> append_unique group.pieces acc
      | _ -> acc)
      alarms [] 
  in

  let merge_race_warnings alarms =
    let merged_pieces = merge_group_warnings alarms in
    let alarm = D.choose alarms in (* TODO: choose the shown alarm reasonably instead of random *)
    convert_to_single alarm merged_pieces |> reposmessage_to_message
  in 

  (* Merge the grouped alarms to one grouped message. *)
  let merge_alarms_with_same_cond cond alarms =
    let filtered_alarms = D.filter (fun alarm -> RM.Cond.equal alarm.cond cond) alarms in
    let merged_pieces = merge_group_warnings filtered_alarms in
    let alarm = D.choose filtered_alarms in (* TODO: choose the shown alarm reasonably instead of random *)
    let merged = convert_to_single alarm merged_pieces in
    match alarm.cond with
    | Acc (_,_,group) -> HA.modify_def (D.empty ()) group (D.add merged) accGroupsHM
    | _ -> reposmessage_to_message merged

  in

  (* Find the corresponding messages for the sinked alarms from solution_av and print them out. *)
  HM.iter (fun n condset -> 
      CondSet.iter (fun c ->
          let msg = HM.find avSolHM n in
          match D.is_empty msg with (* TODO: this shouldn't actually happen: something being in sinkHM and not in avSolHM? *)
          | false -> merge_alarms_with_same_cond c msg
          | true ->
            let msg = HM.find antSolHM n in (* until this bug is fixed, there is a fix to ask antSolHM instead *)
            match D.is_empty msg with
            | false -> merge_alarms_with_same_cond c msg
            | true -> ()
        ) condset
    ) sinkHM;

  HA.iter (fun _ alarms -> merge_race_warnings alarms) accGroupsHM;
