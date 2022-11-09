open Prelude.Ana
open Analyses
open Node0

(* 
  An implementation of the alarm repositioning analysis 
  from the article "Repositioning of Static Analysis Alarms" 
  by Tukaram Muske, Rohith Talluri andAlexander Serebrenik
*)

module Idx = PreValueDomain.IndexDomain

module D = SetDomain.ToppedSet(Printable.Prod (Printable.Prod (ExpDomain) (Idx)) (Printable.Strings)) (struct let topname = "top" end)

module NH = Hashtbl.Make (Node)

let alarmsNH = NH.create 100

module CondSet = SetDomain.ToppedSet(Printable.Prod (ExpDomain) (Idx)) (struct let topname = "top" end)

let ask : (Node0.t -> Queries.ask) ref = ref (fun a -> assert false)

(* Anticipable Alarm Conditions Analysis *)
module Ant = 
struct

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

  module Var = Constraints.Var2 (VarNodeL) (VarNodeG)

  type v = Var.t
 
  module Dom = D

  type d = Dom.t

  let box _ _ _ = failwith "TODO"

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
        let node_contains_exp_def (exp : ExpDomain.t) = 
          match exp with
          | `Lifted exp -> List.fold (fun acc var -> Basetype.CilExp.occurs var exp || acc) false assigned_vars
          | _ -> false in
          Dom.filter (fun ((exp, _), alarm) -> node_contains_exp_def exp) x
      | _ -> Dom.empty ()
      end
    | _ -> Dom.empty ()

  let conds_in (s : d) = Dom.fold (fun (x,y) conds -> CondSet.add x conds) s (CondSet.empty ())

  let dep_gen node x = Dom.empty ()

  let process (alarm : ((ExpDomain.t * Idx.t) * string)) y = alarm

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
          (* ignore @@ Pretty.printf "meet: %a" Dom.pretty c; c *)
      in
      Some f

  let increment = Analyses.empty_increment_data ()

  let iter_vars _ _ _ = ()
end

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "antConds"
  
  module D = SetDomain.Make(Printable.Prod (CilType.Exp) (Idx))
  module C = D

  let startstate v = D.bot ()
  let exitstate  v = D.bot ()

  let event ctx (event: Events.t) octx =
    match event with
    | ArrayIndex {exp; value} -> D.add (exp, value) ctx.local
    | Events.Access {exp=e; lvals; kind; reach} -> failwith "TODO"
    | _ -> ctx.local

  (* let tuplesOf (x,y) =  *)

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
    let module HM = Hashtbl.Make (Ant.Var) in
    let module Solver = Td3.WP (IncrSolverArg) (Ant) (HM) in
    let (solution, _) = Solver.solve Ant.box [] [start_node] in

    HM.iter (fun k v -> ignore (Pretty.printf "%a->%a\n" Ant.Var.pretty_trace k Ant.Dom.pretty v)) solution;

    let filter_always_true node cs = CondSet.filter (fun (exp, l) -> 
      match exp with
      | `Lifted exp ->
        let q = (!ask node).f (EvalInt exp) in
        let v = Idx.of_interval (Cilfacade.ptrdiff_ikind ()) (Option.get @@ Queries.ID.minimal q, Option.get @@ Queries.ID.maximal q) in
        (* ignore (Pretty.printf "eval %a%a" Node.pretty node Idx.pretty v); *)
        let idx_before_end = Idx.to_bool (Idx.lt v l) (* check whether index is before the end of the array *)
        and idx_after_start = Idx.to_bool (Idx.ge v (Idx.of_int Cil.ILong Z.zero)) in (* check whether the index is non-negative *)
        begin match (idx_before_end, idx_after_start) with
        | Some true, Some true -> false (* Certainly in bounds on both sides.*)
        | _ -> true
        end
      | _ -> failwith "TODO"
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

    let hoist = HM.create 10 in
    HM.iter (fun k v -> 
      match k with
      | `L Function _ -> ()
      | `G Function _ -> ()
      | `L node -> HM.replace hoist k (hoist_entry node)
      | `G node -> HM.replace hoist k @@ hoist_exit node
        ) solution;
      
    print_endline "hoist";
    
    HM.iter (fun k v -> ignore (Pretty.printf "%a->%a\n" Ant.Var.pretty_trace k CondSet.pretty v)) hoist

end



let _ = MCP.register_analysis (module Spec : MCPSpec)
