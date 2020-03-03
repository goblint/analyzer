open WitnessUtil
open MyCFG

module type Node =
sig
  include Hashtbl.HashedType

  val cfgnode: t -> MyCFG.node
  val to_string: t -> string

  val move_opt: t -> MyCFG.node -> t option
end

(* Abstract Reachability Graph *)
module type S =
sig
  module Node: Node

  val main_entry: Node.t
  val next: Node.t -> (MyCFG.edge * Node.t) list
end

module StackNode (Node: Node):
  Node with type t = Node.t list =
struct
  include HashedList (Node)

  let cfgnode nl = Node.cfgnode (List.hd nl)
  let to_string nl =
    nl
    |> List.map Node.to_string
    |> String.concat "@"

  let move_opt nl to_node = match nl with
    | [] -> None
    | n :: stack ->
      Node.move_opt n to_node
      |> BatOption.map (fun to_n -> to_n :: stack)
end

module Stack (Cfg:CfgForward) (Arg: S):
  S with module Node = StackNode (Arg.Node) =
struct
  module Node = StackNode (Arg.Node)

  let main_entry = [Arg.main_entry]

  let next = function
    | [] -> failwith "StackArg.next: empty"
    | n :: stack ->
      let cfgnode = Arg.Node.cfgnode n in
      match cfgnode with
      | Function _ -> (* TODO: can this be done without Cfg? *)
        begin match stack with
          (* | [] -> failwith "StackArg.next: return stack empty" *)
          | [] -> [] (* main return *)
          | call_n :: call_stack ->
            let call_cfgnode = Arg.Node.cfgnode call_n in
            let call_next =
              Cfg.next call_cfgnode
              (* filter because infinite loops starting with function call
                 will have another Neg(1) edge from the head *)
              |> List.filter (fun (locedges, to_node) ->
                  List.exists (function
                      | (_, Proc _) -> true
                      | (_, _) -> false
                    ) locedges
                )
            in
            begin match call_next with
              | [] -> failwith "StackArg.next: call next empty"
              | [(_, return_node)] ->
                begin match Arg.Node.move_opt call_n return_node with
                  (* TODO: Is it possible to have a calling node without a returning node? *)
                  (* | None -> [] *)
                  | None -> failwith "StackArg.next: no return node"
                  | Some return_n ->
                    (* TODO: Instead of next & filter, construct unique return_n directly. Currently edge missing. *)
                    Arg.next n
                    |> List.filter (fun (edge, to_n) ->
                        (* let to_cfgnode = Arg.Node.cfgnode to_n in
                        MyCFG.Node.equal to_cfgnode return_node *)
                        Arg.Node.equal to_n return_n
                      )
                    |> List.map (fun (edge, to_n) ->
                        let to_n' = to_n :: call_stack in
                        (edge, to_n')
                      )
                end
              | _ :: _ :: _ -> failwith "StackArg.next: call next ambiguous"
            end
        end
      | _ ->
        Arg.next n
        |> List.map (fun (edge, to_n) ->
            let to_cfgnode = Arg.Node.cfgnode to_n in
            let to_n' = match to_cfgnode with
              | FunctionEntry _ -> to_n :: n :: stack
              | _ -> to_n :: stack
            in
            (edge, to_n')
          )
end

module type IsInteresting =
sig
  type node
  val is_interesting: node -> MyCFG.edge -> node -> bool
end

module InterestingArg (Arg: S) (IsInteresting: IsInteresting with type node := Arg.Node.t):
  S with module Node = Arg.Node =
struct
  include Arg

  (* too aggressive, duplicates some interesting edges *)
  (* let rec next node =
       Arg.next node
       |> List.map (fun (edge, to_node) ->
           if IsInteresting.is_interesting node edge to_node then
             [(edge, to_node)]
           else
             next to_node
         )
       |> List.flatten *)

  let rec next node =
    Arg.next node
    |> List.map (fun (edge, to_node) ->
        if IsInteresting.is_interesting node edge to_node then
          [(edge, to_node)]
        else begin
          let to_node_next = next to_node in
          if List.exists (fun (edge, to_node) ->
              IsInteresting.is_interesting node edge to_node
            ) to_node_next then
            [(edge, to_node)] (* don't shortcut if node has outdoing interesting edges, e.g. control *)
          else
            to_node_next
        end
      )
    |> List.flatten
end


module type SIntra =
sig
  val next: MyCFG.node -> (MyCFG.edge * MyCFG.node) list
end

module type SIntraOpt =
sig
  include SIntra
  val next_opt: MyCFG.node -> ((MyCFG.edge * MyCFG.node) list) option
end

module CfgIntra (Cfg:CfgForward): SIntraOpt =
struct
  let next node =
    Cfg.next node
    |> List.map (fun (es, to_n) ->
        List.map (fun (_, e) -> (e, to_n)) es
      )
    |> List.flatten
  let next_opt _ = None
end

let partition_if_next if_next_n =
  (* TODO: refactor, check extra edges for error *)
  let test_next b = List.find (function
      | (Test (_, b'), _) when b = b' -> true
      | (_, _) -> false
    ) if_next_n
  in
  (* assert (List.length if_next <= 2); *)
  match test_next true, test_next false with
  | (Test (e_true, true), if_true_next_n), (Test (e_false, false), if_false_next_n) when Expcompare.compareExp e_true e_false ->
    (e_true, if_true_next_n, if_false_next_n)
  | _, _ -> failwith "partition_if_next: bad branches"

module UnCilLogicIntra (Arg: SIntraOpt): SIntraOpt =
struct
  open Cil

  let is_equiv_stmtkind sk1 sk2 = match sk1, sk2 with
    | Instr is1, Instr is2 -> List.for_all2 (=) is1 is2
    | Return _, Return _ -> sk1 = sk2
    | _, _ -> false (* TODO: also consider others? not sure if they ever get duplicated *)
  let is_equiv_stmt s1 s2 = is_equiv_stmtkind s1.skind s2.skind (* TODO: also consider labels *)
  let is_equiv_node n1 n2 = match n1, n2 with
    | Statement s1, Statement s2 -> is_equiv_stmt s1 s2
    | _, _ -> false (* TODO: also consider FunctionEntry & Function? *)
  let is_equiv_edge e1 e2 = match e1, e2 with
    | Entry f1, Entry f2 -> f1 == f2 (* physical equality for fundec to avoid cycle *)
    | Ret (exp1, f1), Ret (exp2, f2) -> exp1 = exp2 && f1 == f2 (* physical equality for fundec to avoid cycle *)
    | _, _ -> e1 = e2
  let rec is_equiv_chain n1 n2 =
    MyCFG.Node.equal n1 n2 || (is_equiv_node n1 n2 && is_equiv_chain_next n1 n2)
  and is_equiv_chain_next n1 n2 = match Arg.next n1, Arg.next n2 with
    | [(e1, to_n1)], [(e2, to_n2)] ->
      is_equiv_edge e1 e2 && is_equiv_chain to_n1 to_n2
    | _, _-> false


  let rec next_opt' n = match n with
    | Statement {sid; skind=If (_, _, _, loc)} when GobConfig.get_bool "exp.uncilwitness" ->
      let (e, if_true_next_n,  if_false_next_n) = partition_if_next (Arg.next n) in
      (* avoid infinite recursion with sid <> sid2 in if_nondet_var *)
      (* TODO: why physical comparison if_false_next_n != n doesn't work? *)
      (* TODO: need to handle longer loops? *)
      begin match if_true_next_n, if_false_next_n with
        (* && *)
        | Statement {sid=sid2; skind=If (_, _, _, loc2)}, _ when sid <> sid2 && loc = loc2 ->
          (* get e2 from edge because recursive next returns it there *)
          let (e2, if_true_next_true_next_n, if_true_next_false_next_n) = partition_if_next (next if_true_next_n) in
          if is_equiv_chain if_false_next_n if_true_next_false_next_n then
            let exp = BinOp (LAnd, e, e2, intType) in
            Some [
              (Test (exp, true), if_true_next_true_next_n);
              (Test (exp, false), if_false_next_n)
            ]
          else
            None
        (* || *)
        | _, Statement {sid=sid2; skind=If (_, _, _, loc2)} when sid <> sid2 && loc = loc2 ->
          (* get e2 from edge because recursive next returns it there *)
          let (e2, if_false_next_true_next_n, if_false_next_false_next_n) = partition_if_next (next if_false_next_n) in
          if is_equiv_chain if_true_next_n if_false_next_true_next_n then
            let exp = BinOp (LOr, e, e2, intType) in
            Some [
              (Test (exp, true), if_true_next_n);
              (Test (exp, false), if_false_next_false_next_n)
            ]
          else
            None
        | _, _ -> None
      end
    | _ -> None
  and next_opt n = match next_opt' n with
    | Some _ as next_opt -> next_opt
    | None -> Arg.next_opt n
  and next n = match next_opt' n with
    | Some next -> next
    | None -> Arg.next n
end

module UnCilTernaryIntra (Arg: SIntraOpt): SIntraOpt =
struct
  open Cil

  let ternary e_cond e_true e_false =
    if e_true = Cil.one && e_false = Cil.zero then
      (* avoid unnecessary ternary *)
      e_cond
    else
      Question(e_cond, e_true, e_false, typeOf e_false)

  let rec next_opt' n = match n with
    | Statement {skind=If (_, _, _, loc)} when GobConfig.get_bool "exp.uncilwitness" ->
      let (e_cond, if_true_next_n, if_false_next_n) = partition_if_next (Arg.next n) in
      if MyCFG.getLoc if_true_next_n = loc && MyCFG.getLoc if_false_next_n = loc then
        match Arg.next if_true_next_n, Arg.next if_false_next_n with
        | [(Assign (v_true, e_true), if_true_next_next_n)], [(Assign (v_false, e_false), if_false_next_next_n)] when v_true = v_false && MyCFG.Node.equal if_true_next_next_n if_false_next_next_n ->
          let exp = ternary e_cond e_true e_false in
          Some [
            (Assign (v_true, exp), if_true_next_next_n)
          ]
        | _, _ -> None
      else
        None
    | _ -> None
  let next_opt n = match next_opt' n with
    | Some _ as next_opt -> next_opt
    | None -> Arg.next_opt n
  let next n = match next_opt n with
    | Some next -> next
    | None -> Arg.next n
end

module Intra (ArgIntra: SIntraOpt) (Arg: S):
  S with module Node = Arg.Node =
struct
  include Arg
  open GobConfig

  let next node =
    match ArgIntra.next_opt (Node.cfgnode node) with
    | None -> Arg.next node
    | Some next ->
      next
      |> BatList.filter_map (fun (e, to_n) ->
          Node.move_opt node to_n
          |> BatOption.map (fun to_node -> (e, to_node))
        )
end
