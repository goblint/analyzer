(** Abstract reachability graph. *)

open MyCFG
open GoblintCil

module type Node =
sig
  include Hashtbl.HashedType
  include Set.OrderedType with type t := t

  val cfgnode: t -> MyCFG.node
  val context_id: t -> int
  val path_id: t -> int
  val to_string: t -> string
end

module type Edge =
sig
  type t [@@deriving eq, ord]

  val embed: MyCFG.edge -> t
  val to_string: t -> string
end

module CFGEdge: Edge with type t = MyCFG.edge =
struct
  type t = Edge.t [@@deriving eq, ord]

  let embed e = e
  let to_string e = GobPretty.sprint Edge.pretty_plain e
end

type inline_edge =
  | CFGEdge of Edge.t
  | InlineEntry of CilType.Lval.t option * CilType.Fundec.t * CilType.Exp.t list
  | InlineReturn of CilType.Lval.t option * CilType.Fundec.t * CilType.Exp.t list
  | InlinedEdge of Edge.t
  | ThreadEntry of CilType.Lval.t option * CilType.Varinfo.t * CilType.Exp.t list
[@@deriving eq, ord, hash]

let pretty_inline_edge () = function
  | CFGEdge e -> Edge.pretty_plain () e
  | InlineEntry (_, _, args) -> Pretty.dprintf "InlineEntry '(%a)'" (Pretty.d_list ", " Cil.d_exp) args
  | InlineReturn (None, _, _) -> Pretty.dprintf "InlineReturn"
  | InlineReturn (Some ret, _, _) -> Pretty.dprintf "InlineReturn '%a'" Cil.d_lval ret
  | InlinedEdge e -> Pretty.dprintf "Inlined %a" Edge.pretty_plain e
  | ThreadEntry (_, _, args) -> Pretty.dprintf "ThreadEntry '(%a)'" (Pretty.d_list ", " Cil.d_exp) args

let inline_edge_to_yojson = function
  | CFGEdge e ->
    `Assoc [
      ("cfg", Edge.to_yojson e)
    ]
  | InlineEntry (lval, function_, args) ->
    `Assoc [
      ("entry", `Assoc [
          ("lval", [%to_yojson: CilType.Lval.t option] lval);
          ("function", CilType.Fundec.to_yojson function_);
          ("args", [%to_yojson: CilType.Exp.t list] args);
        ]);
    ]
  | InlineReturn (lval, function_, args) ->
    `Assoc [
      ("return", `Assoc [
          ("lval", [%to_yojson: CilType.Lval.t option] lval);
          ("function", CilType.Fundec.to_yojson function_);
          ("args", [%to_yojson: CilType.Exp.t list] args);
        ]);
    ]
  | InlinedEdge e ->
    `Assoc [
      ("inlined", Edge.to_yojson e)
    ]
  | ThreadEntry (lval, function_, args) ->
    `Assoc [
      ("thread", `Assoc [
          ("lval", [%to_yojson: CilType.Lval.t option] lval);
          ("function", CilType.Varinfo.to_yojson function_);
          ("args", [%to_yojson: CilType.Exp.t list] args);
        ]);
    ]

module InlineEdgePrintable: Printable.S with type t = inline_edge =
struct
  include Printable.StdLeaf
  type t = inline_edge [@@deriving eq, ord, hash, to_yojson]

  let name () = "inline edge"

  let pretty = pretty_inline_edge
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
    (* TODO: deriving to_yojson gets overridden by SimplePretty *)
end

module InlineEdge: Edge with type t = inline_edge =
struct
  type t = inline_edge [@@deriving eq, ord]

  let embed e = CFGEdge e
  let to_string e = InlineEdgePrintable.show e
end

(* Abstract Reachability Graph *)
module type S =
sig
  module Node: Node
  module Edge: Edge

  val main_entry: Node.t
  val next: Node.t -> (Edge.t * Node.t) list
end

module StackNode (Node: Node):
  Node with type t = Node.t list =
struct
  type t = Node.t list [@@deriving eq, ord, hash]

  let cfgnode nl = Node.cfgnode (List.hd nl)
  let context_id nl = Node.context_id (List.hd nl)
  let path_id nl = Node.path_id (List.hd nl)
  let to_string nl =
    nl
    |> List.map Node.to_string
    |> String.concat "@"
end

module Stack (Arg: S with module Edge = InlineEdge):
  S with module Node = StackNode (Arg.Node) and module Edge = Arg.Edge =
struct
  module Node = StackNode (Arg.Node)
  module Edge = Arg.Edge

  let main_entry = [Arg.main_entry]

  let next =
    let open GobList.Syntax in
    function
    | [] -> failwith "StackArg.next: empty"
    | n :: stack ->
      let cfgnode = Arg.Node.cfgnode n in
      match cfgnode with
      | Function cfgnode_fd -> (* TODO: can this be done without cfgnode? *)
        begin match stack with
          (* | [] -> failwith "StackArg.next: return stack empty" *)
          | [] -> [] (* main return *)
          | call_n :: call_stack ->
            let call_next =
              Arg.next call_n
              (* filter because infinite loops starting with function call
                 will have another Neg(1) edge from the head *)
              |> List.filter_map (fun (edge, to_n) ->
                  match edge with
                  | InlinedEdge _ -> Some to_n
                  | _ -> None
                )
            in
            let (entry_lval, entry_args) =
              Arg.next call_n
              (* filter because infinite loops starting with function call
                 will have another Neg(1) edge from the head *)
              |> List.filter_map (fun (edge, to_n) ->
                  match edge with
                  | InlineEntry (lval, _, args) -> Some (lval, args)
                  | _ -> None
                )
              |> List.sort_uniq [%ord: CilType.Lval.t option * CilType.Exp.t list] (* TODO: deduplicate unique element in O(n) *)
              |> (function
                  | [lval_args] -> lval_args
                  | _ -> assert false (* all calls from a node must have same args and lval, even if called function might be different via function pointer *)
                )
            in
            Arg.next n
            |> List.filter_map (fun (edge, to_n) ->
                match edge with
                | InlineReturn (lval, fd, args) ->
                  assert (CilType.Fundec.equal fd cfgnode_fd); (* fd in return node should be the same as in InlineReturn edge *)
                  if BatList.mem_cmp Arg.Node.compare to_n call_next && [%eq: CilType.Lval.t option] lval entry_lval && [%eq: CilType.Exp.t list] args entry_args then (
                    let to_n' = to_n :: call_stack in
                    Some (edge, to_n')
                  )
                  else
                    None
                | _ -> assert false
              )
        end
      | _ ->
        let+ (edge, to_n) = Arg.next n in
        let to_cfgnode = Arg.Node.cfgnode to_n in
        let to_n' = match to_cfgnode with
          | FunctionEntry _ -> to_n :: n :: stack
          | _ -> to_n :: stack
        in
        (edge, to_n')

  (* Avoid infinite stack nodes for recursive programs
     by dropping down to repeated stack node. *)
  let drop_prefix n stack =
    let rec drop = function
      | [] -> n :: stack
      | (x :: _) as stack when Arg.Node.equal x n -> stack
      | _ :: xs -> drop xs
    in
    drop stack
  let dedup = function
    | [] -> failwith "StackArg.next: dedup empty"
    | n :: stack -> drop_prefix n stack
  let next node =
    next node
    |> List.map (BatTuple.Tuple2.map2 dedup)
end

module type IsInteresting =
sig
  type node
  type edge
  val is_interesting: node -> edge -> node -> bool
end

(* Unused *)
module InterestingArg (Arg: S) (IsInteresting: IsInteresting with type node := Arg.Node.t and type edge := Arg.Edge.t):
  S with module Node = Arg.Node and module Edge = Arg.Edge =
struct
  include Arg

  (* too aggressive, duplicates some interesting edges *)
  (* let rec next node =
       Arg.next node
       |> List.concat_map (fun (edge, to_node) ->
           if IsInteresting.is_interesting node edge to_node then
             [(edge, to_node)]
           else
             next to_node
         ) *)

  let rec next node =
    let open GobList.Syntax in
    let* (edge, to_node) = Arg.next node in
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
end

type cfg_path = (MyCFG.edge * MyCFG.node) list

module type SIntra =
sig
  val next: MyCFG.node -> (MyCFG.edge * MyCFG.node * cfg_path list) list
  (** @return Also the original CFG paths corresponding to the step. *)
end

module type SIntraOpt =
sig
  include SIntra
  val next_opt: MyCFG.node -> ((MyCFG.edge * MyCFG.node * cfg_path list) list) option
  (** @return Also the original CFG paths corresponding to the step. *)
end

module CfgIntra (Cfg:CfgForward): SIntraOpt =
struct
  let next node =
    let open GobList.Syntax in
    let* (es, to_n) = Cfg.next node in
    let+ (_, e) = es in
    (e, to_n, [[(e, to_n)]])
  let next_opt _ = None
end

let cartesian_concat_paths (ps : cfg_path list) (qs : cfg_path list) : cfg_path list = List.concat_map (fun p -> List.map (fun q -> p @ q) qs) ps

let partition_if_next if_next =
  let (if_next_trues, if_next_falses) = List.partition (function
      | (Test (_, b), _, _) -> b
      | (_, _, _) -> failwith "partition_if_next: not Test edge"
    ) if_next
  in
  match if_next_trues, if_next_falses with
  | [(Test (e_true, true), if_true_next_n, if_true_next_ps)], [(Test (e_false, false), if_false_next_n, if_false_next_ps)] when Basetype.CilExp.equal e_true e_false ->
    (e_true, (if_true_next_n, if_true_next_ps), (if_false_next_n, if_false_next_ps))
  | _, _ ->
    (* This fails due to any of the following:
       - Either true or false branch is missing.
       - Either true or false branch has multiple different exps or nodes (same exp, branch and node should only occur once by construction/assumption).
       - True and false branch have different exps. *)
    failwith "partition_if_next: bad branches"

module UnCilLogicIntra (Arg: SIntraOpt): SIntraOpt =
struct
  open Cil

  let () =
    assert (not !Cabs2cil.allowDuplication) (* duplication makes it more annoying to detect cilling *)

  let rec next_opt' n = match n with
    | Statement {sid; skind=If _; _} when GobConfig.get_bool "exp.arg.uncil" ->
      let (e, (if_true_next_n, if_true_next_ps), (if_false_next_n, if_false_next_ps)) = partition_if_next (Arg.next n) in
      (* avoid infinite recursion with sid <> sid2 in if_nondet_var *)
      (* TODO: why physical comparison if_false_next_n != n doesn't work? *)
      (* TODO: need to handle longer loops? *)
      let loc = Node.location n in
      begin match if_true_next_n, if_false_next_n with
        (* && *)
        | Statement {sid=sid2; skind=If _; _}, _ when sid <> sid2 && CilType.Location.equal loc (Node.location if_true_next_n) ->
          (* get e2 from edge because recursive next returns it there *)
          let (e2, (if_true_next_true_next_n, if_true_next_true_next_ps), (if_true_next_false_next_n, if_true_next_false_next_ps)) = partition_if_next (next if_true_next_n) in
          if Node.equal if_false_next_n if_true_next_false_next_n then
            let exp = BinOp (LAnd, e, e2, intType) in
            Some [
              (Test (exp, true), if_true_next_true_next_n, cartesian_concat_paths if_true_next_ps if_true_next_true_next_ps);
              (Test (exp, false), if_true_next_false_next_n, if_false_next_ps @ cartesian_concat_paths if_true_next_ps if_true_next_false_next_ps) (* concat two different path families to same false node *)
            ]
          else
            None
        (* || *)
        | _, Statement {sid=sid2; skind=If _; _} when sid <> sid2 && CilType.Location.equal loc (Node.location if_false_next_n) ->
          (* get e2 from edge because recursive next returns it there *)
          let (e2, (if_false_next_true_next_n, if_false_next_true_next_ps), (if_false_next_false_next_n, if_false_next_false_next_ps)) = partition_if_next (next if_false_next_n) in
          if Node.equal if_true_next_n if_false_next_true_next_n then
            let exp = BinOp (LOr, e, e2, intType) in
            Some [
              (Test (exp, true), if_false_next_true_next_n, if_true_next_ps @ cartesian_concat_paths if_false_next_ps if_false_next_true_next_ps); (* concat two different path families to same true node *)
              (Test (exp, false), if_false_next_false_next_n, cartesian_concat_paths if_false_next_ps if_false_next_false_next_ps)
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
      Question(e_cond, e_true, e_false, Cilfacade.typeOf e_false)

  let next_opt' n = match n with
    | Statement {skind=If _; _} when GobConfig.get_bool "exp.arg.uncil" ->
      let (e_cond, (if_true_next_n, if_true_next_ps), (if_false_next_n, if_false_next_ps)) = partition_if_next (Arg.next n) in
      let loc = Node.location n in
      if CilType.Location.equal (Node.location if_true_next_n) loc && CilType.Location.equal (Node.location if_false_next_n) loc then
        match Arg.next if_true_next_n, Arg.next if_false_next_n with
        | [(Assign (v_true, e_true), if_true_next_next_n, if_true_next_next_ps)], [(Assign (v_false, e_false), if_false_next_next_n, if_false_next_next_ps)] when v_true = v_false && Node.equal if_true_next_next_n if_false_next_next_n ->
          let exp = ternary e_cond e_true e_false in
          Some [
            (Assign (v_true, exp), if_true_next_next_n, cartesian_concat_paths if_true_next_ps if_true_next_next_ps @ cartesian_concat_paths if_false_next_ps if_false_next_next_ps) (* concat two different path families with same variable to same node *)
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
  S with module Node = Arg.Node and module Edge = Arg.Edge =
struct
  include Arg

  (** Starting from ARG node [node], follow CFG path [p] to the resulting ARG node.
      Returns multiple ARG nodes if ARG contains path-sensitivity splits on the same CFG path. *)
  let rec follow node p =
    let open GobList.Syntax in
    match p with
    | [] -> [node]
    | (e, to_n) :: p' ->
      let* (_, node') = List.filter (fun (e', to_node) ->
          Edge.equal (Edge.embed e) e' && Node0.equal to_n (Node.cfgnode to_node)
        ) (Arg.next node)
      in
      follow node' p'

  let next node =
    let open GobList.Syntax in
    match ArgIntra.next_opt (Node.cfgnode node) with
    | None -> Arg.next node
    | Some next ->
      next
      |> BatList.concat_map (fun (e, to_n, p) ->
          let* p in
          let+ to_node = follow node p in
          assert (Node0.equal to_n (Node.cfgnode to_node)); (* should always hold by follow filter above *)
          (Edge.embed e, to_node)
        )
      |> BatList.unique_cmp ~cmp:[%ord: Edge.t * Node.t] (* after following paths, there may be duplicates because same ARG node can be reached via same ARG edge via multiple uncilled CFG paths *) (* TODO: avoid generating duplicates in the first place? *)
end
