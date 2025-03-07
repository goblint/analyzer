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

  val move_opt: t -> MyCFG.node -> t option
  val equal_node_context: t -> t -> bool
end

module type Edge =
sig
  type t

  val embed: MyCFG.edge -> t
  val to_string: t -> string
end

module CFGEdge: Edge with type t = MyCFG.edge =
struct
  type t = edge

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
  type t = inline_edge

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

  let move_opt nl to_node =
    let open GobOption.Syntax in
    match nl with
    | [] -> None
    | n :: stack ->
      let+ to_n = Node.move_opt n to_node in
      to_n :: stack
  let equal_node_context _ _ = failwith "StackNode: equal_node_context"
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
      | Function _ -> (* TODO: can this be done without cfgnode? *)
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
            Arg.next n
            |> List.filter_map (fun (edge, to_n) ->
                if BatList.mem_cmp Arg.Node.compare to_n call_next then (
                  let to_n' = to_n :: call_stack in
                  Some (edge, to_n')
                )
                else
                  None
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
    let open GobList.Syntax in
    let* (es, to_n) = Cfg.next node in
    let+ (_, e) = es in
    (e, to_n)
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
  | (Test (e_true, true), if_true_next_n), (Test (e_false, false), if_false_next_n) when Basetype.CilExp.equal e_true e_false ->
    (e_true, if_true_next_n, if_false_next_n)
  | _, _ -> failwith "partition_if_next: bad branches"

module UnCilLogicIntra (Arg: SIntraOpt): SIntraOpt =
struct
  open Cil
  (* TODO: questionable (=) and (==) use here *)

  let is_equiv_stmtkind sk1 sk2 = match sk1, sk2 with
    | Instr is1, Instr is2 -> GobList.equal (=) is1 is2
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
    Node.equal n1 n2 || (is_equiv_node n1 n2 && is_equiv_chain_next n1 n2)
  and is_equiv_chain_next n1 n2 = match Arg.next n1, Arg.next n2 with
    | [(e1, to_n1)], [(e2, to_n2)] ->
      is_equiv_edge e1 e2 && is_equiv_chain to_n1 to_n2
    | _, _-> false


  let rec next_opt' n = match n with
    | Statement {sid; skind=If _; _} when GobConfig.get_bool "witness.graphml.uncil" ->
      let (e, if_true_next_n,  if_false_next_n) = partition_if_next (Arg.next n) in
      (* avoid infinite recursion with sid <> sid2 in if_nondet_var *)
      (* TODO: why physical comparison if_false_next_n != n doesn't work? *)
      (* TODO: need to handle longer loops? *)
      let loc = Node.location n in
      begin match if_true_next_n, if_false_next_n with
        (* && *)
        | Statement {sid=sid2; skind=If _; _}, _ when sid <> sid2 && CilType.Location.equal loc (Node.location if_true_next_n) ->
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
        | _, Statement {sid=sid2; skind=If _; _} when sid <> sid2 && CilType.Location.equal loc (Node.location if_false_next_n) ->
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
      Question(e_cond, e_true, e_false, Cilfacade.typeOf e_false)

  let next_opt' n = match n with
    | Statement {skind=If _; _} when GobConfig.get_bool "witness.graphml.uncil" ->
      let (e_cond, if_true_next_n, if_false_next_n) = partition_if_next (Arg.next n) in
      let loc = Node.location n in
      if CilType.Location.equal (Node.location if_true_next_n) loc && CilType.Location.equal (Node.location if_false_next_n) loc then
        match Arg.next if_true_next_n, Arg.next if_false_next_n with
        | [(Assign (v_true, e_true), if_true_next_next_n)], [(Assign (v_false, e_false), if_false_next_next_n)] when v_true = v_false && Node.equal if_true_next_next_n if_false_next_next_n ->
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
  S with module Node = Arg.Node and module Edge = Arg.Edge =
struct
  include Arg

  let next node =
    let open GobOption.Syntax in
    match ArgIntra.next_opt (Node.cfgnode node) with
    | None -> Arg.next node
    | Some next ->
      next
      |> BatList.filter_map (fun (e, to_n) ->
          let+ to_node = Node.move_opt node to_n in
          (Edge.embed e, to_node)
        )
end
