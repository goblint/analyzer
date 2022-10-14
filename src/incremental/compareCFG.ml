open MyCFG
open Queue
open GoblintCil
open CilMaps
include CompareAST

let eq_node (x, fun1) (y, fun2) =
  let empty_rename_mapping: rename_mapping = (StringMap.empty, VarinfoMap.empty) in
  match x,y with
  | Statement s1, Statement s2 -> eq_stmt ~cfg_comp:true (s1, fun1) (s2, fun2) empty_rename_mapping
  | Function f1, Function f2 -> eq_varinfo f1.svar f2.svar empty_rename_mapping
  | FunctionEntry f1, FunctionEntry f2 -> eq_varinfo f1.svar f2.svar empty_rename_mapping
  | _ -> false

(* TODO: compare ASMs properly instead of simply always assuming that they are not the same *)
let eq_edge x y =
  let empty_rename_mapping: rename_mapping = (StringMap.empty, VarinfoMap.empty) in
  match x, y with
  | Assign (lv1, rv1), Assign (lv2, rv2) -> eq_lval lv1 lv2 empty_rename_mapping && eq_exp rv1 rv2 empty_rename_mapping
  | Proc (None,f1,ars1), Proc (None,f2,ars2) -> eq_exp f1 f2 empty_rename_mapping && GobList.equal (eq_exp2 empty_rename_mapping) ars1 ars2
  | Proc (Some r1,f1,ars1), Proc (Some r2,f2,ars2) ->
    eq_lval r1 r2 empty_rename_mapping && eq_exp f1 f2 empty_rename_mapping && GobList.equal (eq_exp2 empty_rename_mapping) ars1 ars2
  | Entry f1, Entry f2 -> eq_varinfo f1.svar f2.svar empty_rename_mapping
  | Ret (None,fd1), Ret (None,fd2) -> eq_varinfo fd1.svar fd2.svar empty_rename_mapping
  | Ret (Some r1,fd1), Ret (Some r2,fd2) -> eq_exp r1 r2 empty_rename_mapping && eq_varinfo fd1.svar fd2.svar empty_rename_mapping
  | Test (p1,b1), Test (p2,b2) -> eq_exp p1 p2 empty_rename_mapping && b1 = b2
  | ASM _, ASM _ -> false
  | Skip, Skip -> true
  | VDecl v1, VDecl v2 -> eq_varinfo v1 v2 empty_rename_mapping
  | _ -> false

(* The order of the edges in the list is relevant. Therefore compare them one to one without sorting first *)
let eq_edge_list xs ys = GobList.equal eq_edge xs ys

let to_edge_list : (location * edge) list -> edge list = List.map (fun (loc, edge) -> edge)

module NH = Hashtbl.Make(Node)
type biDirectionNodeMap = {node1to2: node NH.t; node2to1: node NH.t}

(* This function compares two CFGs by doing a breadth-first search on the old CFG. Matching node tuples are stored in same,
 * nodes from the old CFG for which no matching node can be found are added to diff. For each matching node tuple
 * (fromNode1, fromNode2) found, one iterates over the successors of fromNode1 from the old CFG and checks for a matching node
 * in the succesors of fromNode2 in the new CFG. Matching node tuples are added to the waitingList to repeat the matching
 * process on their successors. If a node from the old CFG can not be matched, it is added to diff and no further
 * comparison is done for its successors. The two function entry nodes make up the tuple to start the comparison from. *)
let compareCfgs (module CfgOld : CfgForward) (module CfgNew : CfgForward) fun1 fun2 =
  let diff = NH.create 113 in
  let same = {node1to2=NH.create 113; node2to1=NH.create 113} in
  let waitingList : (node * node) t = Queue.create () in

  let rec compareNext () =
    if Queue.is_empty waitingList then ()
    else
      let fromNode1, fromNode2 = Queue.take waitingList in
      let outList1 = CfgOld.next fromNode1 in
      let outList2 = CfgNew.next fromNode2 in

      (* Find a matching edge and successor node for (edgeList1, toNode1) in the list of successors of fromNode2.
       * If successful, add the matching node tuple to same, else add toNode1 to the differing nodes. *)
      let findMatch (edgeList1, toNode1) =
        let rec aux remSuc = match remSuc with
          | [] -> NH.replace diff toNode1 ()
          | (locEdgeList2, toNode2)::remSuc' ->
            let edgeList2 = to_edge_list locEdgeList2 in
            (* TODO: don't allow pseudo return node to be equal to normal return node, could make function unchanged, but have different sallstmts *)
            if eq_node (toNode1, fun1) (toNode2, fun2) && eq_edge_list edgeList1 edgeList2 then
              begin
                match NH.find_opt same.node1to2 toNode1 with
                | Some n2 -> if not (Node.equal n2 toNode2) then NH.replace diff toNode1 ()
                | None -> NH.replace same.node1to2 toNode1 toNode2; NH.replace same.node2to1 toNode2 toNode1; Queue.add (toNode1, toNode2) waitingList
              end
            else aux remSuc' in
        aux outList2 in
      (* For a toNode1 from the list of successors of fromNode1, check whether it might have duplicate matches.
       * In that case declare toNode1 as differing node. Else, try finding a match in the list of successors
       * of fromNode2 in the new CFG using findMatch. *)
      let iterOuts (locEdgeList1, toNode1) =
        let edgeList1 = to_edge_list locEdgeList1 in
        (* Differentiate between a possibly duplicate Test(1,false) edge and a single occurence. In the first
         * case the edge is directly added to the diff set to avoid undetected ambiguities during the recursive
         * call. *)
        let testFalseEdge edge = match edge with
          | Test (p,b) -> p = Cil.one && b = false
          | _ -> false in
        let posAmbigEdge edgeList = let findTestFalseEdge (ll,_) = testFalseEdge (snd (List.hd ll)) in
          let numDuplicates l = List.length (List.find_all findTestFalseEdge l) in
          testFalseEdge (List.hd edgeList) && (numDuplicates outList1 > 1 || numDuplicates outList2 > 1) in
        if posAmbigEdge edgeList1 then NH.replace diff toNode1 ()
        else findMatch (edgeList1, toNode1) in
      List.iter iterOuts outList1; compareNext () in

  let entryNode1, entryNode2 = (FunctionEntry fun1, FunctionEntry fun2) in
  NH.replace same.node1to2 entryNode1 entryNode2; NH.replace same.node2to1 entryNode2 entryNode1;
  Queue.push (entryNode1,entryNode2) waitingList; compareNext (); (same, diff)

(* This is the second phase of the CFG comparison of functions. It removes the nodes from the matching node set 'same'
 * that have an incoming backedge in the new CFG that can be reached from a differing new node. This is important to
 * recognize new dependencies between unknowns that are not contained in the infl from the previous run. *)
let reexamine f1 f2 (same : biDirectionNodeMap) (diffNodes1 : unit NH.t) (module CfgOld : CfgForward) (module CfgNew : CfgBidir) =
  let rec repeat () =
    let check_all_nodes_in_same ps n =
      match List.find_opt (fun p -> not (NH.mem same.node2to1 p)) ps with
      | None -> true
      | Some p ->
        begin
          let n1 = NH.find same.node2to1 n in
          NH.replace diffNodes1 n1 ();
          NH.remove same.node1to2 n1; NH.remove same.node2to1 n;
          false
        end in
    let cond n2 = Node.equal n2 (FunctionEntry f2) || check_all_nodes_in_same (List.map snd (CfgNew.prev n2)) n2 in
    let forall = NH.fold (fun n2 n1 acc -> acc && cond n2) same.node2to1 true in
    if not forall then repeat () in
  repeat ();
  NH.to_seq same.node1to2, NH.to_seq_keys diffNodes1


(** To provide separation of concerns in the implementation,
    the following functions apply to arbitrary digraphs parameterized
    over types of nodes and edges. When CFGs are used as diagraphs,
    the type of nodes is [Node.t] and the type of edges is [Edge.t list].
    The diagraph type is a function from a node to a list of pairs,
    where each pair is an outgoing edge along with the node reachable over that edge. *)
type ('n, 'e) digraph = 'n -> ('e * 'n) list

let digraph_of_cfg (cfg : cfg) : _ digraph =
  fun n -> List.map (fun (es, n) -> (to_edge_list es, n)) (cfg n)

(** reverse-postorder *)
let linearize_digraph (type n e)
    (module N : Hashtbl.HashedType with type t = n)
    (graph : (n, e) digraph) (start : n) : (n * e list) list =

  let module HashtblN = Hashtbl.Make (N) in
  let visited : unit HashtblN.t = HashtblN.create 101 in

  let rec go (n : n) (acc : (n * e list) list) : (n * e list) list =

    if HashtblN.mem visited n
      then acc
      else begin
        HashtblN.replace visited n () ;
        let es, ns = graph n |> List.split in
        (* right-fold instead of left-fold is important if order of outbound edges is relevant *)
        (n, es) :: List.fold_right go ns acc
      end

  in go start []

(** For each pair in the given matching, determine whether the dependencies
    of the old node are isomorphic to the dependencies of the new node
    under the given matching, i.e., [map m (deps v) = deps (m v)].
    Digraph arguments must be reversed, e.g., made from [CfgBackward.prev]. *)
let same_deps (type n1 n2 e1 e2)
    (module N : Hashtbl.HashedType with type t = n1)
    (rev_digraph_old : (n1, e1) digraph) (rev_digraph_new : (n2, e2) digraph)
    (eq_node : n2 -> n2 -> bool) (eq_edge : e1 -> e2 -> bool)
    (matching0 : (n1 * n2) list) : bool list =

  let module HashtblN = Hashtbl.Make (N) in
  let matching = HashtblN.of_seq (List.to_seq matching0) in

  (* TODO: does CFG *edge* need to be the same too? --> yes, if the statement changes, the dependency has changed
     what about the order of inbound edges? --> currently must be the same *)
  matching0 |> List.map (fun (n_old, n_new) ->
    let deps_old, deps_new = (rev_digraph_old n_old, rev_digraph_new n_new) in
    List.compare_lengths deps_old deps_new = 0
    && List.for_all2
      (fun (e_old, n_old') (e_new, n_new') ->
        eq_edge e_old e_new
        && Option.equal eq_node (HashtblN.find_opt matching n_old') (Some n_new'))
      deps_old deps_new
    )

(** matches the given linearized digraphs using Myers' diff algorithm *)
let match_lin_diff (type n1 n2 e1 e2)
    (nes_equal : n1 * e1 list -> n2 * e2 list -> bool) lin_old lin_new =
  DiffLib.myers nes_equal lin_old lin_new
  |> DiffLib.unify lin_old lin_new
  |> List.filter_map (function
      | DiffLib.UUnchanged ((o, _), (n, _)) -> Some (o, n)
      | _ -> None)

(** matches the given linearized digraphs based on their linearized order *)
let match_lin_1to1 (type n1 n2 e1 e2)
    (nes_can_match : n1 * e1 list -> n2 * e2 list -> bool) lin_old lin_new =
  (* for each node in the old list, take the first possible node from the new list *)
  let[@tail_mod_cons] rec helper xs ys =
    match xs, ys with
    | _, [] -> []
    | (o, _ as x) :: xs', (n, _ as y) :: ys'
        when nes_can_match x y ->
        (o, n) :: helper xs' ys'
    | xs', _ :: ys' -> helper xs' ys'
  in
  helper lin_old lin_new


(* The remaining functions are specific to Goblint CFGs,
   and use the functions that apply to arbitrary CFGs above *)

type node_match = { old_node : node; new_node : node; same_dep_vals : bool }
let node_match ?(same_dep_vals = true) old_node new_node =
  { old_node ; new_node ; same_dep_vals }

type cfg_compare_type = Forward | Diff | OneToOne [@@deriving show]

type nodes_diff = {
  (* the results for these nodes can be reused as-is ... *)
  matched_nodes : node_match list;
  (* ... assuming all nodes reachable from these are destabilized *)
  destabilize_nodes : node list;
  (* only used for tracing *)
  compare_type : cfg_compare_type;
}

let pretty_nodes_diff (nd : nodes_diff) =
  let open Pretty in
  let open Goblintutil.Pretty in
  pretty_record [
    pretty_record_field "match"
    @@ pretty_list
      (fun nm ->
        dprintf "<@[%a -->@?%a@?same_dep_vals=%b@]>"
          Node.pretty_trace nm.old_node
          Node.pretty_trace nm.new_node
          nm.same_dep_vals)
      nd.matched_nodes ;
    pretty_record_field "destabilize"
    @@ pretty_list (Node.pretty_trace ()) nd.destabilize_nodes ;
    pretty_record_field "compare_by"
    @@ text (show_cfg_compare_type nd.compare_type)
  ]

let compare_forwards (module CfgOld : CfgForward) (module CfgNew : CfgBidir) fun_old fun_new =
  let same, diff = Stats.time "forwards-compare-phase1" (fun () -> compareCfgs (module CfgOld) (module CfgNew) fun_old fun_new) () in
  let unchanged, diffNodes1 = Stats.time "forwards-compare-phase2" (fun () -> reexamine fun_old fun_new same diff (module CfgOld) (module CfgNew)) () in
  { matched_nodes = List.of_seq unchanged |> List.map (fun (o, n) -> node_match o n) ;
    destabilize_nodes = List.of_seq diffNodes1 ;
    compare_type = Forward }

let linearize_cfg cfg fundec = (* type n = Node.t, type e = edge list *)
  linearize_digraph (module Node) (digraph_of_cfg cfg) (FunctionEntry fundec)

(** pass to [match_lin_diff] for matching CFGs *)
let nes_equal_cfg fun_old fun_new (n1, es1) (n2, es2) =
  eq_node (n1, fun_old) (n2, fun_new)
  (* && List.compare_lengths es1 es2 = 0 *)
  && List.equal eq_edge_list es1 es2

let nes_can_match_cfg (n1, es1) (n2, es2) =
  match n1, n2 with
  | Statement _, Statement _
    | FunctionEntry _, FunctionEntry _
    | Function _, Function _ -> true
  | _ -> false

let same_deps_cfg rev_cfg_old rev_cfg_new =
  same_deps (module Node) (digraph_of_cfg rev_cfg_old) (digraph_of_cfg rev_cfg_new) Node.equal eq_edge_list

let cfg_matching_of_fuzzy_match cmp_by rev_cfg_old rev_cfg_new fun_old fun_new fuzzy_match =
  let matched_nodes =
    List.map2
      (fun (o, n) same_dep_vals -> node_match ~same_dep_vals o n)
      fuzzy_match
      (same_deps_cfg rev_cfg_old rev_cfg_new fuzzy_match)
  in
  (* for fuzzy matches, destabilize the entire function by marking the entry node *)
  { matched_nodes ; destabilize_nodes = [ FunctionEntry fun_old ] ; compare_type = cmp_by }

let cfg_compare_type_of_string = function
  | "forward" -> Forward
  | "diff" -> Diff
  | "1-to-1" -> OneToOne
  | s -> failwith ("unknown cfg compare type: " ^ s)

let compare_fun ?compare_type (module CfgOld : CfgBidir) (module CfgNew : CfgBidir) fun_old fun_new =
  let compare_type' = match compare_type with
    | None -> GobConfig.get_string "incremental.compare-cfg.by" |> cfg_compare_type_of_string
    | Some cmp -> cmp
  in
  match compare_type' with
  | Forward -> compare_forwards (module CfgOld) (module CfgNew) fun_old fun_new
  | Diff | OneToOne as cmp_by ->
      let lin_old = linearize_cfg CfgOld.next fun_old in
      let lin_new = linearize_cfg CfgNew.next fun_new in
      (match cmp_by with
      | Diff -> match_lin_diff (nes_equal_cfg fun_old fun_new) lin_old lin_new
      | OneToOne | _ -> match_lin_1to1 nes_can_match_cfg lin_old lin_new)
      |> cfg_matching_of_fuzzy_match cmp_by CfgOld.prev CfgNew.prev fun_old fun_new
