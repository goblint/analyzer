(** Comparison of CFGs. *)

open MyCFG
open Queue
open GoblintCil
include CompareAST

(*Non propagating version of &&>>. Discards the new rename_mapping and alwas propagates the one in prev_result. However propagates the renames_on_success*)
let (&&<>) (prev_result: bool * rename_mapping) f : bool * rename_mapping =
  let (prev_equal, prev_rm) = prev_result in
  let (a, b, c, _) = prev_rm in

  if prev_equal then
    let (r, ((_, _, _, updated_renames_on_success) : rename_mapping)) = f ~rename_mapping:prev_rm in
    (r, (a, b, c, updated_renames_on_success))
  else false, prev_rm

let eq_node (x, fun1) (y, fun2) ~rename_mapping =
  let isPseudoReturn f sid =
    let pid = Cilfacade.get_pseudo_return_id f in
    sid = pid in
  match x,y with
  | Statement s1, Statement s2 ->
    let p1 = isPseudoReturn fun1 s1.sid in
    let p2 = isPseudoReturn fun2 s2.sid in
    ((p1 && p2) || not (p1 || p2), rename_mapping) &&>> eq_stmt ~cfg_comp:true (s1, fun1) (s2, fun2)
  | Function f1, Function f2 -> eq_varinfo f1.svar f2.svar ~rename_mapping
  | FunctionEntry f1, FunctionEntry f2 -> eq_varinfo f1.svar f2.svar ~rename_mapping
  | _ -> false, rename_mapping

(* TODO: compare ASMs properly instead of simply always assuming that they are not the same *)
let eq_edge x y ~rename_mapping =
  match x, y with
  | Assign (lv1, rv1), Assign (lv2, rv2) -> eq_lval lv1 lv2 ~rename_mapping &&<> eq_exp rv1 rv2
  | Proc (None,f1,ars1), Proc (None,f2,ars2) -> eq_exp f1 f2 ~rename_mapping &&<> forward_list_equal eq_exp ars1 ars2
  | Proc (Some r1,f1,ars1), Proc (Some r2,f2,ars2) ->
    eq_lval r1 r2 ~rename_mapping &&<> eq_exp f1 f2 &&<> forward_list_equal eq_exp ars1 ars2
  | Entry f1, Entry f2 -> eq_varinfo f1.svar f2.svar ~rename_mapping
  | Ret (None,fd1), Ret (None,fd2) -> eq_varinfo fd1.svar fd2.svar ~rename_mapping
  | Ret (Some r1,fd1), Ret (Some r2,fd2) -> eq_exp r1 r2 ~rename_mapping &&<> eq_varinfo fd1.svar fd2.svar
  | Test (p1,b1), Test (p2,b2) -> eq_exp p1 p2 ~rename_mapping &&> (b1 = b2)
  | ASM _, ASM _ -> false, rename_mapping
  | Skip, Skip -> true, rename_mapping
  | VDecl v1, VDecl v2 -> eq_varinfo v1 v2 ~rename_mapping
  | _ -> false, rename_mapping

(* The order of the edges in the list is relevant. Therefore compare them one to one without sorting first *)
let eq_edge_list xs ys = forward_list_equal ~propF:(&&<>) eq_edge xs ys

let to_edge_list ls = List.map (fun (loc, edge) -> edge) ls

module NH = Hashtbl.Make(Node)
type biDirectionNodeMap = {node1to2: node NH.t; node2to1: node NH.t}

(* This function compares two CFGs by doing a breadth-first search on the old CFG. Matching node tuples are stored in same,
 * nodes from the old CFG for which no matching node can be found are added to diff. For each matching node tuple
 * (fromNode1, fromNode2) found, one iterates over the successors of fromNode1 from the old CFG and checks for a matching node
 * in the succesors of fromNode2 in the new CFG. Matching node tuples are added to the waitingList to repeat the matching
 * process on their successors. If a node from the old CFG can not be matched, it is added to diff and no further
 * comparison is done for its successors. The two function entry nodes make up the tuple to start the comparison from. *)

let compareCfgs (module CfgOld : CfgForward) (module CfgNew : CfgForward) fun1 fun2 rename_mapping : biDirectionNodeMap * unit NH.t * rename_mapping =
  let diff = NH.create 113 in
  let same = {node1to2=NH.create 113; node2to1=NH.create 113} in
  let waitingList : (node * node) t = Queue.create () in

  let rec compareNext rename_mapping : rename_mapping =
    if Queue.is_empty waitingList then rename_mapping
    else
      let fromNode1, fromNode2 = Queue.take waitingList in
      let outList1 = CfgOld.next fromNode1 in
      let outList2 = CfgNew.next fromNode2 in

      (* Find a matching edge and successor node for (edgeList1, toNode1) in the list of successors of fromNode2.
       * If successful, add the matching node tuple to same, else add toNode1 to the differing nodes. *)
      let findMatch (edgeList1, toNode1) rename_mapping : rename_mapping =
        let rec aux remSuc rename_mapping : rename_mapping = match remSuc with
          | [] -> NH.replace diff toNode1 (); rename_mapping
          | (locEdgeList2, toNode2)::remSuc' ->
            let edgeList2 = to_edge_list locEdgeList2 in
            let (isEq, updatedRenameMapping) = (true, rename_mapping) &&>> eq_node (toNode1, fun1) (toNode2, fun2) &&>> eq_edge_list edgeList1 edgeList2 in
            if isEq then
              begin
                match NH.find_opt same.node1to2 toNode1 with
                | Some n2 -> if not (Node.equal n2 toNode2) then NH.replace diff toNode1 (); updatedRenameMapping
                | None -> NH.replace same.node1to2 toNode1 toNode2; NH.replace same.node2to1 toNode2 toNode1; Queue.add (toNode1, toNode2) waitingList;
                  updatedRenameMapping
              end
            else aux remSuc' updatedRenameMapping in
        aux outList2 rename_mapping in
      (* For a toNode1 from the list of successors of fromNode1, check whether it might have duplicate matches.
       * In that case declare toNode1 as differing node. Else, try finding a match in the list of successors
       * of fromNode2 in the new CFG using findMatch. *)
      let iterOuts (locEdgeList1, toNode1) rename_mapping : rename_mapping =
        let edgeList1 = to_edge_list locEdgeList1 in
        (* Differentiate between a possibly duplicate Test(1,false) edge and a single occurence. In the first
         * case the edge is directly added to the diff set to avoid undetected ambiguities during the recursive
         * call. *)
        let testFalseEdge edge = match edge with
          | Test (p,false) -> p = Cil.one
          | _ -> false in
        let posAmbigEdge edgeList = let findTestFalseEdge (ll,_) = testFalseEdge (snd (List.hd ll)) in
          let numDuplicates l = List.length (List.find_all findTestFalseEdge l) in
          testFalseEdge (List.hd edgeList) && (numDuplicates outList1 > 1 || numDuplicates outList2 > 1) in
        if posAmbigEdge edgeList1 then (NH.replace diff toNode1 (); rename_mapping)
        else findMatch (edgeList1, toNode1) rename_mapping in
      let updatedRenameMapping = List.fold_left (fun rm e -> iterOuts e rm) rename_mapping outList1 in
      compareNext updatedRenameMapping
  in

  let entryNode1, entryNode2 = (FunctionEntry fun1, FunctionEntry fun2) in
  NH.replace same.node1to2 entryNode1 entryNode2;
  NH.replace same.node2to1 entryNode2 entryNode1;
  Queue.push (entryNode1,entryNode2) waitingList;
  let updatedRenameMapping = compareNext rename_mapping in
  same, diff, updatedRenameMapping

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
    let forall = NH.fold (fun n2 n1 acc -> acc && cond n2) same.node2to1 true in (* nosemgrep: fold-for_all *) (* cond does side effects *)
    if not forall then repeat () in
  repeat ();
  NH.to_seq same.node1to2, NH.to_seq_keys diffNodes1


let compareFun (module CfgOld : CfgForward) (module CfgNew : CfgBidir) fun1 fun2 rename_mapping : (node * node) list * node list * rename_mapping =
  let same, diff, rename_mapping = Timing.wrap "compare-phase1" (fun () -> compareCfgs (module CfgOld) (module CfgNew) fun1 fun2 rename_mapping) () in
  let unchanged, diffNodes1 = Timing.wrap "compare-phase2" (fun () -> reexamine fun1 fun2 same diff (module CfgOld) (module CfgNew)) () in
  List.of_seq unchanged, List.of_seq diffNodes1, rename_mapping
