open MyCFG
open GoblintCil
open Pretty
open GobConfig

module H = NodeH
module NH = NodeH


let find_backwards_reachable ~initial_size (module Cfg:CfgBackward) (node:node): unit NH.t =
  let reachable = NH.create initial_size in

  (* DFS, copied from Control is_sink *)
  let rec iter_node node =
    if not (NH.mem reachable node) then begin
      NH.replace reachable node ();
      List.iter (fun (_, prev_node) ->
          iter_node prev_node
        ) (Cfg.prev node)
    end
  in

  iter_node node;
  reachable


(** Strongly connected component. *)
module SCC =
struct
  type t = {
    nodes: unit NH.t; (** Set of nodes in SCC, mutated during [computeSCCs]. *)
    next: (edges * node) list NH.t; (** Successor edges from this SCC to another SCC, mutated during [computeSCCs]. *)
    prev: (edges * node) list NH.t; (** Predecessor edges from another SCC to this SCC, mutated during [computeSCCs]. *)
  }
  (* Identity by physical equality. *)
  let equal = (==)
  let hash = Hashtbl.hash
end

(** Compute strongly connected components (SCCs) of [nodes] in [Cfg].
    Returns list of SCCs and a mapping from nodes to those SCCs. *)
let computeSCCs (module Cfg: CfgBidir) nodes =
  let nodes_length = List.length nodes in
  (* Kosaraju's algorithm *)
  let finished_rev =
    (* first DFS to construct list of nodes in reverse finished order *)
    let visited = NH.create nodes_length in

    let rec dfs_inner node finished_rev =
      if not (NH.mem visited node) then (
        NH.replace visited node ();
        node :: List.fold_left (fun finished_rev (_, next_node) ->
            dfs_inner next_node finished_rev
          ) finished_rev (Cfg.next node)
      )
      else
        finished_rev
    in

    (* outer DFS loop over unconnected components *)
    List.fold_left (fun finished_rev node ->
        dfs_inner node finished_rev
      ) [] nodes
  in

  let open SCC in (* open for SCC.t constructors *)
  let (sccs, node_scc) as r =
    (* second DFS to construct SCCs on transpose graph *)
    let node_scc = NH.create nodes_length in (* like visited, but values are assigned SCCs *)

    let rec dfs_inner node scc =
      (* assumes: not (NH.mem node_scc node) *)
      NH.replace node_scc node scc;
      NH.replace scc.nodes node ();
      List.iter (fun (edges, prev_node) ->
          if not (NH.mem node_scc prev_node) then
            dfs_inner prev_node scc
          else if not (NH.mem scc.nodes prev_node) then (
            (* prev_node has been visited, but not in current SCC, therefore is backwards edge to predecessor scc *)
            if Messages.tracing then Messages.trace "cfg" "SCC edge: %s -> %s\n" (Node.show_id prev_node) (Node.show_id node);
            NH.modify_def [] node (List.cons (edges, prev_node)) scc.prev;
            NH.modify_def [] prev_node (List.cons (edges, node)) (NH.find node_scc prev_node).next;
          )
        ) (Cfg.prev node) (* implicitly transpose graph by moving backwards *)
    in

    (* outer DFS loop over unconnected components *)
    let sccs = List.fold_left (fun sccs node ->
        if not (NH.mem node_scc node) then
          let scc = {
            nodes = NH.create 1;
            next = NH.create 1;
            prev = NH.create 1
          }
          in
          dfs_inner node scc;
          scc :: sccs
        else
          sccs
      ) [] finished_rev
    in
    (sccs, node_scc)
  in

  if Messages.tracing then (
    List.iter (fun scc ->
        let nodes = scc.nodes |> NH.keys |> BatList.of_enum in
        Messages.trace "cfg" "SCC: %a\n" (d_list " " (fun ppf node -> text (Node.show_id node) ppf)) nodes;
        NH.iter (fun node _ ->
            Messages.trace "cfg" "SCC entry: %s\n" (Node.show_id node)
          ) scc.prev
      ) sccs
  );
  r

let computeSCCs x = Stats.time "computeSCCs" (computeSCCs x)

let rec pretty_edges ppf = function
  | [] -> Pretty.dprintf "" ppf
  | [_,x] -> Edge.pretty_plain ppf x
  | (_,x)::xs -> Pretty.dprintf "%a; %a" Edge.pretty_plain x pretty_edges xs ppf

let get_pseudo_return_id fd =
  let start_id = 10_000_000_000 in (* TODO get max_sid? *)
  let sid = Hashtbl.hash fd.svar.vid in (* Need pure sid instead of Cil.new_sid for incremental, similar to vid in Goblintutil.create_var. We only add one return stmt per loop, so the hash from the functions vid should be unique. *)
  if sid < start_id then sid + start_id else sid

let node_scc_global = NH.create 113

exception Not_connect of fundec

let () = Printexc.register_printer (function
    | Not_connect fd ->
      Some (Printf.sprintf "CfgTools.Not_connect(%s)" (CilType.Fundec.show fd))
    | _ -> None (* for other exceptions *)
  )

let createCFG (file: file) =
  let cfgF = H.create 113 in
  let cfgB = H.create 113 in
  if Messages.tracing then Messages.trace "cfg" "Starting to build the cfg.\n\n";

  let fd_nodes = NH.create 113 in

  let addEdges fromNode edges toNode =
    if Messages.tracing then
      Messages.trace "cfg" "Adding edges [%a] from\n\t%a\nto\n\t%a ... "
        pretty_edges edges
        Node.pretty_trace fromNode
        Node.pretty_trace toNode;
    NH.replace fd_nodes fromNode ();
    NH.replace fd_nodes toNode ();
    H.modify_def [] toNode (List.cons (edges,fromNode)) cfgB;
    H.modify_def [] fromNode (List.cons (edges,toNode)) cfgF;
    if Messages.tracing then Messages.trace "cfg" "done\n\n"
  in
  let addEdge fromNode edge toNode = addEdges fromNode [edge] toNode in
  let addEdge_fromLoc fromNode edge toNode = addEdge fromNode (Node.location fromNode, edge) toNode in

  (* Find real (i.e. non-empty) successor of statement.
     CIL CFG contains some unnecessary intermediate statements.
     If stmt is succ of parent, then optional argument parent must be passed
     to also detect cycle ending with parent itself.
     If not_found is true, then a stmt without succs will raise Not_found
     instead of returning that stmt. *)
  let find_real_stmt ?parent ?(not_found=false) stmt =
    if Messages.tracing then Messages.tracei "cfg" "find_real_stmt not_found=%B stmt=%d\n" not_found stmt.sid;
    let rec find visited_sids stmt =
      if Messages.tracing then Messages.trace "cfg" "find_real_stmt visited=[%a] stmt=%d: %a\n" (d_list "; " (fun ppf x -> Pretty.text (string_of_int x) ppf)) visited_sids stmt.sid dn_stmt stmt;
      if List.mem stmt.sid visited_sids then (* mem uses structural equality on ints, which is fine *)
        stmt (* cycle *)
      else
        match stmt.skind with
        | Goto _ (* 1 succ *)
        | Instr [] (* CIL inserts like unlabelled goto, 0-1 succs *)
        | Block _ (* just container for stmts, 0-1 succs *)
        | Loop _ -> (* just container for (prepared) body, 1 succ *)
          begin match stmt.succs with
            | [] ->
              if not_found then
                raise Not_found
              else
                stmt
            | [next] ->
              find (stmt.sid :: visited_sids) next
            | _ -> (* >1 succ *)
              failwith "MyCFG.createCFG.find_real_stmt: >1 succ"
          end

        | Instr _
        | If _
        | Return _ ->
          stmt

        | Continue _
        | Break _
        | Switch _ ->
          (* Should be removed by Cil.prepareCFG. *)
          failwith "MyCFG.createCFG: unprepared stmt"

        | ComputedGoto _->
          failwith "MyCFG.createCFG: unsupported stmt"
    in
    try
      let initial_visited_sids = match parent with
        | Some parent -> [parent.sid]
        | None -> []
      in
      let r = find initial_visited_sids stmt in
      if Messages.tracing then Messages.traceu "cfg" "-> %d\n" r.sid;
      r
    with Not_found ->
      if Messages.tracing then Messages.traceu "cfg" "-> Not_found\n";
      raise Not_found
  in
  addEdge_fromLoc (FunctionEntry dummy_func) (Ret (None, dummy_func)) (Function dummy_func);
  (* We iterate over all globals looking for functions: *)
  iterGlobals file (fun glob ->
      match glob with
      | GFun (fd, fd_loc) ->
        if Messages.tracing then Messages.trace "cfg" "Looking at the function %s.\n" fd.svar.vname;

        if get_bool "dbg.cilcfgdot" then
          Cfg.printCfgFilename ("cilcfg." ^ fd.svar.vname ^ ".dot") fd;

        NH.clear fd_nodes;

        (* Find the first statement in the function *)
        let entrynode = find_real_stmt (Cilfacade.getFirstStmt fd) in
        (* Add the entry edge to that node *)
        addEdge (FunctionEntry fd) (fd_loc, Entry fd) (Statement entrynode);
        (* Return node to be used for infinite loop connection to end of function
         * lazy, so it's only added when actually needed *)
        let pseudo_return = lazy (
          let newst = mkStmt (Return (None, fd_loc)) in
          newst.sid <- get_pseudo_return_id fd;
          Cilfacade.StmtH.add Cilfacade.pseudo_return_to_fun newst fd;
          let newst_node = Statement newst in
          addEdge newst_node (fd_loc, Ret (None, fd)) (Function fd);
          newst_node
        )
        in
        let loop_head_neg1 = NH.create 3 in
        (* So for each statement in the function body, we do the following: *)
        let handle stmt =
          if Messages.tracing then Messages.trace "cfg" "Statement %d at %a.\n" stmt.sid d_loc (Cilfacade.get_stmtLoc stmt);

          let real_succs () = List.map (find_real_stmt ~parent:stmt) stmt.succs in

          match stmt.skind with
          | Instr [] ->
            (* CIL sometimes inserts empty Instr, which is like a goto without label. *)
            (* Without this special case, CFG would contain edges without label or transfer function,
               which is unwanted because such flow is undetectable by the analysis (especially for witness generation). *)
            (* Generally these are unnecessary and unwanted because find_real_stmt skips over these. *)
            (* CIL uses empty Instr self-loop for empty Loop, so a Skip self-loop must be added to not lose the loop. *)
            begin match real_succs () with
              | [] -> () (* if stmt.succs is empty (which in other cases requires pseudo return), then it isn't a self-loop to add anyway *)
              | [succ] ->
                if CilType.Stmt.equal succ stmt then (* self-loop *)
                  let loc = Cilfacade.get_stmtLoc stmt in (* get location from label because Instr [] itself doesn't have one *)
                  addEdge (Statement stmt) (loc, Skip) (Statement succ)
              | _ -> failwith "MyCFG.createCFG: >1 Instr [] succ"
            end

          | Instr instrs -> (* non-empty Instr *)
            let edge_of_instr = function
              | Set (lval,exp,loc,eloc) -> eloc, Assign (lval, exp) (* TODO: eloc loc fallback if unknown here and If *)
              | Call (lval,func,args,loc,eloc) -> eloc, Proc (lval,func,args)
              | Asm (attr,tmpl,out,inp,regs,loc) -> loc, ASM (tmpl,out,inp)
              | VarDecl (v, loc) -> loc, VDecl(v)
            in
            let edges = List.map edge_of_instr instrs in
            let add_succ_node succ_node = addEdges (Statement stmt) edges succ_node in
            begin match real_succs () with
              | [] -> add_succ_node (Lazy.force pseudo_return) (* stmt.succs can be empty if last instruction calls non-returning function (e.g. exit), so pseudo return instead *)
              | [succ] -> add_succ_node (Statement succ)
              | _ -> failwith "MyCFG.createCFG: >1 non-empty Instr succ"
            end

          | If (exp, _, _, loc, eloc) ->
            (* Cannot use true and false blocks from If constructor, because blocks don't have succs (stmts do).
               Cannot use first stmt in block either, because block may be empty (e.g. missing branch). *)
            (* Hence we rely on implementation detail of the If case in CIL's succpred_stmt.
               First, true branch's succ is consed (to empty succs list).
               Second, false branch's succ is consed (to previous succs list).
               CIL doesn't cons duplicate succs, so if both branches have the same succ, then singleton list is returned instead. *)
            let (true_stmt, false_stmt) = match real_succs () with
              | [false_stmt; true_stmt] -> (true_stmt, false_stmt)
              | [same_stmt] -> (same_stmt, same_stmt)
              | _ -> failwith "MyCFG.createCFG: invalid number of If succs"
            in
            addEdge (Statement stmt) (eloc, Test (exp, true )) (Statement true_stmt);
            addEdge (Statement stmt) (eloc, Test (exp, false)) (Statement false_stmt)

          | Loop (_, loc, eloc, Some cont, Some brk) -> (* TODO: use loc for something? *)
            (* CIL already converts Loop logic to Gotos and If. *)
            (* CIL eliminates the constant true If corresponding to constant true Loop.
               Then there is no Goto to after the loop and the CFG is unconnected (to Function node).
               An extra Neg(1) edge is added in such case. *)
            if Messages.tracing then Messages.trace "cfg" "loop %d cont=%d brk=%d\n" stmt.sid cont.sid brk.sid;
            begin match find_real_stmt ~not_found:true brk with (* don't specify stmt as parent because if find_real_stmt finds cycle, it should not return the Loop statement *)
              | break_stmt ->
                (* break statement is what follows the (constant true) Loop *)
                (* Neg(1) edges are lazily added only when unconnectedness is detected at the end,
                   so break statement is just remembered here *)
                let loop_stmt = find_real_stmt stmt in
                NH.add loop_head_neg1 (Statement loop_stmt) (Statement break_stmt)
              | exception Not_found ->
                (* if the (constant true) Loop and its break statement are at the end of the function,
                   then find_real_stmt doesn't find a non-empty statement. *)
                (* pseudo return is used instead by default, so nothing to do here *)
                ()
            end

          | Loop (_, _, _, _, _) ->
            (* CIL's xform_switch_stmt (via prepareCFG) always adds both continue and break statements to all Loops. *)
            failwith "MyCFG.createCFG: unprepared Loop"

          | Return (exp, loc) ->
            addEdge (Statement stmt) (loc, Ret (exp, fd)) (Function fd)

          | Goto (_, loc) ->
            (* Gotos are generally unnecessary and unwanted because find_real_stmt skips over these. *)
            (* CIL uses Goto self-loop for empty goto-based loop, so a Skip self-loop must be added to not lose the loop. *)
            (* real_succs are used instead of stmt.succs to handle empty goto-based loops with multiple mutual gotos. *)
            (* stmt.succs for Goto just contains the target ref. *)
            begin match real_succs () with
              | [] -> failwith "MyCFG.createCFG: 0 Goto succ" (* target ref is always succ *)
              | [succ] ->
                if CilType.Stmt.equal succ stmt then (* self-loop *)
                  addEdge (Statement stmt) (loc, Skip) (Statement succ)
              | _ -> failwith "MyCFG.createCFG: >1 Goto succ"
            end

          | Block {bstmts = []; _} ->
            (* Blocks are generally unnecessary and unwanted because find_real_stmt skips over these. *)
            (* CIL inserts empty Blocks before empty goto-loops which contain a semicolon, so a Skip self-loop must be added to not lose the loop. *)
            (* real_succs are used instead of stmt.succs to handle empty goto-based loops with multiple mutual gotos. *)
            begin match real_succs () with
              | [] -> () (* if stmt.succs is empty (which in other cases requires pseudo return), then it isn't a self-loop to add anyway *)
              | [succ] ->
                if CilType.Stmt.equal succ stmt then (* self-loop *)
                  let loc = Cilfacade.get_stmtLoc stmt in (* get location from label because Block [] itself doesn't have one *)
                  addEdge (Statement stmt) (loc, Skip) (Statement succ)
              | _ -> failwith "MyCFG.createCFG: >1 Block [] succ"
            end

          | Block _ -> (* non-empty Block *)
            (* Nothing to do, find_real_stmt skips over these. *)
            ()

          | Continue _
          | Break _
          | Switch _ ->
            (* Should be removed by Cil.prepareCFG. *)
            failwith "MyCFG.createCFG: unprepared stmt"

          | ComputedGoto _ ->
            failwith "MyCFG.createCFG: unsupported stmt"
        in
        Stats.time "handle" (List.iter handle) fd.sallstmts;

        if Messages.tracing then Messages.trace "cfg" "Over\n";

        (* Connect remaining infinite loops (e.g made using goto) to end of function
         * via pseudo return node for demand driven solvers *)
        let module TmpCfg: CfgBidir =
        struct
          let next n = H.find_default cfgF n []
          let prev n = H.find_default cfgB n []
        end
        in

        let rec iter_connect () =
          let (sccs, node_scc) = computeSCCs (module TmpCfg) (NH.keys fd_nodes |> BatList.of_enum) in

          let added_connect = ref false in

          (* DFS over SCCs starting from FunctionEntry SCC *)
          let module SH = Hashtbl.Make (SCC) in
          let visited_scc = SH.create (List.length sccs) in
          let rec iter_scc scc =
            if not (SH.mem visited_scc scc) then (
              SH.replace visited_scc scc ();
              if NH.is_empty scc.next then (
                if not (NH.mem scc.nodes (Function fd)) then (
                  (* scc has no successors but also doesn't contain return node, requires additional connections *)
                  (* find connection candidates from loops *)
                  let targets =
                    NH.keys scc.nodes
                    |> BatEnum.concat_map (fun fromNode ->
                        NH.find_all loop_head_neg1 fromNode
                        |> BatList.enum
                        |> BatEnum.filter (fun toNode ->
                            not (NH.mem scc.nodes toNode) (* exclude candidates into the same scc, those wouldn't help *)
                          )
                        |> BatEnum.map (fun toNode ->
                            (fromNode, toNode)
                          )
                      )
                    |> BatList.of_enum
                  in
                  let targets = match targets with
                    | [] ->
                      let scc_node =
                        NH.keys scc.nodes
                        |> BatList.of_enum
                        |> BatList.min ~cmp:Node.compare (* use min for consistency for incremental CFG comparison *)
                      in
                      (* default to pseudo return if no suitable candidates *)
                      [(scc_node, Lazy.force pseudo_return)]
                    | targets -> targets
                  in
                  List.iter (fun (fromNode, toNode) ->
                      addEdge_fromLoc fromNode (Test (one, false)) toNode;
                      added_connect := true;
                      match NH.find_option node_scc toNode with
                      | Some toNode_scc -> iter_scc toNode_scc (* continue to target scc as normally, to ensure they are also connected *)
                      | None -> () (* pseudo return, wasn't in scc, but is fine *)
                    ) targets
                )
              )
              else
                NH.iter (fun _ nexts ->
                    List.iter (fun (_, toNode) ->
                        iter_scc (NH.find node_scc toNode)
                      ) nexts
                  ) scc.next
            )
          in
          iter_scc (NH.find node_scc (FunctionEntry fd));

          if !added_connect then
            iter_connect () (* added connect edge might have made a cycle of SCCs, have to recompute SCCs to see if it needs connecting *)
          else
            NH.iter (NH.replace node_scc_global) node_scc; (* there's no merge inplace *)
        in
        Stats.time "iter_connect" iter_connect ();

        (* Verify that function is now connected *)
        let reachable_return' = find_backwards_reachable ~initial_size:(NH.keys fd_nodes |> BatEnum.hard_count) (module TmpCfg) (Function fd) in
        (* TODO: doesn't check that all branches are connected, but only that there exists one which is *)
        if not (NH.mem reachable_return' (FunctionEntry fd)) then
          raise (Not_connect fd)
      | _ -> ()
    );
  if Messages.tracing then Messages.trace "cfg" "CFG building finished.\n\n";
  if get_bool "dbg.verbose" then
    ignore (Pretty.eprintf "cfgF (%a), cfgB (%a)\n" GobHashtbl.pretty_statistics (GobHashtbl.magic_stats cfgF) GobHashtbl.pretty_statistics (GobHashtbl.magic_stats cfgB));
  cfgF, cfgB

let createCFG = Stats.time "createCFG" createCFG


let minimizeCFG (fw,bw) =
  let keep = H.create (H.length bw) in
  let comp_keep t (_,f) =
    if (List.compare_length_with (H.find_default bw t []) 1 <> 0) || (List.compare_length_with (H.find_default fw t []) 1 <> 0) then
      H.replace keep t ();
    if (List.compare_length_with (H.find_default bw f []) 1 <> 0) || (List.compare_length_with (H.find_default fw f []) 1 <> 0) then
      H.replace keep f ()
  in
  let comp_keep t es = List.iter (comp_keep t) es in
  H.iter comp_keep bw;
  (* H.iter comp_keep fw; *)
  let cfgB = H.create (H.length bw) in
  let cfgF = H.create (H.length fw) in
  let ready = H.create (H.length bw) in
  let rec add a b t (e,f)=
    if H.mem keep f then begin
      H.modify_def [] b (List.cons (e@a,f)) cfgB;
      H.modify_def [] f (List.cons (e@a,b)) cfgF;
      if H.mem ready b then begin
        H.replace ready f ();
        List.iter (add [] f f) (H.find_default bw f [])
      end
    end else begin
      List.iter (add (e@a) b f) (H.find_default bw f [])
    end
  in
  H.iter (fun k _ -> List.iter (add [] k k) (H.find_default bw k [])) keep;
  H.clear ready;
  H.clear keep;
  cfgF, cfgB


module type CfgPrinters =
sig
  val defaultNodeStyles: string list
  val printNodeStyle: Format.formatter -> node -> unit
  val printEdgeStyle: Format.formatter -> node -> (edges * node) -> unit
end

module type NodeStyles =
sig
  val defaultNodeStyles: string list
  val extraNodeStyles: node -> string list
end

module CfgPrinters (NodeStyles: NodeStyles) =
struct
  include NodeStyles

  let p_node out n = Format.fprintf out "%s" (Node.show_id n)

  (* escape string in label, otherwise dot might fail *)
  let p_edge (out: Format.formatter) x = Format.fprintf out "%s" (String.escaped (Pretty.sprint ~width:max_int (fun ppf -> Edge.pretty ppf x)))

  let rec p_edges out = function
    | [] -> Format.fprintf out ""
    | [(_, x)] -> Format.fprintf out "%a" p_edge x
    | (_,x)::xs -> Format.fprintf out "%a\n%a" p_edge x p_edges xs

  let printEdgeStyle out (toNode: node) ((edges:(location * edge) list), (fromNode: node)) =
    Format.fprintf out "\t%a -> %a [label = \"%a\"] ;\n" p_node fromNode p_node toNode p_edges edges

  let printNodeStyle out (n:node) =
    let label = match n with
      | Statement _ -> [] (* use default label *)
      | _ -> ["label=\"" ^ String.escaped (Node.show_cfg n) ^ "\""]
    in
    let shape = match n with
      | Statement {skind=If (_,_,_,_,_); _}  -> ["shape=diamond"]
      | Statement _     -> [] (* use default shape *)
      | Function _
      | FunctionEntry _ -> ["shape=box"]
    in
    let styles = String.concat "," (label @ shape @ extraNodeStyles n) in
    Format.fprintf out ("\t%a [%s];\n") p_node n styles
end

let fprint_dot (module CfgPrinters: CfgPrinters) iter_edges out =
  let node_table = NH.create 113 in
  Format.fprintf out "digraph cfg {\n";
  Format.fprintf out "\tnode [%s];\n" (String.concat "," CfgPrinters.defaultNodeStyles);
  let printEdge (toNode: node) ((edges:(location * edge) list), (fromNode: node)) =
    CfgPrinters.printEdgeStyle out toNode (edges, fromNode);
    NH.replace node_table toNode ();
    NH.replace node_table fromNode ()
  in
  iter_edges printEdge;
  NH.iter (fun node _ -> CfgPrinters.printNodeStyle out node) node_table;

  if get_bool "dbg.cfg.loop-clusters" then (
    let node_scc_done = NH.create 113 in
    NH.iter (fun node _ ->
        if not (NH.mem node_scc_done node) then (
          match NH.find_option node_scc_global node with
          | Some scc when NH.length scc.nodes > 1 ->
            Format.fprintf out "\tsubgraph cluster {\n\t\t";
            NH.iter (fun node _ ->
                NH.replace node_scc_done node ();
                Format.fprintf out ("%s; ") (Node.show_id node)
              ) scc.nodes;
            Format.fprintf out "\n\t}\n";
          | _ -> ()
        )
      ) node_table
  );

  Format.fprintf out "}\n"

let fprint_hash_dot cfg  =
  let module NoExtraNodeStyles =
  struct
    let defaultNodeStyles = []
    let extraNodeStyles node = []
  end
  in
  let out = open_out "cfg.dot" in
  let iter_edges f = H.iter (fun n es -> List.iter (f n) es) cfg in
  let ppf = Format.formatter_of_out_channel out in
  fprint_dot (module CfgPrinters (NoExtraNodeStyles)) iter_edges ppf;
  Format.pp_print_flush ppf ();
  close_out out


let getCFG (file: file) : cfg * cfg =
  let cfgF, cfgB = createCFG file in
  let cfgF, cfgB =
    if get_bool "exp.mincfg" then
      Stats.time "minimizing the cfg" minimizeCFG (cfgF, cfgB)
    else
      (cfgF, cfgB)
  in
  if get_bool "justcfg" then fprint_hash_dot cfgB;
  (fun n -> H.find_default cfgF n []), (fun n -> H.find_default cfgB n [])


let iter_fd_edges (module Cfg : CfgBackward) fd =
  let ready      = NH.create 113 in
  let rec printNode (toNode : node) f =
    if not (NH.mem ready toNode) then begin
      NH.replace ready toNode ();
      let prevs = Cfg.prev toNode in
      List.iter (f toNode) prevs;
      List.iter (fun (_,x) -> printNode x f) prevs
    end
  in
  printNode (Function fd)

let fprint_fundec_html_dot (module Cfg : CfgBidir) live fd out =
  let module HtmlExtraNodeStyles =
  struct
    let defaultNodeStyles = ["id=\"\\N\""; "URL=\"javascript:show_info('\\N');\""; "style=filled"; "fillcolor=white"] (* The \N is graphviz special for node ID. *)
    let extraNodeStyles n =
      if live n then
        []
      else
        ["fillcolor=orange"]
  end
  in
  let iter_edges = iter_fd_edges (module Cfg) fd in
  fprint_dot (module CfgPrinters (HtmlExtraNodeStyles)) iter_edges out

let sprint_fundec_html_dot (module Cfg : CfgBidir) live fd =
  fprint_fundec_html_dot (module Cfg) live fd Format.str_formatter;
  Format.flush_str_formatter ()

let dead_code_cfg (file:file) (module Cfg : CfgBidir) live =
  iterGlobals file (fun glob ->
      match glob with
      | GFun (fd,loc) ->
        (* ignore (Printf.printf "fun: %s\n" fd.svar.vname); *)
        let base_dir = Goblintutil.create_dir (Fpath.v "cfgs") in
        let c_file_name = Str.global_substitute (Str.regexp Filename.dir_sep) (fun _ -> "%2F") loc.file in
        let dot_file_name = fd.svar.vname^".dot" in
        let file_dir = Goblintutil.create_dir Fpath.(base_dir / c_file_name) in
        let fname = Fpath.(file_dir / dot_file_name) in
        let out = open_out (Fpath.to_string fname) in
        let ppf = Format.formatter_of_out_channel out in
        fprint_fundec_html_dot (module Cfg : CfgBidir) live fd ppf;
        Format.pp_print_flush ppf ();
        close_out out
      | _ -> ()
    )


let getGlobalInits (file: file) : edges  =
  (* runtime with fast_global_inits: List: 36.25s, Hashtbl: 0.56s *)
  let inits = Hashtbl.create 13 in
  let fast_global_inits = get_bool "exp.fast_global_inits" in
  let rec doInit lval loc init is_zero =
    let initoffs offs init typ lval =
      doInit (addOffsetLval offs lval) loc init is_zero;
      lval
    in
    let rec all_index = function
      | Index (e,o) -> Index (all_array_index_exp, all_index o)
      | Field (f,o) -> Field (f, all_index o)
      | NoOffset -> NoOffset
    in
    let all_index (lh,offs) = lh, all_index offs in
    match init with
    | SingleInit exp ->
      let assign lval = (loc, Assign (lval, exp)) in
      (* This is an optimization so that we don't get n*m assigns for an array a[n][m].
         Instead, we get one assign for each distinct value in the array *)
      if not fast_global_inits then
        Hashtbl.add inits (assign lval) ()
      else if not (Hashtbl.mem inits (assign (all_index lval))) then
        Hashtbl.add inits (assign (all_index lval)) ()
      else
        ()
    | CompoundInit (typ, lst) ->
      let ntyp = match typ, lst with
        | TArray(t, None, attr), [] -> TArray(t, Some zero, attr) (* set initializer type to t[0] for flexible array members of structs that are intialized with {} *)
        | _, _ -> typ
      in
      ignore (foldLeftCompound ~implicit:true ~doinit:initoffs ~ct:ntyp ~initl:lst ~acc:lval)
  in
  let f glob =
    match glob with
    | GVar ({vtype=vtype; _} as v, init, loc) -> begin
        let init, is_zero = match init.init with
          | None -> makeZeroInit vtype, true
          | Some x -> x, false
        in
        doInit (var v) loc init is_zero
      end
    | _ -> ()
  in
  iterGlobals file f;
  let initfun = emptyFunction "__goblint_dummy_init" in
  (* order is not important since only compile-time constants can be assigned *)
  ({line = 0; file="initfun"; byte= 0; column = 0; endLine = -1; endByte = -1; endColumn = -1; synthetic = true}, Entry initfun) :: (BatHashtbl.keys inits |> BatList.of_enum)


let numGlobals file =
  let n = ref 0 in
  (* GVar Cannot have storage Extern or function type *)
  Cil.iterGlobals file (function GVar _ -> incr n | _ -> ());
  !n
