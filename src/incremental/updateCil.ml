open Cil
open CompareCIL
open MaxIdUtil
open MyCFG

module NodeMap = Hashtbl.Make(Node)

let location_map = ref (NodeMap.create 103: location NodeMap.t)

let getLoc (node: Node.t) =
  (* In case this belongs to a changed function, we will find the true location in the map*)
  try
    NodeMap.find !location_map node
  with Not_found ->
    Node.location node

let store_node_location (n: Node.t) (l: location): unit =
  NodeMap.add !location_map n l

let update_ids (old_file: file) (ids: max_ids) (new_file: file) (map: global GlobalMap.t) (changes: change_info) =
  let vid_max = ref ids.max_vid in
  let sid_max = ref ids.max_sid in

  let update_vid_max vid = update_id_max vid_max vid in
  let update_sid_max sid = update_id_max sid_max sid  in

  let make_vid () =
    incr vid_max;
    !vid_max
  in
  let make_sid () =
    incr sid_max;
    !sid_max
  in
  let override_fundec (target: fundec) (src: fundec) =
    target.sbody <- src.sbody;
    target.sallstmts <- src.sallstmts;
    target.sformals <- src.sformals;
    target.slocals <- src.slocals;
    target.smaxid <- src.smaxid;
    target.smaxstmtid <- src.smaxstmtid;
    target.svar <- src.svar;
  in
  let reset_fun (f: fundec) (old_f: fundec) =
    f.svar.vid <- old_f.svar.vid;
    List.iter2 (fun l o_l -> l.vid <- o_l.vid) f.slocals old_f.slocals;
    List.iter2 (fun lo o_f -> lo.vid <- o_f.vid) f.sformals old_f.sformals;
    List.iter2 (fun s o_s -> s.sid <- o_s.sid) f.sallstmts old_f.sallstmts;
    List.iter (fun s -> store_node_location (Statement s) (Cilfacade.get_stmtLoc s)) f.sallstmts;

    store_node_location (Function f) f.svar.vdecl;
    store_node_location (FunctionEntry f) f.svar.vdecl;
    override_fundec f old_f;
    List.iter (fun l -> update_vid_max l.vid) f.slocals;
    List.iter (fun f -> update_vid_max f.vid) f.sformals;
    List.iter (fun s -> update_sid_max s.sid) f.sallstmts;
    update_vid_max f.svar.vid;
  in
  let reset_var (v: varinfo) (old_v: varinfo)=
    v.vid <- old_v.vid;
    update_vid_max v.vid;
  in
  let reset_globals (glob: global) =
    try
      let old_glob = GlobalMap.find (CompareCFG.identifier_of_global glob) map in
      match glob, old_glob with
      | GFun (nw, _), GFun (old, _) -> reset_fun nw old
      | GVar (nw, _, _), GVar (old, _, _) -> reset_var nw old
      | GVarDecl (nw, _), GVarDecl (old, _) -> reset_var nw old
      | _ -> ()
    with Failure m -> ()
  in
  let assign_same_id fallstmts (old_n, n) = match old_n, n with
    | Statement old_s, Statement s -> if List.exists (fun s' -> Node.equal n (Statement s')) fallstmts then (s.sid <- old_s.sid; update_sid_max s.sid)
    | FunctionEntry old_f, FunctionEntry f -> f.svar.vid <- old_f.svar.vid; update_vid_max f.svar.vid
    | Function old_f, Function f -> f.svar.vid <- old_f.svar.vid; update_vid_max f.svar.vid
    | _ -> raise (Failure "Node tuple falsely classified as unchanged nodes")
  in
  let reset_changed_stmt (unchangedNodes: node list) s =
    if not (List.exists (fun n -> Node.equal n (Statement s)) unchangedNodes) then s.sid <- make_sid ()
  in
  let reset_changed_fun (f: fundec) (old_f: fundec) unchangedHeader (diff: nodes_diff option) =
    f.svar.vid <- old_f.svar.vid;
    update_vid_max f.svar.vid;
    if unchangedHeader then
      List.iter2 (fun f old_f -> f.vid <- old_f.vid; update_vid_max f.vid) f.sformals old_f.sformals
    else List.iter (fun f -> f.vid <- make_vid ()) f.sformals;
    (* diff is None if the function header changed or locals and the cfg was not compared. In this case, proceed as before
       and renew all ids of the function. Otherwise the function header and locals are unchanged and the cfg was compared.
       Then we can reset all ids of f's varinfo, its locals, formals and unchanged nodes and renew all ids of the remaining nodes*)
    match diff with
    | None -> List.iter (fun l -> l.vid <- make_vid ()) f.slocals;
      List.iter (fun s -> s.sid <- make_sid ()) f.sallstmts;
    | Some d -> List.iter2 (fun l o_l -> l.vid <- o_l.vid) f.slocals old_f.slocals;
      List.iter (fun l -> update_vid_max l.vid) f.slocals;
      (* Keeping this order when updating ids is very important since Node.equal in assign_same_id tests only
         for id equality. Otherwise some new nodes might not receive a new id and lead to duplicate ids in the
         respective function *)
      List.iter (reset_changed_stmt (List.map snd d.unchangedNodes)) f.sallstmts;
      List.iter (assign_same_id f.sallstmts) d.unchangedNodes
  in
  let reset_changed_globals (changed: changed_global) =
    match (changed.current, changed.old) with
    | GFun (nw, _), GFun (old, _) -> reset_changed_fun nw old changed.unchangedHeader changed.diff
    | _ -> ()
  in
  let update_fun (f: fundec) =
    f.svar.vid <- make_vid ();
    List.iter (fun l -> l.vid <- make_vid ()) f.slocals;
    List.iter (fun f -> f.vid <- make_vid ()) f.sformals;
    List.iter (fun s -> s.sid <- make_sid ()) f.sallstmts;
  in
  let update_var (v: varinfo) =
    v.vid <- make_vid ()
  in
  let update_globals (glob: global) =
    try
      let old_glob = GlobalMap.find (CompareCFG.identifier_of_global glob) map in
      match glob, old_glob with
      | GFun (nw, _), GFun (old, _) -> update_fun nw
      | GVar (nw, _, _), GVar (old, _, _) -> update_var nw
      | GVarDecl (nw, _), GVarDecl (old, _) -> update_var nw
      | _ -> ()
    with Failure m -> ()
  in
  List.iter reset_globals changes.unchanged;
  List.iter reset_changed_globals changes.changed;
  List.iter update_globals changes.added;

  (* Update the sid_max and vid_max *)
  Cil.iterGlobals new_file (update_max_ids ~sid_max ~vid_max);
  (* increment the sid so that the *unreachable* nodes that are introduced afterwards get unique sids *)
  while !sid_max > Cil.new_sid () do
    ()
  done;
  {max_sid = !sid_max; max_vid = !vid_max}
