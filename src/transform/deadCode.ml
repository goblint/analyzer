open Batteries
open GoblintCil
open GobConfig
open MyCFG

let dpf fmt = Pretty.gprintf (fun doc -> print_endline @@ Pretty.sprint ~width:Int.max_num doc) fmt

let empty_block () = { battrs = [] ; bstmts = [] }

(** Filter statements out of a block (recursively). CFG fields (prev/next,
    Loop continue/break) are no longer valid after calling. Returns true if
    anything is left in block, false if the block is now empty. Invariants:
    - f (goto label) ==> f (labelled stmt), i.e. if a goto statement is not
      filtered out, the target may not be filtered out either.
    - block may not contain switch statements. *)
let filter_map_block ?(keep_empty_loops = true) f (block : Cil.block) : bool =
  (* blocks and statements: modify in place, then return true if should be kept *)
  let rec impl_block block =
    block.bstmts <- List.filter impl_stmt block.bstmts;
    block.bstmts <> []
  and impl_stmt stmt =
    let keep_stmt = f stmt in
    dpf "id=%d loc=%a keep=%b\n%a\n"
      stmt.sid CilType.Location.pretty (Cilfacade.get_stmtLoc stmt)
      keep_stmt
      Node.pretty_plain (Statement stmt);
    let skind'_opt = match stmt.skind with
    | If (cond, b_true, b_false, loc, exp_loc) ->
      (* Filter statements (recursively) from the true and false blocks.
      
          TODOC: Keep the
          resulting if statement if either block should be kept. Be careful to not
          short-circuit, since call to impl_block b2 is always needed for side-effects *)
      let keep_b_true, keep_b_false = impl_block b_true, impl_block b_false in
      let keep_both, keep_one = keep_b_true && keep_b_false, keep_b_true || keep_b_false in
      
      if keep_both || (keep_stmt && keep_one) then
        Option.some @@ If (
          (if keep_stmt then cond else GoblintCil.integer 1),
          (if keep_b_true then b_true else empty_block ()),
          (if keep_b_false then b_false else empty_block ()),
          loc, exp_loc)
      else if keep_one then
        Option.some @@ Block (if keep_b_true then b_true else b_false)
      else
        None

(*       begin match keep_stmt, keep_b_true, keep_b_false with
      (* both blocks *)
      | true, true, true -> Option.some @@ If (cond, b_true, b_false, loc, exp_loc)
      | false, true, true -> Option.some @@ If (GoblintCil.integer 1, b_true, b_false, loc, exp_loc)

      (* one block, keep outer if *)
      | true, true, false -> Option.some @@ If (cond, b_true, empty_block, loc, exp_loc)
      | true, false, true -> Option.some @@ If (cond, empty_block, b_false, loc, exp_loc) (* TODO: negate cond and swap blocks? *)

      (* one block, discard outer if *)
      | false, true, false -> Option.some @@ Block b_true
      | false, false, true -> Option.some @@ Block b_false

      (* no blocks *)
      | _, false, false -> None
      end *)

    | Switch _ ->
      (* Handling switch statements correctly would be very difficult; consider that
          the switch labels may be located within arbitrarily nested statements within
          the switch statement's block. *)
      failwith "switch statements must be removed"(* TODO: fail on more unsupported constructs *)

    | Loop (body, loc, exp_loc, _, _) ->
      (* TODOC: Filter statements from the body of a loop. Always keep the resulting loop, even if it
          is empty; an empty infinite loop is different from having nothing at all. *)
      let keep_body = impl_block body in

      if keep_stmt then
        Option.some @@ Loop (
          (if keep_body then body else empty_block ()),
          loc, exp_loc, None, None)
      else if keep_body then Option.some @@ Block body
      else None

      (* begin match keep_stmt, keep_body with
      | true, true -> Option.some @@ Loop (body, loc, exp_loc, None, None)
      | false, true -> Option.some @@ Block body
      | true, false -> Option.some @@ Loop (empty_block, loc, exp_loc, None, None)
      | false, false -> None
      end *)
    | Block b as skind ->
      (* Filter the statements inside the block,
          keep the block if it still contains any statements. *)
      let keep_block = impl_block b in
      if keep_stmt || keep_block then Some skind else None
    | Instr _ | Return _ | Goto _ | ComputedGoto _ | Break _ | Continue _ as skind ->
      (* No further statements are contained recursively here, so nothing left to do. *)
      if keep_stmt then Some skind else None
    in
    Stdlib.Option.iter (fun skind' -> stmt.skind <- skind') skind'_opt;
    (* TODO: might want to preserve statement (replace with empty) instead of removing entirely, to preserve attributes (labels should be OK) *)
    Option.is_some skind'_opt
  in
  impl_block block

(* module StmtH = BatHashtbl.Make(CilType.Stmt) *)

let find_dead_statements (node_live : node -> bool) (skipped_statements : node -> edges -> node -> stmt list) (cfg : cfg) (start : node) =
  let seen_nodes = NodeH.create 13 in
  let module IS = BatSet.Int in
  let unions = List.fold_left IS.union IS.empty in
  let node_id_set = function
    | Statement stmt -> IS.singleton stmt.sid
    | _ -> IS.empty

  in
  let rec impl (n : node) (ns : node list) (* (seen_stmts : IS.t) *) (live_stmts : IS.t) =
    NodeH.replace seen_nodes n ();
    let ns', (* seen_stmts', *) live_stmts' =
      cfg n |> List.fold_left
        (fun (ns0, (* seen_stmts0, *) live_stmts0) (edges, n') ->
          let edge_stmt_ids =
            IS.union
              (skipped_statements n edges n' |> List.map (fun stmt -> stmt.sid) |> IS.of_list)
              (node_id_set n')
          in
          let __l = node_live n' in
          dpf"from=%s to=%s live=%b sids=[%s]" (Node.show_id n) (Node.show_id n') __l (IS.to_list edge_stmt_ids |> List.map string_of_int |> String.join ",");
          (if NodeH.mem seen_nodes n' then ns0 else n' :: ns0),
          (* IS.union seen_stmts0 edge_stmt_ids, *)
          (if __l then IS.union live_stmts0 edge_stmt_ids else live_stmts0)
          )
        (ns, (* seen_stmts, *) live_stmts)
        (* (match n with
        | Statement stmt -> ns, BatSet.Int.add stmt.sid seen_stmts, if node_live n then BatSet.Int.add stmt.sid live_stmts else live_stmts
        | _ -> ns, seen_stmts, live_stmts) *)
    in
    match ns' with
    | [] -> (* seen_stmts', *) live_stmts'
    | n' :: ns'_tail -> impl n' ns'_tail (* seen_stmts' *) live_stmts'

  in

  let (* seen, *) live = impl start [] (* (node_id_set start) *) (if node_live start then node_id_set start else IS.empty) in
  (* (match start with Function fd | FunctionEntry fd -> fd.svar.vname | _ -> "???") ^ ": " ^ (BatSet.Int.to_list seen
    |> List.map (fun sid -> Printf.sprintf (if BatSet.Int.mem sid live then "%d" else "%d(dead)") sid)
    |> String.concat ",") |> print_endline
  ; *)
  (* BatSet.Int.diff seen live *)
  (* seen, *) live


module RemoveDeadCode : Transform.S = struct
  let transform (q : Transform.queries) (file : file) : unit =

    (* whether a statement (might) still be live, and should therefore be kept *)
    let stmt_live : stmt -> bool = not % q.must_be_dead in

    (* whether a global function (might) still be live, and should therefore be kept *)
    let fundec_live : fundec -> bool = not % q.must_be_uncalled in

    (* Step 1: Remove statements found to be dead. *)
    Cil.iterGlobals file
      (function
        | GFun (fd, _) ->
          dpf "=== begin %s ===\n" fd.svar.vname;
          let (* seen, *) live = find_dead_statements (not % q.must_be_dead_node) q.skipped_statements q.cfg_forward (FunctionEntry fd) in
          print_endline "";
          (* let dead_stmts = BatSet.Int.diff seen live in *)
          (* TODOC: Invariants of filter_map_block satisfied: switch statements removed by CFG transform,
             and a live label implies its target is live. Ignore the result of filter_map_block:
             even if the function body is now empty, the function may still be referenced
             (e.g. by taking its address) and should thus be retained. *)
          (* if fundec_live fd then *)
          filter_map_block ~keep_empty_loops:false (fun stmt -> (* not @@ BatSet.Int.mem stmt.sid dead_stmts *) BatSet.Int.mem stmt.sid live) fd.sbody |> ignore;
          (* else
            fd.sbody <- empty_block (); (* TODO: why are nodes within uncalled functions live? maybe because they don't appear in the result at all? ~~but that isn't the case~~ yes it is *) *)
          dpf "=== end %s ===" fd.svar.vname;
        | _ -> ());

    (* Step 2: Remove globals that are (transitively) unreferenced by live functions. Dead functions
       and globals are removed, since there is no syntactic reference to them in [main], or any of
       the globals referenced by main, or the globals referenced by those globals, and so on. *)
    let open GoblintCil.Rmtmps in
    let keepUnused0 = !keepUnused in
    Fun.protect ~finally:(fun () -> keepUnused := keepUnused0) (fun () ->
        keepUnused := false;
        removeUnusedTemps ~isRoot:(function GFun (fd, _) -> fundec_live fd | _ -> false) file
      )

  let name = "remove_dead_code"

  let requires_file_output = true

end

let _ = Transform.register (module RemoveDeadCode)
