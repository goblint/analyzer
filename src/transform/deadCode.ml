open GoblintCil


let f = Printf.sprintf
let pf fmt = Printf.ksprintf print_endline fmt
let df fmt = Pretty.gprintf (Pretty.sprint ~width:max_int) fmt
let dpf fmt = Pretty.gprintf (fun doc -> print_endline @@ Pretty.sprint ~width:max_int doc) fmt

(* what to do about goto to removed statements? probably just check if the target of a goto should be removed, if so remove the goto? <- don't do that.
   but if the target is not live, the goto can't be live anyway *)

(** Filter statements out of a block (recursively). *)
let filter_map_block f (block : Cil.block) : bool =
  (* blocks and statements: modify in place, then return true if should be kept *)
  let rec impl_block block =
    block.bstmts <- List.filter impl_stmt block.bstmts;
    block.bstmts <> []
  and impl_stmt stmt =
    if not (f stmt) then false
    else
      let skind', keep =
      (* TODO: if sk is not changed in the end, simplify here *)
        match stmt.skind with
        | If (_, b1, b2, _, _) as sk ->
          (* be careful to not short-circuit, since call to impl_block b2 is always needed for side-effects *)
          sk, let keep_b1, keep_b2 = impl_block b1, impl_block b2 in keep_b1 || keep_b2
        | Switch (e, b, stmts, l1, l2) ->
          (* TODO: why a block and a statement list in a Switch? *)
          let keep_b = impl_block b in
          let stmts' = List.filter impl_stmt stmts in
          Switch (e, b, stmts', l1, l2), keep_b || stmts' <> []
        | Loop (b, _, _, _, _) as sk ->
          sk, impl_block b (* TODO: remove the stmt options in sk? since they point to possibly removed statements *)
        | Block b as sk ->
          sk, impl_block b
        | sk -> sk, true
      in
      stmt.skind <- skind';
      keep
    in
    impl_block block


module RemoveDeadCode : Transform.S = struct
  let transform (ask : ?node:Node.t -> Cil.location -> Queries.ask) (file : file) : unit =
    let open DeadcodeTransform in (* TODO inline into this file, if still needed *)

    (* TODO *)
    let liveness _ = true in
    let module Result = struct let mem _ _ = false end in
    let local_xml = () in
    let is_uncalled ?bad_only _ _ = false in

    (* step 1: remove statements found to be dead *)
    (* TODO: the marking of statements as live/not live doesn't seem to be completely accurate
       current fix: only eliminate statements *that are in the result (local_xml)* and marked live, this should be the correct behaviour *)
    Cil.iterGlobals file (function
    | GFun (fd, _) ->
      pf "global name=%s" fd.svar.vname;
      let keep =
        filter_map_block
          (fun stmt ->
            let cfgStmt = MyCFG.Statement stmt in
            let live = liveness cfgStmt in
            let in_result = Result.mem local_xml cfgStmt in
            dpf "<%a> live=%b in_result=%b" Node.pretty_plain cfgStmt live in_result;
            not in_result || live)
            (* match stmt.skind with
            | If _ | Switch _ | Loop _ | Block _ -> true (* compound statements are sometimes marked not live even though they contain live statements *)
            | _ -> live) *)
          fd.sbody
      in
      pf "keep=%b" keep; (* TODO: use keep? discard function if keep is false. should not be necessary, function should be dead already *)
    | _ -> ());

    (* step 2: remove function globals found to be dead *)
    file.globals <-
      List.filter
        (function
        | GFun (fd, l) -> not (is_uncalled ~bad_only:false fd.svar l)
        | _ -> true)
        file.globals;

    (* step 3: track dependencies between globals *)
    let refsVisitor = new globalReferenceTrackerVisitor in
    Cil.visitCilFileSameGlobals (refsVisitor :> Cil.cilVisitor) file;

    (* step 4: find globals referenced by remaining (live) functions and remove them *)
    let live_globinfo =
      find_live_globinfo'
        (file.globals |> List.to_seq |> Seq.filter (function GFun _ -> true | _ -> false))
        (refsVisitor#get_references_raw ())
    in
    file.globals <-
      List.filter
        (fun g -> match globinfo_of_global g with
        | Some gi -> GlobinfoH.mem live_globinfo gi
        | None -> true (* dependencies for some types of globals (e.g. assembly) are not tracked, always keep them *)
        )
      file.globals;

end

let _ = Transform.register "remove_dead_code" (module RemoveDeadCode)
