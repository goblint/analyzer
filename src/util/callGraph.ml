open Analyses

module Make (S: Spec): Spec =
struct
  include S

  module CGNode = Printable.Prod (CilType.Fundec) (S.C)
  module CGNS = Set.Make (CGNode)
  module CGNH = BatHashtbl.Make (CGNode)

  let callers: CGNS.t CGNH.t = CGNH.create 113

  let init marshal =
    S.init marshal;
    CGNH.clear callers

  let enter ctx lv f args =
    (*let () = Printf.printf "<<<<ENTER IN CAllGRAPH>>>>\n" in*) 
    let r = S.enter ctx lv f args in
    begin
      try
        let caller = (Node.find_fundec ctx.prev_node, ctx.context ()) in
        List.iter (fun (_, (fd:D.t)) ->
            let callee = (f, S.context f fd) in
            CGNH.modify_def CGNS.empty callee (CGNS.add caller) callers
            (* CGNH.add callers callee caller *)
          ) r
      with Ctx_failure _ ->
        ()
    end;
    
    (*CGNH.iter (fun callee callers' ->
        Printf.printf "<<<<CALLEE: %d >>>>\n" (CGNode.hash callee);
        CGNS.iter (fun caller ->
            Printf.printf "<<<<CALLER :D :D :D: %d >>>>\n" (CGNode.hash caller)
          ) callers'
      ) callers;*)
    r

  let write_dot () =
    (*let () = Printf.printf "<<<<WRITE IN CAllGRAPH>>>>\n" in *)
    let out = open_out "callgraph.dot" in
    Printf.fprintf out "digraph callgraph {\n";

    let written_nodes = CGNH.create 113 in
    let write_node cgnode =
      if not (CGNH.mem written_nodes cgnode) then (
        CGNH.replace written_nodes cgnode ();
        (*let () = Printf.printf "<<<<NEW node IN CAllGRAPH: %d has a label %s(%d)>>>>\n" (CGNode.hash cgnode) ((fst cgnode).svar.vname) (S.C.hash (snd cgnode)) in*) 
        Printf.fprintf out "\t%d [label=\"%s(%d)\"];\n" (CGNode.hash cgnode) ((fst cgnode).svar.vname) (S.C.hash (snd cgnode))
      )
    in

    CGNH.iter (fun callee callers' ->
        write_node callee;
        CGNS.iter write_node callers'
      ) callers;

    CGNH.iter (fun callee callers' ->
        CGNS.iter (fun caller ->
            (*let () = Printf.printf "<<<<NEW edge IN CAllGRAPH: %d -> %d>>>>\n" (CGNode.hash caller) (CGNode.hash callee) in *)
            Printf.fprintf out "\t%d -> %d;\n" (CGNode.hash caller) (CGNode.hash callee)
          ) callers'
      ) callers;

    Printf.fprintf out "}\n";
    (*let () = List.iter (fun (a, b) -> let text:string = CGNode.show a in Printf.printf "<<<<WRITTEN NODE: %s>>>>\n" text) (CGNH.to_list written_nodes) in *)

    close_out_noerr out

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | Parent -> let () = Printf.printf "<<<< ??? Queried ??? >>>>\n" in Result.top q
    | _ -> query ctx q

  let update_config fundec context =
    let performed_update = 
        match Hashtbl.find_opt PrecisionUtil.function_config fundec with
        | Some p -> p != (true, true, true, true)
        | None -> true in
    Hashtbl.replace PrecisionUtil.function_config fundec (true, true, true, true);
    PrecisionUtil.changed_configs := performed_update;
    Printf.printf "<<<< Should do something about function %s >>>>\n" (CilType.Fundec.show fundec)

  let finalize () =
    let marsh = S.finalize () in
    PrecisionUtil.changed_configs := false;
    (* Look in a hastbl(poorman set, fundec and context) if there are any asserts that base wants to be refined *)
    Hashtbl.iter update_config Base.failing_asserts;
    (* if yes, propagate(update some config hashtable - should have fundec -> activated int domains) them up the call graph using *)
    (* reanalyze - exception! *)
    if !PrecisionUtil.changed_configs then
      raise Refinement.RestartAnalysis
    else
      write_dot ();
    marsh

end