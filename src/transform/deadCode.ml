open Batteries
open GoblintCil
open GobConfig

(** Filter statements out of a block (recursively). CFG fields (prev/next,
    Loop continue/break) are no longer valid after calling. Returns true if
    anything is left in block, false if the block is now empty. Invariants:
    - f (goto label) ==> f (labelled stmt), i.e. if a goto statement is not
      filtered out, the target may not be filtered out either.
    - block may not contain switch statements. *)
let filter_map_block f (block : Cil.block) : bool =
  (* blocks and statements: modify in place, then return true if should be kept *)
  let rec impl_block block =
    block.bstmts <- List.filter impl_stmt block.bstmts;
    block.bstmts <> []
  and impl_stmt stmt =
    if not (f stmt) then false
    else
      match stmt.skind with
      | If (_, b1, b2, _, _) ->
        (* Filter statements (recursively) from the true and false blocks. Keep the
           resulting if statement should if either block should be kept. Be careful to not
           short-circuit, since call to impl_block b2 is always needed for side-effects *)
        let keep_b1, keep_b2 = impl_block b1, impl_block b2 in keep_b1 || keep_b2
      | Switch _ -> failwith "switch statements must be removed"
        (* Handling switch statements correctly would be very difficult; consider that
           the switch labels may be located within arbitrarily nested statements within
           the switch statement's block. *)
      | Loop (b, _, _, _, _) ->
        (* Filter statements from the body of a loop. Always keep the resulting loop, even if it
           is empty; an empty infinite loop is different from having nothing at all. *)
        impl_block b |> ignore; true
      | Block b ->
        (* Filter the statements inside the block,
           keep the block if it still contains any statements. *)
        impl_block b
      | Instr _ | Return _ | Goto _ | ComputedGoto _ | Break _ | Continue _ ->
        (* No further statements are contained recursively here, so nothing left to do. *)
        true
  in
  impl_block block


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
        (* Invariants of filter_map_block satisfied: switch statements removed by CFG transform,
           and a live label implies its target is live. Ignore the result of filter_map_block:
           even if the function body is now empty, the function may still be referenced
           (e.g. by taking its address) and should thus be retained. *)
        filter_map_block stmt_live fd.sbody |> ignore
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
