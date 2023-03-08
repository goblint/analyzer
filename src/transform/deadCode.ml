open Batteries
open GoblintCil


(** Filter statements out of a block (recursively). CFG fields (prev, next, etc.) are no longer valid after calling.
    Returns true if anything is left in block, false if the block is now empty.
    Invariants:
    - f (goto label) ==> f (labelled stmt), i.e. if a goto statement is not filtered out, the target may not be filtered out either.
    - block may not contain switch statements *)
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
        (* be careful to not short-circuit, since call to impl_block b2 is always needed for side-effects *)
        let keep_b1, keep_b2 = impl_block b1, impl_block b2 in keep_b1 || keep_b2
      | Switch _ -> failwith "switch statements must be removed"
        (* handling switch statements correctly would be very difficult; consider that the switch
        labels may be located within arbitrarily nested statements within the switch statement's block *)
      | Loop (b, _, _, _, _) ->
        impl_block b
      | Block b ->
        impl_block b
      | sk -> true
  in
  impl_block block


module RemoveDeadCode : Transform.S = struct
  let transform (q : Transform.queries) (file : file) : unit =

    (* whether a statement (might) still be live, and should therefore be kept *)
    let stmt_live : stmt -> bool = not % q.must_be_dead in

    (* whether a global function (might) still be live, and should therefore be kept *)
    let fundec_live : fundec -> bool = not % q.must_be_uncalled in

    (* step 1: remove statements found to be dead *)
    Cil.iterGlobals file
      (function
      | GFun (fd, _) ->
        (* invariants of filter_map_block satisfied: switch statements removed by CFG transform,
           and a live label implies its target is live *)
        filter_map_block stmt_live fd.sbody |> ignore
      | _ -> ());

    (* step 2: remove globals that are (transitively) unreferenced by live functions
       - dead functions: removed by answering 'false' for isRoot
       - unreferenced globals and types: removeUnusedTemps keeps only globals referenced by live functions *)
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
