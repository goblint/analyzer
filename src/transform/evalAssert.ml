open Prelude
open GoblintCil
open Formatcil

(** Instruments a program by inserting asserts either:
    - After an assignment to a variable (unless witness.invariant.full is activated) and
    - At join points about all local variables

                OR

    - Only after pthread_mutex_lock (witness.invariant.after-lock), about all locals and globals

    Limitations without witness.invariant.after-lock:
    - Currently only works for top-level variables (not inside an array, a struct, ...)
    - Does not work for accesses through pointers
    - At join points asserts all locals, but ideally should only assert ones that are
        modified in one of the branches

    Limitations in general:
    - Removes comments, so if the original program had //UNKNOWN assertions, the annotation
        will be removed and they will fail on the next iteration
*)

module EvalAssert = struct
  (* should asserts be surrounded by __VERIFIER_atomic_{begin,end}? *)
  let surroundByAtomic = true

  let atomicBegin = makeVarinfo true "__VERIFIER_atomic_begin" (TVoid [])
  let atomicEnd = makeVarinfo true "__VERIFIER_atomic_end" (TVoid [])


  class visitor (ask: ?node:Node.t -> Cil.location -> Queries.ask) = object(self)
    inherit nopCilVisitor
    val full = GobConfig.get_bool "witness.invariant.full"
    (* TODO: handle witness.invariant.loop-head *)
    val emit_after_lock = GobConfig.get_bool "witness.invariant.after-lock"
    val emit_other = GobConfig.get_bool "witness.invariant.other"
    (* variable for generating __goblint_check(exp) instead of __VERIFIER_assert(exp) *)
    val goblintCheck = GobConfig.get_bool "trans.goblint-check"

    method! vstmt s =
      let is_lock exp args =
        match exp with
        | Lval(Var v,_) when LibraryFunctions.is_special v ->
          let desc = LibraryFunctions.find v in
          (match desc.special args with
           | Lock _ -> true
           | _ -> false)
        | _ -> false
      in

      (* Cannot use Cilfacade.name_fundecs as assert() is external and has no fundec *)
      (* Moved in method because Options can not be checked in the module *)
      let ass = if goblintCheck then makeVarinfo true "__goblint_check" (TVoid []) else makeVarinfo true "__VERIFIER_assert" (TVoid []) in

      let make_assert ~node loc lval =
        let lvals = match lval with
          | None -> CilLval.Set.top ()
          | Some lval -> CilLval.(Set.singleton lval)
        in
        let context = {Invariant.default_context with lvals} in
        match (ask ~node loc).f (Queries.Invariant context) with
        | `Lifted e ->
          let es = WitnessUtil.InvariantExp.process_exp e in
          let asserts = List.map (fun e -> cInstr ("%v:assert (%e:exp);") loc [("assert", Fv ass); ("exp", Fe e)]) es in
          if surroundByAtomic && not goblintCheck then
            let abegin = (cInstr ("%v:__VERIFIER_atomic_begin();") loc [("__VERIFIER_atomic_begin", Fv atomicBegin)]) in
            let aend = (cInstr ("%v:__VERIFIER_atomic_end();") loc [("__VERIFIER_atomic_end", Fv atomicEnd)]) in
            abegin :: (asserts @ [aend])
          else
            asserts
        | _ -> []
      in

      let instrument_instructions il s =
        let instrument ~node i loc =
          let instrument' lval =
            let lval_arg = if full then None else lval in
            make_assert ~node loc lval_arg
          in
          match i with
          | Call (_, exp, args, _, _) when emit_after_lock && is_lock exp args -> instrument' None
          | Set  (lval, _, _, _) when emit_other -> instrument' (Some lval)
          | Call (lval, _, _, _, _) when emit_other -> instrument' lval
          | _ -> []
        in
        let instrument_instructions il = match il, s.succs with
          | [], _ -> []
          | [i], [succ] -> (* Single successor *)
            let stmt = List.hd s.succs in
            let loc = Cilfacade.get_stmtLoc stmt in
            let node = Node.Statement stmt in
            i :: (instrument ~node i loc)
          | [i], [] ->
            (* No successor; do not add an assertion *)
            [i]
          | [_], _ :: (_ :: _) ->
            (* More than one successor means that there is some branching is happening, that is not expected for an instruction. *)
            failwith "Instruction has more than one successor; at most one is expected."
          | _ :: (_ :: _), _ ->
            failwith "There were multiple instructions in one statement, but only one is expected."
        in
        instrument_instructions il
      in

      let instrument_join s =
        match s.preds with
        | [p1; p2] when emit_other ->
          (* exactly two predecessors -> join point, assert locals if they changed *)
          let join_loc = Cilfacade.get_stmtLoc s in
          (* Possible enhancement: It would be nice to only assert locals here that were modified in either branch if witness.invariant.full is false *)
          let asserts = make_assert ~node:(Node.Statement s) join_loc None in
          self#queueInstr asserts; ()
        | _ -> ()
      in

      let instrument_statement s =
        instrument_join s;
        match s.skind with
        | Instr il ->
          s.skind <- Instr (instrument_instructions il s);
          s
        | If (e, b1, b2, l,l2) ->
          let vars = Basetype.CilExp.get_vars e in
          let asserts ~node loc vs = if full then make_assert ~node loc None else List.map (fun x -> make_assert ~node loc (Some (Var x,NoOffset))) vs |> List.concat in
          let add_asserts block =
            if block.bstmts <> [] then
              let with_asserts =
                let stmt = List.hd block.bstmts in
                let node = Node.Statement stmt in
                let b_loc = Cilfacade.get_stmtLoc stmt in
                let b_assert_instr = asserts ~node b_loc vars in
                [cStmt "{ %I:asserts %S:b }" (fun n t -> makeVarinfo true "unknown" (TVoid [])) b_loc [("asserts", FI b_assert_instr); ("b", FS block.bstmts)]]
              in
              block.bstmts <- with_asserts
            else
              ()
          in
          if emit_other then (add_asserts b1; add_asserts b2);
          s
        | _ -> s
      in
      ChangeDoChildrenPost (s, instrument_statement)
  end

  let transform (q : Transform.queries) file =
    visitCilFile (new visitor q.ask) file;

    (* Add function declarations before function definitions.
       This way, asserts may reference functions defined later. *)
    Cilfacade.add_function_declarations file

  let name = "assert"

  let requires_file_output = true

end
let _ = Transform.register (module EvalAssert)
