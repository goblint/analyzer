open Prelude
open Cil
open Formatcil
module ES = SetDomain.Make(Exp.Exp)

(** Instruments a program by inserting asserts either:
    - After an assignment to a variable (unless trans.assert.full is activated) and
    - At join points about all local variables

                OR

    - Only after pthread_mutex_lock (trans.assert.only-at-locks), about all locals and globals

    Limitations without trans.assert.only-at locks:
    - Currently only works for top-level variables (not inside an array, a struct, ...)
    - Does not work for accesses through pointers
    - At join points asserts all locals, but ideally should only assert ones that are
        modified in one of the branches

    Limitations in general:
    - Removes comments, so if the original program had //UNKNOWN assertions, the annotation
        will be removed and they will fail on the next iteration
*)

module EvalAssert = struct
  (* should asserts of conjuncts be one-by-one instead of one big assert?  *)
  let distinctAsserts = true

  (* should asserts be surrounded by __VERIFIER_atomic_{begin,end}? *)
  let surroundByAtomic = true

  (* Cannot use Cilfacade.name_fundecs as assert() is external and has no fundec *)
  let ass = makeVarinfo true "assert" (TVoid [])
  let atomicBegin = makeVarinfo true "__VERIFIER_atomic_begin" (TVoid [])
  let atomicEnd = makeVarinfo true "__VERIFIER_atomic_end" (TVoid [])


  (* Turns an expression into alist of conjuncts, pulling out common conjuncts from top-level disjunctions *)
  let rec pullOutCommonConjuncts e =
    let rec to_conjunct_set = function
      | BinOp(LAnd,e1,e2,_) -> ES.join (to_conjunct_set e1) (to_conjunct_set e2)
      | e -> ES.singleton e
    in
    let combine_conjuncts es = ES.fold (fun e acc -> match acc with | None -> Some e | Some acce -> Some (BinOp(LAnd,acce,e,Cil.intType))) es None in
    match e with
    | BinOp(LOr, e1, e2,t) ->
      let e1s = pullOutCommonConjuncts e1 in
      let e2s = pullOutCommonConjuncts e2 in
      let common = ES.inter e1s e2s in
      let e1s' = ES.diff e1s e2s in
      let e2s' = ES.diff e2s e1s in
      (match combine_conjuncts e1s', combine_conjuncts e2s' with
       | Some e1e, Some e2e -> ES.add (BinOp(LOr,e1e,e2e,Cil.intType)) common
       | _ -> common (* if one of the disjuncts is empty, it is equivalent to true here *)
      )
    | e -> to_conjunct_set e

  class visitor (ask:Cil.location -> Queries.ask) = object(self)
    inherit nopCilVisitor
    val full = GobConfig.get_bool "trans.assert.full"
    val only_at_locks = GobConfig.get_bool "trans.assert.only-at-locks"

    method! vstmt s =
      let is_lock exp args =
        match exp with
        | Lval(Var v,_) ->
          (match LibraryFunctions.classify v.vname args with
           | `Lock _ -> true
           | _ -> false)
        | _ -> false
      in

      let make_assert loc lval =
        let context:Queries.invariant_context = {
          scope=Cilfacade.find_stmt_fundec s;
          lval=lval;
          offset=Cil.NoOffset;
        } in
        match (ask loc).f (Queries.Invariant context) with
        | `Lifted e ->
          let es = if distinctAsserts then ES.elements (pullOutCommonConjuncts e) else [e] in
          let asserts = List.map (fun e -> cInstr ("%v:assert (%e:exp);") loc [("assert", Fv ass); ("exp", Fe e)]) es in
          if surroundByAtomic then
            let abegin = (cInstr ("%v:__VERIFIER_atomic_begin();") loc [("__VERIFIER_atomic_begin", Fv atomicBegin)]) in
            let aend = (cInstr ("%v:__VERIFIER_atomic_end();") loc [("__VERIFIER_atomic_end", Fv atomicEnd)]) in
            abegin :: (asserts @ [aend])
          else
            asserts
        | _ -> []
      in

      let instrument_instructions il s =
        (* Does this statement have a successor that has only on predecessor? *)
        let unique_succ = s.succs <> [] && (List.hd s.succs).preds |> List.length < 2 in
        let instrument i loc =
          let instrument' lval =
            let lval_arg = if full || only_at_locks then None else lval in
            make_assert loc lval_arg
          in
          match i with
          | Set  (lval, _, _, _) when not only_at_locks -> instrument' (Some lval)
          | Call (lval, _, _, _, _) when not only_at_locks -> instrument' lval
          | Call (_, exp, args, _, _) when is_lock exp args -> instrument' None
          | _ -> []
        in
        let rec instrument_instructions = function
          | i1 :: ((i2 :: _) as is) ->
            (* List contains successor statement, use location of successor for values *)
            let loc = get_instrLoc i2 in
            i1 :: ((instrument i1 loc) @ instrument_instructions is)
          | [i] when unique_succ ->
            (* Last statement in list *)
            (* Successor of it has only one predecessor, we can query for the value there *)
            let loc = get_stmtLoc (List.hd s.succs).skind in
            i :: (instrument i loc)
          | [i] when s.succs <> [] ->
            (* Successor has multiple predecessors, results may be imprecise but remain correct *)
            let loc = get_stmtLoc (List.hd s.succs).skind in
            i :: (instrument i loc)
          | x -> x
        in
        instrument_instructions il
      in

      let rec get_vars e =
        match e with
        | Lval (Var v, _) -> [v]
        | UnOp (_, e, _) -> get_vars e
        | BinOp (_, e1, e2, _) -> (get_vars e1) @ (get_vars e2)
        | _ -> []
      in

      let instrument_join s =
        match s.preds with
        | [p1; p2] when not only_at_locks ->
          (* exactly two predecessors -> join point, assert locals if they changed *)
          let join_loc = get_stmtLoc s.skind in
          (* Possible enhancement: It would be nice to only assert locals here that were modified in either branch if trans.assert.full is false *)
          let asserts = make_assert join_loc None in
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
          let vars = get_vars e in
          let asserts loc vs = if full then make_assert loc None else List.map (fun x -> make_assert loc (Some (Var x,NoOffset))) vs |> List.concat in
          let add_asserts block =
            if block.bstmts <> [] then
              let with_asserts =
                let b_loc = get_stmtLoc (List.hd block.bstmts).skind in
                let b_assert_instr = asserts b_loc vars in
                [cStmt "{ %I:asserts %S:b }" (fun n t -> makeVarinfo true "unknown" (TVoid [])) b_loc [("asserts", FI b_assert_instr); ("b", FS block.bstmts)]]
              in
              block.bstmts <- with_asserts
            else
              ()
          in
          if not only_at_locks then (add_asserts b1; add_asserts b2);
          s
        | _ -> s
      in
      ChangeDoChildrenPost (s, instrument_statement)
  end
  let transform (ask:Cil.location -> Queries.ask) file = begin
    visitCilFile (new visitor ask) file;
    let assert_filename = GobConfig.get_string "trans.output" in
    let oc = Stdlib.open_out assert_filename in
    dumpFile defaultCilPrinter oc assert_filename file; end
end
let _ = Transform.register "assert" (module EvalAssert)
