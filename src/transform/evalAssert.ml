open Prelude
open Cil
open Formatcil

(** Instruments a program by inserting asserts:
      - After an assignment to a variable
      - At join points about all local variables

    Limitations:
      - Currently only works for top-level variables (not inside an array, a struct, ...)
      - Does not work for accesses through pointers
      - At join points asserts all locals, but ideally should only assert ones that are
        modified in one of the branches
      - Removes comments, so if the original program had //UNKNOWN assertions, the unknown
        will be removed and they will fail on the next iteration
*)
module EvalAssert = struct
  (* Cannot use Cilfacade.name_fundecs as assert() is external and has no fundec *)
  let ass = ref (makeVarinfo true "assert" (TVoid []))
  let separateLands = true

  let rec separateLand = function
    | BinOp(LAnd,e1,e2,_) -> separateLand e1 @ separateLand e2
    | e -> [e]

  let locals = ref []
  class visitor (ask:Cil.location -> Queries.ask) = object(self)
    inherit nopCilVisitor
    val full = GobConfig.get_bool "trans.assert.full"

    method! vfunc f =
      locals := !locals @ f.slocals;
      DoChildren

    method! vglob g = match g with
      | GVarDecl (v, l) ->
        if v.vname = "assert" then begin
          ass := v;
          SkipChildren end
        else DoChildren
      | _ -> DoChildren

    method! vstmt s =
      let make_assert loc lval =
        try
          let context: Invariant.context = {
            scope=Cilfacade.find_stmt_fundec s;
            i= -1; (* Not used here *)
            lval=lval;
            offset=Cil.NoOffset;
            deref_invariant=(fun _ _ _ -> Invariant.none)
          } in

          let res = (ask loc).f (Queries.Invariant context) in
          if Queries.ES.is_bot res || Queries.ES.is_top res then
            []
          else
            let e = Queries.ES.choose res in
            let es = if separateLands then separateLand e else [e] in
            List.map (fun e -> cInstr ("%v:assert (%e:exp);") loc [("assert", Fv !ass); ("exp", Fe e)]) es
        with
          Not_found -> []
      in

      let rec instrument_instructions il s = match il with
        | i1 :: i2 :: is ->
          begin
            match i1 with
            | Set (lval, _, _, _)
            | Call (Some lval, _, _, _, _) ->
              let a = if full then (make_assert (get_instrLoc i2) None) else (make_assert (get_instrLoc i2) (Some lval)) in
              [i1] @ a @ instrument_instructions (i2 :: is) s
            | _ -> i1 :: instrument_instructions (i2 :: is) s
          end
        | [i] ->
          if List.length s.succs > 0 && (List.hd s.succs).preds |> List.length < 2 then begin
            (* If the successor of this has more than one predecessor, it is a join point, and we can not query for the value there *)
            let loc = get_stmtLoc (List.hd s.succs).skind in
            match i with
            | Set (lval, _, _, _)
            | Call (Some lval, _, _, _, _) ->
              let a = if full then make_assert loc None else make_assert loc (Some lval) in
              [i] @ a
            | _ -> [i]
          end
          else [i]
        | _ -> []
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
        | [p1; p2] ->
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
            if List.length block.bstmts > 0 then
              let with_asserts =
                let b_loc = get_stmtLoc (List.hd block.bstmts).skind in
                let b_assert_instr = asserts b_loc vars in
                [cStmt "{ %I:asserts %S:b }" (fun n t -> makeVarinfo true "unknown" (TVoid [])) b_loc [("asserts", FI b_assert_instr); ("b", FS block.bstmts)]]
              in
              block.bstmts <- with_asserts
            else
              ()
          in

          add_asserts b1;
          add_asserts b2;
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
