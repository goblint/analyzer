open GoblintCil

module VarToStmt = Map.Make(CilType.Varinfo) (* maps varinfos (= loop counter variable) to the statement of the corresponding loop*)

class loopCounterVisitor lc (fd : fundec) = object(self)
  inherit nopCilVisitor

  (* Counter of variables inserted for termination *)
  val mutable vcounter = ref 0

  method! vfunc _ =
    vcounter := 0;
    DoChildren

  method! vstmt s =

    let specialFunction name =
      { svar  = makeGlobalVar name (TFun(voidType, Some [("exp", intType, [])], false,[]));
        smaxid = 0;
        slocals = [];
        sformals = [];
        sbody = mkBlock [];
        smaxstmtid = None;
        sallstmts = [];
      } in

    let min_int_exp = Const(CInt(Cilint.shift_left_cilint Cilint.mone_cilint ((bytesSizeOfInt IInt)*8-1), IInt, None)) in

    let f_bounded  = Lval (var (specialFunction "__goblint_bounded").svar) in

    let action s = match s.skind with
      | Loop (b, loc, eloc, _, _) ->
        let vname = "term" ^ string_of_int loc.line ^ "_" ^ string_of_int loc.column ^ "-id" ^ (string_of_int !vcounter) in
        incr vcounter;
        let v = Cil.makeLocalVar fd vname Cil.intType in (*Not tested for incremental mode*)
        let init_stmt = mkStmtOneInstr @@ Set (var v, min_int_exp, loc, eloc) in
        let inc_stmt = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc, eloc) in
        let inc_stmt2 = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc, eloc) in
        let exit_stmt = mkStmtOneInstr @@ Call (None, f_bounded, [Lval (var v)], loc, locUnknown) in
        (match b.bstmts with
         | s :: ss ->   (*duplicate increment statement here to fix inconsistencies in nested loops*)
           b.bstmts <- exit_stmt :: inc_stmt :: s :: inc_stmt2 :: ss;
         | ss ->
           b.bstmts <- exit_stmt :: inc_stmt :: ss;
        );
        lc := VarToStmt.add (v: varinfo) (s: stmt) !lc;
        let nb = mkBlock [init_stmt; mkStmt s.skind] in
        s.skind <- Block nb;
        s
      | Goto (sref, l) ->
        let goto_jmp_stmt = sref.contents.skind in
        let loc_stmt = Cil.get_stmtLoc goto_jmp_stmt in
        if CilType.Location.compare l loc_stmt >= 0 then (
          (* is pos if first loc is greater -> below the second loc *)
          (* problem: the program might not terminate! *)
          let open Cilfacade in
          let current = FunLocH.find_opt funs_with_upjumping_gotos fd in
          let current = BatOption.default (LocSet.create 13) current in
          LocSet.replace current l ();
          FunLocH.replace funs_with_upjumping_gotos fd current;
        );
        s
      | _ -> s
    in ChangeDoChildrenPost (s, action);
end
