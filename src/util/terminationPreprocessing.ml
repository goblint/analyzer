open GoblintCil
(* module Z = Big_int_Z *)

module VarToStmt = Map.Make(CilType.Varinfo) (* maps varinfos (= loop counter variable) to the statement of the corresponding loop*)

let counter_ikind = IULongLong
let counter_typ = TInt (counter_ikind, [])
let min_int_exp =
  (* Currently only tested for IInt type, which is signed *)
  if Cil.isSigned counter_ikind then
    Const(CInt(Z.shift_left Cilint.mone_cilint ((bytesSizeOfInt counter_ikind)*8-1), IInt, None))
  else
    Const(CInt(Z.zero, counter_ikind, None))

class loopCounterVisitor lc (fd : fundec) = object(self)
  inherit nopCilVisitor

  (* Counter of variables inserted for termination *)
  val mutable vcounter = ref 0

  method! vfunc _ =
    vcounter := 0;
    DoChildren

  method! vstmt s =

    let specialFunction name =
      { svar  = makeGlobalVar name (TFun(voidType, Some [("exp", counter_typ, [])], false,[]));
        smaxid = 0;
        slocals = [];
        sformals = [];
        sbody = mkBlock [];
        smaxstmtid = None;
        sallstmts = [];
      } in

    let f_bounded  = Lval (var (specialFunction "__goblint_bounded").svar) in

    (* Yields increment expression e + 1 where the added "1" that has the same type as the expression [e].
       Using Cil.increm instead does not work for non-[IInt] ikinds. *)
    let increment_expression e =
      let et = typeOf e in
      let bop = PlusA in
      let one = Const (CInt (Cilint.one_cilint, counter_ikind, None)) in
      constFold false (BinOp(bop, e, one, et)) in

    let action s = match s.skind with
      | Loop (b, loc, eloc, _, _) ->
        let vname = "term" ^ string_of_int loc.line ^ "_" ^ string_of_int loc.column ^ "_id" ^ (string_of_int !vcounter) in
        incr vcounter;
        let v = Cil.makeLocalVar fd vname counter_typ in (*Not tested for incremental mode*)
        let lval = Lval (Var v, NoOffset) in
        let init_stmt = mkStmtOneInstr @@ Set (var v, min_int_exp, loc, eloc) in
        let inc_stmt = mkStmtOneInstr @@ Set (var v, increment_expression lval, loc, eloc) in
        let exit_stmt = mkStmtOneInstr @@ Call (None, f_bounded, [lval], loc, locUnknown) in
        b.bstmts <- exit_stmt :: inc_stmt :: b.bstmts;
        lc := VarToStmt.add (v: varinfo) (s: stmt) !lc;
        let nb = mkBlock [init_stmt; mkStmt s.skind] in
        s.skind <- Block nb;
        s
      | Goto (sref, l) ->
        let goto_jmp_stmt = sref.contents in
        let loc_stmt = Cilfacade.get_stmtLoc goto_jmp_stmt in
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
