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

  (** Statements of the function body in textual (pre-)order, computed on first use. *)
  val stmts_in_order = lazy (
    let stmts = ref [] in
    let collector = object
      inherit nopCilVisitor
      method! vstmt s =
        stmts := s :: !stmts;
        DoChildren
    end
    in
    ignore (visitCilBlock collector fd.sbody);
    List.rev !stmts
  )

  (** Position of [stmt] in the textual order of the function body, if it occurs there. *)
  method private position (stmt: stmt): int option =
    BatList.index_ofq stmt (Lazy.force stmts_in_order)

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

    (** Whether the jump from [jump_stmt] at [jump_loc] to [target_stmt] goes backwards.
        Jumps synthesized by CIL (e.g., for [&&] outside of conditions) may have the same location as their target;
        then, the textual order of the statements decides. A jump to itself goes backwards. *)
    let jumps_backwards jump_stmt jump_loc target_stmt =
      let target_precedes_jump () =
        match self#position target_stmt, self#position jump_stmt with
        | Some i_target, Some i_jump -> i_target <= i_jump
        | _ -> true (* conservatively *)
      in
      let c = CilType.Location.compare jump_loc (Cilfacade.get_stmtLoc target_stmt) in
      c > 0 || (c = 0 && target_precedes_jump ())
    in

    let action_goto jump_stmt jump_loc target_ref =
      if jumps_backwards jump_stmt jump_loc target_ref.contents then (
        (* problem: the program might not terminate! *)
        let open Cilfacade in
        let current = FunLocH.find_opt funs_with_upjumping_gotos fd in
        let current = BatOption.default (LocSet.create 13) current in
        LocSet.replace current jump_loc ();
        FunLocH.replace funs_with_upjumping_gotos fd current;
      )
    in

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
        action_goto s l sref;
        s
      | Asm {gotos; loc; _} ->
        List.iter (action_goto s loc) gotos;
        s
      | _ -> s
    in ChangeDoChildrenPost (s, action);
end
