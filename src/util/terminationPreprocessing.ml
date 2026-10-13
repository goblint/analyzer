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
      | _ -> s
    in ChangeDoChildrenPost (s, action);
end

(** A jump of a goto (or asm goto) statement [jump_stmt] at [jump_loc] to [target_stmt]. *)
type jump = {
  jump_stmt: stmt;
  jump_loc: location;
  target_stmt: stmt;
}

(** Collects the jumps of gotos into [jumps] and the continue statements of loops into [continue_stmts]. *)
class jumpCollector (jumps: jump list ref) (continue_stmts: stmt list ref) = object
  inherit nopCilVisitor

  method! vstmt s =
    let add_jump jump_loc target_ref =
      jumps := { jump_stmt = s; jump_loc; target_stmt = target_ref.contents } :: !jumps
    in
    begin match s.skind with
      | Loop (_, _, _, Some continue_stmt, _) -> continue_stmts := continue_stmt :: !continue_stmts
      | Goto (target_ref, loc) -> add_jump loc target_ref
      | Asm {gotos; loc; _} -> List.iter (add_jump loc) gotos
      | _ -> ()
    end;
    DoChildren
end

(** Whether [jump] goes backwards, i.e., to a statement with a smaller or equal [sid]. *)
let jumps_backwards (jump: jump): bool =
  jump.target_stmt.sid <= jump.jump_stmt.sid

(** Locations of the gotos in [fd] that jump backwards.
    Jumps to the continue statement of a loop are not included, since the loop counter bounds them.
    The CFG of [fd] must already be computed, as this relies on [sid]s and on the continue statements of loops introduced by [prepareCFG]. *)
let upjumping_gotos (fd: fundec): location list =
  let jumps = ref [] in
  let continue_stmts = ref [] in
  ignore (visitCilBlock (new jumpCollector jumps continue_stmts) fd.sbody);
  let continues_loop jump = List.memq jump.target_stmt !continue_stmts in
  let is_upjumping jump = jumps_backwards jump && not (continues_loop jump) in
  let location jump = jump.jump_loc in
  List.rev !jumps
  |> List.filter is_upjumping
  |> List.map location
