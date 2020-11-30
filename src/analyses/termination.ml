(** Termination of loops. *)

open Prelude.Ana
open Analyses

module M = Messages
let (%?) = Option.bind
let (||?) a b = match a,b with Some x,_ | _, Some x -> Some x | _ -> None

module TermDomain = struct
  include SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All Variables" end)
end

class loopCounterVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor
  method! vstmt s =
    let action s = match s.skind with
      | Loop (b, loc, _, _) ->
        (* insert loop counter variable *)
        let name = "term"^string_of_int loc.line in
        let typ = intType in (* TODO the type should be the same as the one of the original loop counter *)
        let v = Goblintutil.create_var (makeLocalVar fd name ~init:(SingleInit zero) typ) in
        (* make an init stmt since the init above is apparently ignored *)
        let init_stmt = mkStmtOneInstr @@ Set (var v, zero, loc) in
        (* increment it every iteration *)
        let inc_stmt = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc) in
        b.bstmts <- inc_stmt :: b.bstmts;
        let nb = mkBlock [init_stmt; mkStmt s.skind] in
        s.skind <- Block nb;
        s
      | _ -> s
    in ChangeDoChildrenPost (s, action)
end

let loopBreaks : (int, location) Hashtbl.t = Hashtbl.create 13 (* break stmt sid -> corresponding loop *)
class loopBreaksVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor
  method! vstmt s =
    (match s.skind with
     | Loop (b, loc, Some continue, Some break) -> Hashtbl.add loopBreaks break.sid loc
     | Loop _ -> failwith "Termination.preprocess: every loop should have a break and continue stmt after prepareCFG"
     | _ -> ());
    DoChildren
end

(* if the given block contains a goto while_break.* we have the termination condition for a loop *)
let exits = function
  | { bstmts = [{ skind = Goto (stmt, loc); _ }]; _ } -> Hashtbl.find_option loopBreaks !stmt.sid
  | _ -> None (* TODO handle return (need to find out what loop we are in) *)

let lvals_of_expr =
  let rec f a = function
    | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ | AddrOfLabel _ -> a
    | Lval l | AddrOf l | StartOf l -> l :: a
    | SizeOfE e | AlignOfE e | UnOp (_,e,_) | CastE (_,e) | Imag e | Real e -> f a e
    | BinOp (_,e1,e2,_) -> f a e1 @ f a e2
    | Question (c,t,e,_) -> f a c @ f a t @ f a e
  in f []

let loopVars : (location, lval) Hashtbl.t = Hashtbl.create 13 (* loop location -> lval used for exit *)
class loopVarsVisitor (fd : fundec) = object
  inherit nopCilVisitor
  method! vstmt s =
    let add_exit_cond e loc =
      match lvals_of_expr e with
      | [lval] when typeOf e |> isArithmeticType -> Hashtbl.add loopVars loc lval
      | _ -> ()
    in
    (match s.skind with
     | If (e, tb, fb, loc) -> Option.map_default (add_exit_cond e) () (exits tb ||? exits fb)
     | _ -> ());
    DoChildren
end

let stripCastsDeep e =
  let v = object
    inherit nopCilVisitor
    method! vexpr e = ChangeTo (stripCasts e)
  end
  in visitCilExpr v e

(* keep the enclosing loop for statements *)
let cur_loop = ref None (* current loop *)
let cur_loop' = ref None (* for nested loops *)
let makeVar fd loc name =
  let id = name ^ "__" ^ string_of_int loc.line in
  try List.find (fun v -> v.vname = id) fd.slocals
  with Not_found ->
    let typ = intType in (* TODO the type should be the same as the one of the original loop counter *)
    Goblintutil.create_var (makeLocalVar fd id ~init:(SingleInit zero) typ)
let f_commit = Lval (var (emptyFunction "__goblint_commit").svar)
let f_check  = Lval (var (emptyFunction "__goblint_check").svar)
class loopInstrVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor
  method! vstmt s =
    (match s.skind with
     | Loop (_, loc, _, _) ->
       cur_loop' := !cur_loop;
       cur_loop := Some loc
     | _ -> ());
    let action s =
      (* first, restore old cur_loop *)
      (match s.skind with
       | Loop (_, loc, _, _) ->
         cur_loop := !cur_loop';
       | _ -> ());
      let in_loop () = Option.is_some !cur_loop && Hashtbl.mem loopVars (Option.get !cur_loop) in
      match s.skind with
      | Loop (b, loc, Some continue, Some break) when Hashtbl.mem loopVars loc ->
        (* find loop var for current loop *)
        let x = Hashtbl.find loopVars loc in
        (* insert loop counter and diff to loop var *)
        let t = var @@ makeVar fd loc "t" in
        let d1 = var @@ makeVar fd loc "d1" in
        let d2 = var @@ makeVar fd loc "d2" in
        (* make init stmts *)
        let t_init = mkStmtOneInstr @@ Set (t, zero, loc) in
        let d1_init = mkStmtOneInstr @@ Set (d1, Lval x, loc) in
        let d2_init = mkStmtOneInstr @@ Set (d2, Lval x, loc) in
        (* increment/decrement in every iteration *)
        let t_inc = mkStmtOneInstr @@ Set (t, increm (Lval t) 1, loc) in
        let d1_inc = mkStmtOneInstr @@ Set (d1, increm (Lval d1) (-1), loc) in
        let d2_inc = mkStmtOneInstr @@ Set (d2, increm (Lval d2)   1 , loc) in
        let typ = intType in
        let e1 = BinOp (Eq, Lval t, BinOp (MinusA, Lval x, Lval d1, typ), typ) in
        let e2 = BinOp (Eq, Lval t, BinOp (MinusA, Lval d2, Lval x, typ), typ) in
        let inv1 = mkStmtOneInstr @@ Call (None, f_commit, [e1], loc) in
        let inv2 = mkStmtOneInstr @@ Call (None, f_commit, [e2], loc) in
        (match b.bstmts with
         | cont :: cond :: ss ->
           (* changing succs/preds directly doesn't work -> need to replace whole stmts  *)
           b.bstmts <- cont :: cond :: inv1 :: inv2 :: d1_inc :: d2_inc :: t_inc :: ss;
           let nb = mkBlock [t_init; d1_init; d2_init; mkStmt s.skind] in
           s.skind <- Block nb;
         | _ -> ());
        s
      | Loop (b, loc, Some continue, Some break) ->
        print_endline @@ "WARN: Could not determine loop variable for loop on line " ^ string_of_int loc.line;
        s
      | _ when Hashtbl.mem loopBreaks s.sid -> (* after a loop, we check that t is bounded/positive (no overflow happened) *)
        let loc = Hashtbl.find loopBreaks s.sid in
        let t = var @@ makeVar fd loc "t" in
        let e3 = BinOp (Ge, Lval t, zero, intType) in
        let inv3 = mkStmtOneInstr @@ Call (None, f_check, [e3], loc) in
        let nb = mkBlock [mkStmt s.skind; inv3] in
        s.skind <- Block nb;
        s
      | Instr [Set (lval, e, loc)] when in_loop () ->
        (* find loop var for current loop *)
        let cur_loop = Option.get !cur_loop in
        let x = Hashtbl.find loopVars cur_loop in
        if x <> lval then
          s
        else (* we only care about the loop var *)
          let d1 = makeVar fd cur_loop "d1" in
          let d2 = makeVar fd cur_loop "d2" in
          (match stripCastsDeep e with
           | BinOp (op, Lval x', e2, typ) when (op = PlusA || op = MinusA) && x' = x && isArithmeticType typ -> (* TODO x = 1 + x, MinusA! *)
             (* increase diff by same expr *)
             let d1_inc = mkStmtOneInstr @@ Set (var d1, BinOp (PlusA, Lval (var d1), e2, typ), loc) in
             let d2_inc = mkStmtOneInstr @@ Set (var d2, BinOp (PlusA, Lval (var d2), e2, typ), loc) in
             let nb = mkBlock [d1_inc; d2_inc; mkStmt s.skind] in
             s.skind <- Block nb;
             s
           | _ ->
             (* otherwise diff is e - counter *)
             let t = makeVar fd cur_loop "t" in
             let dt1 = mkStmtOneInstr @@ Set (var d1, BinOp (MinusA, Lval x, Lval (var t), typeOf e), loc) in
             let dt2 = mkStmtOneInstr @@ Set (var d2, BinOp (MinusA, Lval x, Lval (var t), typeOf e), loc) in
             let nb = mkBlock [mkStmt s.skind; dt1; dt2] in
             s.skind <- Block nb;
             s
          )
      | _ -> s
    in
    ChangeDoChildrenPost (s, action)
end


module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "term"
  module D = TermDomain
  module C = TermDomain
  module G = Lattice.Unit

  (* queries *)
  (*let query ctx (q:Queries.t) : Queries.Result.t =*)
  (*match q with*)
  (*| Queries.MustTerm loc -> `Bool (D.mem v ctx.local)*)
  (*| _ -> Queries.Result.top ()*)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local
  (* if the then-block contains a goto while_break.* we have the termination condition for a loop *)
  (* match !MyCFG.current_node with *)
  (* | Some (MyCFG.Statement({ skind = If (e, tb, fb, loc) })) -> *)
  (*       let str_exit b = match exits b with Some loc -> string_of_int loc.line | None -> "None" in *)
  (*       M.debug_each @@ *)
  (*         "\nCil-exp: " ^ sprint d_exp e *)
  (*         (*^ "; Goblint-exp: " ^ sprint d_exp exp*) *)
  (*         ^ "; Goblint: " ^ sprint Queries.Result.pretty (ctx.ask (Queries.EvalInt exp)) *)
  (*         ^ "\nCurrent block: " ^ (if tv then "Then" else "Else") *)
  (*         ^ "\nThen block (exits " ^ str_exit tb ^ "): " ^ sprint d_block tb *)
  (*         ^ "\nElse block (exits " ^ str_exit fb ^ "): " ^ sprint d_block fb *)
  (*         ; *)
  (*       ctx.local *)
  (* | _ -> ctx.local *)

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = D.bot ()
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.bot ()
end

class recomputeVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor
  method! vfunc fd =
    computeCFGInfo fd true;
    SkipChildren
end

let _ =
  (* Cilfacade.register_preprocess Spec.name (new loopCounterVisitor); *)
  Cilfacade.register_preprocess (Spec.name ()) (new loopBreaksVisitor);
  Cilfacade.register_preprocess (Spec.name ()) (new loopVarsVisitor);
  Cilfacade.register_preprocess (Spec.name ()) (new loopInstrVisitor);
  Cilfacade.register_preprocess (Spec.name ()) (new recomputeVisitor);
  Hashtbl.clear loopBreaks; (* because the sids are now different *)
  Cilfacade.register_preprocess (Spec.name ()) (new loopBreaksVisitor);
  MCP.register_analysis (module Spec : MCPSpec)
