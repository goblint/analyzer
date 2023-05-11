(** Termination of loops. *)

open Prelude.Ana
open Analyses

module M = Messages
(* J: returns if a and b contain a value
  if yes: return this x, otherwise nothing *)
let (||?) a b = match a,b with Some x,_ | _, Some x -> Some x | _ -> None

module TermDomain = struct
  include SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All Variables" end)
end

(* some kind of location string suitable for variable names? *)
(* J: returns a string_ "lineNr_columnNr" *)
(* J: for location (10,5) it evaluates to: 10_5*)
let show_location_id l =
  string_of_int l.line ^ "_" ^ string_of_int l.column

(* J: the new variable is created here and inserted into the code
  in the code the variable is set to 0 (before the loop) and incremented (after the loop)
  is it ever checked if the newly created variable is really new???*)
(* J: ??? Who defines the Loop, what are the variables*)
class loopCounterVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor
  method! vstmt s =
    let action s = match s.skind with
      | Loop (b, loc, eloc, _, _) ->
        (* insert loop counter variable *)
        (* J: for location (10,5) it evaluates to: term10_5*)
        let name = "term"^show_location_id loc in
        let typ = intType in (* TODO the type should be the same as the one of the original loop counter *)
        let v = Goblintutil.create_var (makeLocalVar fd name ~init:(SingleInit zero) typ) in
        (* make an init stmt since the init above is apparently ignored *)
        let init_stmt = mkStmtOneInstr @@ Set (var v, zero, loc, eloc) in
        (* increment it every iteration *)
        let inc_stmt = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc, eloc) in
        b.bstmts <- inc_stmt :: b.bstmts;
        let nb = mkBlock [init_stmt; mkStmt s.skind] in (* J: s.kind = Loop(b, loc, eloc, ...)*)
        s.skind <- Block nb;
        s
      | _ -> s
    in ChangeDoChildrenPost (s, action)
end

(* J: creates a new hash table with size 13 for loop breaks*)
(* J: int: is a number which is unique in a function*)
(* J: ??? Why 13*)
let loopBreaks : (int, location) Hashtbl.t = Hashtbl.create 13 (* break stmt sid -> corresponding loop *)
(* J: if there is some break associated with the loop (?) we add this break to the hash table
   key = break.sid
   data = location *)
class loopBreaksVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor
  method! vstmt s =
    (match s.skind with
     | Loop (b, loc, eloc, Some continue, Some break) -> Hashtbl.add loopBreaks break.sid loc (* TODO: use eloc? *)
     | Loop _ -> failwith "Termination.preprocess: every loop should have a break and continue stmt after prepareCFG"
     | _ -> ());
    DoChildren
end

(* if the given block contains a goto while_break.* we have the termination condition for a loop *)
(* J: returns binding from loopBreaks hash table associated with a number which is uniquely identifie with the statement
   if there is a Goto, otherwise nothing*)
(* J: stmt.sid = A number (>= 0) that is unique in a function. *)
(* J: ???*)
let exits = function
  | { bstmts = [{ skind = Goto (stmt, loc); _ }]; _ } -> Hashtbl.find_option loopBreaks !stmt.sid
  | _ -> None (* TODO handle return (need to find out what loop we are in) *)

(* J: ???*)
let lvals_of_expr =
  let rec f a = function
    | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ | AddrOfLabel _ -> a
    | Lval l | AddrOf l | StartOf l -> l :: a
    | SizeOfE e | AlignOfE e | UnOp (_,e,_) | CastE (_,e) | Imag e | Real e -> f a e
    | BinOp (_,e1,e2,_) -> f a e1 @ f a e2
    | Question (c,t,e,_) -> f a c @ f a t @ f a e
  in f []

(* J: create hash table of size 13 for variables*)
let loopVars : (location, lval) Hashtbl.t = Hashtbl.create 13 (* loop location -> lval used for exit *)
(* J: adds the location and left varibale to the loopVars, if one block of the if statement contains a goto block*)
class loopVarsVisitor (fd : fundec) = object
  inherit nopCilVisitor
  method! vstmt s =
    let add_exit_cond e loc =
      match lvals_of_expr e with
      (* J: ??? Same as when isArithmeticType Cilfacade.typeOf e*)
      | [lval] when Cilfacade.typeOf e |> isArithmeticType -> Hashtbl.add loopVars loc lval
      (* J : add lval to hash table when the expression on location loc is of arithmetic type*)
      | _ -> ()
    in
    (match s.skind with
    (* J: map_default f x (Some v) returns f v and map_default f x None returns x.*)
    (* J: If there exists a goto statement: call add_exit_cond e (SOME exits tb ||? exits fb)
            If e is of arithmetic type: add the location to the loopVars hash table *)
     | If (e, tb, fb, loc, eloc) -> Option.map_default (add_exit_cond e) () (exits tb ||? exits fb)
     | _ -> ());
    DoChildren
end

(* J: ??? visits the expression e and removes all casts from the expression*)
let stripCastsDeep e =
  let v = object
    inherit nopCilVisitor
    (* J: ChangeTo: Replace the expression with the given one*)
    (* J: Removes casts from this expression, but ignores casts within other expression constructs. 
       So we delete the (A) and (B) casts from "(A)(B)(x + (C)y)", but leave the (C) cast.*)
    method! vexpr e = ChangeTo (stripCasts e)
  end
  in visitCilExpr v e

(* keep the enclosing loop for statements *)
(* J: store pointer pointing to Nothing for loops*)
let cur_loop = ref None (* current loop *)
let cur_loop' = ref None (* for nested loops *)
(* J: searches if the variable name__<lineNr>_<columnNr> for the given location is present in the function definition
   if not: the variable is created and initialized to 0*)
let makeVar fd loc name =
  (* J: for location = (10,5) and name = "hi" the id evaluates to: hi__10_5*)
  let id = name ^ "__" ^ show_location_id loc in
  (* J: fd.slocals = Locals of the function definition*)
  (* J: returns the first element which is a local and which name is id (for example hi__10_5)*)
  try List.find (fun v -> v.vname = id) fd.slocals
  (* J: when the variable is not found in the function definition then it is newly created and initialized with 0*)
  with Not_found ->
    let typ = intType in (* TODO the type should be the same as the one of the original loop counter *)
    Goblintutil.create_var (makeLocalVar fd id ~init:(SingleInit zero) typ)
(* J: creates an empty function with name "__goblint_assume" and makes a lvalue out of it*)
let f_assume = Lval (var (emptyFunction "__goblint_assume").svar)
(* J: creates an empty function with name "__goblint_check" and makes a lvalue out of it*)
let f_check  = Lval (var (emptyFunction "__goblint_check").svar)
(* J: ??? Loop pointer handling: Why do we fist set cur_loop' = cur_loop and then reverse this operation???*)
(* J: inserts new variable t with init and increment and adds a check if t is bounded*)
class loopInstrVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor
  method! vstmt s =
    (* TODO: use Loop eloc? *)
    (match s.skind with
    (* J: if the statement is a loop adjust the loop pointer*)
     | Loop (_, loc, eloc, _, _) ->
       cur_loop' := !cur_loop; (* J: set the nested loop to the current loop*)
       cur_loop := Some loc (* J: set the newly found loop as current loop*)
     | _ -> ());
    (* J: adds the variable t to the loop, increments it and checks after the loop if it is bounded
          adds also the variables d1 and d2, with incrementation, which affects t
          depending on the structure, d1 and d2 are set differently*)
    let action s =
      (* first, restore old cur_loop *)
      (* J: if the statement is a loop set the nested loop as the current loop*)
      (match s.skind with
       | Loop (_, loc, eloc, _, _) ->
         cur_loop := !cur_loop'; (* J: current loop is the nested loop*)
       | _ -> ());
      (* J: true if the current loop variable is set and this variable is bound in the hash table*)
      let in_loop () = Option.is_some !cur_loop && Hashtbl.mem loopVars (Option.get !cur_loop) in
      (* J: t is the new variable which should be bounded
            if there is a loop with bounded variable: add code for init and increment (for t, d1, d2)
            if there is a loop with unbounded variable: do nothing
            if a loop ended: check if t is bounded
            if there is an instruction with an assignment: increment d1 and d2 if the assignment affects the loop var
            do this recursively for all children*)
      match s.skind with
        (* J: if the statement is a loop, and when the location is bound in the hash table: 
            - add the creational and initializational code for t, d1, d2 before the loop
            - add the incrementational code for t, d1, d2 in the loop
          *)
      | Loop (b, loc, eloc, Some continue, Some break) when Hashtbl.mem loopVars loc ->
        (* find loop var for current loop *)
        let x = Hashtbl.find loopVars loc in
        (* insert loop counter and diff to loop var *)
        (* J: create variables t, d1, d2 with names: t__<line>_<column>, d1__<line>_<column>, d2__<line>_<column>*)
        let t = var @@ makeVar fd loc "t" in
        let d1 = var @@ makeVar fd loc "d1" in
        let d2 = var @@ makeVar fd loc "d2" in
        (* make init stmts *)
        (* J: set t=0, d1, d2 = lvalue of x = value in the hashtable associated with the loop variable*)
        let t_init = mkStmtOneInstr @@ Set (t, zero, loc, eloc) in
        let d1_init = mkStmtOneInstr @@ Set (d1, Lval x, loc, eloc) in
        let d2_init = mkStmtOneInstr @@ Set (d2, Lval x, loc, eloc) in
        (* increment/decrement in every iteration *)
        (* J: increment t and d2, decrement d1*)
        let t_inc = mkStmtOneInstr @@ Set (t, increm (Lval t) 1, loc, eloc) in (* J: t = t + 1*)
        let d1_inc = mkStmtOneInstr @@ Set (d1, increm (Lval d1) (-1), loc, eloc) in (* J: d1 = d1 - 1*)
        let d2_inc = mkStmtOneInstr @@ Set (d2, increm (Lval d2)   1 , loc, eloc) in (* J: d2 = d2 + 1*)
        let typ = intType in (* J: Note: x is the loop variable*)
        let e1 = BinOp (Eq, Lval t, BinOp (MinusA, Lval x, Lval d1, typ), typ) in (* J: t = (x - d1) *)
        let e2 = BinOp (Eq, Lval t, BinOp (MinusA, Lval d2, Lval x, typ), typ) in (* J: t = (d2 - x) *)
        (* J: make a statement for e1 and e2*)
        (* J: ??? what happens with the call*)
        let inv1 = mkStmtOneInstr @@ Call (None, f_assume, [e1], loc, eloc) in 
        let inv2 = mkStmtOneInstr @@ Call (None, f_assume, [e2], loc, eloc) in
        (match b.bstmts with (* J: we are still in a loop*)
         | cont :: cond :: ss ->
           (* changing succs/preds directly doesn't work -> need to replace whole stmts  *)
           (* from: cont :: cond :: ss
              to:   cont :: 
                    cond :: 
                    t = (x - d1) :: t = (d2 - x) :: (??? Is this correct with the call???)
                    d1 = d1 - 1 :: d2 = d2 + 1 :: t = t + 1 ::
                    ss
              *)
           b.bstmts <- cont :: cond :: inv1 :: inv2 :: d1_inc :: d2_inc :: t_inc :: ss; (* J: in the loop*)
           let nb = mkBlock [t_init; d1_init; d2_init; mkStmt s.skind] in (* J: make a block out of the init statements before the loop*)
           s.skind <- Block nb; 
         | _ -> ());
        s (* J: return s with added code for init and increment*)
      (* J: if the variable in the loops is not bounded, it is not possible to continue*)
      | Loop (b, loc, eloc, Some continue, Some break) ->
        print_endline @@ "WARN: Could not determine loop variable for loop at " ^ CilType.Location.show loc;
        s
      (* J: when the statement is not a loop and a loop ended:
            - add t >= 0 in the code after the loop*)
      | _ when Hashtbl.mem loopBreaks s.sid -> (* after a loop, we check that t is bounded/positive (no overflow happened) *)
        let loc = Hashtbl.find loopBreaks s.sid in (* J: holds the current binding of the number of the current function in the hash table*)
        let t = var @@ makeVar fd loc "t" in (* J: get the name for variable t = t__<line>_<column>*)
        let e3 = BinOp (Ge, Lval t, zero, intType) in (* J: t >= 0*)
        let inv3 = mkStmtOneInstr @@ Call (None, f_check, [e3], loc, locUnknown) in (* J: make a statement to check t >= 0*)
        let nb = mkBlock [mkStmt s.skind; inv3] in (* J: add the statement to the block*)
        s.skind <- Block nb;
        s (* J: return s with the added check*)
      (* J: If there is an instruction (containing an assignment) and it is in a loop:
                If the loop variable and lvalue of Set are structural unequal
                  Do nothing
                else
                  add an incrementation step for d1 and d2 (depending on the binOp)*)
      | Instr [Set (lval, e, loc, eloc)] when in_loop () ->
        (* find loop var for current loop *)
        let cur_loop = Option.get !cur_loop in
        let x = Hashtbl.find loopVars cur_loop in (* J: holds the current binding of the number of the current function in the hash table*)
        if x <> lval then (* J: x and lval are structural unequal*)
          s
        else (* we only care about the loop var *)
          (* J: create the variables d1 and d2 with name:  d1__<line>_<column>, d2__<line>_<column>*)
          let d1 = makeVar fd cur_loop "d1" in
          let d2 = makeVar fd cur_loop "d2" in
          (match stripCastsDeep e with
            (* J: if x' + e2 or x' - e2 with x' = x and the type arithmetic:
                  - adds incrementation for d1 and d2 to the code*)
            (* J: if the loopVar is changed*)
           | BinOp (op, Lval x', e2, typ) when (op = PlusA || op = MinusA) && x' = x && isArithmeticType typ -> (* TODO x = 1 + x, MinusA! *)
             (* increase diff by same expr *)
             let d1_inc = mkStmtOneInstr @@ Set (var d1, BinOp (PlusA, Lval (var d1), e2, typ), loc, eloc) in (* J: d1 = d1 + e2*)
             let d2_inc = mkStmtOneInstr @@ Set (var d2, BinOp (PlusA, Lval (var d2), e2, typ), loc, eloc) in (* J: d2 = d2 + e2*)
             let nb = mkBlock [d1_inc; d2_inc; mkStmt s.skind] in (* J: add the incrementation steps at the front*)
             s.skind <- Block nb;
             s (* J: return s with the added incrementation*)
           | _ ->
             (* otherwise diff is e - counter *)
             let t = makeVar fd cur_loop "t" in (* J: varibale name for t*)
             let te = Cilfacade.typeOf e in (* J: type of e*)
             let dt1 = mkStmtOneInstr @@ Set (var d1, BinOp (MinusA, Lval x, Lval (var t), te), loc, eloc) in (* J: d1 = x - t*)
             let dt2 = mkStmtOneInstr @@ Set (var d2, BinOp (MinusA, Lval x, Lval (var t), te), loc, eloc) in (* J: d2 = x - t*)
             let nb = mkBlock [mkStmt s.skind; dt1; dt2] in (* J: add the incrementation steps at the end*)
             s.skind <- Block nb;
             s
          )
      | _ -> s
    in
    (* J: *)
    ChangeDoChildrenPost (s, action) (* J: continue with the children*)
end


module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "term"
  module D = TermDomain
  module C = TermDomain

  (* queries *)
  (*let query ctx (q:Queries.t) : Queries.Result.t =*)
  (*match q with*)
  (*| Queries.MustTerm loc -> `Bool (D.mem v ctx.local)*)
  (*| _ -> Queries.Result.top ()*)

  (* transfer functions *)

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local
  (* if the then-block contains a goto while_break.* we have the termination condition for a loop *)
  (* match !MyCFG.current_node with *)
  (* | Some (MyCFG.Statement({ skind = If (e, tb, fb, loc) })) -> *)
  (*       let str_exit b = match exits b with Some loc -> string_of_int loc.line | None -> "None" in *)
  (*       M.debug @@ *)
  (*         "\nCil-exp: " ^ sprint d_exp e *)
  (*         (*^ "; Goblint-exp: " ^ sprint d_exp exp*) *)
  (*         ^ "; Goblint: " ^ sprint Queries.Result.pretty (ctx.ask (Queries.EvalInt exp)) *)
  (*         ^ "\nCurrent block: " ^ (if tv then "Then" else "Else") *)
  (*         ^ "\nThen block (exits " ^ str_exit tb ^ "): " ^ sprint d_block tb *)
  (*         ^ "\nElse block (exits " ^ str_exit fb ^ "): " ^ sprint d_block fb *)
  (*         ; *)
  (*       ctx.local *)
  (* | _ -> ctx.local *)

  let startstate v = D.bot () (* J: start with bottom*)
  let threadenter ctx lval f args = [D.bot ()] (* J: enter threads with bottom*)
  let exitstate  v = D.bot ()
end

class recomputeVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor
  method! vfunc fd =
    computeCFGInfo fd true; (* J: make the cfg and return a list of statements with global statement number*)
    SkipChildren (* J: don't visit children*)
end

let _ =
  (* Cilfacade.register_preprocess Spec.name (new loopCounterVisitor); *)
  Cilfacade.register_preprocess (Spec.name ()) (new loopBreaksVisitor); (* J: fill hash table loopBreaks: containing breaks ?*)
  Cilfacade.register_preprocess (Spec.name ()) (new loopVarsVisitor); (* J: fill hash table loopVars: containing varibales identified with loops ?*)
  Cilfacade.register_preprocess (Spec.name ()) (new loopInstrVisitor); (* J: inserts new variable with init, increment, and bounded check to code*)
  Cilfacade.register_preprocess (Spec.name ()) (new recomputeVisitor); (* J: ??? *)
  Hashtbl.clear loopBreaks; (* because the sids are now different *) (* J: delete entries in loopBreaks*)
  Cilfacade.register_preprocess (Spec.name ()) (new loopBreaksVisitor); (* J: newly set hash table loopBreaks with goto statements*)
  MCP.register_analysis (module Spec : MCPSpec) (* J: ???*)