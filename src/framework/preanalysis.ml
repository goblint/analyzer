open Cfg 
open Cil

let relatedVars = Hashtbl.create 123

let print_table h =
  let () = print_endline "Printing Hashtable" in
   Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" x y) h

let f_check  = Lval (var (emptyFunction "__goblint_check").svar)

let makeVar fd loc name =
  let id = name ^ "__" ^ string_of_int loc.line in
  try List.find (fun v -> v.vname = id) fd.slocals
  with Not_found ->
    let typ = intType in (* TODO the type should be the same as the one of the original loop counter *)
    Goblintutil.create_var (makeLocalVar fd id ~init:(SingleInit zero) typ)

let rec list_instr_to_string l = match l with
  | [] -> ""
  | head::body -> 
  begin
    (Pretty.sprint 20 (Cil.d_instr () head))^(list_instr_to_string body)
  end

let rec get_vnames exp = match exp with
  | Lval lval -> 
    let lhost, offset = lval in 
    (match lhost with
      | Var vinfo -> vinfo.vname
      | _ -> "")
  | UnOp (unop, e, typ) -> get_vnames e
  | BinOp (binop, e1, e2, typ) -> (get_vnames e1)^" "^(get_vnames e2)
  | _ -> ""
  
let rec pair_variables v exp = match exp with
| Lval lval -> 
  let lhost, offset = lval in 
  (match lhost with
    | Var vinfo -> 
      let () = print_endline ("Adding to table!!! "^v^" "^vinfo.vname) in 
      Hashtbl.add relatedVars v vinfo.vname
    | _ -> ())
| UnOp (unop, e, typ) -> pair_variables v e
| BinOp (binop, e1, e2, typ) -> let () = (pair_variables v e1) in (pair_variables v e2)
| _ -> ()

let pairs_from_instr instr = match instr with
  | Set (lval, exp, location) ->  
    let lhost, offset = lval in
    (match lhost with
      | Var vinfo -> let () = pair_variables vinfo.vname exp in 
      let () = print_table relatedVars in
      vinfo.vname^" <-> ["^(get_vnames exp)^"]"
      | _ -> "")
    (*" "^(Pretty.sprint 20 (Cil.d_lval () lval))^" is "^(Pretty.sprint 20 (Cil.d_exp () exp))^" | "*)
  | VarDecl (varinfo, location) -> " "^varinfo.vname^" is declared | "
  | _ -> ""

let rec get_related_pairs l = match l with
  | [] -> ""
  | head::body -> 
  begin
    (pairs_from_instr head)^(get_related_pairs body)
  end

class expressionVisitor (fd : fundec) = object(self)
inherit nopCilVisitor
method! vstmt s =
  let action s = match s.skind with
    | Instr inst -> 
      (*let () = print_endline (list_instr_to_string inst) in*)
      let () = print_endline ("Related pairs "^(get_related_pairs inst)) in
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


class loopCounterVisitor (fd : fundec) = object(self)
inherit nopCilVisitor
method! vstmt s =
  let action s = match s.skind with
    | Loop (b, loc, _, _) -> 
      (* insert loop counter variable *)
      let t = var @@ makeVar fd loc "t" in
      (* initialise the loop counter to 0 *)
      let t_init = mkStmtOneInstr @@ Set (t, zero, loc) in
      (* increment the loop counter by 1 in every iteration *)
      let t_inc = mkStmtOneInstr @@ Set (t, increm (Lval t) 1, loc) in
      (match b.bstmts with
       | cont :: cond :: ss ->
         (* changing succs/preds directly doesn't work -> need to replace whole stmts  *)
         b.bstmts <- cont :: cond :: t_inc :: ss;
         let nb = mkBlock [t_init; mkStmt s.skind] in
         s.skind <- Block nb;
       | _ -> ());
      s
    | _ when Hashtbl.mem loopBreaks s.sid -> (* after a loop, we check that t is bounded/positive (no overflow happened) *)
      let loc = Hashtbl.find loopBreaks s.sid in
      let t = var @@ makeVar fd loc "t" in
      let e3 = BinOp (Ge, Lval t, zero, intType) in
      let inv3 = mkStmtOneInstr @@ Call (None, f_check, [e3], loc) in
      let nb = mkBlock [mkStmt s.skind; inv3] in
      s.skind <- Block nb;
      s
    | _ -> s
  in ChangeDoChildrenPost (s, action)
end

class recomputeVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor
  method! vfunc fd =
    computeCFGInfo fd true;
    SkipChildren
end

let add_visitors = 
  let () =print_endline "Adding the visitor is called!!!" in 
  Cilfacade.register_preprocess "octApron" (new loopBreaksVisitor);
  Cilfacade.register_preprocess "octApron" (new loopCounterVisitor);
  Cilfacade.register_preprocess "octApron" (new recomputeVisitor);
  Hashtbl.clear loopBreaks; (* because the sids are now different *)
  Cilfacade.register_preprocess "octApron" (new loopBreaksVisitor);
  Cilfacade.register_preprocess "octApron" (new expressionVisitor)