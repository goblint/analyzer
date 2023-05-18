(* - code in src/analysis/termination.ml contains loopCounterVisitor which might be interesting
   - check if overflow happend with new variable
   - how do we deal with nested loops?
   - make sure only the analyzed files are appended with the code
   - return variables that are newly created   
   *)

open GoblintCil

let show_location_id l =
   string_of_int l.line ^ "_" ^ string_of_int l.column


class loopCounterVisitor (fd : fundec) = object(self)
inherit nopCilVisitor
method! vstmt s =
   let action s = match s.skind with
      | Loop (b, loc, eloc, _, _) ->
      let name = "term"^show_location_id loc in
      let typ = intType in 
      let v = Goblintutil.create_var (makeLocalVar fd name ~init:(SingleInit zero) typ) in
      let inc_stmt = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc, eloc) in
      b.bstmts <- inc_stmt :: b.bstmts;
      let nb = mkBlock [mkStmt s.skind] in
      s.skind <- Block nb;
      s
      | _ -> s
   in ChangeDoChildrenPost (s, action)
end
 
(*let action (fd : fundec) s =
   let a s = match s.skind with
   | Loop (b, loc, eloc, _, _) ->
      let name = "term"^show_location_id loc in
      let typ = intType in 
      let v = Goblintutil.create_var (makeLocalVar fd name ~init:(SingleInit zero) typ) in
      let inc_stmt = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc, eloc) in
      b.bstmts <- inc_stmt :: b.bstmts;
      let nb = mkBlock [mkStmt s.skind] in
      s.skind <- Block nb;
      s
   | _ -> s
in ChangeDoChildrenPost (s, a)*)
