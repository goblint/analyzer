(* - code in src/analysis/termination.ml contains loopCounterVisitor which might be interesting
   - check if overflow happend with new variable
   - how do we deal with nested loops?
   - make sure only the analyzed files are appended with the code
   - return variables that are newly created   
   *)

open GoblintCil
open Printf

let extract_file_name s =                    (*There still may be a need to filter more chars*)
   let ls = String.split_on_char '/' s in    (*Assuming '/' as path seperator*)
   let ls = List.rev ls in
   let s' = List.nth ls 0 in
   let ls = String.split_on_char '.' s' in
   let s' = List.nth ls 0 in
   let without_spaces = String.split_on_char ' ' s' in
   let s' = String.concat "" without_spaces in
   s'   

let show_location_id l =
   string_of_int l.line ^ "_" ^ string_of_int l.column (*extract_file_name l.file ^ "_" ^ *)

class loopCounterVisitor (fd : fundec) = object(self)
   inherit nopCilVisitor
   method! vstmt s =
      let action s = match s.skind with
         | Loop (b, loc, eloc, _, _) ->
         let name = "term"^show_location_id loc in
         let typ = Cil.intType in 
         let v = (Cil.makeLocalVar fd name typ) in
         (*let init_stmt = mkStmt (Instr [Set (var v, zero, loc, eloc)]) in*)
         let init_stmt = mkStmtOneInstr @@ Set (var v, zero, loc, eloc) in
         let inc_stmt = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc, eloc) in
         (match b.bstmts with
            | cont :: cond :: ss ->
            b.bstmts <- cont :: inc_stmt :: cond :: ss; (*cont :: cond :: inc_stmt :: ss = it is also possible, but for loops with cond at the end, inc is also at the end*)
            | _ -> ());
         let nb = mkBlock [init_stmt; mkStmt s.skind] in
         s.skind <- Block nb;
         s
         | _ -> s
      in ChangeDoChildrenPost (s, action);
   end

(* just a test
class loopCounterVisitor (fd : fundec) = object(self)
inherit nopCilVisitor
method! vstmt s =
   match s.skind with
      | Loop (b, loc, eloc, _, _) ->
      let name = "term"^show_location_id loc in
      let typ = intType in 
      let v = Goblintutil.create_var (makeLocalVar fd name ~init:(SingleInit zero) typ) in
      let init_stmt = mkStmtOneInstr @@ Set (var v, zero, loc, eloc) in
      let inc_stmt = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc, eloc) in
      b.bstmts <- inc_stmt :: b.bstmts;
      let nb = mkBlock [init_stmt; mkStmt s.skind] in (* init_stmt; *)
      ChangeDoChildrenPost (s, (fun _ -> s.skind <- Block(nb); s))
      | _ -> DoChildren
end 

let add_var_loopTerm fd f =
   let thisVisitor = new loopCounterVisitor in
   visitCilFileSameGlobals (thisVisitor fd ) f*)
(*
let action (fd : fundec) s =
   let a s = match s.skind with
   | Loop (b, loc, eloc, _, _) ->
      let name = "term"^show_location_id loc in
      let typ = intType in 
      let v = Goblintutil.create_var (makeLocalVar fd name ~init:(SingleInit zero) typ) in
      let init_stmt = mkStmtOneInstr @@ Set (var v, zero, loc, eloc) in
      let inc_stmt = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc, eloc) in
      b.bstmts <- inc_stmt :: b.bstmts;
      let nb = mkBlock [init_stmt; mkStmt s.skind] in (* *)
      s.skind <- Block nb;
      s
      | _ -> s
in ChangeDoChildrenPost (s, a)
*)

