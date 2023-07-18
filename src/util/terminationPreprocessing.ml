open GoblintCil

module VarToStmt = Map.Make(CilType.Varinfo) (* maps varinfos (= loop counter variable) to the statement of the corresponding loop*)



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
  string_of_int l.line ^ "_" ^ string_of_int l.column ^ "-file" ^ "_" ^  extract_file_name l.file

class loopCounterVisitor lc lg (fd : fundec) = object(self)
   inherit nopCilVisitor
   
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
      
      let f_bounded  = Lval (var (specialFunction "__goblint_bounded").svar) in
      
      let action s = match s.skind with
         | Loop (b, loc, eloc, _, _) ->
         let name = "term"^show_location_id loc in
         let typ = Cil.intType in 
         let v = (Cil.makeLocalVar fd name typ) in (*Not tested for incremental mode*)
         let init_stmt = mkStmtOneInstr @@ Set (var v, zero, loc, eloc) in
         let inc_stmt = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc, eloc) in
         let inc_stmt2 = mkStmtOneInstr @@ Set (var v, increm (Lval (var v)) 1, loc, eloc) in
         let exit_stmt = mkStmtOneInstr @@ Call (None, f_bounded, [Lval (var v)], loc, locUnknown) in
         (match b.bstmts with
            | s :: ss ->   (*duplicate increment statement here to fix inconsistencies in nested loops*)
               b.bstmts <- inc_stmt :: exit_stmt :: s :: inc_stmt2 :: ss;
            | ss ->
               b.bstmts <- inc_stmt :: exit_stmt :: ss;
         );
         lc := VarToStmt.add (v: varinfo) (s: stmt) !lc;
         let nb = mkBlock [init_stmt; mkStmt s.skind] in
         s.skind <- Block nb;
         s
         | Goto (sref, l) -> 
            let goto_jmp_stmt = sref.contents.skind in 
            let loc_stmt = get_stmtLoc goto_jmp_stmt in 
            if CilType.Location.compare l loc_stmt >= 0 (*is pos if first loc is greater -> below the second loc*)
               then 
                  lg := List.append !lg ([l] : location list); (*problem: the program might not terminate!*)
            s
         | _ -> s
      in ChangeDoChildrenPost (s, action);
   end