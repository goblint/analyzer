open OUnit2
open GoblintCil
open Goblint_lib

module U = LoopUnrolling0

let with_copyof f _ =
  U.reset_copyof ();
  Fun.protect ~finally:U.reset_copyof f

let test_numbering () =
  let original = mkEmptyStmt () in
  let copy = mkEmptyStmt () in
  let nested_copy = mkEmptyStmt () in
  let unrelated = mkEmptyStmt () in
  let pairs = [copy, original; nested_copy, copy] in
  let fd = emptyFunction "test" in
  fd.sbody <- mkBlock [original; copy; nested_copy; unrelated];
  computeCFGInfo fd true;
  U.rebuild_copyof pairs;
  assert_bool "immediate provenance is preserved" (match U.find_copyof nested_copy with Some s -> s == copy | None -> false);
  assert_bool "nested copies resolve to the original" (U.find_original nested_copy == original);
  assert_bool "identical syntax is not identity" (U.find_original unrelated == unrelated);
  U.reset_copyof ();
  U.rebuild_copyof [];
  assert_bool "a new file discards old provenance" (U.find_original copy == copy)

(* Exercise real unrolling and CFG preparation, including nested copies and
   statements rewritten by prepareCFG (switch, break, continue and goto). *)
let parse_unrolled names =
  let filename = Filename.temp_file "unrolling-provenance" ".c" in
  Fun.protect ~finally:(fun () -> Sys.remove filename) (fun () ->
      let out = open_out filename in
      List.iter (fun name ->
          Printf.fprintf out {|int %s(int n) {
  int x = 0;
  while (x < n) {
    x++;
    while (x < n) {
      switch (x) { case 1: x++; break; default: x += 2; }
      if (x == 4) continue;
      if (x == 5) break;
      if (x == 6) goto next;
      x++;
      next: x++;
    }
  }
  return x;
}
|} name
        ) names;
      close_out out;
      let file = Cilfacade.getAST (Fpath.v filename) in
      Cilfacade.current_file := file;
      CilCfg.createCFG file;
      Cilfacade.reset_lazy ();
      file
    )

let fundec file name =
  List.find_map (function GFun (fd, _) when fd.svar.vname = name -> Some fd | _ -> None) file.globals
  |> Option.get

let relationships fd =
  let statements = U.CopyOfHashTable.create 113 in
  List.iter (fun s -> U.CopyOfHashTable.replace statements s ()) fd.sallstmts;
  U.CopyOfHashTable.fold (fun copy original pairs ->
      if U.CopyOfHashTable.mem statements copy then (copy, original) :: pairs else pairs
    ) U.copyof []

let check_relationships pairs =
  assert_bool "fixture must contain unrolled copies" (pairs <> []);
  List.iter (fun (copy, original) ->
      assert_bool "immediate provenance uses retained objects"
        (match U.find_copyof copy with Some s -> s == original | None -> false);
      assert_bool "nested provenance terminates at an original"
        (U.find_original copy == U.find_original original)
    ) pairs

let test_incremental ~cfg_diff ~serialized () =
  let factor = GobConfig.get_int "exp.unrolling-factor" in
  let previous_file = !Cilfacade.current_file in
  Fun.protect ~finally:(fun () ->
      GobConfig.set_int "exp.unrolling-factor" factor;
      Cilfacade.current_file := previous_file;
      Cilfacade.reset_lazy ()
    ) (fun () ->
      GobConfig.set_int "exp.unrolling-factor" 2;
      let old_file = parse_unrolled ["stable"; "changed"] in
      let stable_pairs = relationships (fundec old_file "stable") in
      (* CLI incremental runs load the previous AST from disk. Preserve sharing
         with our expected relationships while giving it fresh object identities. *)
      let old_file, stable_pairs =
        if serialized then Marshal.from_string (Marshal.to_string (old_file, stable_pairs) []) 0
        else (old_file, stable_pairs)
      in
      let ids = MaxIdUtil.get_file_max_ids old_file in
      let global fd = CompareCIL.{decls = None; def = Some (Fun fd)} in
      let update old_file ids =
        let file = parse_unrolled ["stable"; "changed"; "added"] in
        let changed_pairs = relationships (fundec file "changed") in
        let added_pairs = relationships (fundec file "added") in
        (* Verify all provenance endpoints survive CFG preparation and can be
           paired when an unchanged function body is replaced. *)
        let all_statements = U.CopyOfHashTable.create 113 in
        iterGlobals file (function
            | GFun (fd, _) ->
              List.iter (fun s -> U.CopyOfHashTable.replace all_statements s ()) fd.sallstmts
            | _ -> ());
        U.CopyOfHashTable.iter (fun copy original ->
            assert_bool "copy is numbered" (U.CopyOfHashTable.mem all_statements copy);
            assert_bool "original is numbered" (U.CopyOfHashTable.mem all_statements original)
          ) U.copyof;
        let old_changed = fundec old_file "changed" in
        let changed = fundec file "changed" in
        let diff = if cfg_diff then
            Some CompareCIL.{unchangedNodes = [MyCFG.Statement (List.hd old_changed.sallstmts), MyCFG.Statement (List.hd changed.sallstmts)]; primObsoleteNodes = []}
          else None in
        let changes = CompareCIL.empty_change_info () in
        changes.unchanged <- [CompareCIL.{old = global (fundec old_file "stable"); current = global (fundec file "stable")}];
        changes.changed <- [CompareCIL.{old = global old_changed; current = global changed; unchangedHeader = true; diff}];
        changes.added <- [global (fundec file "added")];
        let ids = UpdateCil.update_ids old_file ids file changes in
        check_relationships stable_pairs;
        check_relationships changed_pairs;
        check_relationships added_pairs;
        assert_equal ~msg:"provenance must not accumulate across reparses"
          (List.length stable_pairs + List.length changed_pairs + List.length added_pairs)
          (U.CopyOfHashTable.length U.copyof);
        (file, ids)
      in
      let file, ids = update old_file ids in
      (* A second update models repeated server reparsing, with stable's body
         still referring to the objects retained from the first file. *)
      ignore (update file ids)
    )

let tests =
  "loopUnrollingTest" >::: [
    "numbering" >:: with_copyof test_numbering;
    "incremental_ast" >:: with_copyof (test_incremental ~cfg_diff:false ~serialized:true);
    "incremental_cfg" >:: with_copyof (test_incremental ~cfg_diff:true ~serialized:true);
    "server_reparse" >:: with_copyof (test_incremental ~cfg_diff:true ~serialized:false);
  ]
