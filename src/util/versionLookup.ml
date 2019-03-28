open CompareAST
open Cil
open Serialize

type commitID = string

let updateMap (oldFile: Cil.file) (newFile: Cil.file) (newCommitID: commitID) (ht: (string, Cil.global * commitID) Hashtbl.t) = 
  let changes = compareCilFiles oldFile newFile in  
  (* TODO: For updateCIL, we have to show whether the new data is from an changed or added functiong  *)
  List.iter (fun (glob: global) ->  Hashtbl.replace ht (name_of_global glob) (glob, newCommitID)) (List.map (fun a -> a.current) changes.changed);
  List.iter (fun (glob: global) ->  Hashtbl.replace ht (name_of_global glob) (glob, newCommitID)) changes.added;
  (ht, changes)

let create_map (new_file: Cil.file) (commit: commitID) =
    let add_to_hashtbl tbl (global: Cil.global) =
        match global with
            | GFun (fund, loc) as f -> Hashtbl.replace tbl fund.svar.vname (f, commit)
            | GVar (var, _, _) as v -> Hashtbl.replace tbl var.vname (v, commit)
            | GVarDecl (var, _) as v -> Hashtbl.replace tbl var.vname (v, commit)
            | other -> ()
    in
    let tbl : (string, Cil.global * commitID) Hashtbl.t = Hashtbl.create 1000 in
    Cil.iterGlobals new_file (add_to_hashtbl tbl);
    tbl

(** For debugging purposes: print the mapping from function name to commit *)
let print_mapping (function_name: string) (dec, commit: Cil.global * commitID) =
  print_string function_name;
  print_string " -> ";
  print_endline commit

(** load the old cil.file, load the corresponding map, update the map, return it-
    restoreMap glob_folder old_file new_file *)
let restoreMap (folder: string) (old_commit: commitID) (new_commit: commitID) (oldFile: Cil.file) (newFile: Cil.file)= 
    let commitFolder = Filename.concat folder old_commit in
    let versionFile = Filename.concat commitFolder versionMapFilename in
    let (oldMap, max_ids) = Serialize.unmarshall versionFile in
   (* let astFile = Filename.concat commitFolder Serialize.cilFileName in
    let oldAST = Cil.loadBinaryFile astFile in *)
    let (updated, changes) = updateMap oldFile newFile new_commit oldMap in
    (updated, changes, max_ids)

let restore_map (src_files: string list) (folder: string) (old_file: Cil.file) (new_file: Cil.file) =
    match Serialize.current_commit src_files with 
    |Some new_commit -> 
      (match Serialize.last_analyzed_commit src_files with
        |Some old_commit -> restoreMap folder old_commit new_commit old_file new_file 
        |None -> raise (Failure "No commit has been analyzed yet. Restore map failed."))
    |None -> raise (Failure "Working directory is dirty. Restore map failed.")