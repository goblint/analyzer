open CompareAST
open Cil
open Serialize

type commitID = string

type max_ids = {
  max_sid: int;
  max_vid: int;
}

let updateMap (oldFile: Cil.file) (newFile: Cil.file) (newCommitID: commitID) (ht: (global_identifier, Cil.global * commitID) Hashtbl.t) = 
  let changes = compareCilFiles oldFile newFile in
  (* TODO: For updateCIL, we have to show whether the new data is from an changed or added functiong  *)
  List.iter (fun (glob: global) ->  Hashtbl.replace ht (identifier_of_global glob) (glob, newCommitID)) (List.map (fun a -> a.current) changes.changed);
  List.iter (fun (glob: global) ->  Hashtbl.replace ht (identifier_of_global glob) (glob, newCommitID)) changes.added;
  (ht, changes)

let create_map (new_file: Cil.file) (commit: commitID) =
  let max_sid = ref 0 in
  let max_vid = ref 0 in
  let update_sid sid = if sid > !max_sid then max_sid := sid in
  let update_vid vid = if vid > !max_vid then max_vid := vid in
  let add_to_hashtbl tbl (global: Cil.global) =
    match global with
    | GFun (fund, loc) as f -> update_vid fund.svar.vid; update_sid fund.smaxid; Hashtbl.replace tbl (identifier_of_global f) (f, commit)
    | GVar (var, _, _) as v -> update_vid var.vid; Hashtbl.replace tbl (identifier_of_global v) (v, commit)
    | GVarDecl (var, _) as v -> update_vid var.vid; Hashtbl.replace tbl (identifier_of_global v) (v, commit)
    | other -> ()
  in
  let tbl : (global_identifier, Cil.global * commitID) Hashtbl.t = Hashtbl.create 1000 in
  Cil.iterGlobals new_file (add_to_hashtbl tbl);
  tbl, {max_sid = !max_sid; max_vid =  !max_vid}

(* Load and update the version map *)
let load_and_update_map (folder: string) (old_commit: commitID) (new_commit: commitID) (oldFile: Cil.file) (newFile: Cil.file) = 
  let commitFolder = Filename.concat folder old_commit in
  let versionFile = Filename.concat commitFolder version_map_filename in
  let (oldMap, max_ids) = Serialize.unmarshall versionFile in
  let (updated, changes) = updateMap oldFile newFile new_commit oldMap in
  (updated, changes, max_ids)

let restore_map (folder: string) (old_file: Cil.file) (new_file: Cil.file) =
  match Serialize.current_commit () with
  |Some new_commit ->
    (match (Serialize.last_analyzed_commit ()) with
     |Some old_commit -> load_and_update_map folder old_commit new_commit old_file new_file
     |None -> raise (Failure "No commit has been analyzed yet. Restore map failed."))
  |None -> raise (Failure "Working directory is dirty. Restore map failed.")