open CompareAST
open Cil

type max_ids = {
  max_sid: int;
  max_vid: int;
}

let updateMap (oldFile: Cil.file) (newFile: Cil.file) (ht: (global_identifier, Cil.global) Hashtbl.t) =
  let changes = compareCilFiles oldFile newFile in
  (* TODO: For updateCIL, we have to show whether the new data is from an changed or added functiong  *)
  List.iter (fun (glob: global) ->  Hashtbl.replace ht (identifier_of_global glob) glob) (List.map (fun a -> a.current) changes.changed);
  List.iter (fun (glob: global) ->  Hashtbl.replace ht (identifier_of_global glob) glob) changes.added;
  (ht, changes)

let create_map (new_file: Cil.file) =
  let max_sid = ref 0 in
  let max_vid = ref 0 in
  let update_sid sid = if sid > !max_sid then max_sid := sid in
  let update_vid vid = if vid > !max_vid then max_vid := vid in
  let add_to_hashtbl tbl (global: Cil.global) =
    match global with
    | GFun (fund, _) as f -> update_vid fund.svar.vid; update_sid fund.smaxid; Hashtbl.replace tbl (identifier_of_global f) f
    | GVar (var, _, _) as v -> update_vid var.vid; Hashtbl.replace tbl (identifier_of_global v) v
    | GVarDecl (var, _) as v -> update_vid var.vid; Hashtbl.replace tbl (identifier_of_global v) v
    | other -> ()
  in
  let tbl : (global_identifier, Cil.global) Hashtbl.t = Hashtbl.create 1000 in
  Cil.iterGlobals new_file (add_to_hashtbl tbl);
  tbl, {max_sid = !max_sid; max_vid = !max_vid}

(* Load and update the version map *)
let load_and_update_map (oldFile: Cil.file) (newFile: Cil.file) =
  if not (Serialize.results_exist ()) then
    failwith "Cannot restore map when no results exist."
  else begin
    let oldMap, max_ids = Serialize.load_data Serialize.VersionData in
    let updated, changes = updateMap oldFile newFile oldMap in
    updated, changes, max_ids
  end
