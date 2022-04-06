open CompareCIL
open Cil

type max_ids = {
  max_sid: int;
  max_vid: int;
}

let update_id_max (id_max : int ref) id =
  if id > !id_max then id_max := id

let update_sids sid_max (glob: global) = match glob with
  | GFun (fn, loc) -> List.iter (fun s -> update_id_max sid_max s.sid) fn.sallstmts
  | _ -> ()

let update_vids vid_max (glob: global) = match glob with
  | GFun (fn, loc) -> update_id_max vid_max fn.svar.vid; List.iter (List.iter (fun v -> update_id_max vid_max v.vid)) [fn.slocals; fn.sformals]
  | GVar (v,_,_) -> update_id_max vid_max v.vid
  | GVarDecl (v,_) -> update_id_max vid_max v.vid
  | _ -> ()

let update_max_ids vid_max sid_max (glob: global) =
  update_vids vid_max glob; update_sids sid_max glob

let updateMap (oldFile: Cil.file) (newFile: Cil.file) =
  Stats.time "compareCilFiles" (fun () -> compareCilFiles oldFile newFile) ()
  (* TODO: For updateCIL, we have to show whether the new data is from an changed or added functiong  *)

let create_map (new_file: Cil.file) =
  let max_sid = ref 0 in
  let max_vid = ref 0 in
  let add_to_hashtbl tbl (global: Cil.global) =
    match global with
    | GFun _
    | GVar _
    | GVarDecl _ -> Hashtbl.replace tbl (identifier_of_global global) global
    | _ -> () in
  let tbl : (global_identifier, Cil.global) Hashtbl.t = Hashtbl.create 1000 in
  Cil.iterGlobals new_file (fun g -> add_to_hashtbl tbl g; update_max_ids max_vid max_sid g);
  tbl, {max_sid = !max_sid; max_vid = !max_vid}

(* Load and update the version map *)
let load_and_update_map (oldFile: Cil.file) (newFile: Cil.file) =
  if not (Serialize.results_exist ()) then
    failwith "Cannot restore map when no results exist."
  else begin
    let max_ids = Serialize.load_data Serialize.VersionData in
    let changes, map = updateMap oldFile newFile in
    changes, map, max_ids
  end
