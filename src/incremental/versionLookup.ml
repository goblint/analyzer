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

(** Obtains the maximum sid and vid from a Cil.file *)
let get_file_max_ids (new_file: Cil.file) =
  let max_sid = ref 0 in
  let max_vid = ref 0 in
  Cil.iterGlobals new_file (fun g -> update_max_ids max_vid max_sid g);
  {max_sid = !max_sid; max_vid = !max_vid}

(* Loads the max sid and vid from a previous run *)
let load_max_ids () =
  Serialize.load_data Serialize.VersionData
