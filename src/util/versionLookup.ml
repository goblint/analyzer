open CompareAST
open Cil
open Serialize

type commitID = string

let updateMap (oldFile: Cil.file) (newFile: Cil.file) (newCommitID: commitID) (ht: (string, Cil.fundec * commitID) Hashtbl.t) = 
    let assocList = compareCilFiles oldFile newFile in
    print_endline "before list.iter";
    List.iter (fun (fundec: fundec) ->  Hashtbl.replace ht fundec.svar.vname (fundec, newCommitID)) assocList;
    print_endline "after list.iter";
    ht


let create_map (new_file: Cil.file) (commit: commitID) =
    let add_to_hashtbl tbl (global: Cil.global) =
        match global with
            | Cil.GFun (fund, loc) ->
              Hashtbl.replace tbl fund.svar.vname (fund, commit) 
            | other -> ()
    in
    let tbl : (string, Cil.fundec * commitID) Hashtbl.t = Hashtbl.create 1000 in
    Cil.iterGlobals new_file (add_to_hashtbl tbl);
    tbl

(** load the old cil.file, load the corresponding map, update the map, return it-
    restoreMap glob_folder old_file new_file *)
let restoreMap (folder: string) (commit: commitID) (oldFile: Cil.file) (newFile: Cil.file)= 
    let commitFolder = Filename.concat folder commit in
    let versionFile = Filename.concat commitFolder versionMapFilename in
    let oldMap = Serialize.unmarshall versionFile in
   (* let astFile = Filename.concat commitFolder Serialize.cilFileName in
    let oldAST = Cil.loadBinaryFile astFile in *)
    updateMap oldFile newFile commit oldMap