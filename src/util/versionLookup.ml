open CompareAST
open Cil
open Serialize

type commitID = string

let updateMap (oldFile: Cil.file) (newFile: Cil.file) (newCommitID: commitID) (ht: (string, Cil.varinfo * commitID) Hashtbl.t) = 
    let assocList = compareCilFiles oldFile newFile in
    List.iter (fun (fundec: fundec) ->  Hashtbl.replace ht fundec.svar.vname (fundec.svar, newCommitID)) assocList;
    ht


let create_map (new_file: Cil.file) (commit: commitID) =
    let add_to_hashtbl tbl (global: Cil.global) =
        match global with
            | Cil.GFun (fund, loc) ->
              Hashtbl.replace tbl fund.svar.vname (fund, commit) 
            | other -> ()
    in
    let tbl = Hashtbl.create 1000 in
    Cil.iterGlobals new_file (add_to_hashtbl tbl)

(* load the old cil.file, load the corresponding map, update the map, return it*)
let restoreMap (folder: string) (commit: commitID) (newFile: Cil.file)= 
    let commitFolder = Filename.concat folder commit in
    let versionFile = Filename.concat commitFolder versionMapFilename in
    let oldMap = Serialize.unmarshall versionFile in
    let astFile = Filename.concat commitFolder versionMapFilename in
    let oldAST = Cil.loadBinaryFile astFile in
    updateMap oldAST newFile commit oldMap