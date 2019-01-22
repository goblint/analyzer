open CompareAST
open Cil
type commitID = string

let versionMapFilename = "version.data"
let cilFileName = "ast.data"

let updateMap (oldFile: Cil.file) (newFile: Cil.file) (newCommitID: commitID) (ht: (string, Cil.varinfo * commitID) Hashtbl.t) = 
    let assocList = compareCilFiles oldFile newFile in
    List.iter (fun (fundec: fundec) ->  Hashtbl.replace ht fundec.svar.vname (fundec.svar, newCommitID)) assocList;
    ht

(* load the old cil.file, load the corresponding map, update the map, return it*)
let restoreMap (folder: string) (commit: commitID) (newFile: Cil.file)= 
    let commitFolder = Filename.concat folder commit in
    let versionFile = Filename.concat commitFolder versionMapFilename in
    let oldMap = Serialize.unmarshall versionFile in
    let astFile = Filename.concat commitFolder versionMapFilename in
    let oldAST = Cil.loadBinaryFile astFile in
    updateMap oldAST newFile commit oldMap