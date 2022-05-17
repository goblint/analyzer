open Cil
open MyCFG
include CompareAST
include CompareCFG

(*Maps the function name as keys*)
module FundecMap = Map.Make(String);;

type functionStatus = Identical | Renamed | Changed | New | Deleted
type results = (fundec, functionStatus) Hashtbl.t

(*A dependency mapps the function it depends on to the name the function has to be changed to*)
type dependencies = (fundec, string) Hashtbl.t

type earlyFunctionStatus = Unchanged of dependencies | Unknown

let getFunctionMap (ast: file) : fundec FundecMap.t =
  Cil.foldGlobals ast (fun map global ->
      match global with
      | GFun (fundec, _) -> FundecMap.add fundec.svar.vname fundec map
      | _ -> map
    ) FundecMap.empty

let seperateUnchangedFunctions (oldFunctionMap: fundec FundecMap.t) (newFunctionMap: fundec FundecMap.t) : earlyFunctionStatus FundecMap.t =
  FundecMap.map (fun f ->
      let matchingNewFundec = FundecMap.find_opt f.svar.vname newFunctionMap in
      match matchingNewFundec with
      | Some newFun ->
        (*Compare if they are similar*)
        let result = CompareCIL.eqF f newFun None (Hashtbl.create 0) in
        Unknown
      | None -> Unknown
    ) oldFunctionMap

let detectRenamedFunctions (oldAST: file) (newAST: file) : results = begin
  let oldFunctionMap = getFunctionMap oldAST in
  let newFunctionMap = getFunctionMap newAST in

  (*1. detect function which names have not changed*)
  let unchangedNameFunctions = FundecMap.map (fun _ fundec -> ) oldFunctionMap in

  Hashtbl.create 0
end
