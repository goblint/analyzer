open Cil
open MyCFG
include CompareAST
include CompareCFG

module StringSet = Set.Make(String)

type f = fundec * location

module FundecForMap = struct
  type t = Cil.fundec

  let compare x y = String.compare x.svar.vname y.svar.vname
end

module FundecMap = Map.Make(FundecForMap)

(*A dependency maps the function it depends on to the name the function has to be changed to*)
type dependencies = string StringMap.t

(*Renamed: newName * dependencies; Modified=now*unchangedHeader*)
type functionStatus = SameName of fundec | Renamed of fundec | Created | Deleted | Modified of fundec * bool
type outputFunctionStatus = Unchanged of global | UnchangedButRenamed of global | Added | Removed | Changed of global * bool

type output = global * outputFunctionStatus

let pretty (f: functionStatus) =
  match f with
  | SameName _ -> "SameName"
  | Renamed x -> "Renamed to " ^ x.svar.vname
  | Created -> "Added"
  | Deleted -> "Removed"
  | Modified _ -> "Changed"

let printFundecMap elemToString map = begin
  Seq.iter (fun (f, e) ->
      ignore@@Pretty.printf "%s->%s;" f.svar.vname (elemToString e);
    ) (FundecMap.to_seq map)
end

let getFunctionMap (ast: file) : f StringMap.t =
  Cil.foldGlobals ast (fun map global ->
      match global with
      | GFun (fundec, location) -> StringMap.add fundec.svar.vname (fundec, location) map
      | _ -> map
    ) StringMap.empty

let getDependencies fromEq = StringMap.map (fun assumption -> assumption.new_method_name) fromEq

type carryType = {
  statusForOldFunction: functionStatus FundecMap.t;
  statusForNowFunction: functionStatus FundecMap.t;
  methodMapping: fundec FundecMap.t;
  reverseMethodMapping: fundec FundecMap.t}

let registerStatusForOldF f status data =
  {statusForOldFunction = FundecMap.add f status data.statusForOldFunction;
   statusForNowFunction=data.statusForNowFunction;
   methodMapping=data.methodMapping;
   reverseMethodMapping=data.reverseMethodMapping}

let registerStatusForNowF f status data =
  {statusForOldFunction = data.statusForOldFunction;
   statusForNowFunction=FundecMap.add f status data.statusForNowFunction;
   methodMapping=data.methodMapping;
   reverseMethodMapping=data.reverseMethodMapping}

let registerBiStatus (oldF: fundec) (nowF: fundec) (status: functionStatus) data =
  {statusForOldFunction=FundecMap.add oldF status data.statusForOldFunction;
   statusForNowFunction=FundecMap.add nowF status data.statusForNowFunction;
   methodMapping=data.methodMapping;
   reverseMethodMapping=data.reverseMethodMapping}

let registerMapping oldF nowF data =
  {statusForOldFunction=data.statusForOldFunction;
   statusForNowFunction=data.statusForNowFunction;
   methodMapping=FundecMap.add oldF nowF data.methodMapping;
   reverseMethodMapping=FundecMap.add nowF oldF data.reverseMethodMapping}

(*returns true iff for all dependencies it is true, that the dependency has a corresponding function with the new name and matches the without having dependencies itself and the new name is not already present on the old AST. *)
let doAllDependenciesMatch (dependencies: dependencies) (oldFunctionMap: f StringMap.t) (newFunctionMap: f StringMap.t) (data: carryType) : bool * carryType =
  StringMap.fold (fun oldName newName (allEqual, data) ->
      (*Early cutoff if a previous dependency returned false or the newName is already present in the old map*)
      if allEqual && not (StringMap.mem newName oldFunctionMap) then

        let (oldFundec, _) = StringMap.find oldName oldFunctionMap in

        let knownMapping = FundecMap.find_opt oldFundec data.methodMapping in

        (*To avoid inconsitencies, if a function has already been mapped to a function, that mapping is reused again.*)
        match knownMapping with
        | Some(knownFundec) ->
          (*This function has already been mapped*)
          knownFundec.svar.vname = newName, data
        | None ->
          let newFundecOption = StringMap.find_opt newName newFunctionMap in

          match newFundecOption with
          | Some((newFundec, _)) ->
            let doMatch, _, _, dependencies = CompareGlobals.eqF oldFundec newFundec None StringMap.empty in

            if doMatch && StringMap.is_empty dependencies then
              true, registerMapping oldFundec newFundec data
            else false, data

          | None -> false, data
      else false, data
    ) dependencies (true, data)

(*Check if f has already been assigned a status. If yes do nothing.
   If not, check if the function took part in the mapping, then register it to have been renamed. Otherwise register it as the supplied status*)
let assignStatusToUnassignedFunction data f registerStatus statusMap mapping status =
  if not (FundecMap.mem f statusMap) then
    if (FundecMap.mem f mapping) then
      registerStatus f (Renamed(FundecMap.find f mapping)) data
    else
      (*this function has been added/removed*)
      registerStatus f status data
  else
    data

let detectRenamedFunctions (oldAST: file) (newAST: file) : output FundecMap.t = begin
  let oldFunctionMap = getFunctionMap oldAST in
  let nowFunctionMap = getFunctionMap newAST in

  let initialData: carryType = {statusForOldFunction = FundecMap.empty;
                                statusForNowFunction = FundecMap.empty;
                                methodMapping=FundecMap.empty;
                                reverseMethodMapping=FundecMap.empty} in

  (*Go through all functions, for all that have not been renamed *)
  let finalData =
    StringMap.fold (fun _ (f, _) (data: carryType) ->
        let matchingNewFundec = StringMap.find_opt f.svar.vname nowFunctionMap in
        match matchingNewFundec with
        | Some (newFun, _) ->
          (*Compare if they are similar*)
          let doMatch, unchangedHeader, _, dependencies = CompareGlobals.eqF f newFun None StringMap.empty in

          let actDependencies = getDependencies dependencies in

          if doMatch then
            let doDependenciesMatch, updatedData = doAllDependenciesMatch actDependencies oldFunctionMap nowFunctionMap data in
            if doDependenciesMatch then
              registerBiStatus f newFun (SameName(newFun)) updatedData
            else
              registerStatusForOldF f (Modified(newFun, unchangedHeader)) data |>
              registerStatusForNowF newFun (Modified(f, unchangedHeader))
          else
            registerStatusForOldF f (Modified(newFun, unchangedHeader)) data |>
            registerStatusForNowF newFun (Modified(f, unchangedHeader))
        | None -> data
      ) oldFunctionMap initialData |>
    (*Now go through all old functions again. Those who have not been assigned a status are removed*)
    StringMap.fold (fun _ (f, _) (data: carryType) ->
        assignStatusToUnassignedFunction data f registerStatusForOldF data.statusForOldFunction data.methodMapping Deleted
      ) oldFunctionMap |>
    (*now go through all new functions. Those have have not been assigned a mapping are added.*)
    StringMap.fold (fun _ (nowF, _) (data: carryType) ->
        assignStatusToUnassignedFunction data nowF registerStatusForNowF data.statusForNowFunction data.reverseMethodMapping Created
      ) nowFunctionMap

  in

  (*Map back to GFun and exposed function status*)
  let extractOutput funMap invertedFunMap f (s: functionStatus) =
    let getGFun f2 map =
      let (f, l) = StringMap.find f2.svar.vname map in
      GFun(f, l)
    in

    let outputS = match s with
      | SameName x -> Unchanged(getGFun x invertedFunMap)
      | Renamed x -> UnchangedButRenamed(getGFun x invertedFunMap)
      | Created -> Added
      | Deleted -> Removed
      | Modified (x, unchangedHeader) -> Changed(getGFun x invertedFunMap, unchangedHeader)
    in
    getGFun f funMap, outputS
  in

  FundecMap.merge (fun _ a b ->
      if Option.is_some a then a
      else if Option.is_some b then b
      else None
    )
    (FundecMap.mapi (extractOutput oldFunctionMap nowFunctionMap) finalData.statusForOldFunction)
    (FundecMap.mapi (extractOutput nowFunctionMap oldFunctionMap) finalData.statusForNowFunction)
end
