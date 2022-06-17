open Cil
include CompareAST
include CompareCFG

module StringSet = Set.Make(String)

type f = fundec * location
type v = varinfo * initinfo * location

type globalElem = Fundec of fundec | GlobalVar of varinfo

let globalElemName elem = match elem with
  | Fundec(f) -> f.svar.vname
  | GlobalVar(v) -> v.vname

module GlobalElemForMap = struct
  type t = globalElem

  let compare x y = String.compare (globalElemName x) (globalElemName y)
end

module FundecForMap = struct
  type t = Cil.fundec

  (*x.svar.uid cannot be used, as they may overlap between old and now AST*)
  let compare x y = String.compare x.svar.vname y.svar.vname
end

module FundecMap = Map.Make(FundecForMap)

module GlobalElemMap = Map.Make(GlobalElemForMap)

(*A dependency maps the function it depends on to the name the function has to be changed to*)
type functionDependencies = string StringMap.t


(*Renamed: newName * dependencies; Modified=now*unchangedHeader*)
type status = SameName of globalElem | Renamed of globalElem | Created | Deleted | Modified of globalElem * bool
type outputFunctionStatus = Unchanged of global | UnchangedButRenamed of global | Added | Removed | Changed of global * bool

type output = global * outputFunctionStatus



let pretty (f: status) =
  match f with
  | SameName _ -> "SameName"
  | Renamed x -> ("Renamed to " ^ globalElemName x)
  | Created -> "Added"
  | Deleted -> "Removed"
  | Modified _ -> "Changed"

let printFundecMap elemToString map = begin
  Seq.iter (fun (f, e) ->
      ignore@@Pretty.printf "%s->%s;" f.svar.vname (elemToString e);
    ) (FundecMap.to_seq map)
end

let getFunctionAndGVarMap (ast: file) : f StringMap.t * v StringMap.t =
  Cil.foldGlobals ast (fun (functionMap, gvarMap) global ->
      match global with
      | GFun (fundec, location) -> (StringMap.add fundec.svar.vname (fundec, location) functionMap, gvarMap)
      | GVar (varinfo, initinfo, location) -> (functionMap, StringMap.add varinfo.vname (varinfo, initinfo, location) gvarMap)
      | _ -> functionMap, gvarMap
    ) (StringMap.empty, StringMap.empty)

let performRenames (renamesOnSuccess: renamesOnSuccess) = 
  begin
  let (compinfoRenames, enumRenames) = renamesOnSuccess in
  List.iter (fun (compinfo2, compinfo1) -> compinfo2.cname <- compinfo1.cname) compinfoRenames;
  List.iter (fun (enum2, enum1) -> enum2.ename <- enum1.ename) enumRenames;
  end

let getDependencies fromEq = StringMap.map (fun assumption -> assumption.new_method_name) fromEq

(*Data type that holds the important data while checking for renames.
   statusForOldElem: Status we have already figured out for a fundec from oldAST;
   statusForNowElem: see statusForOldElem;
   mapping: Mappings from (fundec of old AST) -> (fundec of now AST) we have already figured out to hold.
   reversemapping: see method mapping, but from now -> old
*)
type carryType = {
  statusForOldElem: status GlobalElemMap.t;
  statusForNowElem: status GlobalElemMap.t;
  mapping: globalElem GlobalElemMap.t;
  reverseMapping: globalElem GlobalElemMap.t;
}

(*Carry type manipulation functions.*)

let registerStatusForOldF f status data =
  {statusForOldElem = GlobalElemMap.add f status data.statusForOldElem;
   statusForNowElem=data.statusForNowElem;
   mapping=data.mapping;
   reverseMapping=data.reverseMapping;
  }

let registerStatusForNowF f status data =
  {statusForOldElem = data.statusForOldElem;
   statusForNowElem=GlobalElemMap.add f status data.statusForNowElem;
   mapping=data.mapping;
   reverseMapping=data.reverseMapping;
  }

let registerBiStatus (oldF: globalElem) (nowF: globalElem) (status: status) data =
  {statusForOldElem=GlobalElemMap.add oldF status data.statusForOldElem;
   statusForNowElem=GlobalElemMap.add nowF status data.statusForNowElem;
   mapping=data.mapping;
   reverseMapping=data.reverseMapping;
  }

let registerMapping oldF nowF data =
  {statusForOldElem=data.statusForOldElem;
   statusForNowElem=data.statusForNowElem;
   mapping=GlobalElemMap.add oldF nowF data.mapping;
   reverseMapping=GlobalElemMap.add nowF oldF data.reverseMapping;
  }

let registerGVarMapping oldV nowV data = {
  statusForOldElem=data.statusForOldElem;
  statusForNowElem=data.statusForNowElem;
  mapping=data.mapping;
  reverseMapping=data.reverseMapping;
}


(*returns true iff for all dependencies it is true, that the dependency has a corresponding function with the new name and matches the without having dependencies itself and the new name is not already present on the old AST. *)
let doAllDependenciesMatch (dependencies: functionDependencies) 
(global_var_dependencies: glob_var_rename_assumptions) 
(oldFunctionMap: f StringMap.t) 
(nowFunctionMap: f StringMap.t) 
(oldGVarMap: v StringMap.t) 
(nowGVarMap: v StringMap.t) (data: carryType) : bool * carryType =

  let isConsistent = fun old nowName allEqual getName getGlobal oldMap nowMap getNowOption data ->
    (*Early cutoff if a previous dependency returned false.
      We never create a mapping between globs where the now name was already part of the old set or the old name is part of the now set.
      But only if now and old differ.
    *)
    if allEqual && (getName old = nowName || (not (StringMap.mem nowName oldMap) && not (StringMap.mem (getName old) nowMap))) then
      let globalElem = getGlobal old in
      let knownMapping = GlobalElemMap.find_opt globalElem data.mapping in

      (*To avoid inconsitencies, if a function has already been mapped to a function, that mapping is reused again.*)
      match knownMapping with
      | Some(knownElem) ->
        (*This function has already been mapped*)
        globalElemName knownElem = nowName, data
      | None ->
        let nowElemOption = getNowOption nowName in

        match nowElemOption with
        | Some(nowElem) ->
          let compare = fun old now ->
            match (old, now) with
            | Fundec(oF), Fundec(nF) ->
              let doMatch, _, _, function_dependencies, global_var_dependencies, renamesOnSuccess = CompareGlobals.eqF oF nF None StringMap.empty VarinfoMap.empty in
              doMatch, function_dependencies, global_var_dependencies, renamesOnSuccess
            | GlobalVar(oV), GlobalVar(nV) ->
              let (equal, (_, function_dependencies, global_var_dependencies, renamesOnSuccess)) = eq_varinfo oV nV emptyRenameMapping in
              (*eq_varinfo always comes back with a self dependency. We need to filter that out.*)
              equal, function_dependencies, (VarinfoMap.filter (fun vi name -> not (vi.vname = oV.vname && name = nowName)) global_var_dependencies), renamesOnSuccess
            | _, _ -> failwith "Unknown or incompatible global types"
          in


          let doMatch, function_dependencies, global_var_dependencies, renamesOnSuccess = compare globalElem nowElem in

          (*let _ = Printf.printf "%s <-> %s: %b %b %b\n" (getName old) (globalElemName nowElem) doMatch (StringMap.is_empty function_dependencies) (VarinfoMap.is_empty global_var_dependencies) in

            let _ = Printf.printf "%s\n" (rename_mapping_to_string (StringMap.empty, function_dependencies, global_var_dependencies)) in
          *)
          if doMatch && StringMap.is_empty function_dependencies && VarinfoMap.is_empty global_var_dependencies then
            let _ = performRenames renamesOnSuccess in
            true, registerMapping globalElem nowElem data
          else false, data

        | None ->
          false, data
    else false, data
  in

  StringMap.fold (fun oldName nowName (allEqual, data) ->
      let (old, _) = StringMap.find oldName oldFunctionMap in
      isConsistent
        old
        nowName
        allEqual
        (fun x -> x.svar.vname)
        (fun x -> Fundec(x))
        oldFunctionMap
        nowFunctionMap
        (fun x ->
           Option.bind (StringMap.find_opt x nowFunctionMap) (fun (x, _) -> Some(Fundec(x)))
        )
        data
    ) dependencies (true, data) |>
  VarinfoMap.fold (fun oldVarinfo nowName (allEqual, data) ->
      isConsistent
        oldVarinfo
        nowName
        allEqual
        (fun x -> x.vname)
        (fun x -> GlobalVar(x))
        oldGVarMap
        nowGVarMap
        (fun x ->
           Option.bind (StringMap.find_opt x nowGVarMap) (fun (x, _, _) -> Some(GlobalVar(x)))
        )
        data
    )
    global_var_dependencies

(*Check if f has already been assigned a status. If yes do nothing.
   If not, check if the function took part in the mapping, then register it to have been renamed. Otherwise register it as the supplied status.*)
let assignStatusToUnassignedElem data f registerStatus statusMap mapping status =
  if not (GlobalElemMap.mem f statusMap) then
    if (GlobalElemMap.mem f mapping) then
      registerStatus f (Renamed (GlobalElemMap.find f mapping)) data
    else
      (*this function has been added/removed*)
      registerStatus f status data
  else
    data

let detectRenamedFunctions (oldAST: file) (newAST: file) : output GlobalElemMap.t = begin
  let oldFunctionMap, oldGVarMap = getFunctionAndGVarMap oldAST in
  let nowFunctionMap, nowGVarMap = getFunctionAndGVarMap newAST in

  let initialData: carryType = {statusForOldElem = GlobalElemMap.empty;
                                statusForNowElem = GlobalElemMap.empty;
                                mapping=GlobalElemMap.empty;
                                reverseMapping=GlobalElemMap.empty;
                               } in

  (*Go through all functions, for all that have not been renamed *)
  let finalData =
    StringMap.fold (fun _ (f, _) (data: carryType) ->
        let matchingNewFundec = StringMap.find_opt f.svar.vname nowFunctionMap in
        match matchingNewFundec with
        | Some (newFun, _) ->
          (*Compare if they are similar*)
          let doMatch, unchangedHeader, _, function_dependencies, global_var_dependencies, renamesOnSuccess =
            CompareGlobals.eqF f newFun None StringMap.empty VarinfoMap.empty in

          (*Before renamesOnSuccess, functions with the same name have always been compared. 
            In this comparison, the renaming on compinfo and enum was always performed, no matter if the comparison
            was a success or not. This call mimics this behaviour.*)
          let _ = performRenames renamesOnSuccess in

          (*let _ = Pretty.printf "%s <-> %s: %b %s\n" f.svar.vname newFun.svar.vname doMatch (rename_mapping_to_string (StringMap.empty, function_dependencies, global_var_dependencies)) in

          let _ = Pretty.printf "old locals: %s\n" (String.concat ", " (List.map (fun x -> x.vname) f.slocals)) in
          let _ = Pretty.printf "now locals: %s\n" (String.concat ", " (List.map (fun x -> x.vname) newFun.slocals)) in*)


          let actDependencies = getDependencies function_dependencies in

          let oldG = Fundec(f) in
          let nowG = Fundec(newFun) in


          if doMatch then
            let doDependenciesMatch, updatedData = doAllDependenciesMatch actDependencies global_var_dependencies oldFunctionMap nowFunctionMap oldGVarMap nowGVarMap data in
            if doDependenciesMatch then
              registerBiStatus oldG nowG (SameName(oldG)) updatedData
            else
              registerStatusForOldF oldG (Modified(nowG, unchangedHeader)) data |>
              registerStatusForNowF nowG (Modified(oldG, unchangedHeader))
          else
            registerStatusForOldF oldG (Modified(nowG, unchangedHeader)) data |>
            registerStatusForNowF nowG (Modified(oldG, unchangedHeader))
        | None -> data
      ) oldFunctionMap initialData |>
    (*At this point we already know of the functions that have changed and stayed the same. We now assign the correct status to all the functions that
       have been mapped. The functions that have not been mapped are added/removed.*)
    (*Now go through all old functions again. Those who have not been assigned a status are removed*)
    StringMap.fold (fun _ (f, _) (data: carryType) ->
        assignStatusToUnassignedElem data (Fundec(f)) registerStatusForOldF data.statusForOldElem data.mapping Deleted
      ) oldFunctionMap |>
    (*now go through all new functions. Those have have not been assigned a mapping are added.*)
    StringMap.fold (fun _ (nowF, _) (data: carryType) ->
        assignStatusToUnassignedElem data (Fundec(nowF)) registerStatusForNowF data.statusForNowElem data.reverseMapping Created
      ) nowFunctionMap |>
    StringMap.fold (fun _ (v, _, _) data ->
        assignStatusToUnassignedElem data (GlobalVar(v)) registerStatusForOldF data.statusForOldElem data.mapping Deleted
      ) oldGVarMap |>
    StringMap.fold (fun _ (nowV, _, _) (data: carryType) ->
        assignStatusToUnassignedElem data (GlobalVar(nowV)) registerStatusForNowF data.statusForNowElem data.reverseMapping Created
      ) nowGVarMap
  in

  (*Done with the analyis, the following just adjusts the output types.*)

  (*Map back to GFun and exposed function status*)
  let extractOutput funMap invertedFunMap gvarMap invertedGvarMap f (s: status) =
    let getGlobal gT fundecMap gVarMap =
      match gT with
      | Fundec(f2) ->
        let (f, l) = StringMap.find f2.svar.vname fundecMap in
        GFun(f, l)
      | GlobalVar(v2) ->
        let (v, i, l) = StringMap.find v2.vname gVarMap in
        GVar(v, i, l)
    in

    let outputS = match s with
      | SameName x -> Unchanged(getGlobal x invertedFunMap invertedGvarMap)
      | Renamed x -> UnchangedButRenamed(getGlobal x invertedFunMap invertedGvarMap)
      | Created -> Added
      | Deleted -> Removed
      | Modified (x, unchangedHeader) -> Changed(getGlobal x invertedFunMap invertedGvarMap, unchangedHeader)
    in
    getGlobal f funMap gvarMap, outputS
  in

  (*Merge together old and now functions*)
  GlobalElemMap.merge (fun _ a b ->
      if Option.is_some a then a
      else if Option.is_some b then b
      else None
    )
    (GlobalElemMap.mapi (extractOutput oldFunctionMap nowFunctionMap oldGVarMap nowGVarMap) finalData.statusForOldElem)
    (GlobalElemMap.mapi (extractOutput nowFunctionMap oldFunctionMap nowGVarMap oldGVarMap) finalData.statusForNowElem)
end
