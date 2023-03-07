open GoblintCil
include CompareGlobals
open CilMaps

module StringSet = Set.Make(String)

type f = fundec * location
type v = varinfo * initinfo * location

(*A dependency maps the function it depends on to the name the function has to be changed to*)
type functionDependencies = string VarinfoMap.t

(*Renamed: newName * dependencies; Modified=now*unchangedHeader*)
type status = SameName of global_col | Renamed of global_col | Created | Deleted | Modified of global_col * bool

let pretty (f: status) =
  match f with
  | SameName _ -> "SameName"
  | Renamed x -> ("Renamed to " ^ CompareGlobals.name_of_global_col x)
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
    List.iter (fun (compinfo2, compinfo1) -> compinfo2.cname <- compinfo1.cname; compinfo2.ckey <- compinfo1.ckey) compinfoRenames;
    List.iter (fun (enum2, enum1) -> enum2.ename <- enum1.ename) enumRenames;
  end

let getDependencies fromEq = VarinfoMap.map (fun assumption -> assumption.new_method_name) fromEq

(*Data type that holds the important data while checking for renames.
   statusForOldElem: Status we have already figured out for a fundec from oldAST;
   statusForNowElem: see statusForOldElem;
   mapping: Mappings from (fundec of old AST) -> (fundec of now AST) we have already figured out to hold.
   reversemapping: see method mapping, but from now -> old
*)
type carryType = {
  statusForOldElem : status GlobalColMap.t;
  statusForNowElem : status GlobalColMap.t;
  mapping: global_col GlobalColMap.t;
  reverseMapping: global_col GlobalColMap.t;
}

let emptyCarryType = {
  statusForOldElem = GlobalColMap.empty;
  statusForNowElem = GlobalColMap.empty;
  mapping = GlobalColMap.empty;
  reverseMapping = GlobalColMap.empty;
}

(*Carry type manipulation functions.*)

let registerStatusForOldF f status data =
  {statusForOldElem = GlobalColMap.add f status data.statusForOldElem;
   statusForNowElem=data.statusForNowElem;
   mapping=data.mapping;
   reverseMapping=data.reverseMapping;
  }

let registerStatusForNowF f status data =
  {statusForOldElem = data.statusForOldElem;
   statusForNowElem=GlobalColMap.add f status data.statusForNowElem;
   mapping=data.mapping;
   reverseMapping=data.reverseMapping;
  }

let registerBiStatus (oldF: global_col) (nowF: global_col) (status: status) data =
  {statusForOldElem=GlobalColMap.add oldF status data.statusForOldElem;
   statusForNowElem=GlobalColMap.add nowF status data.statusForNowElem;
   mapping=data.mapping;
   reverseMapping=data.reverseMapping;
  }

let registerMapping oldF nowF data =
  {statusForOldElem=data.statusForOldElem;
   statusForNowElem=data.statusForNowElem;
   mapping=GlobalColMap.add oldF nowF data.mapping;
   reverseMapping=GlobalColMap.add nowF oldF data.reverseMapping;
  }

let registerGVarMapping oldV nowV data = {
  statusForOldElem=data.statusForOldElem;
  statusForNowElem=data.statusForNowElem;
  mapping=data.mapping;
  reverseMapping=data.reverseMapping;
}

(*True iff the global var rename assumptions contains only entries that are identity mappings*)
let areGlobalVarRenameAssumptionsEmpty (mapping: glob_var_rename_assumptions) : bool =
  VarinfoMap.for_all (fun varinfo newName -> varinfo.vname = newName) mapping

(*returns true iff for all dependencies it is true, that the dependency has a corresponding function with the new name and matches the without having dependencies itself and the new name is not already present on the old AST. *)
let doAllDependenciesMatch (dependencies: functionDependencies)
    (global_var_dependencies: glob_var_rename_assumptions)
    (oldMap: global_col StringMap.t)
    (newMap: global_col StringMap.t) (data: carryType) : bool * carryType =

  let isConsistent = fun old nowName allEqual getName oldMap nowMap getNowOption data ->
    (*Early cutoff if a previous dependency returned false.
      We never create a mapping between globs where the now name was already part of the old set or the old name is part of the now set.
      But only if now and old differ.
    *)
    if allEqual && (getName old = nowName || (not (StringMap.mem nowName oldMap) && not (StringMap.mem (getName old) nowMap))) then
      let knownMapping = GlobalColMap.find_opt old data.mapping in

      (*let _ = Printf.printf "Dep: %s -> %s\n" (globalElemName2 globalElem) nowName in*)

      (*To avoid inconsitencies, if a function has already been mapped to a function, that mapping is reused again.*)
      match knownMapping with
      | Some(knownElem) ->
        (*This function has already been mapped*)
        (*let _ = Printf.printf "Already mapped. %s = %s\n" (globalElemName2 knownElem) nowName in*)
        name_of_global_col knownElem = nowName, data
      | None ->
        let nowElemOption = getNowOption nowName in

        match nowElemOption with
        | Some(nowElem) -> (
            let compare = fun old now ->
              let compareVar oV nV = let (equal, (_, function_dependencies, global_var_dependencies, renamesOnSuccess)) = eq_varinfo oV nV ~rename_mapping:empty_rename_mapping in
                (*eq_varinfo always comes back with a self dependency. We need to filter that out.*)
                unchanged_to_change_status equal, function_dependencies, (VarinfoMap.filter (fun vi name -> not (vi.vname = oV.vname && name = nowName)) global_var_dependencies), renamesOnSuccess
              in
              match (old.def, now.def) with
              | Some (Fun oF), Some (Fun nF) ->
                let doMatch, _, function_dependencies, global_var_dependencies, renamesOnSuccess = CompareGlobals.eqF oF nF None VarinfoMap.empty VarinfoMap.empty in
                doMatch, function_dependencies, global_var_dependencies, renamesOnSuccess
              | Some (Var oV), Some (Var nV) -> compareVar oV nV
              | None, None -> (match old.decls, now.decls with
                  | Some oV, Some nV -> compareVar oV nV
                  | _ -> failwith "Unknown or incompatible global types")
              | _, _ -> failwith "Unknown or incompatible global types"
            in

            let doMatch, function_dependencies, global_var_dependencies, renamesOnSuccess = compare old nowElem in

            (*Having a dependency on yourself is ok.*)
            let hasNoExternalDependency = VarinfoMap.is_empty function_dependencies || (
                VarinfoMap.cardinal function_dependencies = 1 && (
                  VarinfoMap.fold (fun varinfo dependency _ -> varinfo.vname = name_of_global_col old && dependency.new_method_name = name_of_global_col nowElem) function_dependencies true
                )
              ) in

            (*let _ = Printf.printf "%s <-> %s: %b %b %b\n" (globalElemName2 globalElem) (globalElemName2 nowElem) doMatch hasNoExternalDependency (VarinfoMap.is_empty global_var_dependencies) in

              let _ = Printf.printf "%s\n" (rename_mapping_to_string (StringMap.empty, function_dependencies, global_var_dependencies, ([], []))) in*)

            match doMatch with
            | Unchanged when hasNoExternalDependency && areGlobalVarRenameAssumptionsEmpty global_var_dependencies ->
              let _ = performRenames renamesOnSuccess in
              true, registerMapping old nowElem data
            | _ -> false, data
          )
        | None ->
          (*Printf.printf "No elem with name %s found \n" nowName;*)
          (*Return true assumes external globs never change. Which is ok for now*)
          true, data
    else false, data
  in

  VarinfoMap.fold (fun old nowName (allEqual, data) ->
      let old = StringMap.find old.vname oldMap in
      isConsistent
        old
        nowName
        allEqual
        (fun x -> name_of_global_col x)
        oldMap
        newMap
        (fun x -> StringMap.find_opt x newMap)
        data
    ) dependencies (true, data) |>
  VarinfoMap.fold (fun oldVarinfo nowName (allEqual, data) ->
      isConsistent
        (GlobalMap.find oldVarinfo.vname oldMap)
        nowName
        allEqual
        (fun x -> name_of_global_col x)
        oldMap
        newMap
        (fun x -> StringMap.find_opt x newMap)
        data
    )
    global_var_dependencies

(*Check if f has already been assigned a status. If yes do nothing.
   If not, check if the function took part in the mapping, then register it to have been renamed. Otherwise register it as the supplied status.*)
let assignStatusToUnassignedElem data f registerStatus statusMap mapping status =
  if not (GlobalColMap.mem f statusMap) then
    if (GlobalColMap.mem f mapping) then
      registerStatus f (Renamed (GlobalColMap.find f mapping)) data
    else
      (*this function has been added/removed*)
      registerStatus f status data
  else
    data

let findSameNameMatchingGVars (oldMap : global_col StringMap.t) (newMap : global_col StringMap.t) data =
  let compare_varinfo v1 v2 data =
    let identical, _ = eq_varinfo v1 v2 ~rename_mapping:empty_rename_mapping in
    let oldG, nowG = GlobalMap.find v1.vname oldMap, GlobalMap.find v2.vname newMap in
    if identical then
      registerBiStatus oldG nowG (SameName nowG) data
    else
      registerStatusForOldF oldG (Modified(nowG, false)) data |>
      registerStatusForNowF nowG (Modified(oldG, false))
  in
  StringMap.fold (fun name gc_old (data: carryType) ->
      try
        let gc_new = StringMap.find name newMap in
        match gc_old.def, gc_new.def with
        | Some (Var v1), Some (Var v2) -> compare_varinfo v1 v2 data
        | None, None -> (match gc_old.decls, gc_new.decls with
            | Some v1, Some v2 -> compare_varinfo v1 v2 data
            | _ -> data)
        | _ -> data
      with Not_found -> data
    ) oldMap data

(*Goes through all old functions and looks for now-functions with the same name. If a pair has been found, onMatch is called with the comparison result.
   On match then modifies the carryType. Returns (list of the functions that have the same name and match, the updated carry type)*)
let findSameNameMatchingFunctions
    oldFunctionMap
    nowFunctionMap
    (initialData: 'a)
    (onMatch: fundec -> fundec -> change_status -> string VarinfoMap.t -> CompareGlobals.glob_var_rename_assumptions -> CompareGlobals.renamesOnSuccess -> 'a -> 'a) : 'a =
  StringMap.fold (fun name oldFun data ->
      try
        let newFun = StringMap.find name nowFunctionMap in
        match oldFun.def, newFun.def with
        | Some (Fun f1), Some (Fun f2) ->
          let doMatch, _, function_dependencies, global_var_dependencies, renamesOnSuccess = CompareGlobals.eqF f1 f2 None VarinfoMap.empty VarinfoMap.empty in
          let actDependencies = getDependencies function_dependencies in
          onMatch f1 f2 doMatch actDependencies global_var_dependencies renamesOnSuccess data
        | _ -> data
      with Not_found -> data
    ) oldFunctionMap initialData

let fillStatusForUnassignedElems oldMap newMap (data: carryType) =
  data |>
  (*Now go through all old functions again. Those who have not been assigned a status are removed*)
  StringMap.fold (fun name f (data: carryType) ->
      assignStatusToUnassignedElem data f registerStatusForOldF data.statusForOldElem data.mapping Deleted
    ) oldMap |>
  (*now go through all new functions. Those have have not been assigned a mapping are added.*)
  StringMap.fold (fun name nowF (data: carryType) ->
      assignStatusToUnassignedElem data nowF registerStatusForNowF data.statusForNowElem data.reverseMapping Created
    ) newMap

let detectRenamedFunctions (oldMap : global_col StringMap.t) (newMap : global_col StringMap.t) : carryType =
  let initialData: carryType = findSameNameMatchingGVars oldMap newMap emptyCarryType in

  (*Go through all functions, for all that have not been renamed *)
  findSameNameMatchingFunctions oldMap newMap initialData (fun oldF nowF change_status functionDependencies global_var_dependencies renamesOnSuccess data ->
      let oldG = GlobalMap.find oldF.svar.vname oldMap in
      let nowG = GlobalMap.find nowF.svar.vname newMap in

      match change_status with
      | Unchanged ->
        let doDependenciesMatch, updatedData = doAllDependenciesMatch functionDependencies global_var_dependencies oldMap newMap data in

        if doDependenciesMatch then
          registerBiStatus oldG nowG (SameName(oldG)) updatedData
        else
          registerStatusForOldF oldG (Modified (nowG, true)) data |>
          registerStatusForNowF nowG (Modified (oldG, true))
      | Changed ->
        registerStatusForOldF oldG (Modified (nowG, true)) data |>
        registerStatusForNowF nowG (Modified (oldG, true))
      | _ ->
        registerStatusForOldF oldG (Modified (nowG, false)) data |>
        registerStatusForNowF nowG (Modified (oldG, false))
    ) |>
                  (*At this point we already know of the functions that have changed and stayed the same. We now assign the correct status to all the functions that
                    have been mapped. The functions that have not been mapped are added/removed.*)
                  fillStatusForUnassignedElems oldMap newMap
