open Cil
open MyCFG
include CompareAST
include CompareCFG

module StringSet = Set.Make(String)

type f = fundec * location

type dependencyPointer = {oldName: string; dependencyCount: int}

module OldFunNameWithDependencyCount = struct
  type t = dependencyPointer
  let compare x y = Int.compare x.dependencyCount y.dependencyCount
end

module FundecForMap = struct
  type t = Cil.fundec

  let compare x y = Int.compare x.svar.vid y.svar.vid
end

module DependencyHeap = BatHeap.Make(OldFunNameWithDependencyCount)

module FundecMap = Map.Make(FundecForMap)

(*A dependency maps the function it depends on to the name the function has to be changed to*)
type dependencies = string StringMap.t

(*The dependents map the functions that depend with the name they need it to be changed to*)
type dependents = string StringMap.t

(*hadWrongAssumption: set to true, if one of the depencies this node had, was wrong. Thus this node is changed.*)
type nodeData = {nowName: string; dependencies: dependencies; dependents: dependents; hadWrongAssumption: bool}

type renameData = {nowName: string; dependencies: dependencies}

(*A direct match means that the function name stayed the same and the old and new function match (with contraints defined by dependencies). *)
type earlyFunctionStatus = DirectMatch of dependencies | Changed | Unknown

(*Renamed: newName * dependencies*)
type functionStatus = SameName of dependencies | Renamed of renameData | ChangedOrNewOrDeleted
type results = functionStatus StringMap.t

type output = global * functionStatus


let getFunctionMap (ast: file) : f StringMap.t =
  Cil.foldGlobals ast (fun map global ->
      match global with
      | GFun (fundec, location) -> StringMap.add fundec.svar.vname (fundec, location) map
      | _ -> map
    ) StringMap.empty

let getDependencies fromEq = StringMap.map (fun assumption -> assumption.new_method_name) fromEq

(*Split the functions up in those which have not been renamed, and such which have been renamed, are new or have been deleted*)
let seperateUnchangedFunctions (oldFunctionMap: f StringMap.t) (nowFunctionMap: f StringMap.t) : earlyFunctionStatus StringMap.t =
  StringMap.map (fun (f, _) ->
      let matchingNewFundec = StringMap.find_opt f.svar.vname nowFunctionMap in
      match matchingNewFundec with
      | Some (newFun, _) ->
        (*Compare if they are similar*)
        let doMatch, _, _, dependencies = CompareGlobals.eqF f newFun None StringMap.empty in
        if doMatch then DirectMatch(getDependencies dependencies)
        else Unknown
      | None -> Unknown
    ) oldFunctionMap

(*
Tries to find a partner for each method that is not a direct match.
Returns the found partner for each unknown function with the rename dependencies or ChangedOrNewOrDeleted if no partner was found.
Use sets instead of lists, because member lookups are faster in sets.*)
let categorizeUnknownFunctions
    (unknownFunctions: StringSet.t)
    (directMatchFunctions: StringSet.t)
    (oldFunctionMap: f StringMap.t)
    (nowFunctionMap: f StringMap.t) : functionStatus StringMap.t =
  let nowFunctionMapWithoutDirectMatchFunctions = StringMap.filter (fun key _ -> not (StringSet.mem key directMatchFunctions)) nowFunctionMap in

  StringSet.fold (fun functionWithUnknownStatusName map ->
      (*The unknown functions directly come from the oldFunctionMap, so there has to be an entry.*)
      let (functionWithUnknownStatusFundec, _) = StringMap.find functionWithUnknownStatusName oldFunctionMap in

      (*Find the first match in all new unknown functions: O(all_functions - direct_functions)*)
      let foundFunctionMatch =
        StringMap.to_seq nowFunctionMapWithoutDirectMatchFunctions |>
        Seq.map (fun (name, (f, _)) -> name, f) |>
        Seq.find_map (fun (nowFunName, nowFunFundec) ->
            let doMatch, _, _, dependencies = CompareGlobals.eqF functionWithUnknownStatusFundec nowFunFundec None StringMap.empty in
            if doMatch then Option.some (
                {nowName = nowFunName; dependencies = getDependencies dependencies}
              ) else None
          ) in

      match foundFunctionMatch with
      | Some renameData -> StringMap.add functionWithUnknownStatusName (Renamed(renameData)) map
      | None -> StringMap.add functionWithUnknownStatusName ChangedOrNewOrDeleted map
    ) unknownFunctions StringMap.empty


(*Marks the changed node as changed in the results and also marks all nodes that depend on that node as changed. *)
let rec propagateChangedNode (changedNodeOldName: string)
    (nodeMap: nodeData StringMap.t)
    (dependencyHeap: DependencyHeap.t)
    (currentResults: results) : (nodeData StringMap.t) * (DependencyHeap.t) * (results) =
  let resultsWithChangedNode = StringMap.add changedNodeOldName ChangedOrNewOrDeleted currentResults in
  let changedNodeData = StringMap.find changedNodeOldName nodeMap in
  (*BatHeap does not support removing an element directly. Maybe we should use a different implementation.*)
  let dependencyHeapWithoutChangedNode: DependencyHeap.t =
    dependencyHeap |>
    DependencyHeap.to_list |>
    List.filter (fun pointer -> pointer.oldName <> changedNodeOldName) |>
    DependencyHeap.of_list in

  changedNodeData.dependents |>
  StringMap.to_seq |>
  Seq.fold_left (fun (nodeMap, dependencyHeap, currentResults) (dependentName, _) ->
      propagateChangedNode dependentName nodeMap dependencyHeap currentResults
    ) (nodeMap, dependencyHeapWithoutChangedNode, resultsWithChangedNode)

(* Takes the node with the currently least dependencies and tries to reduce the graph from that node.
   Cyclic dependency graphs are currently not supported. If a cyclic dependency is found, all remaining nodes are marked as changed.

   Function is applied recursivly until no nodes remain in the graph.
*)
let rec reduceNodeGraph (nodeMap: nodeData StringMap.t) (dependencyHeap: DependencyHeap.t) (currentResults: results) : results =
  if DependencyHeap.size dependencyHeap = 0 then currentResults
  else
    let topDependencyPointer = DependencyHeap.find_min dependencyHeap in
    let currentNode = StringMap.find topDependencyPointer.oldName nodeMap in

    let newDependencyHeap = DependencyHeap.del_min dependencyHeap in

    if topDependencyPointer.dependencyCount = 0 then
      (*Remove this node from the dependecies of the nodes that depend on it.
         The nodes that depend on the wrong name are set to be changed.*)
      let newNodeMap = currentNode.dependents |>
                       StringMap.to_seq |>
                       Seq.fold_left (fun nodeMap (dependingFun, dependingOnName) ->
                           let dependeeNodeData: nodeData = StringMap.find dependingFun nodeMap in

                           (*Remove the dependency of current node from the dependencies of the dependee*)
                           let newDependencies = dependeeNodeData.dependencies |>
                                                 StringMap.filter (fun dependingName _ -> dependingName <> topDependencyPointer.oldName)
                           in

                           let hadWrongAssumption = if currentNode.nowName <> dependingOnName then true
                             else dependeeNodeData.hadWrongAssumption
                           in

                           let newNodeData = {
                             nowName = dependeeNodeData.nowName;
                             dependencies = newDependencies;
                             dependents = dependeeNodeData.dependents;
                             hadWrongAssumption = hadWrongAssumption
                           } in

                           (*Replace node data in map*)
                           let newNodeMap = StringMap.add dependingFun newNodeData nodeMap in

                           newNodeMap

                         ) nodeMap in

      let status = if currentNode.hadWrongAssumption then ChangedOrNewOrDeleted else Renamed({nowName=currentNode.nowName; dependencies=currentNode.dependencies}) in

      let newResults = StringMap.add topDependencyPointer.oldName status currentResults in

      reduceNodeGraph newNodeMap newDependencyHeap newResults
    else
      (*Cyclic dependency found. *)
      (*Mark all remaining nodes with dependencies as changed.*)
      DependencyHeap.to_list dependencyHeap |>
      List.fold_left (fun results dependencyPointer ->
          StringMap.add dependencyPointer.oldName ChangedOrNewOrDeleted results
        ) currentResults

let detectRenamedFunctions (oldAST: file) (newAST: file) : output FundecMap.t = begin
  let oldFunctionMap = getFunctionMap oldAST in
  let nowFunctionMap = getFunctionMap newAST in

  (*1. detect function which names have not changed*)
  let statusForFunction = seperateUnchangedFunctions oldFunctionMap nowFunctionMap in

  let directMatchFunctions, knownChangedFunctions, unknownFunctions, initialCategorization = StringMap.fold (
      fun funName earlyStatus (directMatchFunctions, knownChangedFunctions, unknownFunctions, initialCategorization) -> match earlyStatus with
        | DirectMatch d -> (
            StringSet.add funName directMatchFunctions,
            knownChangedFunctions,
            unknownFunctions,
            StringMap.add funName (SameName(d)) initialCategorization
          )
        | Changed -> (
            directMatchFunctions,
            StringSet.add funName knownChangedFunctions,
            unknownFunctions,
            StringMap.add funName ChangedOrNewOrDeleted initialCategorization
          )
        | Unknown -> (
            directMatchFunctions,
            knownChangedFunctions,
            StringSet.add funName unknownFunctions,
            initialCategorization
          )
    ) statusForFunction (StringSet.empty, StringSet.empty, StringSet.empty, StringMap.empty) in

  (*2. get dependencies of those functions we did match in 1.
     These function statuses are just early guesses. They still need to be checked and adapted in the graph analysis.*)
  let categorizationResults = categorizeUnknownFunctions unknownFunctions directMatchFunctions oldFunctionMap nowFunctionMap in

  (*3. build dependency graph*)
  let categorizationMap = StringMap.union (fun _ _ _ -> None) initialCategorization categorizationResults in

  (*dependentsMap<oldFunName, dependentsMap<fun that depends on this function, the function name it should have now>>*)
  (*Generate the dependents map now, so it does not have to be done when generating the node map*)
  let dependentsMap: string StringMap.t StringMap.t = StringMap.fold (fun oldFunName functionStatus dependentsMap ->
      (*Go through all dependencies and add itself to the list of dependents*)
      let addDependents dependencies = StringMap.fold (fun dependingOn hasToBeNamed dependentsMap ->
          let currentDependents = StringMap.find_opt dependingOn dependentsMap |>
                                  Option.value ~default:StringMap.empty in

          let newDependents = StringMap.add oldFunName hasToBeNamed currentDependents in

          StringMap.add dependingOn newDependents dependentsMap
        ) dependencies dependentsMap
      in

      match functionStatus with
      | SameName dependencies -> addDependents dependencies
      | Renamed renameData -> addDependents renameData.dependencies
      | ChangedOrNewOrDeleted -> dependentsMap
    ) categorizationMap StringMap.empty in

  (*The nodes are represented in the node map<oldName, nodeData>. The node data contains the nowName,
    and the nodes it depends on as well as the nodes that depend on that node.
    The dependencyHeap points to the function name with the currently least dependencies.*)
  let (nodeMap: nodeData StringMap.t), (dependencyHeap: DependencyHeap.t) =
    StringMap.fold (fun oldFunName functionStatus (nodeMap, dependencyHeap) ->
        let dependents = StringMap.find_opt oldFunName dependentsMap |>
                         Option.value ~default:StringMap.empty in

        let getNodeEntry dependencies = {nowName=oldFunName; dependencies = dependencies; dependents = dependents; hadWrongAssumption = false} in
        let getDependencyPointer dependencies = {oldName=oldFunName; dependencyCount=StringMap.cardinal dependencies} in

        match functionStatus with
        | SameName dependencies ->
          (
            StringMap.add oldFunName (getNodeEntry dependencies) nodeMap,
            DependencyHeap.add (getDependencyPointer dependencies) dependencyHeap
          )
        | Renamed renameData ->
          (
            StringMap.add oldFunName (getNodeEntry renameData.dependencies) nodeMap,
            DependencyHeap.add (getDependencyPointer renameData.dependencies) dependencyHeap
          )
        | ChangedOrNewOrDeleted -> (nodeMap, dependencyHeap)
      ) categorizationMap (StringMap.empty, DependencyHeap.empty) in


  let result = reduceNodeGraph nodeMap dependencyHeap StringMap.empty in

  let x = StringMap.to_seq result |>
    Seq.map (fun (oldName, status) ->
      let (f, l) = StringMap.find oldName oldFunctionMap in
      f, (GFun(f, l), status)) in

  FundecMap.add_seq x FundecMap.empty
end
