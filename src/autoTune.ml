open GobConfig
open GoblintCil
open AutoTune0


(*Create maps that map each function to the ones called in it and the ones calling it
   Only considers static calls!*)
module FunctionSet = Set.Make(CilType.Varinfo)
module FunctionCallMap = Map.Make(CilType.Varinfo)

let addOrCreateMap fd = function
  | Some (set, i) -> Some (FunctionSet.add fd set, i+1)
  | None     -> Some (FunctionSet.singleton fd, 1)

class collectFunctionCallsVisitor(callSet, calledBy, argLists, fd) = object
  inherit nopCilVisitor

  method! vinst = function
    | Call (_,Lval ((Var info), NoOffset),args,_,_)->
      callSet := FunctionSet.add info !callSet;
      calledBy := FunctionCallMap.update info (addOrCreateMap fd) !calledBy;
      (*We collect the argument list so we can use LibraryFunctions.find to classify functions*)
      argLists := FunctionCallMap.add info args !argLists;
      DoChildren
    | _ -> DoChildren
end

class functionVisitor(calling, calledBy, argLists) = object
  inherit nopCilVisitor

  method! vfunc fd =
    let callSet = ref FunctionSet.empty in
    let callVisitor = new collectFunctionCallsVisitor (callSet, calledBy, argLists, fd.svar) in
    ignore @@ Cil.visitCilFunction callVisitor fd;
    calling := FunctionCallMap.add fd.svar !callSet !calling;
    SkipChildren
end

let functionCallMaps = ResettableLazy.from_fun (fun () ->
    let calling = ref FunctionCallMap.empty in
    let calledBy = ref FunctionCallMap.empty in
    let argLists = ref FunctionCallMap.empty in
    let thisVisitor = new functionVisitor(calling,calledBy, argLists) in
    visitCilFileSameGlobals thisVisitor (!Cilfacade.current_file);
    !calling, !calledBy, !argLists)

(* Only considers static calls!*)
let calledFunctions fd = ResettableLazy.force functionCallMaps |> fun (x,_,_) -> x |> FunctionCallMap.find_opt fd |> Option.value ~default:FunctionSet.empty
let callingFunctions fd = ResettableLazy.force functionCallMaps |> fun (_,x,_) -> x |> FunctionCallMap.find_opt fd |> Option.value ~default:(FunctionSet.empty, 0) |> fst
let timesCalled fd = ResettableLazy.force functionCallMaps |> fun (_,x,_) -> x |> FunctionCallMap.find_opt fd |> Option.value ~default:(FunctionSet.empty, 0) |> snd
let functionArgs fd = ResettableLazy.force functionCallMaps |> fun (_,_,x) -> x |> FunctionCallMap.find_opt fd

let findMallocWrappers () =
  let isMalloc f =
    let desc = LibraryFunctions.find f in
    match (functionArgs f) with
    | None -> false
    | Some args ->
      match desc.special args with
      | Malloc _ -> true
      | _ -> false
  in
  ResettableLazy.force functionCallMaps
  |> (fun (x,_,_) -> x)
  |> FunctionCallMap.filter (fun _ allCalled -> FunctionSet.exists isMalloc allCalled)
  |> FunctionCallMap.filter (fun f _ -> timesCalled f > 10)
  |> FunctionCallMap.bindings
  |> List.map (fun (v,_) -> v.vname)
  |> List.iter (fun n -> print_endline ("malloc wrapper: " ^ n); GobConfig.set_auto "ana.malloc.wrappers[+]" n)


(*Functions for determining if the congruence analysis should be enabled *)
let isExtern = function
  | Extern -> true
  | _ -> false

let rec setCongruenceRecursive fd depth neigbourFunction =
  if depth >= 0 then (
    fd.svar.vattr <- addAttributes (fd.svar.vattr) [Attr ("goblint_precision",[AStr "congruence"])];
    FunctionSet.iter
      (fun vinfo ->
         print_endline ("    " ^ vinfo.vname);
         setCongruenceRecursive (Cilfacade.find_varinfo_fundec vinfo) (depth -1) neigbourFunction
      )
      (FunctionSet.filter (*for extern and builtin functions there is no function definition in CIL*)
         (fun x -> not (isExtern x.vstorage || String.starts_with ~prefix:"__builtin" x.vname))
         (neigbourFunction fd.svar)
      )
    ;
  )

exception ModFound
class modVisitor = object
  inherit nopCilVisitor

  method! vexpr = function
    | BinOp (Mod,_,_,_) ->
      raise ModFound;
    | _ -> DoChildren
end

class modFunctionAnnotatorVisitor = object
  inherit nopCilVisitor

  method! vfunc fd =
    let thisVisitor = new modVisitor in
    try ignore (visitCilFunction thisVisitor fd) with
    | ModFound ->
      print_endline ("function " ^ (CilType.Fundec.show fd) ^" uses mod, enable congruence domain recursively for:");
      print_endline ("  \"down\":");
      setCongruenceRecursive fd 6 calledFunctions;
      print_endline ("  \"up\":");
      setCongruenceRecursive fd 3 callingFunctions;
      ;
      SkipChildren
end

let addModAttributes file =
  set_bool "annotation.int.enabled" true;
  let thisVisitor = new modFunctionAnnotatorVisitor in
  ignore (visitCilFileSameGlobals thisVisitor file)


let disableIntervalContextsInRecursiveFunctions () =
  ResettableLazy.force functionCallMaps |> fun (x,_,_) -> x |> FunctionCallMap.iter (fun f set ->
      (*detect direct recursion and recursion with one indirection*)
      if FunctionSet.mem f set || (not @@ FunctionSet.disjoint (calledFunctions f) (callingFunctions f)) then (
        print_endline ("function " ^ (f.vname) ^" is recursive, disable interval context");
        f.vattr <- addAttributes (f.vattr) [Attr ("goblint_context",[AStr "base.no-interval"])];
      )
    )

(*If only one thread is used in the program, we can disable most thread analyses*)
(*The exceptions are analyses that are depended on by others: base -> mutex -> mutexEvents, access*)
(*escape is also still enabled, because otherwise we get a warning*)
(*does not consider dynamic calls!*)

let notNeccessaryThreadAnalyses = ["deadlock"; "maylocks"; "symb_locks"; "thread"; "threadflag"; "threadid"; "threadJoins"; "threadreturn"]
let reduceThreadAnalyses () =
  let hasThreadCreate () =
    ResettableLazy.force functionCallMaps
    |> fun (_,x,_) -> x  (*every function that is called*)
                      |> FunctionCallMap.exists
                        (fun var (callers,_) ->
                           let desc = LibraryFunctions.find var in
                           match (functionArgs var) with
                           | None -> false;
                           | Some args ->
                             match desc.special args with
                             | ThreadCreate _ ->
                               print_endline @@ "thread created by " ^ var.vname ^ ", called by:";
                               FunctionSet.iter ( fun c -> print_endline @@ "  " ^ c.vname) callers;
                               true;
                             | _ -> false;
                        )
  in
  if not @@ hasThreadCreate () then (
    print_endline @@ "no thread creation -> disabeling thread analyses \"" ^ (String.concat ", " notNeccessaryThreadAnalyses) ^ "\"";
    let disableAnalysis = GobConfig.set_auto "ana.activated[-]" in
    List.iter disableAnalysis notNeccessaryThreadAnalyses;

  )

let focusOnSpecification () =
  match Svcomp.Specification.of_option () with
  | UnreachCall s -> ()
  | NoDataRace -> (*enable all thread analyses*)
    print_endline @@ "Specification: NoDataRace -> enabeling thread analyses \"" ^ (String.concat ", " notNeccessaryThreadAnalyses) ^ "\"";
    let enableAnalysis = GobConfig.set_auto "ana.activated[+]" in
    List.iter enableAnalysis notNeccessaryThreadAnalyses;
  | NoOverflow -> (*We focus on integer analysis*)
    set_bool "ana.int.def_exc" true;
    set_bool "ana.int.interval" true

(*Detect enumerations and enable the "ana.int.enums" option*)
exception EnumFound
class enumVisitor = object
  inherit nopCilVisitor

  method! vglob = function
    | GEnumTag _
    | GEnumTagDecl _ ->
      raise EnumFound;
    | _ -> SkipChildren;
end

let hasEnums file =
  let thisVisitor = new enumVisitor in
  try
    ignore (visitCilFileSameGlobals thisVisitor file);
    false;
  with EnumFound -> true


class addTypeAttributeVisitor = object
  inherit nopCilVisitor

  (*large arrays -> partitioned*)
  (*because unroll only is usefull if most values are actually unrolled*)
  method! vvdec info =
    (if is_large_array info.vtype && not @@ hasAttribute "goblint_array_domain" (typeAttrs info.vtype) then
       info.vattr <- addAttribute (Attr ("goblint_array_domain", [AStr "partitioned"])) info.vattr);
    DoChildren

  (*Set arrays with important types to unroll*)
  method! vtype typ =
    if is_important_type typ && not @@ hasAttribute "goblint_array_domain" (typeAttrs typ) then
      ChangeTo (typeAddAttributes [Attr ("goblint_array_domain", [AStr "unroll"])] typ)
    else SkipChildren
end

let selectArrayDomains file =
  set_bool "annotation.array" true;
  let thisVisitor = new addTypeAttributeVisitor in
  ignore (visitCilFileSameGlobals thisVisitor file)
(*small unrolled loops also set domain of accessed arrays to unroll, at the point where loops are unrolled*)


(*option that can be selected based on value/cost*)
type option = {
  value:int;
  cost:int;
  activate: unit -> unit
}

(*Option for activating the octagon apron domain on selected vars*)
module VariableMap = Map.Make(CilType.Varinfo)
module VariableSet = Set.Make(CilType.Varinfo)

let isComparison = function
  | Lt | Gt |	Le | Ge | Ne | Eq -> true
  | _ -> false

let rec extractVar = function
  | UnOp (Neg, e, _) -> extractVar e
  | Lval ((Var info),_) -> Some info
  | _ -> None

let extractOctagonVars = function
  | BinOp (PlusA, e1,e2, (TInt _))
  | BinOp (MinusA, e1,e2, (TInt _)) -> (
      match extractVar e1, extractVar e2 with
      | Some a, Some b -> Some (Either.Left (a,b))
      | Some a, None
      | None, Some a -> if isConstant e1 then Some (Either.Right a) else None
      | _,_ -> None
    )
  | _ -> None

let addOrCreateVarMapping varMap key v globals = if key.vglob = globals then varMap :=
      if VariableMap.mem key !varMap then
        let old = VariableMap.find key !varMap in
        VariableMap.add key (old + v) !varMap
      else
        VariableMap.add key v !varMap

let handle varMap v globals = function
  | Some (Either.Left (a,b)) ->
    addOrCreateVarMapping varMap a v globals;
    addOrCreateVarMapping varMap b v globals;
  | Some (Either.Right a) ->  addOrCreateVarMapping varMap a v globals;
  | None -> ()

class octagonVariableVisitor(varMap, globals) = object
  inherit nopCilVisitor

  method! vexpr = function
    (*an expression of type +/- a +/- b where a,b are either variables or constants*)
    | BinOp (op, e1,e2, (TInt _)) when isComparison op -> (
        handle varMap 5 globals (extractOctagonVars e1) ;
        handle varMap 5 globals (extractOctagonVars e2) ;
        DoChildren
      )
    | Lval ((Var info),_) -> handle varMap 1 globals (Some (Either.Right info)) ; SkipChildren
    (*Traverse down only operations fitting for linear equations*)
    | UnOp (Neg, _,_)
    | BinOp (PlusA,_,_,_)
    | BinOp (MinusA,_,_,_)
    | BinOp (Mult,_,_,_)
    | BinOp (LAnd,_,_,_)
    | BinOp (LOr,_,_,_) -> DoChildren
    | _ -> SkipChildren
end

let topVars n varMap=
  let rec take n l =
    if n <= 0 then
      []
    else
      match l with
      | x :: xs -> x :: take (n-1) xs
      | [] -> []
  in
  let compareValueDesc = (fun (_,v1) (_,v2) -> - (compare v1 v2)) in
  varMap
  |> VariableMap.bindings
  |> List.sort compareValueDesc
  |> take n
  |> List.map fst

class octagonFunctionVisitor(list, amount) = object
  inherit nopCilVisitor

  method! vfunc f =
    let varMap = ref VariableMap.empty in
    let visitor = new octagonVariableVisitor(varMap, false) in
    ignore (visitCilFunction visitor f);

    list := topVars amount !varMap ::!list;
    SkipChildren

end

let apronOctagonOption factors file =
  let locals =
    if List.mem "specification" (get_string_list "ana.autotune.activated" ) && get_string "ana.specification" <> "" then
      match Svcomp.Specification.of_option () with
      | NoOverflow -> 12
      | _ -> 8
    else 8
  in let globals = 2 in
  let selectedLocals =
    let list = ref [] in
    let visitor = new octagonFunctionVisitor(list, locals) in
    visitCilFileSameGlobals visitor file;
    List.concat !list
  in
  let selectedGlobals =
    let varMap = ref VariableMap.empty in
    let visitor = new octagonVariableVisitor(varMap, true) in
    visitCilFileSameGlobals visitor file;
    topVars globals !varMap
  in
  let allVars = (selectedGlobals @ selectedLocals) in
  let cost = (Batteries.Int.pow (locals + globals) 3) * (factors.instructions / 70) in
  let activateVars () =
    print_endline @@ "Octagon: " ^ string_of_int cost;
    set_bool "annotation.track_apron" true;
    set_string "ana.apron.domain" "octagon";
    set_auto "ana.activated[+]" "apron";
    set_bool "ana.apron.threshold_widening" true;
    set_string "ana.apron.threshold_widening_constants" "comparisons";
    print_endline "Enabled octagon domain for:";
    print_endline @@ String.concat ", " @@ List.map (fun info -> info.vname) allVars;
    List.iter (fun info -> info.vattr <- addAttribute (Attr("goblint_apron_track",[])) info.vattr) allVars
  in
  {
    value = 50 * (List.length allVars) ;
    cost = cost;
    activate = activateVars;
  }


let wideningOption factors file =
  let amountConsts = List.length @@ WideningThresholds.upper_thresholds () in
  let cost = amountConsts * (factors.loops * 5 + factors.controlFlowStatements) in
  {
    value = amountConsts * (factors.loops * 5 + factors.controlFlowStatements);
    cost = cost;
    activate = fun () ->
      print_endline @@ "Widening: " ^ string_of_int cost;
      set_bool "ana.int.interval_threshold_widening" true;
      set_string "ana.int.interval_threshold_widening_constants" "comparisons";
      print_endline "Enabled widening thresholds";
  }


let estimateComplexity factors file =
  let pathsEstimate = factors.loops + factors.controlFlowStatements / 90 in
  let operationEstimate = factors.instructions + (factors.expressions / 60) in
  let callsEstimate = factors.functionCalls * factors.loops / factors.functions / 10 in
  let globalVars = fst factors.pointerVars * 2 + fst factors.arrayVars * 4 + fst factors.integralVars in
  let localVars = snd factors.pointerVars * 2 + snd factors.arrayVars * 4 + snd factors.integralVars in
  let varEstimates = globalVars + localVars / factors.functions in
  pathsEstimate * operationEstimate * callsEstimate + varEstimates / 10

let totalTarget = 30000
(*A simple greedy approximation to the knapsack problem:
  take options with the highest use/cost ratio that still fit*)
let chooseFromOptions costTarget options =
  let ratio o = Float.of_int o.value /. Float.of_int o.cost in
  let compareRatio o1 o2 = Float.compare (ratio o1) (ratio o2) in
  let rec takeFitting remainingTarget options =
    if remainingTarget < 0 then (print_endline @@ "Total: " ^ string_of_int (totalTarget - remainingTarget); [] ) else match options with
      | o::os ->
        if o.cost < remainingTarget + costTarget / 20 then (*because we are already estimating, we allow overshooting *)
          o::takeFitting (remainingTarget - o.cost) os
        else
          takeFitting (remainingTarget - o.cost) os
      | [] -> print_endline @@ "Total: " ^ string_of_int (totalTarget - remainingTarget); []
  in
  takeFitting costTarget @@ List.sort compareRatio options

let isActivated a = get_bool "ana.autotune.enabled" && List.mem a @@ get_string_list "ana.autotune.activated"

let chooseConfig file =
  if isActivated "congruence" then
    addModAttributes file;

  if isActivated "noRecursiveIntervals" then
    disableIntervalContextsInRecursiveFunctions ();

  if isActivated "mallocWrappers" then
    findMallocWrappers ();

  if isActivated "specification" && get_string "ana.specification" <> "" then
    focusOnSpecification ();

  if isActivated "enums" && hasEnums file then
    set_bool "ana.int.enums" true;

  if isActivated "singleThreaded" then
    reduceThreadAnalyses ();

  if isActivated "arrayDomain" then
    selectArrayDomains file;

  let factors = collectFactors visitCilFileSameGlobals file in
  let fileCompplexity = estimateComplexity factors file in

  print_endline "Collected factors:";
  printFactors factors;
  print_endline "";
  print_endline "Complexity estimates:";
  print_endline @@ "File: " ^ string_of_int fileCompplexity;

  let options = [] in
  let options = if isActivated "octagon" then (apronOctagonOption factors file)::options else options in
  let options = if isActivated "wideningThresholds" then (wideningOption factors file)::options else options in

  List.iter (fun o -> o.activate ()) @@ chooseFromOptions (totalTarget - fileCompplexity) options


let reset_lazy () = ResettableLazy.reset functionCallMaps
