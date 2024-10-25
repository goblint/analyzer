(** Autotuning of the configuration based on syntactic heuristics. *)

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

class functionVisitor(calling, calledBy, argLists, dynamicallyCalled) = object
  inherit nopCilVisitor

  method! vglob = function
    | GVarDecl (vinfo,_) ->
      if vinfo.vaddrof && isFunctionType vinfo.vtype then dynamicallyCalled := FunctionSet.add vinfo !dynamicallyCalled;
      DoChildren
    | _ -> DoChildren

  method! vfunc fd =
    let callSet = ref FunctionSet.empty in
    let callVisitor = new collectFunctionCallsVisitor (callSet, calledBy, argLists, fd.svar) in
    ignore @@ Cil.visitCilFunction callVisitor fd;
    calling := FunctionCallMap.add fd.svar !callSet !calling;
    DoChildren
end

type functionCallMaps = {
  calling: FunctionSet.t FunctionCallMap.t;
  calledBy: (FunctionSet.t * int) FunctionCallMap.t;
  argLists: Cil.exp list FunctionCallMap.t;
  dynamicallyCalled: FunctionSet.t;
}

let functionCallMaps = ResettableLazy.from_fun (fun () ->
    let calling = ref FunctionCallMap.empty in
    let calledBy = ref FunctionCallMap.empty in
    let argLists = ref FunctionCallMap.empty in
    let dynamicallyCalled = ref FunctionSet.empty in
    let thisVisitor = new functionVisitor(calling,calledBy, argLists, dynamicallyCalled) in
    visitCilFileSameGlobals thisVisitor (!Cilfacade.current_file);
    {calling = !calling; calledBy = !calledBy; argLists = !argLists; dynamicallyCalled= !dynamicallyCalled})

(* Only considers static calls!*)
let calledFunctions fd = (ResettableLazy.force functionCallMaps).calling |> FunctionCallMap.find_opt fd |> Option.value ~default:FunctionSet.empty
let callingFunctions fd = (ResettableLazy.force functionCallMaps).calledBy |> FunctionCallMap.find_opt fd |> Option.value ~default:(FunctionSet.empty, 0) |> fst
let timesCalled fd = (ResettableLazy.force functionCallMaps).calledBy |> FunctionCallMap.find_opt fd |> Option.value ~default:(FunctionSet.empty, 0) |> snd
let functionArgs fd = (ResettableLazy.force functionCallMaps).argLists |> FunctionCallMap.find_opt fd

let findMallocWrappers () =
  let isMalloc f =
    Goblint_backtrace.wrap_val ~mark:(Cilfacade.FunVarinfo f) @@ fun () ->
    if LibraryFunctions.is_special f then (
      let desc = LibraryFunctions.find f in
      match functionArgs f with
      | None -> false
      | Some args ->
        match desc.special args with
        | Malloc _ -> true
        | _ -> false
    )
    else
      false
  in
  (ResettableLazy.force functionCallMaps).calling
  |> FunctionCallMap.filter (fun _ allCalled -> FunctionSet.exists isMalloc allCalled)
  |> FunctionCallMap.filter (fun f _ -> timesCalled f > 10)
  |> FunctionCallMap.bindings
  |> List.map (fun (v,_) -> v.vname)
  |> List.iter (fun n -> Logs.info "malloc wrapper: %s" n; GobConfig.set_auto "ana.malloc.wrappers[+]" n)


(*Functions for determining if the congruence analysis should be enabled *)
let isExtern = function
  | Extern -> true
  | _ -> false

let rec setCongruenceRecursive fd depth neigbourFunction =
  if depth >= 0 then (
    fd.svar.vattr <- addAttributes (fd.svar.vattr) [Attr ("goblint_precision",[AStr "congruence"])];
    FunctionSet.iter
      (fun vinfo ->
         Logs.info "    %s" vinfo.vname;
         match Cilfacade.find_varinfo_fundec vinfo with
         | fd -> setCongruenceRecursive fd (depth -1) neigbourFunction
         | exception Not_found -> () (* Happens for __goblint_bounded *)
      )
      (FunctionSet.filter (*for extern and builtin functions there is no function definition in CIL*)
         (fun x -> not (isExtern x.vstorage || String.starts_with x.vname ~prefix:"__builtin"))
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
      Logs.info "function %a uses mod, enable congruence domain recursively for:" CilType.Fundec.pretty fd;
      Logs.info "  \"down\":";
      setCongruenceRecursive fd 6 calledFunctions;
      Logs.info "  \"up\":";
      setCongruenceRecursive fd 3 callingFunctions;
      ;
      SkipChildren
end

let addModAttributes file =
  set_bool "annotation.int.enabled" true;
  let thisVisitor = new modFunctionAnnotatorVisitor in
  ignore (visitCilFileSameGlobals thisVisitor file)


let disableIntervalContextsInRecursiveFunctions () =
  (ResettableLazy.force functionCallMaps).calling |> FunctionCallMap.iter (fun f set ->
      (*detect direct recursion and recursion with one indirection*)
      if FunctionSet.mem f set || (not @@ FunctionSet.disjoint (calledFunctions f) (callingFunctions f)) then (
        Logs.info "function %s is recursive, disable interval and interval_set contexts" f.vname;
        f.vattr <- addAttributes (f.vattr) [Attr ("goblint_context",[AStr "base.no-interval"; AStr "base.no-interval_set"; AStr "relation.no-context"])];
      )
    )

let hasFunction pred =
  let relevant_static var =
    Goblint_backtrace.wrap_val ~mark:(Cilfacade.FunVarinfo var) @@ fun () ->
    if LibraryFunctions.is_special var then
      let desc = LibraryFunctions.find var in
      GobOption.exists (fun args -> pred desc args) (functionArgs var)
    else
      false
  in
  let relevant_dynamic var =
    Goblint_backtrace.wrap_val ~mark:(Cilfacade.FunVarinfo var) @@ fun () ->
    if LibraryFunctions.is_special var then
      let desc = LibraryFunctions.find var in
      (* We don't really have arguments at hand, so we cheat and just feed it a list of MyCFG.unknown_exp of appropriate length *)
      match unrollType var.vtype with
      | TFun (_, args, _, _) ->
        let args = BatOption.map_default (List.map (fun (x,_,_) -> MyCFG.unknown_exp)) [] args in
        pred desc args
      | _ -> false
    else
      false
  in
  let calls = ResettableLazy.force functionCallMaps in
  calls.calledBy |> FunctionCallMap.exists (fun var _ -> relevant_static var) ||
  calls.dynamicallyCalled |> FunctionSet.exists relevant_dynamic

let disableAnalyses anas =
  List.iter (GobConfig.set_auto "ana.activated[-]") anas

let enableAnalyses anas =
  List.iter (GobConfig.set_auto "ana.activated[+]") anas

(*If only one thread is used in the program, we can disable most thread analyses*)
(*The exceptions are analyses that are depended on by others: base -> mutex -> mutexEvents, access; termination -> threadflag *)
(*escape is also still enabled, because otherwise we get a warning*)
(*does not consider dynamic calls!*)

let notNeccessaryThreadAnalyses = ["race"; "deadlock"; "maylocks"; "symb_locks"; "thread"; "threadid"; "threadJoins"; "threadreturn"; "mhp"; "region"; "pthreadMutexType"]
let reduceThreadAnalyses () =
  let isThreadCreate (desc: LibraryDesc.t) args =
    match desc.special args with
    | LibraryDesc.ThreadCreate _ -> true
    | _ -> LibraryDesc.Accesses.find_kind desc.accs Spawn args <> []
  in
  let hasThreadCreate = hasFunction isThreadCreate in
  if not @@ hasThreadCreate then (
    Logs.info "no thread creation -> disabling thread analyses \"%s\"" (String.concat ", " notNeccessaryThreadAnalyses);
    disableAnalyses notNeccessaryThreadAnalyses;
  )

let focusOnMemSafetySpecification (spec: Svcomp.Specification.t) =
  match spec with
  | ValidMemtrack
  | ValidMemcleanup ->
    if (get_int "ana.malloc.unique_address_count") < 1 then (
      Logs.info "Setting \"ana.malloc.unique_address_count\" to 5";
      set_int "ana.malloc.unique_address_count" 5;
    );
  | _ -> ()

let focusOnMemSafetySpecification () =
  List.iter focusOnMemSafetySpecification (Svcomp.Specification.of_option ())

let focusOnTermination (spec: Svcomp.Specification.t) =
  match spec with
  | Termination ->
    let terminationAnas = ["threadflag"; "apron"] in
    Logs.info "Specification: Termination -> enabling termination analyses \"%s\"" (String.concat ", " terminationAnas);
    enableAnalyses terminationAnas;
    set_string "sem.int.signed_overflow" "assume_none";
    set_bool "ana.int.interval" true;
    set_string "ana.apron.domain" "polyhedra"; (* TODO: Needed? *)
    ()
  | _ -> ()

let focusOnTermination () =
  List.iter focusOnTermination (Svcomp.Specification.of_option ())

let focusOnSpecification (spec: Svcomp.Specification.t) =
  match spec with
  | UnreachCall s -> ()
  | NoDataRace -> (*enable all thread analyses*)
    Logs.info "Specification: NoDataRace -> enabling thread analyses \"%s\"" (String.concat ", " notNeccessaryThreadAnalyses);
    enableAnalyses notNeccessaryThreadAnalyses;
  | NoOverflow -> (*We focus on integer analysis*)
    set_bool "ana.int.def_exc" true
  | _ -> ()

let focusOnSpecification () =
  List.iter focusOnSpecification (Svcomp.Specification.of_option ())

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

  (*Set arrays with important types for thread analysis to unroll*)
  method! vtype typ =
    let is_important_type (t: typ): bool = match t with
      | TNamed (info, attr) -> List.mem info.tname ["pthread_mutex_t"; "spinlock_t"; "pthread_t"]
      | TInt (IInt, attr) -> hasAttribute "mutex" attr
      | _ -> false
    in
    if is_important_type typ && not @@ hasAttribute "goblint_array_domain" (typeAttrs typ) then
      ChangeTo (typeAddAttributes [Attr ("goblint_array_domain", [AStr "unroll"])] typ)
    else SkipChildren
end

let selectArrayDomains file =
  set_bool "annotation.goblint_array_domain" true;
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

let isGoblintStub v = List.exists (fun (Attr(s,_)) -> s = "goblint_stub") v.vattr

let rec extractVar = function
  | UnOp (Neg, e, _)
  | CastE (_, e) -> extractVar e
  | Lval ((Var info),_) when not (isGoblintStub info) -> Some info
  | _ -> None

let extractBinOpVars e1 e2 =
  match extractVar e1, extractVar e2 with
  | Some a, Some b -> [a; b]
  | Some a, None when isConstant e2 -> [a]
  | None, Some b when isConstant e1 -> [b]
  | _, _ -> []

let extractOctagonVars = function
  | BinOp (PlusA, e1,e2, (TInt _))
  | BinOp (MinusA, e1,e2, (TInt _)) -> extractBinOpVars e1 e2
  | e -> Option.to_list (extractVar e)

let addOrCreateVarMapping varMap key v globals = if key.vglob = globals then varMap :=
      if VariableMap.mem key !varMap then
        let old = VariableMap.find key !varMap in
        VariableMap.add key (old + v) !varMap
      else
        VariableMap.add key v !varMap

class octagonVariableVisitor(varMap, globals) = object
  inherit nopCilVisitor

  method! vexpr = function
    (*an expression of type +/- a +/- b where a,b are either variables or constants*)
    | BinOp (op, e1,e2, (TInt _)) when isComparison op -> (
        List.iter (fun var -> addOrCreateVarMapping varMap var 5 globals) (extractOctagonVars e1);
        List.iter (fun var -> addOrCreateVarMapping varMap var 5 globals) (extractOctagonVars e2);
        DoChildren
      )
    | Lval ((Var info),_) when not (isGoblintStub info) -> addOrCreateVarMapping varMap info 1 globals; SkipChildren
    (*Traverse down only operations fitting for linear equations*)
    | UnOp (LNot, _,_)
    | UnOp (Neg, _,_)
    | BinOp (PlusA,_,_,_)
    | BinOp (MinusA,_,_,_)
    | BinOp (Mult,_,_,_)
    | BinOp (LAnd,_,_,_)
    | BinOp (LOr,_,_,_) -> DoChildren
    | _ -> SkipChildren
end

let topVars n varMap=
  let compareValueDesc = (fun (_,v1) (_,v2) -> - (compare v1 v2)) in
  varMap
  |> VariableMap.bindings
  |> List.sort compareValueDesc
  |> BatList.take n
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

let congruenceOption factors file =
  let locals, globals = factors.integralVars in
  let cost = (locals + globals) * (factors.instructions / 12) + 5 * factors.functionCalls in
  let value = 5 * locals + globals in
  let activate () =
    Logs.debug "Congruence: %d" cost;
    set_bool "ana.int.congruence" true;
    Logs.info "Enabled congruence domain.";
  in
  {
    value;
    cost;
    activate;
  }

let apronOctagonOption factors file =
  let locals =
    if List.mem "specification" (get_string_list "ana.autotune.activated" ) && get_string "ana.specification" <> "" then
      if List.mem Svcomp.Specification.NoOverflow (Svcomp.Specification.of_option ()) then
        12
      else
        8
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
    Logs.debug "Octagon: %d" cost;
    set_bool "annotation.goblint_relation_track" true;
    set_string "ana.apron.domain" "octagon";
    set_auto "ana.activated[+]" "apron";
    set_bool "ana.apron.threshold_widening" true;
    set_string "ana.apron.threshold_widening_constants" "comparisons";
    Logs.info "Enabled octagon domain ONLY for:";
    Logs.info "%s" @@ String.concat ", " @@ List.map (fun info -> info.vname) allVars;
    List.iter (fun info -> info.vattr <- addAttribute (Attr("goblint_relation_track",[])) info.vattr) allVars
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
      Logs.debug "Widening: %d" cost;
      set_bool "ana.int.interval_threshold_widening" true;
      set_string "ana.int.interval_threshold_widening_constants" "comparisons";
      Logs.info "Enabled widening thresholds";
  }

let activateTmpSpecialAnalysis () =
  let isMathFun (desc: LibraryDesc.t) args =
    match desc.special args with
    | LibraryDesc.Math _ -> true
    | _ -> false
  in
  let hasMathFunctions = hasFunction isMathFun in
  if hasMathFunctions then (
    Logs.info "math function -> enabling tmpSpecial analysis and floating-point domain";
    enableAnalyses ["tmpSpecial"];
    set_bool "ana.float.interval" true;
  )

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
    if remainingTarget < 0 then (Logs.debug "Total: %d" (totalTarget - remainingTarget); [] ) else match options with
      | o::os ->
        if o.cost < remainingTarget + costTarget / 20 then (*because we are already estimating, we allow overshooting *)
          o::takeFitting (remainingTarget - o.cost) os
        else
          takeFitting (remainingTarget - o.cost) os
      | [] -> Logs.debug "Total: %d" (totalTarget - remainingTarget); []
  in
  takeFitting costTarget @@ List.sort compareRatio options

let isActivated a = get_bool "ana.autotune.enabled" && List.mem a @@ get_string_list "ana.autotune.activated"

let isTerminationTask () = List.mem Svcomp.Specification.Termination (Svcomp.Specification.of_option ())

let specificationIsActivated () =
  isActivated "specification" && get_string "ana.specification" <> ""

let specificationTerminationIsActivated () =
  isActivated "termination"

let specificationMemSafetyIsActivated () =
  isActivated "memsafetySpecification"

let chooseConfig file =
  let factors = collectFactors visitCilFileSameGlobals file in
  let fileCompplexity = estimateComplexity factors file in

  Logs.debug "Collected factors:";
  printFactors factors;
  Logs.debug "";
  Logs.debug "Complexity estimates:";
  Logs.debug "File: %d" fileCompplexity;

  if fileCompplexity < totalTarget && isActivated "congruence" then
    addModAttributes file;

  if isActivated "noRecursiveIntervals" then
    disableIntervalContextsInRecursiveFunctions ();

  if isActivated "mallocWrappers" then
    findMallocWrappers ();

  if specificationIsActivated () then
    focusOnSpecification ();

  if isActivated "enums" && hasEnums file then
    set_bool "ana.int.enums" true;

  if isActivated "singleThreaded" then
    reduceThreadAnalyses ();

  if isActivated "arrayDomain" then
    selectArrayDomains file;

  if isActivated "tmpSpecialAnalysis" then
    activateTmpSpecialAnalysis ();

  let options = [] in
  let options = if isActivated "congruence" then (congruenceOption factors file)::options else options in
  (* Termination analysis uses apron in a different configuration. *)
  let options = if isActivated "octagon" && not (isTerminationTask ()) then (apronOctagonOption factors file)::options else options in
  let options = if isActivated "wideningThresholds" then (wideningOption factors file)::options else options in

  List.iter (fun o -> o.activate ()) @@ chooseFromOptions (totalTarget - fileCompplexity) options

let reset_lazy () = ResettableLazy.reset functionCallMaps
