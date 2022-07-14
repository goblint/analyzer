open GobConfig
open Cil

(*Collect stats to be able to make decisions*)
type complexityFactors = {
  mutable functions : int; (*function definitions. Does not include extern functions, but functions that get added by goblint (e.g. bsearch or __VERIFIER_nondet_pointer (sv-comp))*)
  mutable functionCalls : int; (*places where functions are called*)
  mutable loops : int;
  mutable loopBreaks : int; (*Breaks and continues. Cil converts the loop conditions to a break. Only set if collection is done before prepareCFG, afterwards they are replaced by GOTOs*)
  mutable controlFlowStatements : int; (*Return, Goto, break, continue, if, switch. Includes at least one (implicit) return in each function*)
  mutable expressions : int; (* recursively. e.g. x = x + 1 has 4 expressions*)
  mutable instructions : int; (*function calls and assignments*)
  mutable integralVars : (int * int); (*global, local. Does not consider the types that a pointer/array contains*)
  mutable arrayVars : (int * int); 
  mutable pointerVars : (int * int);
}

let printFactors f = 
  Printf.printf "functions: %d\n" f.functions;
  Printf.printf "functionCalls: %d\n" f.functionCalls;
  Printf.printf "loops: %d\n" f.loops;
  Printf.printf "loopBreaks: %d\n" f.loopBreaks;
  Printf.printf "controlFlowStatements: %d\n" f.controlFlowStatements;
  Printf.printf "expressions: %d\n" f.expressions;
  Printf.printf "instructions: %d\n" f.instructions;
  Printf.printf "integralVars: (%d,%d)\n" (fst f.integralVars) (snd f.integralVars);
  Printf.printf "arrayVars: (%d,%d)\n" (fst f.arrayVars) (snd f.arrayVars);
  Printf.printf "pointerVars: (%d,%d)\n" (fst f.pointerVars) (snd f.pointerVars);
  flush stdout;

class collectComplexityFactorsVisitor(factors) = object
  inherit nopCilVisitor

  method! vfunc _ = factors.functions <- factors.functions + 1; DoChildren

  method! vvdec var =
    let incVar (g,l) = if var.vglob then (g + 1, l) else (g, l+1) in
    (if isIntegralType var.vtype then 
      factors.integralVars <- incVar factors.integralVars
    else if isPointerType var.vtype then 
      factors.pointerVars <- incVar factors.pointerVars 
    else if isArrayType var.vtype then 
      factors.arrayVars <- incVar factors.arrayVars
    ); DoChildren

  method! vexpr _ = factors.expressions <- factors.expressions + 1; DoChildren

  method! vinst = function 
    | Set _ -> 
      factors.instructions <- factors.instructions + 1; DoChildren
    | Call (Some _, _,_,_,_) -> 
      factors.instructions <- factors.instructions + 2; (*Count function call and assignment of the result seperately *)
      factors.functionCalls <- factors.functionCalls + 1; DoChildren
    | Call _ -> 
      factors.instructions <- factors.instructions + 1;
      factors.functionCalls <- factors.functionCalls + 1; DoChildren 
    | _ -> DoChildren

  method! vstmt stmt = match stmt.skind with 
    | Loop _ -> 
      factors.controlFlowStatements <- factors.controlFlowStatements + 1;
      factors.loops <- factors.loops + 1; DoChildren
    | If _
    | Switch _
    | Goto _
    | ComputedGoto _
    | Return _ -> factors.controlFlowStatements <- factors.controlFlowStatements + 1; DoChildren
    | Break _
    | Continue _ -> 
      factors.controlFlowStatements <- factors.controlFlowStatements + 1;
      factors.loopBreaks <- factors.loopBreaks + 1; DoChildren
    | _ -> DoChildren

end

let collectFactors visitAction visitedObject =
  let factors = {
    functions = 0;
    functionCalls = 0;
    loops = 0;
    loopBreaks = 0;
    controlFlowStatements = 0;
    expressions = 0;
    instructions = 0;
    integralVars = (0,0);
    arrayVars = (0,0);
    pointerVars = (0,0);
  } in
  let visitor = new collectComplexityFactorsVisitor(factors) in
  ignore (visitAction visitor visitedObject);
  factors

(*Create maps that map each function to the ones called in it and the ones calling it
   Only considers static calls!*)
module FunctionSet = Set.Make(CilType.Varinfo)
module FunctionCallMap = Map.Make(CilType.Varinfo)

let addOrCreateMap fd = function
  | Some set -> Some (FunctionSet.add fd set)
  | None     -> Some (FunctionSet.singleton fd) 

class collectFunctionCallsVisitor(callSet, calledBy, argLists, fd) = object
  inherit nopCilVisitor

  method! vinst = function
    | Call (_,Lval ((Var info), NoOffset),args,_,_)->
      (*ignore @@ Pretty.fprint stdout 50 (printInstr defaultCilPrinter () call) ;*)
      callSet := FunctionSet.add info !callSet;
      calledBy := FunctionCallMap.update info (addOrCreateMap fd) !calledBy;
      (*We collect the argument list so we can use LibraryFunctions.find to classify functions*)
      argLists := FunctionCallMap.add info args !argLists;
      (*print_endline @@ fd.vname ^ " -> " ^ info.vname;
      Pretty.fprint stdout (Pretty.d_list "\n" (fun () e -> printExp defaultCilPrinter () e) () args) ~width:50;
      print_endline "\n";*)
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
      (*print_endline fd.svar.vname;
      ignore (dumpGlobal plainCilPrinter stdout (GFun (fd, !currentLoc)));*)
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
(*TODO Extend to dynamic calls?*)
let calledFunctions fd = ResettableLazy.force functionCallMaps |> fun (x,_,_) -> x |> FunctionCallMap.find_opt fd |> Option.value ~default:FunctionSet.empty
let callingFunctions fd = ResettableLazy.force functionCallMaps |> fun (_,x,_) -> x |> FunctionCallMap.find_opt fd |> Option.value ~default:FunctionSet.empty
let functionArgs fd = ResettableLazy.force functionCallMaps |> fun (_,_,x) -> x |> FunctionCallMap.find_opt fd

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
      (FunctionSet.filter 
        (fun x -> not (isExtern x.vstorage || String.starts_with ~prefix:"__builtin" x.vname)) (*for extern and builtin functions there is no function definition in CIL*)
        (neigbourFunction fd.svar) 
      )
    ;
  );;

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
        print_endline ("function " ^ (CilType.Fundec.show fd) ^" uses mod, enable Congruence recursively for:");
        print_endline ("  \"down\":");
        setCongruenceRecursive fd 6 calledFunctions; (* depth? don't do it repeatedly for same function?*)
        print_endline ("  \"up\":");
        setCongruenceRecursive fd 3 callingFunctions;
        (*ignore (dumpGlobal plainCilPrinter stdout (GFun (fd, !currentLoc)))*)
    ;
    SkipChildren
end

let addModAttributes file =
  let thisVisitor = new modFunctionAnnotatorVisitor in
  ignore (visitCilFileSameGlobals thisVisitor file)
  (*TODO: Overflow analysis has to be enabled/assumed to not occur, otherwise the congruence analysis could be wrong*)

let disableIntervalContextsInRecursiveFunctions () =
  ResettableLazy.force functionCallMaps |> fun (x,_,_) -> x |> FunctionCallMap.iter (fun f set ->
    (*detect direct recursion and recursion with one indirection*)
    if FunctionSet.mem f set || (not @@ FunctionSet.disjoint (calledFunctions f) (callingFunctions f)) then (
      print_endline ("function " ^ (f.vname) ^" is recursive, disable interval analysis");
      f.vattr <- addAttributes (f.vattr) [Attr ("goblint_context",[AStr "base.no-interval"])];
    )
  )

(*If only one thread is used in the program, we can disable most thread analyses*)
(*The exceptions are analyses that are depended on by others: base -> mutex -> mutexEvents, access*)
(*escape is also still enabled, because otherwise we get a warning*)

let notNeccessaryThreadAnalyses = ["deadlock"; "maylocks"; "symb_locks"; "thread"; "threadflag"; "threadid"; "threadJoins"; "threadreturn"]

let reduceThreadAnalyses () = 
  (*TODO also consider dynamic calls!?*)
  let hasThreadCreate () = 
    ResettableLazy.force functionCallMaps 
    |> fun (_,x,_) -> x  (*every function that is called*)
    |> FunctionCallMap.exists
      (fun var callers ->
        let desc = LibraryFunctions.find var in
          match (functionArgs var) with
            | None -> false;
            | Some args -> 
              match desc.special args with
              | ThreadCreate _ -> 
                print_endline @@ "thread created in " ^ var.vname ^ ", called by:"; 
                FunctionSet.iter ( fun c -> print_endline @@ "  " ^ c.vname) callers;
                true;
              | _ -> false; 
      ) 
  in

  (*TODO is there a way to specify only the thread analyses to keep?  *)
  if not @@ hasThreadCreate () then (
      print_endline @@ "no thread creation -> disabeling thread analyses \"" ^ (String.concat ", " notNeccessaryThreadAnalyses) ^ "\"";
      let disableAnalysis = GobConfig.set_auto "ana.activated[-]" in
      List.iter disableAnalysis notNeccessaryThreadAnalyses;

    )

let focusOnSpecification () = 
  match Svcomp.Specification.of_option () with 
    | UnreachCall s -> () (*TODO?*)
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

(*TODO: does calling this at a late point cause any problems?*)
(*      do not overwrite explicit settings?*)
(*      how to better display changed/selected settings?*)
(*      tune all constants*)
let chooseConfig file = 
  set_bool "annotation.int.enabled" true;
  addModAttributes file;
  set_bool "ana.int.interval_threshold_widening" true; (*Do not do this all the time?*)

  disableIntervalContextsInRecursiveFunctions ();

  (*crashes because sometimes bigints are needed?
  print_endline @@ "Upper thresholds: " ^ String.concat " " @@ List.map (fun z -> string_of_int (Z.to_int z)) @@ WideningThresholds.upper_thresholds ();
  print_endline @@ "Lower thresholds: " ^ String.concat " " @@ List.map (fun z -> string_of_int (Z.to_int z)) @@ WideningThresholds.lower_thresholds ();*)
  if get_string "ana.specification" <> "" then focusOnSpecification ();
  if hasEnums file then set_bool "ana.int.enums" true;
  reduceThreadAnalyses ();
  printFactors @@ collectFactors visitCilFileSameGlobals file(*;
  ignore (dumpFile plainCilPrinter stdout "?" file)*)

let reset_lazy () = ResettableLazy.reset functionCallMaps