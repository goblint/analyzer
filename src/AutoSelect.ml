open GobConfig;;
open Cil;;

(*Create maps that map each function to the ones called in it and the ones calling it*)
(* TODO rewrite with Syntacticsearch?: https://github.com/goblint/cil/blob/develop/src/ext/syntacticsearch/funcFunction.ml
  Problems: No access to Storage variable to determine if fuction is extern
            No direct way to map uses to function it is called from
            No included filtering of duplicate functions?

let calledFunctionsQuery = {
    sel = [Name_sel, ID_sel]; (*Can we get the storage directly? *)
    k = Fun_k; 
    tar = All_t;
    f = Uses_f; (*Does this select only function calls without Parameters? *)
    str = None_s; (* Only search in the specific function*)
  }
*)

module Varinfo = struct 
  type t = varinfo
  let compare v1 v2 = Stdlib.compare v1.vid v2.vid
end;;

module FunctionSet = Set.Make(Varinfo);;
module FunctionCallMap = Map.Make(Varinfo);;

let addOrCreateMap fd = function
  | Some set -> Some (FunctionSet.add fd set)
  | None     -> Some (FunctionSet.singleton fd) ;;

class collectFunctionCallsVisitor(callSet, calledBy, argLists, fd) = object
  inherit nopCilVisitor

  method! vinst = function
    | Call (_,Lval ((Var info), NoOffset),args,_,_) as call->
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
end;;

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
  end;;

let functionCallMaps = ResettableLazy.from_fun (fun () ->
  let calling = ref FunctionCallMap.empty in
  let calledBy = ref FunctionCallMap.empty in
  let argLists = ref FunctionCallMap.empty in
  let thisVisitor = new functionVisitor(calling,calledBy, argLists) in
  visitCilFileSameGlobals thisVisitor (!Cilfacade.current_file);
  !calling, !calledBy, !argLists);;

(*TODO Extend to dynamic calls?*)
let calledFunctions fd = ResettableLazy.force functionCallMaps |> fun (x,_,_) -> x |> FunctionCallMap.find_opt fd |> Option.value ~default:FunctionSet.empty;;
let callingFunctions fd = ResettableLazy.force functionCallMaps |> fun (_,x,_) -> x |> FunctionCallMap.find_opt fd |> Option.value ~default:FunctionSet.empty;;
let functionArgs fd = ResettableLazy.force functionCallMaps |> fun (_,_,x) -> x |> FunctionCallMap.find_opt fd;;

(*Functions for determining if the Congruence analysis should be enabled *)
let isNotExtern = function
  | Extern -> false
  | _ -> true;;

let rec setCongruenceRecursive fd depth neigbourFunction =
  if depth >= 0 then (
    fd.svar.vattr <- addAttributes (fd.svar.vattr) [Attr ("goblint_precision",[AStr "congruence"])];
    FunctionSet.iter 
      (fun vinfo -> 
        print_endline ("    " ^ vinfo.vname);
        setCongruenceRecursive (Cilfacade.find_varinfo_fundec vinfo) (depth -1) neigbourFunction
      ) 
      (FunctionSet.filter 
        (fun x -> isNotExtern x.vstorage) 
        (neigbourFunction fd.svar) 
      )
    ;
  );;

(*Because we now ignore the total count and only care if it exists, this should be rewritten to stop after the first mod*)
(* -> No longer a visitor, but a recursive search?*)  
class modCountVisitor(count) = object
  inherit nopCilVisitor

  method! vexpr = function
    | BinOp (Mod,_,_,_) ->
      count := 1 + !count;
      DoChildren
    | _ -> DoChildren
end;;

class modFunctionAnnotatorVisitor = object
  inherit nopCilVisitor

  method! vfunc fd = 
    let count = ref 0 in
    let thisVisitor = new modCountVisitor(count) in
    ignore (visitCilFunction thisVisitor fd);
    if !count > 0 then (
      print_endline ("function " ^ (CilType.Fundec.show fd) ^" uses mod, enable Congruence recursively for:");
      print_endline ("  \"down\":");
      setCongruenceRecursive fd 6 calledFunctions; (* depth? don't do it repeatedly for same function?*)
      print_endline ("  \"up\":");
      setCongruenceRecursive fd 3 callingFunctions;
      (*ignore (dumpGlobal plainCilPrinter stdout (GFun (fd, !currentLoc)))*)
    );
    SkipChildren
end;;  

let addModAttributes file =
  let thisVisitor = new modFunctionAnnotatorVisitor in
  ignore (visitCilFileSameGlobals thisVisitor file);;
  (*TODO: Overflow analysis has to be enabled/assumed to not occur, else there are problems?*)

let disableIntervalContextsInRecursiveFunctions () =
  ResettableLazy.force functionCallMaps |> fun (x,_,_) -> x |> FunctionCallMap.iter (fun f set ->
    (*detect direct recursion and recursion with one indirection*)
    if FunctionSet.mem f set || (not @@ FunctionSet.disjoint (calledFunctions f) (callingFunctions f)) then (
      print_endline ("function " ^ (f.vname) ^" is recursive, disable interval analysis");
      f.vattr <- addAttributes (f.vattr) [Attr ("goblint_context",[AStr "base.no-interval"])];
    )
  );;

(*If only one Thread is used in the program, we can disable most thread analyses*)
(*The exceptions are analyses that are depended on by others: base -> mutex -> mutexEvents, access*)
(*TODO escape is also still enabled, because I do not know if the analysis gets more imprecise if not*)

let toJsonArray list = "[\"" ^ (String.concat "\",\"" list) ^ "\"]";;
let notNeccessaryThreadAnalyses = ["deadlock"; "maylocks"; "symb_locks"; "thread"; "threadflag"; "threadid"; "threadJoins"; "threadreturn"];;

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
      get_string_list "ana.activated" 
      |> List.filter (fun l -> not (List.mem l notNeccessaryThreadAnalyses))
      |> toJsonArray
      |> set_auto "ana.activated"

    )
 ;;

let focusOnSpecification () = 
  match Svcomp.Specification.of_option () with 
    | UnreachCall s -> () (*TODO?*)
    | NoDataRace -> (*enable all thread analyses*)
      print_endline @@ "Specification: NoDataRace -> enabeling thread analyses \"" ^ (String.concat ", " notNeccessaryThreadAnalyses) ^ "\"";
      get_string_list "ana.activated" 
      |> List.filter (fun l -> not (List.mem l notNeccessaryThreadAnalyses)) (*remove duplicates*)
      |> (@) notNeccessaryThreadAnalyses
      |> toJsonArray
      |> set_auto "ana.activated";
    | NoOverflow -> (*We focus on integer analysis*)
      set_bool "ana.int.def_exc" true;
      set_bool "ana.int.enums" true;
      set_bool "ana.int.interval" true;;


(*TODO: does calling this at a late point cause any problems?*)
(*      do not overwrite explicit settings?*)
(*      how to better display changed/selected settings?*)
let chooseConfig file = 
  set_bool "annotation.int.enabled" true;
  addModAttributes file;
  set_bool "ana.int.interval_threshold_widening" true; (*Do not do this all the time?*)

  disableIntervalContextsInRecursiveFunctions ();

(*crashes because sometimes bigints are needed  
  print_endline @@ "Upper thresholds: " ^ String.concat " " @@ List.map (fun z -> string_of_int (Z.to_int z)) @@ WideningThresholds.upper_thresholds ();
  print_endline @@ "Lower thresholds: " ^ String.concat " " @@ List.map (fun z -> string_of_int (Z.to_int z)) @@ WideningThresholds.lower_thresholds ();*)
  if get_string "ana.specification" <> "" then focusOnSpecification ();
  reduceThreadAnalyses ();
;;

let reset_lazy () = ResettableLazy.reset functionCallMaps;;