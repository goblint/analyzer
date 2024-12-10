(** Automatically turning on analyses required to ensure soundness
    based on a given specification (e.g., SV-COMP specification)
    or programming idioms (e.g., longjmp) in the analyzed code,
    but only when it is possible to do so automatically.
    This does not fully exempt from the need for manual configuration.
*)

open GobConfig
open AutoTune

let enableSpecAnalyses spec analyses =
  Logs.info "Specification: %s -> enabling soundness analyses \"%s\"" (Svcomp.Specification.to_string [spec]) (String.concat ", " analyses);
  enableAnalyses analyses

let enableOptions options =
  let enableOpt option =
    Logs.info "Setting \"%s\" to true" option;
    set_bool option true
  in
  List.iter enableOpt options

(** Selecting sound configurations based on SV-COMP specification.
    Activating the analyses and enabling options that are
    required for analyzing the property defined in the specification.
    TODO: have only one function for matching all specifications and find a place where it can be called.
*)
let enableAnalysesForMemSafetySpecification (spec: Svcomp.Specification.t) =
  match spec with
  | ValidFree -> enableSpecAnalyses spec ["base"; "useAfterFree"];
  | ValidDeref ->
    enableSpecAnalyses spec ["base"; "memOutOfBounds"];
    enableOptions ["ana.arrayoob"; "cil.addNestedScopeAttr"]
  | ValidMemtrack
  | ValidMemcleanup -> enableSpecAnalyses spec ["memLeak"];
  | _ -> ()

let enableAnalysesForMemSafetySpecification () =
  List.iter enableAnalysesForMemSafetySpecification (Svcomp.Specification.of_option ())

let enableAnalysesForTerminationSpecification (spec: Svcomp.Specification.t) =
  match spec with
  | Termination -> enableSpecAnalyses spec ["termination"];
  | _ -> ()

let enableAnalysesForTerminationSpecification () =
  List.iter enableAnalysesForTerminationSpecification (Svcomp.Specification.of_option ())

let enableAnalysesForSpecification (spec: Svcomp.Specification.t) =
  match spec with
  | UnreachCall s -> ()
  | NoDataRace -> enableSpecAnalyses spec ["access"; "race"];
  | NoOverflow -> enableOptions ["ana.int.interval"]
  | _ -> ()

let enableAnalysesForSpecification () =
  List.iter enableAnalysesForSpecification (Svcomp.Specification.of_option ())

(* This is run independent of the autotuner being enabled or not to be sound in the presence of setjmp/longjmp  *)
(* It is done this way around to allow enabling some of these analyses also for programs without longjmp *)
let longjmpAnalyses = ["activeLongjmp"; "activeSetjmp"; "taintPartialContexts"; "modifiedSinceSetjmp"; "poisonVariables"; "expsplit"; "vla"]

let activateLongjmpAnalysesWhenRequired () =
  let isLongjmp (desc: LibraryDesc.t) args =
    match desc.special args with
    | LibraryDesc.Longjmp _ -> true
    | _ -> false
  in
  if hasFunction isLongjmp  then (
    Logs.info "longjmp -> enabling longjmp analyses \"%s\"" (String.concat ", " longjmpAnalyses);
    enableAnalyses longjmpAnalyses;
  )
