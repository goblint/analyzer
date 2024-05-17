(** Selecting sound configurations based on SV-COMP specification.
    Activating the analyses and enabling options that are
    required for analyzing the property defined in the specification.
 **)

open GobConfig
open AutoTune

let logEnablingAnalyses spec ana = Logs.info "Specification: %s -> enabling soundness analyses \"%s\"" (Svcomp.Specification.to_string [spec]) (String.concat ", " ana)

let enableOptions options = 
  let enableOpt option = 
    Logs.info "Setting \"%s\" to true" option;
    set_bool option true
  in
  List.iter enableOpt options

(* TODO: have only one function for matching all specifications and find a place where it can be called. *)
let enableAnalysesForMemSafetySpecification (spec: Svcomp.Specification.t) =
  match spec with
  | ValidFree -> (* Enable the soundness analyses for ValidFree spec *)
    let analyses = ["base"; "useAfterFree"] in
    logEnablingAnalyses spec analyses;
    enableAnalyses analyses
  | ValidDeref -> (* Enable the soundness analyses for ValidDeref spec *)
    let analyses = ["base"; "memOutOfBounds"] in
    logEnablingAnalyses spec analyses;
    enableAnalyses analyses;
    let options = ["ana.arrayoob"; "cil.addNestedScopeAttr"] in
    enableOptions options
  | ValidMemtrack
  | ValidMemcleanup -> (* Enable the soundness analyses for ValidMemtrack and ValidMemcleanup specs *)
    let analyses = ["memLeak"] in
    logEnablingAnalyses spec analyses;
    enableAnalyses analyses
  | _ -> ()

let enableAnalysesForMemSafetySpecification () =
  List.iter enableAnalysesForMemSafetySpecification (Svcomp.Specification.of_option ())

let enableAnalysesForTerminationSpecification (spec: Svcomp.Specification.t) =
  match spec with
  | Termination -> (* Enable the soundness analyses for Termination spec *)
    let analyses = ["termination"] in
    logEnablingAnalyses spec analyses;
    enableAnalyses analyses
  | _ -> ()

let enableAnalysesForTerminationSpecification () =
  List.iter enableAnalysesForTerminationSpecification (Svcomp.Specification.of_option ())

let enableAnalysesForSpecification (spec: Svcomp.Specification.t) =
  match spec with
  | UnreachCall s -> ()
  | NoDataRace -> (* Enable the soundness analyses for NoDataRace spec *)
    let analyses = ["access"; "race"] in
    logEnablingAnalyses spec analyses;
    enableAnalyses analyses
  | NoOverflow -> (* Enable the soundness analyses for NoOverflow spec *)
    let options = ["ana.int.interval"] in
    enableOptions options
  | _ -> ()

let enableAnalysesForSpecification () =
  List.iter enableAnalysesForSpecification (Svcomp.Specification.of_option ())
