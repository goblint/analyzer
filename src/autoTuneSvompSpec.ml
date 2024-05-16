(** Autotuning of the activated analyses for soundness based on SV-COMP specification. *)

open GobConfig
open AutoTune

(* TODO: have only one function for matching all specifications and find a place where it can be called. *)
let enableAnalysesForMemSafetySpecification (spec: Svcomp.Specification.t) =
  match spec with
  | ValidFree -> (* Enable the soundness analyses for ValidFree spec *)
    let uafAna = ["base"; "useAfterFree"] in
    Logs.info "Specification: ValidFree -> enabling soundness analyses \"%s\"" (String.concat ", " uafAna);
    enableAnalyses uafAna
  | ValidDeref -> (* Enable the soundness analyses for ValidDeref spec *)
    let memOobAna = ["base"; "memOutOfBounds"] in
    set_bool "ana.arrayoob" true;
    Logs.info "Setting \"cil.addNestedScopeAttr\" to true";
    set_bool "cil.addNestedScopeAttr" true;
    Logs.info "Specification: ValidDeref -> enabling soundness analyses \"%s\"" (String.concat ", " memOobAna);
    enableAnalyses memOobAna
  | ValidMemtrack
  | ValidMemcleanup -> (* Enable the soundness analyses for ValidMemtrack and ValidMemcleanup specs *)
    let memLeakAna = ["memLeak"] in
    Logs.info "Specification: ValidMemtrack and ValidMemcleanup -> enabling soundness analyses \"%s\"" (String.concat ", " memLeakAna);
    enableAnalyses memLeakAna
  | _ -> ()

let enableAnalysesForMemSafetySpecification () =
  List.iter enableAnalysesForMemSafetySpecification (Svcomp.Specification.of_option ())

let enableAnalysesForTerminationSpecification (spec: Svcomp.Specification.t) =
  match spec with
  | Termination -> (* Enable the soundness analyses for Termination spec *)
    let terminationAna = ["termination"] in
    Logs.info "Specification: Termination -> enabling soundness analyses \"%s\"" (String.concat ", " terminationAna);
    enableAnalyses terminationAna
  | _ -> ()

let enableAnalysesForTerminationSpecification () =
  List.iter enableAnalysesForTerminationSpecification (Svcomp.Specification.of_option ())

let enableAnalysesForSpecification (spec: Svcomp.Specification.t) =
  match spec with
  | UnreachCall s -> ()
  | NoDataRace -> (* Enable the soundness analyses for NoDataRace spec *)
    let dataRaceAna = ["access"; "race"] in
    Logs.info "Specification: NoDataRace -> enabling soundness analyses \"%s\"" (String.concat ", " dataRaceAna);
    enableAnalyses dataRaceAna
  | NoOverflow -> (* Enable the soundness analyses for NoOverflow spec *)
    Logs.info "Setting \"ana.int.interval\" to true";
    set_bool "ana.int.interval" true
  | _ -> ()

let enableAnalysesForSpecification () =
  List.iter enableAnalysesForSpecification (Svcomp.Specification.of_option ())
