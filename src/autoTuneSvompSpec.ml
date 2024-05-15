(** Autotuning of the activated analyses for soundness based on SV-COMP specification. *)

open GobConfig
open AutoTune

let enableAnalysesForMemSafetySpecification (spec: Svcomp.Specification.t) =
  match spec with
  | ValidFree -> (* Enable the useAfterFree analysis *)
    let uafAna = ["useAfterFree"] in
    Logs.info "Specification: ValidFree -> enabling useAfterFree analysis \"%s\"" (String.concat ", " uafAna);
    enableAnalyses uafAna
  | ValidDeref -> (* Enable the memOutOfBounds analysis *)
    let memOobAna = ["memOutOfBounds"] in
    set_bool "ana.arrayoob" true;
    Logs.info "Setting \"cil.addNestedScopeAttr\" to true";
    set_bool "cil.addNestedScopeAttr" true;
    Logs.info "Specification: ValidDeref -> enabling memOutOfBounds analysis \"%s\"" (String.concat ", " memOobAna);
    enableAnalyses memOobAna;
  | ValidMemtrack
  | ValidMemcleanup -> (* Enable the memLeak analysis *)
    let memLeakAna = ["memLeak"] in
    Logs.info "Specification: ValidMemtrack and ValidMemcleanup -> enabling memLeak analysis \"%s\"" (String.concat ", " memLeakAna);
    enableAnalyses memLeakAna
  | _ -> ()

let enableAnalysesForMemSafetySpecification () =
  List.iter enableAnalysesForMemSafetySpecification (Svcomp.Specification.of_option ())
