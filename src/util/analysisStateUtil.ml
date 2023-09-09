type mem_safety_violation =
  | InvalidFree
  | InvalidDeref
  | InvalidMemTrack

let set_mem_safety_flag violation_type =
  if !AnalysisState.postsolving then
    match violation_type with
    | InvalidFree -> AnalysisState.svcomp_may_invalid_free := true
    | InvalidDeref -> AnalysisState.svcomp_may_invalid_deref := true
    | InvalidMemTrack -> AnalysisState.svcomp_may_invalid_memtrack := true