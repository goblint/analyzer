(** Statistics for solvers. *)

let vars = ref 0
let evals = ref 0
let narrow_reuses = ref 0

let print () =
  Logs.info "vars = %d    evals = %d    narrow_reuses = %d" !vars !evals !narrow_reuses

let reset () =
  vars := 0;
  evals := 0;
  narrow_reuses := 0
