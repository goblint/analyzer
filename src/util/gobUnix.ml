open Prelude
open Unix

let string_of_process_status = function
  | WEXITED n -> "terminated with exit code " ^ string_of_int n
  | WSIGNALED n -> "killed by signal " ^ string_of_int n
  | WSTOPPED n -> "stopped by signal " ^ string_of_int n
