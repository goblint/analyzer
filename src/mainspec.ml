open Batteries (* otherwise open_in would return wrong type for Ispec *)
open Ispec

let _ =
  (* no arguments -> run interactively (= reading from stdin)  *)
  let repl = Array.length Sys.argv = 1 in
  let cin = if repl then stdin else open_in Sys.argv.(1) in
  ignore(parse ~repl:repl cin ~print:true)
  (* exit 0 *)