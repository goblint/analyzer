open Batteries (* otherwise open_in would return wrong type for SpecUtil *)
open SpecUtil

let _ =
  (* no arguments -> run interactively (= reading from stdin)  *)
  let args = Array.length Sys.argv > 1 in
  if args && Sys.argv.(1) = "-" then
	  ignore(parse ~dot:true stdin)
  else
	  let cin = if args then open_in Sys.argv.(1) else stdin in
	  ignore(parse ~repl:(not args) ~print:true cin)
  (* exit 0 *)