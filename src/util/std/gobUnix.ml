open Unix

let string_of_process_status = function
  | WEXITED n -> "terminated with exit code " ^ string_of_int n
  | WSIGNALED n -> "killed by signal " ^ string_of_int n
  | WSTOPPED n -> "stopped by signal " ^ string_of_int n


let localtime () =
  let open Unix in
  let tm = time () |> localtime in
  Printf.sprintf "%d-%02d-%02d %02d:%02d:%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
