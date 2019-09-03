open Unix

let buff_size = 1024

(* Suffix of files combined by CIL *)
let comb_suffix = "_comb.c"

let exec_command (command: string) =
  let (std_out, std_in) = open_process command in
  let output = Buffer.create buff_size in
  try
    while true do
      let line = input_char std_out in
      Buffer.add_char output line
    done;
    assert false;
  with End_of_file ->
    let exit_code = close_process (std_out,std_in) in
    let output = Buffer.contents output in
    (exit_code, output)

let string_of_process_status = function
  | WEXITED n -> "terminated with exit code " ^ string_of_int n
  | WSIGNALED n -> "was killed by signal " ^ string_of_int n
  | WSTOPPED n -> "was stopped by signal " ^ string_of_int n

(* BFS for a file with a given suffix in a directory or any subdirectoy *)
let find_file_by_suffix (dir: string) (file_name_suffix: string) =
  let list_files d = Array.to_list @@ Sys.readdir d in
  let dirs = Queue.create () in
  let rec search (dir: string) (files: string list) = match files with
    | (h::t) -> let f = Filename.concat dir h in
                if Sys.file_exists f && Sys.is_directory f
                  then (Queue.add f dirs; search dir t)
                  else if Batteries.String.ends_with h file_name_suffix then f else search dir t
    | [] -> if Queue.is_empty dirs then raise (Failure ("find_file_by_suffix found no files with suffix "^file_name_suffix^" in "^dir)) else let d = Queue.take dirs in search d (list_files d)
  in
  search dir (list_files dir)

let run_cilly (path: string) =
    let current_dir = Sys.getcwd () in
    if Sys.file_exists path && Sys.is_directory path then (
        Sys.chdir path;
        (* Clean the directory *)
        let _ = exec_command "make clean" in
        try
          while true do
            let comb = find_file_by_suffix path comb_suffix in
            Sys.remove comb;
          done
        with Failure e -> (); (* Deleted all *_comb.c files in the directory *)

        (* Combine the source files with make *)
        let gcc_path = GobConfig.get_string "exp.gcc_path" in
        let (exit_code, output) = exec_command @@ "make CC=\"cilly --gcc=" ^ gcc_path ^ " --merge --keepmerged\" " ^
                                                       "LD=\"cilly --gcc=" ^ gcc_path ^ " --merge --keepmerged\"" in
        print_string output;
        (* Exit if make failed *)
        if exit_code <> WEXITED 0 then 
          (
            print_endline ("Failure when combining files. Make " ^ (string_of_process_status exit_code) ^ ".");
            exit 1
          );
        Sys.chdir current_dir;
    )

