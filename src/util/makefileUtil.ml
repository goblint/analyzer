open Unix

let buff_size = 1024

let exec_program (program: string) (callArray : string array) =
  let in_channel = openfile "/dev/null" [O_RDONLY] 0o640 in
  let (fd_in, fd_out) = pipe () in
  let args = callArray in
  (* git should output to fdin *)
  let _ = create_process program args in_channel fd_out fd_out in
  close fd_out;
  let input = in_channel_of_descr fd_in in
  let output = Buffer.create buff_size in
  (* Read file until end *)
  try
    while true do
      let line = input_char input in
      Buffer.add_char output line
    done;
    assert false;
  with End_of_file ->
    Buffer.contents output

let run_make (args: string array) =
    exec_program "make" args

let run_cilly (path: string) =
    let current_dir = Sys.getcwd () in
    if Sys.file_exists path && Sys.is_directory path then (
        Sys.chdir path;
        let output = run_make [|"make"; "CC=cilly --gcc=/usr/bin/gcc-6 --merge --keepmerged"; "LD=cilly --gcc=/usr/bin/gcc-6 --merge --keepmerged"|] in
        print_endline output;
        Sys.chdir current_dir;
    )

(* BFS for a file with a given suffix in a directory or any subdirectoy *)
let find_file_by_suffix (dir: string) (file_name_suffix: string) =
  let list_files d = Array.to_list @@ Sys.readdir d in
  let dirs = Queue.create () in

  let rec search (dir: string) (files: string list) = match files with
    | (h::t) -> let f = Filename.concat dir h in
                if Sys.file_exists f && Sys.is_directory f
                  then (Queue.add f dirs; search dir t)
                  else if Batteries.String.ends_with h file_name_suffix then f else search dir t
    | [] -> if Queue.is_empty dirs then raise (Failure "No such file") else let d = Queue.take dirs in search d (list_files d)
  in
  search dir (list_files dir)
