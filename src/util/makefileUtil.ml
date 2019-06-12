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
    if Sys.file_exists path && Sys.is_directory path then (
        Unix.chdir path;
        let output = run_make [|"make"; "CC=cilly --gcc=/usr/bin/gcc-6 --merge"; "LD=cilly --gcc=/usr/bin/gcc-6 --merge --keepmerged"|] in
        print_endline output;
    )

let () = 
    if Array.length Sys.argv > 1 then (
        run_cilly Sys.argv.(1);
    )