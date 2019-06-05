(* Does this work on CygWin? *)
open Unix 

type diff = {original_file: string; new_file: string; original_method_name: string; new_method_name: string}

let buff_size = 1024

let execProgram (program: string) (callArray : string array) =
  let git_in = openfile "/dev/null" [O_RDONLY] 0o640 in
  let (fd_in, fd_out) = pipe () in
  let args = callArray in
  (* git should output to fdin *)
  let _ = create_process program args git_in fd_out fd_out in
  close fd_out;
  let input = in_channel_of_descr fd_in in 
  let gitOutput = Buffer.create buff_size in
  (* Read file until end *)
  try
    while true do         
      let line = input_char input in
      Buffer.add_char gitOutput line 
    done;
    assert false;
  with End_of_file -> 
    Buffer.contents gitOutput;;

let execGit (callArray : string array) =
  execProgram "git" callArray

let runGitDiff () =
  let args = [| "git"; "diff"; "-U0"; "@{1}" |] in
  execGit args

(* white space characters, according to https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html, except \f *)
let whiteSpace = "[ \t\n\x0B\r]"

let diff_to_function_list output =
  let functionNames = ref [] in
  let regex = Str.regexp ("@@.*@@[^(]*\\b\\([a-zA-Z]+\\)" ^ whiteSpace ^ "*(?+.*") in
  let index = ref 0 in
  try 
    while true do 
      let _ = Str.search_forward regex output !index in
      index := Str.match_end ();
      functionNames := (Str.matched_group 1 output) :: !functionNames;
    done; 
    assert false;
  with Not_found ->
    !functionNames

(* Problem with using git log -L:<func_name>:<file> might be that it does not consider changes in the working tree *)
let check_log functionName fileName = 
  let args = [| "git"; "log"; "-L:"^functionName^ ":" ^fileName|] in
  execGit args

let is_clean (directory: string) = 
  let args = [|"git"; "-C"; directory; "diff"; "^HEAD";|] in
  let diff = execGit args in
  0 = String.length diff 

let last_commit_id (directory: string) = 
  let args = [|"git"; "-C"; directory; "rev-parse"; "HEAD"; |] in
  let output = execGit args in
  String.trim output

let current_commit (directory: string) = 
  if is_clean directory then Some (last_commit_id directory)
  else None

let may f opt = match opt with
  | Some x -> f x
  | None -> ()

let git_log dir = 
  let args = [|"git"; "-C"; dir ; "log"; "--pretty=format:%H" |] in
  execGit args

let git_directory path = 
  let dir = if Sys.is_directory path then path else Filename.dirname path in 
  let args = [|"git"; "-C"; dir; "rev-parse";  "--show-toplevel" |] in
  let git_output = execGit args in
  let git_path = Batteries.String.strip git_output in
  if Sys.file_exists git_path then git_path else raise (Failure ("File " ^ path ^ " is not contained in a git repository."))

let run () = 
  let gitOutput = runGitDiff () in 
  print_string gitOutput;
  let found = diff_to_function_list gitOutput in
  print_endline "The following functions were changed:";
  print_endline (String.concat "\n" found);