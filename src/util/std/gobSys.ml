let rec mkdir_parents filename =
  let dirname = Fpath.parent filename in
  let dirname_str = Fpath.to_string dirname in
  if not (Sys.file_exists dirname_str) then (
    mkdir_parents dirname;
    Unix.mkdir dirname_str 0o770; (* TODO: what permissions? *)
  )

let mkdir_or_exists dirname =
  let dirname_str = Fpath.to_string dirname in
  try
    Unix.mkdir dirname_str 0o770 (* TODO: what permissions? *)
  with Unix.Unix_error (Unix.EEXIST, _, _) ->
    assert (Sys.is_directory dirname_str) (* may exist, but as a file *)

(** Creates a directory and returns the absolute path **)
let mkdir_or_exists_absolute name =
  let dirName = GobFpath.cwd_append name in
  mkdir_or_exists dirName;
  dirName

let rmdir_if_empty dirname =
  try
    Unix.rmdir (Fpath.to_string dirname)
  with Unix.Unix_error (Unix.ENOTEMPTY, _, _) ->
    ()

(** Remove directory and its content, as "rm -rf" would do. *)
let rmdir_recursive path =
  let rec f path =
    let path_str = Fpath.to_string path in
    if Sys.is_directory path_str then begin
      let files = Array.map (Fpath.add_seg path) (Sys.readdir path_str) in
      Array.iter f files;
      Unix.rmdir path_str
    end else
      Sys.remove path_str
  in
  f path


let exe_dir = Fpath.(parent (v Sys.executable_name))

let command_line = match Array.to_list Sys.argv with
  | command :: arguments -> Filename.quote_command command arguments
  | [] -> assert false


(* Sys.time gives runtime in seconds as float *)
let split_time () = (* gives CPU time in h,m,s,ms *)
  let f = Sys.time () in
  let i = int_of_float f in
  let ms = int_of_float (BatFloat.modulo f 1.0 *. 1000.) in
  i / 3600, i / 60 mod 60, i mod 60, ms

let string_of_time () = (* CPU time as hh:mm:ss.ms *)
  let h,m,s,ms = split_time () in
  Printf.sprintf "%02d:%02d:%02d.%03d" h m s ms


(* https://ocaml.org/api/Sys.html#2_SignalnumbersforthestandardPOSIXsignals *)
(* https://ocaml.github.io/ocamlunix/signals.html *)
let signal_of_string =
  let open Sys in
  function
  | "sigint"  -> sigint  (* Ctrl+C Interactive interrupt *)
  | "sigtstp" -> sigtstp (* Ctrl+Z Interactive stop *)
  | "sigquit" -> sigquit (* Ctrl+\ Interactive termination *)
  | "sigalrm" -> sigalrm (* Timeout *)
  | "sigkill" -> sigkill (* Termination (cannot be ignored) *)
  | "sigsegv" -> sigsegv (* Invalid memory reference, https://github.com/goblint/analyzer/issues/206 *)
  | "sigterm" -> sigterm (* Termination *)
  | "sigusr1" -> sigusr1 (* Application-defined signal 1 *)
  | "sigusr2" -> sigusr2 (* Application-defined signal 2 *)
  | "sigstop" -> sigstop (* Stop *)
  | "sigprof" -> sigprof (* Profiling interrupt *)
  | "sigxcpu" -> sigxcpu (* Timeout in cpu time *)
  | s -> invalid_arg ("Unhandled signal " ^ s)

let self_signal signal = Unix.kill (Unix.getpid ()) signal
