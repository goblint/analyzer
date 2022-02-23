open Prelude

let base_directory = ref (Sys.getcwd ()) (* base directory where incremental results are stored *)
let goblint_dirname = "incremental_data"
let version_map_filename = "version.data"
let cil_file_name = "ast.data"
let solver_data_file_name = "solver.data"
let analysis_data_file_name = "analysis.data"
let results_dir = "results"
let results_tmp_dir = "results_tmp"
let gob_directory () = let src_dir = !base_directory in
  Filename.concat src_dir goblint_dirname

let gob_results_dir () =
  Filename.concat (gob_directory ()) results_dir

let gob_results_tmp_dir () =
  Filename.concat (gob_directory ()) results_tmp_dir

let server () = GobConfig.get_bool "server.enabled"

let marshal obj fileName  =
  let chan = open_out_bin fileName in
  Marshal.output chan obj;
  close_out chan

let unmarshal fileName =
  if GobConfig.get_bool "dbg.verbose" then print_endline ("Unmarshalling " ^ fileName ^ "... If type of content changed, this will result in a segmentation fault!");
  Marshal.input (open_in_bin fileName)

let results_exist () =
  (* If Goblint did not crash irregularly, the existence of the result directory indicates that there are results *)
  let r = gob_results_dir () in
  Sys.file_exists r && Sys.is_directory r

(* Convenience enumeration of the different data types we store for incremental analysis, so file-name logic is concentrated in one place *)
type incremental_data_kind = SolverData | CilFile | VersionData | AnalysisData

let type_to_file_name = function
  | SolverData -> solver_data_file_name
  | CilFile -> cil_file_name
  | VersionData -> version_map_filename
  | AnalysisData -> analysis_data_file_name

(** Used by the server mode to avoid serializing the solver state to the filesystem *)
let server_solver_data : Obj.t option ref = ref None
let server_analysis_data : Obj.t option ref = ref None

(** Loads data for incremental runs from the appropriate file *)
let load_data (data_type: incremental_data_kind) =
  if server () then
    match data_type with
    | SolverData -> !server_solver_data |> Option.get |> Obj.obj
    | AnalysisData -> !server_analysis_data |> Option.get |> Obj.obj
    | _ -> failwith "Can only load solver and analysis data"
  else
    let p = Filename.concat (gob_results_dir ()) (type_to_file_name data_type) in
    unmarshal p

(** Stores data for future incremental runs at the appropriate file, given the data and what kind of data it is. *)
let store_data (data : 'a) (data_type : incremental_data_kind) =
  if server () then
    match data_type with
    | SolverData -> server_solver_data := Some (Obj.repr data)
    | AnalysisData -> server_analysis_data := Some (Obj.repr data)
    | _ -> ()
  else (
    GobSys.mkdir_or_exists (gob_directory ());
    let d = gob_results_tmp_dir () in
    GobSys.mkdir_or_exists d;
    let p = Filename.concat d (type_to_file_name data_type) in
    marshal data p)

(** Deletes previous analysis results and moves the freshly created results there.*)
let move_tmp_results_to_results () =
  if not (server ()) then (
    if Sys.file_exists (gob_results_dir ()) then begin
      Goblintutil.rm_rf (gob_results_dir ());
    end;
    Sys.rename (gob_results_tmp_dir ()) (gob_results_dir ()))
