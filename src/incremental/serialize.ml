open Prelude

(* TODO: GoblintDir *)
let version_map_filename = "version.data"
let cil_file_name = "ast.data"
let solver_data_file_name = "solver.data"
let analysis_data_file_name = "analysis.data"
let results_dir = "results"
let results_tmp_dir = "results_tmp"

type operation = Save | Load

(** Returns the name of the directory used for loading/saving incremental data *)
let incremental_dirname op = match op with
  | Load -> GobConfig.get_string "incremental.load-dir"
  | Save -> GobConfig.get_string "incremental.save-dir"

let gob_directory op =
  GobFpath.cwd_append (Fpath.v (incremental_dirname op))

let gob_results_dir op =
  Fpath.(gob_directory op / results_dir)

let gob_results_tmp_dir op =
  Fpath.(gob_directory op / results_tmp_dir)

let server () = GobConfig.get_bool "server.enabled"

let marshal obj fileName  =
  let chan = open_out_bin (Fpath.to_string fileName) in
  Marshal.output chan obj;
  close_out chan

let unmarshal fileName =
  if GobConfig.get_bool "dbg.verbose" then Format.printf "Unmarshalling %a... If type of content changed, this will result in a segmentation fault!" Fpath.pp fileName;
  Marshal.input (open_in_bin (Fpath.to_string fileName))

let results_exist () =
  (* If Goblint did not crash irregularly, the existence of the result directory indicates that there are results *)
  let r = gob_results_dir Load in
  let r_str = Fpath.to_string r in
  Sys.file_exists r_str && Sys.is_directory r_str

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
    let p = Fpath.(gob_results_dir Load / type_to_file_name data_type) in
    unmarshal p

(** Stores data for future incremental runs at the appropriate file, given the data and what kind of data it is. *)
let store_data (data : 'a) (data_type : incremental_data_kind) =
  if server () then
    match data_type with
    | SolverData -> server_solver_data := Some (Obj.repr data)
    | AnalysisData -> server_analysis_data := Some (Obj.repr data)
    | _ -> ()
  else (
    GobSys.mkdir_or_exists (gob_directory Save);
    let d = gob_results_tmp_dir Save in
    GobSys.mkdir_or_exists d;
    let p = Fpath.(d / type_to_file_name data_type) in
    marshal data p)

(** Deletes previous analysis results and moves the freshly created results there.*)
let move_tmp_results_to_results () =
  let op = Save in
  if not (server ()) then (
    if Sys.file_exists (Fpath.to_string (gob_results_dir op)) then begin
      Goblintutil.rm_rf (gob_results_dir op);
    end;
    Sys.rename (Fpath.to_string (gob_results_tmp_dir op)) (Fpath.to_string (gob_results_dir op)))
