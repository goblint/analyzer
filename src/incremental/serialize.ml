open Prelude

(* TODO: GoblintDir *)
let incremental_data_file_name = "analysis.data"
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
  if GobConfig.get_bool "dbg.verbose" then
    (* Do NOT replace with Printf because of Gobview: https://github.com/goblint/gobview/issues/10 *)
    print_endline ("Unmarshalling " ^ Fpath.to_string fileName ^ "... If type of content changed, this will result in a segmentation fault!");
  Marshal.input (open_in_bin (Fpath.to_string fileName))

let results_exist () =
  (* If Goblint did not crash irregularly, the existence of the result directory indicates that there are results *)
  let r = gob_results_dir Load in
  let r_str = Fpath.to_string r in
  Sys.file_exists r_str && Sys.is_directory r_str

(** Module to cache the data for incremental analaysis during a run, before it is stored to disk, as well as the server mode *)
module Cache = struct
  type t = {
    solver_data: Obj.t option ref;
    analysis_data: Obj.t option ref;
    version_data: MaxIdUtil.max_ids option ref;
    cil_file: Cil.file option ref;
  }

  let data = ref {
      solver_data = ref None ;
      analysis_data = ref None ;
      version_data = ref None ;
      cil_file = ref None ;
    }

  (* GADT that may be used to query data from the cache *)
  type _ data_query =
    | SolverDataRequest : Obj.t data_query
    | CilFileRequest : Cil.file data_query
    | VersionDataRequest : MaxIdUtil.max_ids data_query
    | AnalysisDataRequest : Obj.t data_query

  (* Data type to pass incremental data into the cache *)
  type incremental_data =
    | SolverData of Obj.t
    | CilFile of Cil.file
    | VersionData of MaxIdUtil.max_ids
    | AnalysisData of Obj.t

  (** Loads data for incremental runs from the appropriate file *)
  let load_data () =
    let p = Fpath.(gob_results_dir Load / incremental_data_file_name) in
    let loaded_data = unmarshal p in
    data := loaded_data

  (** Stores data for future incremental runs at the appropriate file. *)
  let store_data () =
    GobSys.mkdir_or_exists (gob_directory Save);
    let d = gob_results_dir Save in
    GobSys.mkdir_or_exists d;
    let p = Fpath.(d / incremental_data_file_name) in
    marshal !data p

  (** Update the some incremental data in the in-memory cache *)
  let update_data (d : incremental_data) = match d with
    | SolverData s -> !data.solver_data := Some s
    | AnalysisData a -> !data.analysis_data := Some a
    | VersionData v -> !data.version_data := Some v
    | CilFile c -> !data.cil_file := Some c

  (** Reset some incremental data in the in-memory cache to [None]*)
  let reset_data : type a. a data_query -> unit = function
    | SolverDataRequest -> !data.solver_data := None
    | AnalysisDataRequest -> !data.analysis_data := None
    | VersionDataRequest -> !data.version_data := None
    | CilFileRequest -> !data.cil_file := None

  (** Get incremental data from the in-memory cache wrapped in an optional.
      To populate the in-memory cache with data, call [load_data] first. *)
  let get_opt_data : type a. a data_query -> a option = function
    | SolverDataRequest -> !(!data.solver_data)
    | AnalysisDataRequest -> !(!data.analysis_data)
    | VersionDataRequest -> !(!data.version_data)
    | CilFileRequest -> !(!data.cil_file)

  (** Get incremental data from the in-memory cache.
      Same as [get_opt_data], except not yielding an optional and failing when the requested data is not present.
      To populate the in-memory cache with data, call [load_data] first. *)
  let get_data : type a. a data_query -> a =
    fun a ->
    match get_opt_data a with
    | Some d -> d
    | None -> failwith "Requested data is not loaded."
end
