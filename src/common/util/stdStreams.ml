(** Standard streams [stdin], [stdout] and [stderr] and the [FILE]s they point to at program start. *)

open GoblintCil

let names = ["stdin"; "stdout"; "stderr"]

let is_std_stream (variable: varinfo) =
  variable.vglob && List.mem variable.vname names && isPointerType variable.vtype

(** Name of the analysis which reports uses of closed standard streams.
    Standard streams are only tracked if it is active, because otherwise such uses would go unnoticed. *)
let closed_analysis_name = "closedStdStreams"

let is_tracked_std_stream (variable: varinfo) =
  is_std_stream variable && List.mem closed_analysis_name (GobConfig.get_string_list "ana.activated")

(** [FILE] pointed to by a standard stream at program start. *)
module InitialFile = RichVarinfo.Make (struct
    include CilType.Varinfo

    let name_varinfo (stream: varinfo) = "__goblint_initial_" ^ stream.vname

    let typ (stream: varinfo) =
      match unrollType stream.vtype with
      | TPtr (file_type, _) -> file_type
      | _ -> invalid_arg "StdStreams.InitialFile.typ: stream is not a pointer"
  end)

(** Standard stream which points to [variable] at program start, if [variable] is an initial [FILE]. *)
let stream_of_initial_file (variable: varinfo) =
  List.find_map (fun (stream, initial_file) ->
      if CilType.Varinfo.equal initial_file variable then Some stream else None
    ) (InitialFile.bindings ())

let is_initial_file (variable: varinfo) =
  Option.is_some (stream_of_initial_file variable)
