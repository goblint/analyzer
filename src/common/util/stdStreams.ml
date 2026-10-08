(** Standard streams [stdin], [stdout] and [stderr] and the [FILE]s they point to at program start. *)

open GoblintCil

let names = ["stdin"; "stdout"; "stderr"]

let is_std_stream (variable: varinfo) =
  variable.vglob && List.mem variable.vname names && isPointerType variable.vtype

(** [FILE] pointed to by a standard stream at program start. *)
module InitialFile = RichVarinfo.Make (struct
    include CilType.Varinfo

    let name_varinfo (stream: varinfo) = "__goblint_initial_" ^ stream.vname

    let typ (stream: varinfo) =
      match unrollType stream.vtype with
      | TPtr (file_type, _) -> file_type
      | _ -> invalid_arg "StdStreams.InitialFile.typ: stream is not a pointer"
  end)
