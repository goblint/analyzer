(** Analysis of standard streams which may have been closed ([closedStdStreams]).

    While this analysis is active, base tracks [stdin], [stdout] and [stderr] as pointers to their initial [FILE]s (see {!StdStreams}).
    After a [FILE] is closed, the value of a pointer to it is indeterminate (C11 7.21.3p4).
    Hence, this analysis reports accesses to initial [FILE]s which may have been closed by [fclose]. *)

open GoblintCil
open Analyses

(** Initial [FILE]s of standard streams. *)
module InitialFiles = SetDomain.Make (CilType.Varinfo)

module Spec : Analyses.MCPSpec =
struct
  include Analyses.IdentityUnitContextsSpec

  let name () = StdStreams.closed_analysis_name

  (** Initial [FILE]s which may have been closed. *)
  module D = InitialFiles

  (** Initial [FILE]s which may have been closed by any thread. *)
  module G = InitialFiles
  module V = UnitV

  let startstate _ = D.empty ()
  let exitstate _ = D.empty ()

  (** Initial [FILE]s which an address in [ad] may refer to. *)
  let may_refer_to_initial_files (ad: Queries.AD.t) =
    Queries.AD.fold (fun address initial_files ->
        match address with
        | Addr (variable, _) when StdStreams.is_initial_file variable -> InitialFiles.add variable initial_files
        | _ -> initial_files
      ) ad (InitialFiles.empty ())

  let may_be_closed man =
    if ThreadFlag.has_ever_been_multi (Analyses.ask_of_man man) then
      InitialFiles.union man.local (man.global ())
    else
      man.local

  let special man (lval: lval option) (f: varinfo) (arglist: exp list) =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Fclose stream ->
      let closed = may_refer_to_initial_files (man.ask (Queries.MayPointTo stream)) in
      man.sideg () closed;
      InitialFiles.union man.local closed
    | _ ->
      man.local

  let warn_use_of_closed initial_file =
    let stream_name =
      match StdStreams.stream_of_initial_file initial_file with
      | Some stream -> stream.vname
      | None -> initial_file.vname
    in
    AnalysisStateUtil.set_mem_safety_flag InvalidDeref;
    M.warn ~category:MessageCategory.Behavior.Undefined.other ~tags:[CWE 910] "FILE initially pointed to by %s may be used after it has been closed" stream_name;
    Checks.warn Checks.Category.InvalidMemoryAccess "FILE initially pointed to by %s may be used after it has been closed" stream_name

  let event man e oman =
    match e with
    | Events.Access {ad; _} ->
      (* The state before the access is used, such that closing a stream is not reported as its use. *)
      let accessed_closed = InitialFiles.inter (may_refer_to_initial_files ad) (may_be_closed oman) in
      InitialFiles.iter warn_use_of_closed accessed_closed;
      man.local
    | _ ->
      man.local
end

let _ =
  MCP.register_analysis ~dep:["access"] (module Spec : MCPSpec)
