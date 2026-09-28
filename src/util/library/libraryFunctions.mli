(** Hard-coded database of library function specifications. *)

open GoblintCil

val add_lib_funs : string list -> unit

val use_special : string -> bool
(** This is for when we need to use special transfer function on functions calls that have definitions.
*)

val is_safe_uncalled : string -> bool

(** Find library function descriptor for {e special} function (as per {!is_special}). *)
val find: ?nowarn:bool -> Cil.varinfo -> LibraryDesc.t

val is_specified: Cil.varinfo -> bool
(** Whether the function has a specification in an activated library, as against the conservative one {!find} returns for any other function without a definition. *)

val implicit_streams: LibraryDesc.t -> first_may_be_null:(unit -> bool) -> StandardStreams.t list
(** The standard streams a call reads and writes without taking them as arguments: the stream of each [UsesStream], and all three where the function has [AllStreamsIfNull] and [first_may_be_null ()] says its first argument may be null, as for [fflush(NULL)]. Streams from [fopen] are not among them: their addresses are unknown, and each call that attached a buffer to one reported writing through an unknown address. *)

val standard_streams_used: unit -> bool
(** Whether the analyzed file declares a standard stream or a function that uses one without taking it as an argument. The analysis then initializes the streams' objects ({!StandardStreams}). *)

val is_special: Cil.varinfo -> bool
(** Check if function is treated specially. *)


val verifier_atomic_var: Cil.varinfo

val reset_lazy: unit -> unit
