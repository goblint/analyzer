(** A mutex that supports multiple threads on a domain when parallelism is supported
    as detected by the presence of the [domainslib] library. It is mocked to a no-op
    when [domainslib] is not available for backwards compatibility. *)

type t

val create: unit -> t
val unlock: t -> unit
val lock: t -> unit
