module Thread_pool : sig
  type t
  type 'a promise

  val create : int -> t
  val run : t -> (unit -> 'a) -> 'a
  val add_work : t -> (unit -> 'a) -> 'a promise
  val await_all : t -> unit promise list -> unit
  val finished_with : t -> unit
end

