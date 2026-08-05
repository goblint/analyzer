(** Interface for stateless providers which answer analysis queries. *)

module type S = sig
  val name : unit -> string
  val query : 'a Queries.t -> 'a Queries.result
end
