(** Lazy evaluation with a fixed function.
    Allows marshaling. *)

(* Lazy eval extracted here to avoid dependency cycle:
   Node -> CilType -> Printable -> Goblintutil -> GobConfig -> Tracing -> Node *)

module Make (M : sig
    type t
    type result
    val eval : t -> result
  end) : sig
  type t
  val make : M.t -> t
  val is_val : t -> bool
  val force : t -> M.result
end = struct
  type t = { mutable value : [ `Computed of M.result | `Closure of M.t ] }

  let make arg = { value = `Closure arg }

  let is_val l =
    match l.value with
    | `Computed _ -> true
    | `Closure _ -> false

  let force l =
    match l.value with
    | `Closure arg ->
      let v = M.eval arg in
      l.value <- `Computed v;
      v
    | `Computed v -> v
end
