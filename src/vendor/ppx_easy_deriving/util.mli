(** Utility functions. *)

val reduce: unit:'a -> both:('a -> 'a -> 'a) -> 'a list -> 'a
(** Reduce a list of values "optimally",
    i.e. without unnecessary [~unit] and [~both]. *)

val map3: ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
(** Map three lists into one. *)
