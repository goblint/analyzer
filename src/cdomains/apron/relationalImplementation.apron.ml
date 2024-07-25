open Apron

module type Implementation =
sig
  (* Oct *)
  type t
  val to_oct : 'a Apron.Abstract1.t -> t Apron.Abstract1.t
  val of_oct : t Apron.Abstract1.t -> 'a Apron.Abstract1.t
  val widening_thresholds : t Apron.Manager.t -> t Apron.Abstract0.t -> t Apron.Abstract0.t -> Apron.Scalar.t array -> t Apron.Abstract0.t
  val manager_is_oct : 'a Apron.Manager.t -> bool
  val manager_to_oct : 'a Apron.Manager.t -> t Apron.Manager.t
  val narrowing : t Apron.Manager.t -> t Apron.Abstract0.t -> t Apron.Abstract0.t -> t Apron.Abstract0.t
  val manager_alloc : unit -> t Apron.Manager.t
  (* Poly *)
  type pt
  val manager_alloc_loose : unit -> pt Apron.Manager.t
  (* Other *)
  val impl : unit -> string
  val substitute_texpr_with : 'a Apron.Manager.t -> 'a Apron.Abstract1.t -> Apron.Var.t -> Apron.Texpr1.t -> 'a Apron.Abstract1.t option -> unit
  val hash : 'a Apron.Manager.t -> 'a Apron.Abstract1.t -> int
  val bound_texpr : 'a Manager.t -> string -> 'a Abstract1.t -> Texpr1.t -> Interval.t
end