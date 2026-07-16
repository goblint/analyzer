(** Library for easily defining PPX derivers without boilerplate and runtime overhead. *)

(** {1 Interfaces} *)

include Intf (** @inline *)


(** {1 Deriver} *)

module Deriver = Deriver


(** {1 Easier constructs} *)

module Product = Product
