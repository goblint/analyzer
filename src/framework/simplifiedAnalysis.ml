open GoblintCil
open Pretty
open Analyses


(** Man(ager) is passed to transfer functions and allows accessing the
    the context, read values from globals, side-effect values to globals,
    	and query information from other analyses *)
type ('g,'c,'v) man =
  {   ask       : 'a. 'a Queries.t -> 'a Queries.result
  (* To communicate with other analyses *)
  ; edge      : MyCFG.edge
  ; orig_node : MyCFG.node
  ; dest_node : MyCFG.node
  ; context   : 'c
  ; global    : 'v -> 'g
  ; sideg     : 'v -> 'g -> unit
  }

module type UnknownSet = Printable.S

module type SimplifiedSpec = sig

  module V : UnknownSet      (** Set of globals. *)

  module G : Lattice.S       (** Domain for globals. *)
  module D : Lattice.S       (** Domain for locals. *)

  module C : Printable.S     (** Context information. *)

  val name : string          (** Name of the analysis. *)

  val startstate : D.t       (** Initial local state for main. *)
  val startcontext: C.t      (** Initial context for main. *)

  val query : (G.t, C.t, V.t) man -> D.t -> 'a Queries.t -> 'a Queries.result

  (** A transfer function which handles the assignment of a rval to a lval, i.e.,
      it handles program points of the form "lval = rval;" *)
  val assign: (G.t, C.t, V.t) man -> D.t -> lval -> exp -> D.t

  (** A transfer function which handles conditional branching yielding the
      truth value passed as a boolean argument *)
  val branch: (G.t, C.t, V.t) man -> D.t -> exp -> bool -> D.t

  (** A transfer function which handles the return statement, i.e.,
      "return exp" or "return" in the passed function (fundec) *)
  val return: (G.t, C.t, V.t) man -> D.t -> exp option  -> fundec -> D.t

  (** A transfer function which handles going from the start node of a function (fundec) into
      its function body. Meant to handle, e.g., initialization of local variables *)
  val body: (G.t, C.t, V.t) man -> D.t -> fundec -> D.t

  (** For a function call "lval = f(args)" or "f(args)",
      enter returns the initial state of the callee. *)
  val enter   : (G.t, C.t, V.t) man -> D.t -> lval option -> fundec -> exp list -> D.t

  (** Combines the states before and after the call. *)
  val combine: (G.t, C.t, V.t) man -> D.t -> D.t -> lval option -> fundec -> exp list -> D.t

  (** A transfer function which, for a call to a {e special} function f "lval = f(args)" or "f(args)",
      computes the caller state after the function call *)
  val special : (G.t, C.t, V.t) man -> D.t -> lval option -> varinfo -> exp list -> D.t

  (** Compute the context for a function call, given the local state and context at the caller,
      the called function and the local state inside the callee *)
  val context: (G.t, C.t, V.t) man -> (D.t * C.t) -> fundec -> D.t -> C.t

  (** Compute the start state of a new thread starting with the function given by fundec *)
  val threadenter: (G.t, C.t, V.t) man -> D.t -> fundec -> exp list -> D.t
end
