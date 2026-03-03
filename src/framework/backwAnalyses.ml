open GoblintCil
open Analyses

module M  = Messages

module type BackwSpec = 
sig
  module D : Lattice.S
  module G : Lattice.S
  module C : Printable.S
  module V: SpecSysVar (** Global constraint variables. *)
  module P: DisjointDomain.Representative with type elt := D.t (** Path-representative. *)

  module D_forw: Lattice.S
  module G_forw: Lattice.S 
  module V_forw: SpecSysVar (** Global constraint variables. *)
  module P_forw: DisjointDomain.Representative with type elt := D_forw.t (* Path-representative. *)
  val name : unit -> string

  (** Auxiliary data (outside of solution domains) that needs to be marshaled and unmarshaled.
      This includes:
      * hashtables,
      * varinfos (create_var),
      * RichVarinfos. *)
  type marshal

  (** Initialize using unmarshaled auxiliary data (if present). *)
  val init : marshal option -> unit

  (** Finalize and return auxiliary data to be marshaled. *)
  val finalize : unit -> marshal
  (* val finalize : G.t -> unit *)

  val startstate : varinfo -> D.t
  val morphstate : varinfo -> D.t -> D.t
  val exitstate  : varinfo -> D.t

  val context: (D_forw.t, G_forw.t, C.t, V_forw.t) man -> fundec -> D_forw.t -> C.t 
  val startcontext: unit -> C.t

  val sync  : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> [`Normal | `Join | `JoinCall of CilType.Fundec.t | `Return] -> D.t
  val query : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> 'a Queries.t -> 'a Queries.result

  (** A transfer function which handles the assignment of a rval to a lval, i.e.,
      it handles program points of the form "lval = rval;" *)
  val assign: (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> lval -> exp -> D.t

  (** A transfer function used for declaring local variables.
      By default only for variable-length arrays (VLAs). *)
  val vdecl : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> varinfo -> D.t

  (** A transfer function which handles conditional branching yielding the
      truth value passed as a boolean argument *)
  val branch: (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> exp -> bool -> D.t

  (** A transfer function which handles going from the start node of a function (fundec) into
      its function body. Meant to handle, e.g., initialization of local variables *)
  val body  : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> fundec -> D.t

  (** A transfer function which handles the return statement, i.e.,
      "return exp" or "return" in the passed function (fundec) *)
  val return: (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> exp option  -> fundec -> D.t

  (** A transfer function meant to handle inline assembler program points *)
  val asm   : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> D.t

  (** A transfer function which works as the identity function, i.e., it skips and does nothing.
      Used for empty loops. *)
  val skip  : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> D.t

  (** A transfer function which, for a call to a {e special} function f "lval = f(args)" or "f(args)",
      computes the caller state after the function call *)
  val special : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> lval option -> varinfo -> exp list -> D.t

  (** For a function call "lval = f(args)" or "f(args)",
      [enter] returns a caller state, and the initial state of the callee.
      In [enter], the caller state can usually be returned unchanged, as [combine_env] and [combine_assign] (below)
      will compute the caller state after the function call, given the return state of the callee *)
  val enter   : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> lval option -> fundec -> exp list -> (D.t * D.t) list

  (* Combine is split into two steps: *)

  (** Combine environment (global variables, mutexes, etc)
      between local state (first component from enter) and function return.

      This shouldn't yet assign to the lval. *)
  val combine_env : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> lval option -> exp -> fundec -> exp list -> C.t option -> D.t -> Queries.ask -> D.t

  (** Combine return value assignment
      to local state (result from combine_env) and function return.

      This should only assign to the lval. *)
  val combine_assign : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> lval option -> exp -> fundec -> exp list -> C.t option -> D.t -> Queries.ask -> D.t

  (* Paths as sets: I know this is ugly! *)
  val paths_as_set : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> D.t list

  (** Returns initial state for created thread. *)
  val threadenter : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> multiple:bool -> lval option -> varinfo -> exp list -> D.t list

  (** Updates the local state of the creator thread using initial state of created thread. *)
  val threadspawn : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> multiple:bool -> lval option -> varinfo -> exp list -> (D.t, G.t, C.t, V.t) man -> D.t

  val event : (D.t, G.t, C.t, V.t) man -> (D_forw.t, G_forw.t, C.t, V_forw.t) man -> Events.t -> (D.t, G.t, C.t, V.t) man -> D.t
end

module type BackwSpecSpec = functor (ForwSpec : Analyses.Spec) -> sig
  include BackwSpec 
    with module C = ForwSpec.C
    with module D_forw = ForwSpec.D
    with module G_forw = ForwSpec.G
    with module V_forw = ForwSpec.V
    with module P_forw = ForwSpec.P  
end

module DefaultBackwSpec (ForwSpec : Analyses.Spec) =
struct
  module G = Lattice.Unit
  module C = ForwSpec.C
  module V = EmptyV
  module P = EmptyP

  module D_forw: Lattice.S = ForwSpec.D
  module G_forw: Lattice.S = ForwSpec.G
  module V_forw: SpecSysVar = ForwSpec.V (** Global constraint variables. *)
  module P_forw: DisjointDomain.Representative with type elt := ForwSpec.D.t = ForwSpec.P (*Path-representative.*)

  type marshal = unit
  let init _ = ()

  (* This means it does not matter which Spec's context function we use in control and BidirFromSpec. 
   * For understandability in other parts of the code the context-function of the forward spec should be used explicitely*)

  let context = ForwSpec.context 
  let startcontext _ =  ForwSpec.startcontext ()
  let finalize () = ()
  (* no inits nor finalize -- only analyses like Mutex, Base, ... need
     these to do postprocessing or other imperative hacks. *)

  let vdecl man _ _ = man.local

  let asm x _ =
    M.msg_final Info ~category:Unsound "ASM ignored";
    M.info ~category:Unsound "ASM statement ignored.";
    x.local (* Just ignore. *)

  let skip x _ = x.local (* Just ignore. *)

  let query _ _ (type a) (q: a Queries.t) = Queries.Result.top q
  (* Don't know anything --- most will want to redefine this. *)

  let event man _ _ _ = man.local

  let morphstate v d = d
  (* Only for those who track thread IDs. *)

  let sync man _ _ = man.local
  (* Most domains do not have a global part. *)

  (* let context man _ fd x = x *)
  (* Everything is context sensitive --- override in MCP and maybe elsewhere*)

  let paths_as_set man _ = [man.local]

  (* module A = UnitA *)
  (* let access _ _ = () *)
end
