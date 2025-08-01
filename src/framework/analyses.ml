(** {{!Spec} Analysis specification} signatures. *)

open GoblintCil
open Pretty
open GobConfig

module M  = Messages

(** Analysis starts from lists of functions: start functions, exit functions, and
  * other functions. *)
type fundecs = fundec list * fundec list * fundec list


module Var =
struct
  type t = Node.t [@@deriving eq, ord, hash, relift]

  let printXml f n =
    let l = Node.location n in
    BatPrintf.fprintf f "<call id=\"%s\" file=\"%s\" fun=\"%s\" line=\"%d\" order=\"%d\" column=\"%d\">\n" (Node.show_id n) l.file (Node.find_fundec n).svar.vname l.line l.byte l.column

  let var_id = Node.show_id
end


module VarF (LD: Printable.S) =
struct
  type t = Node.t * LD.t [@@deriving eq, ord, hash]
  let relift (n,x) = n, LD.relift x

  let getLocation (n,d) = Node.location n

  let pretty_trace () ((n,c) as x) =
    if get_bool "dbg.trace.context" then dprintf "(%a, %a) on %a" Node.pretty_trace n LD.pretty c CilType.Location.pretty (getLocation x)
    (* if get_bool "dbg.trace.context" then dprintf "(%a, %d) on %a" Node.pretty_trace n (LD.tag c) CilType.Location.pretty (getLocation x) *)
    else dprintf "%a on %a" Node.pretty_trace n CilType.Location.pretty (getLocation x)

  let printXml f (n,c) =
    Var.printXml f n;
    BatPrintf.fprintf f "<context>\n";
    LD.printXml f c;
    BatPrintf.fprintf f "</context>\n"

  let var_id (n,_) = Var.var_id n
  let node (n,_) = n
  let is_write_only _ = false
end

module type SpecSysVar =
sig
  include Printable.S
  include Goblint_constraint.ConstrSys.SysVar with type t := t
end

module GVarF (V: SpecSysVar) =
struct
  include Printable.EitherConf (struct let expand1 = false let expand2 = true end) (V) (CilType.Fundec)
  let name () = "FromSpec"
  let spec x = `Left x
  let contexts x = `Right x

  (* from Basetype.Variables *)
  let var_id = show
  let node _ = MyCFG.Function Cil.dummyFunDec
  let pretty_trace = pretty
  let is_write_only = function
    | `Left x -> V.is_write_only x
    | `Right _ -> true
end

module GVarFC (V:SpecSysVar) (C:Printable.S) =
struct
  include Printable.EitherConf (struct let expand1 = false let expand2 = true end) (V) (Printable.Prod (CilType.Fundec) (C))
  let name () = "FromSpec"
  let spec x = `Left x
  let call (x, c) = `Right (x, c)

  (* from Basetype.Variables *)
  let var_id = show
  let node _ = MyCFG.Function Cil.dummyFunDec
  let pretty_trace = pretty
  let is_write_only = function
    | `Left x -> V.is_write_only x
    | `Right _ -> true
end

module GVarFCNW (V:SpecSysVar) (C:Printable.S) =
struct
  include Printable.EitherConf (struct let expand1 = false let expand2 = true end) (V) (Printable.Prod (CilType.Fundec) (C))
  let name () = "FromSpec"
  let spec x = `Left x
  let return (x, c) = `Right (x, c)

  (* from Basetype.Variables *)
  let var_id = show
  let node _ = MyCFG.Function Cil.dummyFunDec
  let pretty_trace = pretty
  let is_write_only = function
    | `Left x -> V.is_write_only x
    | `Right _ -> false
end

module GVarG (G: Lattice.S) (C: Printable.S) =
struct
  module CSet =
  struct
    include SetDomain.Make (
      struct
        include C
        let printXml f c = BatPrintf.fprintf f "<value>%a</value>" printXml c (* wrap in <value> for HTML printing *)
      end
      )
    let name () = "contexts"
  end

  include Lattice.Lift2 (G) (CSet)

  let spec = function
    | `Bot -> G.bot ()
    | `Lifted1 x -> x
    | _ -> failwith "GVarG.spec"
  let contexts = function
    | `Bot -> CSet.bot ()
    | `Lifted2 x -> x
    | _ -> failwith "GVarG.contexts"
  let create_spec spec = `Lifted1 spec
  let create_contexts contexts = `Lifted2 contexts

  let printXml f = function
    | `Lifted1 x -> G.printXml f x
    | `Lifted2 x -> BatPrintf.fprintf f "<analysis name=\"fromspec-contexts\">%a</analysis>" CSet.printXml x
    | x -> BatPrintf.fprintf f "<analysis name=\"fromspec\">%a</analysis>" printXml x
end

module GVarL (G: Lattice.S) (L: Lattice.S) =
struct
  include Lattice.Lift2 (G) (L)

  let spec = function
    | `Bot -> G.bot ()
    | `Lifted1 x -> x
    | _ -> failwith "GVarG.spec"
  let return = function
    | `Bot -> L.bot ()
    | `Lifted2 x -> x
    | _ -> failwith "GVarG.return"
  let create_spec spec = `Lifted1 spec
  let create_return return = `Lifted2 return

  let printXml f = function
    | `Lifted1 x -> G.printXml f x
    | `Lifted2 x -> L.printXml f x
    | x -> BatPrintf.fprintf f "<analysis name=\"fromspec\">%a</analysis>" printXml x
end



exception Deadcode

(** [Dom (D)] produces D lifted where bottom means dead-code *)
module Dom (LD: Lattice.S) =
struct
  include Lattice.LiftConf (struct
      include Printable.DefaultConf
      let bot_name = "Dead code"
      let top_name = "Totally unknown and messed up"
    end) (LD)

  let lift (x:LD.t) : t = `Lifted x

  let unlift x =
    match x with
    | `Lifted x -> x
    | _ -> raise Deadcode

  let printXml f = function
    | `Top -> BatPrintf.fprintf f "<value>%s</value>" (XmlUtil.escape Printable.DefaultConf.top_name)
    | `Bot -> ()
    | `Lifted x -> LD.printXml f x
end

(** Man(ager) is passed to transfer functions and offers access to various facilities, e.g., to access the local state, the context,
    read values from globals, side-effect values to globals and trigger events. *)
type ('d,'g,'c,'v) man =
  { ask      : 'a. 'a Queries.t -> 'a Queries.result (* Inlined Queries.ask *)
  ; emit     : Events.t -> unit
  ; node     : MyCFG.node
  ; prev_node: MyCFG.node
  ; control_context : unit -> ControlSpecC.t (** top-level Control Spec context, raises [Man_failure] if missing *)
  ; context  : unit -> 'c (** current Spec context, raises [Man_failure] if missing *)
  ; edge     : MyCFG.edge
  ; local    : 'd
  ; global   : 'v -> 'g
  ; spawn    : ?multiple:bool -> lval option -> varinfo -> exp list -> unit
  ; split    : 'd -> Events.t list -> unit
  ; sideg    : 'v -> 'g -> unit
  }

exception Man_failure of string
(** Failure from man, e.g. global initializer *)

let man_failwith s = raise (Man_failure s) (* TODO: use everywhere in man *)

(** Convert [man] to [Queries.ask]. *)
let ask_of_man man: Queries.ask = { Queries.f = man.ask }


module type Spec =
sig
  module D : Lattice.S
  module G : Lattice.S
  module C : Printable.S
  module V: SpecSysVar (** Global constraint variables. *)
  module P: DisjointDomain.Representative with type elt := D.t (** Path-representative. *)

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

  val context: (D.t, G.t, C.t, V.t) man -> fundec -> D.t -> C.t
  val startcontext: unit -> C.t

  val sync  : (D.t, G.t, C.t, V.t) man -> [`Normal | `Join | `JoinCall of CilType.Fundec.t | `Return] -> D.t
  val query : (D.t, G.t, C.t, V.t) man -> 'a Queries.t -> 'a Queries.result

  (** A transfer function which handles the assignment of a rval to a lval, i.e.,
      it handles program points of the form "lval = rval;" *)
  val assign: (D.t, G.t, C.t, V.t) man -> lval -> exp -> D.t

  (** A transfer function used for declaring local variables.
      By default only for variable-length arrays (VLAs). *)
  val vdecl : (D.t, G.t, C.t, V.t) man -> varinfo -> D.t

  (** A transfer function which handles conditional branching yielding the
      truth value passed as a boolean argument *)
  val branch: (D.t, G.t, C.t, V.t) man -> exp -> bool -> D.t

  (** A transfer function which handles going from the start node of a function (fundec) into
      its function body. Meant to handle, e.g., initialization of local variables *)
  val body  : (D.t, G.t, C.t, V.t) man -> fundec -> D.t

  (** A transfer function which handles the return statement, i.e.,
      "return exp" or "return" in the passed function (fundec) *)
  val return: (D.t, G.t, C.t, V.t) man -> exp option  -> fundec -> D.t

  (** A transfer function meant to handle inline assembler program points *)
  val asm   : (D.t, G.t, C.t, V.t) man -> D.t

  (** A transfer function which works as the identity function, i.e., it skips and does nothing.
      Used for empty loops. *)
  val skip  : (D.t, G.t, C.t, V.t) man -> D.t

  (** A transfer function which, for a call to a {e special} function f "lval = f(args)" or "f(args)",
      computes the caller state after the function call *)
  val special : (D.t, G.t, C.t, V.t) man -> lval option -> varinfo -> exp list -> D.t

  (** For a function call "lval = f(args)" or "f(args)",
      [enter] returns a caller state, and the initial state of the callee.
      In [enter], the caller state can usually be returned unchanged, as [combine_env] and [combine_assign] (below)
      will compute the caller state after the function call, given the return state of the callee *)
  val enter   : (D.t, G.t, C.t, V.t) man -> lval option -> fundec -> exp list -> (D.t * D.t) list

  (* Combine is split into two steps: *)

  (** Combine environment (global variables, mutexes, etc)
      between local state (first component from enter) and function return.

      This shouldn't yet assign to the lval. *)
  val combine_env : (D.t, G.t, C.t, V.t) man -> lval option -> exp -> fundec -> exp list -> C.t option -> D.t -> Queries.ask -> D.t

  (** Combine return value assignment
      to local state (result from combine_env) and function return.

      This should only assign to the lval. *)
  val combine_assign : (D.t, G.t, C.t, V.t) man -> lval option -> exp -> fundec -> exp list -> C.t option -> D.t -> Queries.ask -> D.t

  (* Paths as sets: I know this is ugly! *)
  val paths_as_set : (D.t, G.t, C.t, V.t) man -> D.t list

  (** Returns initial state for created thread. *)
  val threadenter : (D.t, G.t, C.t, V.t) man -> multiple:bool -> lval option -> varinfo -> exp list -> D.t list

  (** Updates the local state of the creator thread using initial state of created thread. *)
  val threadspawn : (D.t, G.t, C.t, V.t) man -> multiple:bool -> lval option -> varinfo -> exp list -> (D.t, G.t, C.t, V.t) man -> D.t

  val event : (D.t, G.t, C.t, V.t) man -> Events.t -> (D.t, G.t, C.t, V.t) man -> D.t
end

module type Spec2Spec = functor (S: Spec) -> Spec

module type MCPA =
sig
  include Printable.S
  val may_race: t -> t -> bool
  val should_print: t -> bool (** Whether value should be printed in race output. *)
end

module type MCPSpec =
sig
  include Spec

  module A: MCPA
  val access: (D.t, G.t, C.t, V.t) man -> Queries.access -> A.t
end

type increment_data = {
  server: bool;

  solver_data: Obj.t;
  changes: CompareCIL.change_info;

  (* Globals for which the constraint
     system unknowns should be restarted *)
  restarting: Goblint_constraint.VarQuery.t list;
}

module StdV =
struct
  let is_write_only _ = false
end

module UnitV =
struct
  include Printable.Unit
  include StdV
end

module VarinfoV =
struct
  include CilType.Varinfo (* TODO: or Basetype.Variables? *)
  include StdV
end

module EmptyV =
struct
  include Printable.Empty
  include StdV
end

module UnitA =
struct
  include Printable.Unit
  let may_race _ _ = true
  let should_print _ = false
end

module EmptyP =
struct
  include Printable.Empty
  let of_elt _ = failwith "EmptyP.of_elt: analysis cannot be path-sensitive"
end

module UnitP =
struct
  include Printable.Unit
  let of_elt _ = ()
end

module IdentityP (D: Lattice.S) =
struct
  include D
  let of_elt x = x
end

(** Relatively safe default implementations of some boring Spec functions. *)
module DefaultSpec =
struct
  module G = Lattice.Unit
  module V = EmptyV
  module P = EmptyP

  type marshal = unit
  let init _ = ()
  let finalize () = ()
  (* no inits nor finalize -- only analyses like Mutex, Base, ... need
     these to do postprocessing or other imperative hacks. *)

  let vdecl man _ = man.local

  let asm x =
    M.msg_final Info ~category:Unsound "ASM ignored";
    M.info ~category:Unsound "ASM statement ignored.";
    x.local (* Just ignore. *)

  let skip x = x.local (* Just ignore. *)

  let query _ (type a) (q: a Queries.t) = Queries.Result.top q
  (* Don't know anything --- most will want to redefine this. *)

  let event man _ _ = man.local

  let morphstate v d = d
  (* Only for those who track thread IDs. *)

  let sync man _ = man.local
  (* Most domains do not have a global part. *)

  let context man fd x = x
  (* Everything is context sensitive --- override in MCP and maybe elsewhere*)

  let paths_as_set man = [man.local]

  module A = UnitA
  let access _ _ = ()
end

(* Even more default implementations. Most transfer functions acting as identity functions. *)
module IdentitySpec =
struct
  include DefaultSpec
  let assign man (lval:lval) (rval:exp) =
    man.local

  let branch man (exp:exp) (tv:bool) =
    man.local

  let body man (f:fundec) =
    man.local

  let return man (exp:exp option) (f:fundec) =
    man.local

  let enter man (lval: lval option) (f:fundec) (args:exp list) =
    [man.local, man.local]

  let combine_env man (lval:lval option) fexp (f:fundec) (args:exp list) fc au (f_ask: Queries.ask) =
    au

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc au (f_ask: Queries.ask) =
    man.local

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) =
    man.local

  let threadenter man ~multiple lval f args = [man.local]
  let threadspawn man ~multiple lval f args fman = man.local
end

module IdentityUnitContextsSpec = struct
  include IdentitySpec
  module C = Printable.Unit

  let context man _ _ = ()
  let startcontext () = ()
end

module ValueContexts (D:Lattice.S) = struct
  module C = D
  let startcontext () = D.bot ()
end

module type SpecSys =
sig
  module Spec: Spec
  module EQSys: Goblint_constraint.ConstrSys.DemandGlobConstrSys with module LVar = VarF (Spec.C)
                               and module GVar = GVarF (Spec.V)
                               and module D = Spec.D
                               and module G = GVarG (Spec.G) (Spec.C)
  module LHT: BatHashtbl.S with type key = EQSys.LVar.t
  module GHT: BatHashtbl.S with type key = EQSys.GVar.t
end

module type SpecSysSol =
sig
  module SpecSys: SpecSys
  open SpecSys

  val gh: EQSys.G.t GHT.t
  val lh: SpecSys.Spec.D.t LHT.t (* explicit SpecSys to avoid spurious module cycle *)
end
