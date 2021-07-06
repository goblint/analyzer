(** Structures for the querying subsystem. *)

open Deriving.Cil

module GU = Goblintutil
module ID = IntDomain.Flattened
module LS = SetDomain.ToppedSet (Lval.CilLval) (struct let topname = "All" end)
module TS = SetDomain.ToppedSet (Basetype.CilType) (struct let topname = "All" end)
module ES = SetDomain.Reverse (SetDomain.ToppedSet (Exp.Exp) (struct let topname = "All" end))

module VI = Lattice.Flat (Basetype.Variables) (struct
  let top_name = "Unknown line"
  let bot_name = "Unreachable line"
end)

module PartAccessResult = Access.PartAccessResult
module AR = Basetype.Bools (* TODO: move to BoolDomain? *)

type iterprevvar = int -> (MyCFG.node * Obj.t * int) -> MyARG.inline_edge -> unit
type itervar = int -> unit

module SD = Basetype.Strings

module MayBool = BoolDomain.MayBool
module MustBool = BoolDomain.MustBool

module Unit = Lattice.Unit

(** GADT for queries with specific result type. *)
type _ t =
  | EqualSet: exp -> ES.t t
  | MayPointTo: exp -> LS.t t
  | ReachableFrom: exp -> LS.t t
  | ReachableUkTypes: exp -> TS.t t
  | Regions: exp -> LS.t t
  | MayEscape: varinfo -> MayBool.t t
  | Priority: string -> ID.t t
  | MayBePublic: {global: varinfo; write: bool} -> MayBool.t t (* old behavior with write=false *)
  | MayBePublicWithout: {global: varinfo; write: bool; without_mutex: PreValueDomain.Addr.t} -> MayBool.t t
  | MustBeProtectedBy: {mutex: PreValueDomain.Addr.t; global: varinfo; write: bool} -> MustBool.t t
  | CurrentLockset: LS.t t
  | MustBeAtomic: MustBool.t t
  | MustBeSingleThreaded: MustBool.t t
  | MustBeUniqueThread: MustBool.t t
  | CurrentThreadId: VI.t t
  | MayBeThreadReturn: MayBool.t t
  | EvalFunvar: exp -> LS.t t
  | EvalInt: exp -> ID.t t
  | EvalStr: exp -> SD.t t
  | EvalLength: exp -> ID.t t (* length of an array or string *)
  | BlobSize: exp -> ID.t t (* size of a dynamically allocated `Blob pointed to by exp *)
  | PrintFullState: Unit.t t
  | CondVars: exp -> ES.t t
  | PartAccess: {exp: exp; var_opt: varinfo option; write: bool} -> PartAccessResult.t t
  | IterPrevVars: iterprevvar -> Unit.t t
  | IterVars: itervar -> Unit.t t
  | MustBeEqual: exp * exp -> MustBool.t t (* are two expression known to must-equal ? *)
  | MayBeEqual: exp * exp -> MayBool.t t (* may two expressions be equal? *)
  | MayBeLess: exp * exp -> MayBool.t t (* may exp1 < exp2 ? *)
  | HeapVar: VI.t t
  | IsHeapVar: varinfo -> MayBool.t t (* TODO: is may or must? *)
  | Assert: exp -> AR.t t

type 'a result = 'a

(** Container for explicitly polymorphic [ctx.ask] function out of [ctx].
    To be used when passing entire [ctx] around seems inappropriate.
    Use [Analyses.ask_of_ctx] to convert [ctx] to [ask]. *)
(* Must be in a singleton record due to second-order polymorphism.
   See https://ocaml.org/releases/4.12/htmlman/polymorphism.html#s%3Ahigher-rank-poly. *)
type ask = { f: 'a. 'a t -> 'a result }

(* Result cannot implement Lattice.S because the function types are different due to GADT. *)
module Result =
struct
  let lattice (type a) (q: a t): (module Lattice.S with type t = a) =
    match q with
    (* Cannot group these GADTs... *)
    | EqualSet _ -> (module ES)
    | CondVars _ -> (module ES)
    | MayPointTo _ -> (module LS)
    | ReachableFrom _ -> (module LS)
    | Regions _ -> (module LS)
    | CurrentLockset -> (module LS)
    | EvalFunvar _ -> (module LS)
    | ReachableUkTypes _ -> (module TS)
    | MayEscape _ -> (module MayBool)
    | MayBePublic _ -> (module MayBool)
    | MayBePublicWithout _ -> (module MayBool)
    | MayBeThreadReturn -> (module MayBool)
    | MayBeEqual _ -> (module MayBool)
    | MayBeLess _ -> (module MayBool)
    | IsHeapVar _ -> (module MayBool)
    | MustBeProtectedBy _ -> (module MustBool)
    | MustBeAtomic -> (module MustBool)
    | MustBeSingleThreaded -> (module MustBool)
    | MustBeUniqueThread -> (module MustBool)
    | MustBeEqual _ -> (module MustBool)
    | Priority _ -> (module ID)
    | EvalInt _ -> (module ID)
    | EvalLength _ -> (module ID)
    | BlobSize _ -> (module ID)
    | CurrentThreadId -> (module VI)
    | HeapVar -> (module VI)
    | EvalStr _ -> (module SD)
    | PrintFullState -> (module Unit)
    | IterPrevVars _ -> (module Unit)
    | IterVars _ -> (module Unit)
    | PartAccess _ -> (module PartAccessResult)
    | Assert _ -> (module AR)

  (** Get bottom result for query. *)
  let bot (type a) (q: a t): a result =
    let module Result = (val lattice q) in
    Result.bot ()

  (** Get top result for query. *)
  let top (type a) (q: a t): a result =
    (* let module Result = (val lattice q) in
    Result.top () *)
    (* [lattice] and [top] manually inlined to avoid first-class module
       for every unsupported [query] implementation.
       See benchmarks at: https://github.com/goblint/analyzer/pull/221#issuecomment-842351621. *)
    match q with
    (* Cannot group these GADTs... *)
    | EqualSet _ -> ES.top ()
    | CondVars _ -> ES.top ()
    | MayPointTo _ -> LS.top ()
    | ReachableFrom _ -> LS.top ()
    | Regions _ -> LS.top ()
    | CurrentLockset -> LS.top ()
    | EvalFunvar _ -> LS.top ()
    | ReachableUkTypes _ -> TS.top ()
    | MayEscape _ -> MayBool.top ()
    | MayBePublic _ -> MayBool.top ()
    | MayBePublicWithout _ -> MayBool.top ()
    | MayBeThreadReturn -> MayBool.top ()
    | MayBeEqual _ -> MayBool.top ()
    | MayBeLess _ -> MayBool.top ()
    | IsHeapVar _ -> MayBool.top ()
    | MustBeProtectedBy _ -> MustBool.top ()
    | MustBeAtomic -> MustBool.top ()
    | MustBeSingleThreaded -> MustBool.top ()
    | MustBeUniqueThread -> MustBool.top ()
    | MustBeEqual _ -> MustBool.top ()
    | Priority _ -> ID.top ()
    | EvalInt _ -> ID.top ()
    | EvalLength _ -> ID.top ()
    | BlobSize _ -> ID.top ()
    | CurrentThreadId -> VI.top ()
    | HeapVar -> VI.top ()
    | EvalStr _ -> SD.top ()
    | PrintFullState -> Unit.top ()
    | IterPrevVars _ -> Unit.top ()
    | IterVars _ -> Unit.top ()
    | PartAccess _ -> PartAccessResult.top ()
    | Assert _ -> AR.top ()
end
