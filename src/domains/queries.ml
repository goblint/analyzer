(** Structures for the querying subsystem. *)

open Deriving.Cil

module GU = Goblintutil
module ID = IntDomain.FlattenedBI
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
let compare_itervar _ _ = 0
let compare_iterprevvar _ _ = 0

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

type any_query = Any: 'a t -> any_query

module Query = struct
  type t = any_query
  let compare a b =
    let order (q: t) = 
      match q with
        | Any(EqualSet _) -> 0
        | Any(MayPointTo _) -> 1
        | Any(ReachableFrom _) -> 2
        | Any(ReachableUkTypes _) -> 3
        | Any(Regions _) -> 4
        | Any(MayEscape _) -> 5 
        | Any(Priority _) -> 6 
        | Any(MayBePublic _) -> 7 
        | Any(MayBePublicWithout _) -> 8 
        | Any(MustBeProtectedBy _) -> 9 
        | Any(CurrentLockset) -> 10
        | Any(MustBeAtomic) -> 11 
        | Any(MustBeSingleThreaded) -> 12 
        | Any(MustBeUniqueThread) -> 13
        | Any(CurrentThreadId) -> 14
        | Any(MayBeThreadReturn) -> 15
        | Any(EvalFunvar _) -> 16
        | Any(EvalInt _) -> 17
        | Any(EvalStr _) -> 18
        | Any(EvalLength _) -> 19
        | Any(BlobSize _) -> 20
        | Any(PrintFullState) -> 21
        | Any(CondVars _) -> 22
        | Any(PartAccess _) -> 23
        | Any(IterPrevVars _) -> 24
        | Any(IterVars _) -> 25
        | Any(MustBeEqual _) -> 26
        | Any(MayBeEqual _) -> 27
        | Any(MayBeLess _) -> 28
        | Any(HeapVar) -> 29
        | Any(IsHeapVar _) -> 30
        | Any(Assert _) -> 31
    in 
    let r = Stdlib.compare (order a) (order b) in
      if r <> 0 then
        r
      else
        match a, b with
        | Any(EqualSet e1), Any(EqualSet e2) -> CilType.Exp.compare e1 e2
        | Any(MayPointTo e1), Any(MayPointTo e2) -> CilType.Exp.compare e1 e2
        | Any(ReachableFrom e1), Any(ReachableFrom e2) -> CilType.Exp.compare e1 e2
        | Any(ReachableUkTypes e1), Any(ReachableUkTypes e2) -> CilType.Exp.compare e1 e2
        | Any(Regions e1), Any(Regions e2) -> CilType.Exp.compare e1 e2
        | Any(MayEscape vi1), Any(MayEscape vi2) -> CilType.Varinfo.compare vi1 vi2
        | Any(Priority s1), Any(Priority s2) -> compare s1 s2
        | Any(MayBePublic x1), Any(MayBePublic x2) -> 
          let r2 =  CilType.Varinfo.compare x1.global x2.global in
          if r2 <> 0 then 
            r2
          else
            Bool.compare x1.write x2.write
        | Any(MayBePublicWithout x1), Any(MayBePublicWithout x2) -> 
          let r2 =  CilType.Varinfo.compare x1.global x2.global in
          if r2 <> 0 then 
            r2
          else
            let r3 = Bool.compare x1.write x2.write in
            if r3 <> 0 then
              r3
            else
              PreValueDomain.Addr.compare x1.without_mutex x2.without_mutex
        | Any(MustBeProtectedBy x1), Any(MustBeProtectedBy x2) -> 
          let r2 = PreValueDomain.Addr.compare x1.mutex x2.mutex in
          if r2 <> 0 then 
            r2
          else
            let r3 = CilType.Varinfo.compare x1.global x2.global in
            if r3 <> 0 then
              r3
            else
              Bool.compare x1.write x2.write
        | Any(EvalFunvar e1), Any(EvalFunvar e2) -> CilType.Exp.compare e1 e2
        | Any(EvalInt e1), Any(EvalInt e2) -> CilType.Exp.compare e1 e2
        | Any(EvalStr e1), Any(EvalStr e2) -> CilType.Exp.compare e1 e2
        | Any(EvalLength e1), Any(EvalLength e2) -> CilType.Exp.compare e1 e2
        | Any(BlobSize e1), Any(BlobSize e2) -> CilType.Exp.compare e1 e2
        | Any(CondVars e1), Any(CondVars e2) -> CilType.Exp.compare e1 e2
        | Any(PartAccess p1), Any(PartAccess p2) ->
          let r2 = CilType.Exp.compare p1.exp p2.exp in
          if r2 <> 0 then
            r2
          else
            let r3 = match (p1.var_opt, p2.var_opt) with
              | (Some opt1, Some opt2) -> CilType.Varinfo.compare opt1 opt2 
              | (_, Some opt2) -> -1
              | (Some opt1, _) -> 1
              | (_, _)-> 0
            in
            if r3 <> 0 then
              r3
            else
              Bool.compare p1.write p2.write
        | Any(IterPrevVars ip1), Any(IterPrevVars ip2) -> compare_iterprevvar ip1 ip2
        | Any(IterVars i1), Any(IterVars i2) -> compare_itervar i1 i2
        | Any(MustBeEqual (e1, e2)), Any(MustBeEqual (e3, e4)) -> 
          let r2 = CilType.Exp.compare e1 e3 in
          if r2 <> 0 then
            r2
          else
            CilType.Exp.compare e2 e4
        | Any(MayBeEqual (e1, e2)), Any(MayBeEqual (e3, e4)) -> 
          let r2 = CilType.Exp.compare e1 e3 in
          if r2 <> 0 then
            r2
          else
            CilType.Exp.compare e2 e4
        | Any(MayBeLess (e1, e2)), Any(MayBeEqual (e3, e4)) -> 
        let r2 = CilType.Exp.compare e1 e3 in
          if r2 <> 0 then
            r2
          else
            CilType.Exp.compare e2 e4 
        | Any(IsHeapVar v1), Any(IsHeapVar v2) -> CilType.Varinfo.compare v1 v2
        | Any(Assert a1), Any(Assert a2) -> CilType.Exp.compare a1 a2 
        | _, _ -> Stdlib.compare (order a) (order b)
end