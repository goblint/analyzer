(** Structures for the querying subsystem. *)

open Deriving.Cil
open Pretty

module GU = Goblintutil
module ID = IntDomain.Flattened
module LS = SetDomain.ToppedSet (Lval.CilLval) (struct let topname = "All" end)
module TS = SetDomain.ToppedSet (Basetype.CilType) (struct let topname = "All" end)

(* who uses this? hopefully it's not in the domain *)
module ES_r = SetDomain.ToppedSet (Exp.Exp) (struct let topname = "All" end)
module ES =
struct
  include ES_r
  include Printable.Std
  let bot = ES_r.top
  let top = ES_r.bot
  let leq x y = ES_r.leq y x
  let join = ES_r.meet
  let meet x y = ES_r.join x y
end

module VI = Lattice.Flat (Basetype.Variables) (struct
  let top_name = "Unknown line"
  let bot_name = "Unreachable line"
end)

module PartAccessResult = Access.PartAccessResult

type iterprevvar = int -> (MyCFG.node * Obj.t * int) -> MyARG.inline_edge -> unit
let iterprevvar_to_yojson _ = `Null
type itervar = int -> unit
let itervar_to_yojson _ = `Null

module SD = Basetype.Strings

module MayBool =
struct
  type t = bool
  let bot () = false
  let top () = true
  let equal = Bool.equal
  let compare = Bool.compare
  let leq x y = x == y || y
  let join = (||)
  let widen = (||)
  let meet = (&&)
  let narrow = (&&)
end

module MustBool =
struct
  type t = bool
  let bot () = true
  let top () = false
  let equal = Bool.equal
  let compare = Bool.compare
  let leq x y = x == y || x
  let join = (&&)
  let widen = (&&)
  let meet = (||)
  let narrow = (||)
end

(* phantom types for matching queries with results *)
(* TODO: why do these require constructors for refutation to work? *)
type maybool = MayBool
type mustbool = MustBool

type lvalset = LvalSet
type exprset = ExprSet
type typeset = TypeSet

(* TODO: add phantom types for all variants *)

type _ t =
  | EqualSet: exp -> exprset t
  | MayPointTo: exp -> lvalset t
  | ReachableFrom: exp -> lvalset t
  | ReachableUkTypes: exp -> typeset t
  | Regions: exp -> lvalset t
  | MayEscape: varinfo -> maybool t
  | Priority: string -> ID.t t
  | MayBePublic: {global: varinfo; write: bool} -> maybool t (* old behavior with write=false *)
  | MayBePublicWithout: {global: varinfo; write: bool; without_mutex: PreValueDomain.Addr.t} -> maybool t
  | MustBeProtectedBy: {mutex: PreValueDomain.Addr.t; global: varinfo; write: bool} -> mustbool t
  | CurrentLockset: lvalset t
  | MustBeAtomic: mustbool t
  | MustBeSingleThreaded: mustbool t
  | MustBeUniqueThread: mustbool t
  | CurrentThreadId: VI.t t
  | MayBeThreadReturn: maybool t
  | EvalFunvar: exp -> lvalset t
  | EvalInt: exp -> ID.t t
  | EvalStr: exp -> SD.t t
  | EvalLength: exp -> ID.t t (* length of an array or string *)
  | BlobSize: exp -> ID.t t (* size of a dynamically allocated `Blob pointed to by exp *)
  | PrintFullState: unit t
  | CondVars: exp -> exprset t
  | PartAccess: {exp: exp; var_opt: varinfo option; write: bool} -> PartAccessResult.t t
  | IterPrevVars: iterprevvar -> unit t
  | IterVars: itervar -> unit t
  | MustBeEqual: exp * exp -> mustbool t (* are two expression known to must-equal ? *)
  | MayBeEqual: exp * exp -> maybool t (* may two expressions be equal? *)
  | MayBeLess: exp * exp -> maybool t (* may exp1 < exp2 ? *)
  | TheAnswerToLifeUniverseAndEverything: unit t (* TODO: unused, remove? *)
  | HeapVar: VI.t t
  | IsHeapVar: varinfo -> maybool t
(* [@@deriving to_yojson] *)


type _ result =
  (* | Top: 'a result *)
  | Int: ID.t -> ID.t result
  | Str: SD.t -> SD.t result
  | LvalSet: LS.t -> lvalset result
  | ExprSet: ES.t -> exprset result
  | TypeSet: TS.t -> typeset result
  | Varinfo: VI.t -> VI.t result
  | MustBool: MustBool.t -> mustbool result  (* true \leq false *)
  | MayBool: MayBool.t -> maybool result   (* false \leq true *)
  | PartAccessResult: PartAccessResult.t -> PartAccessResult.t result
  | Unit: unit result
  (* | Bot: 'a result *)
(* [@@deriving to_yojson] *)

type ask = { f: 'a. 'a t -> 'a result }

module Result =
struct
  let bot (type a) (q: a t): a result =
    match q with
    (* Cannot group these GADTs... *)
    | EqualSet _ -> ExprSet (ES.bot ())
    | CondVars _ -> ExprSet (ES.bot ())
    | MayPointTo _ -> LvalSet (LS.bot ())
    | ReachableFrom _ -> LvalSet (LS.bot ())
    | Regions _ -> LvalSet (LS.bot ())
    | CurrentLockset -> LvalSet (LS.bot ())
    | EvalFunvar _ -> LvalSet (LS.bot ())
    | ReachableUkTypes _ -> TypeSet (TS.bot ())
    | MayEscape _ -> MayBool (MayBool.bot ())
    | MayBePublic _ -> MayBool (MayBool.bot ())
    | MayBePublicWithout _ -> MayBool (MayBool.bot ())
    | MayBeThreadReturn -> MayBool (MayBool.bot ())
    | MayBeEqual _ -> MayBool (MayBool.bot ())
    | MayBeLess _ -> MayBool (MayBool.bot ())
    | IsHeapVar _ -> MayBool (MayBool.bot ()) (* TODO: is must? *)
    | MustBeProtectedBy _ -> MustBool (MustBool.bot ())
    | MustBeAtomic -> MustBool (MustBool.bot ())
    | MustBeSingleThreaded -> MustBool (MustBool.bot ())
    | MustBeUniqueThread -> MustBool (MustBool.bot ())
    | MustBeEqual _ -> MustBool (MustBool.bot ())
    | Priority _ -> Int (ID.bot ())
    | EvalInt _ -> Int (ID.bot ())
    | EvalLength _ -> Int (ID.bot ())
    | BlobSize _ -> Int (ID.bot ())
    | CurrentThreadId -> Varinfo (VI.bot ())
    | HeapVar -> Varinfo (VI.bot ())
    | EvalStr _ -> Str (SD.bot ())
    | PrintFullState -> Unit
    | IterPrevVars _ -> Unit
    | IterVars _ -> Unit
    | TheAnswerToLifeUniverseAndEverything -> Unit
    | PartAccess _ -> PartAccessResult (PartAccessResult.bot ())

  let top (type a) (q: a t): a result =
    match q with
    (* Cannot group these GADTs... *)
    | EqualSet _ -> ExprSet (ES.top ())
    | CondVars _ -> ExprSet (ES.top ())
    | MayPointTo _ -> LvalSet (LS.top ())
    | ReachableFrom _ -> LvalSet (LS.top ())
    | Regions _ -> LvalSet (LS.top ())
    | CurrentLockset -> LvalSet (LS.top ())
    | EvalFunvar _ -> LvalSet (LS.top ())
    | ReachableUkTypes _ -> TypeSet (TS.top ())
    | MayEscape _ -> MayBool (MayBool.top ())
    | MayBePublic _ -> MayBool (MayBool.top ())
    | MayBePublicWithout _ -> MayBool (MayBool.top ())
    | MayBeThreadReturn -> MayBool (MayBool.top ())
    | MayBeEqual _ -> MayBool (MayBool.top ())
    | MayBeLess _ -> MayBool (MayBool.top ())
    | IsHeapVar _ -> MayBool (MayBool.top ()) (* TODO: is must? *)
    | MustBeProtectedBy _ -> MustBool (MustBool.top ())
    | MustBeAtomic -> MustBool (MustBool.top ())
    | MustBeSingleThreaded -> MustBool (MustBool.top ())
    | MustBeUniqueThread -> MustBool (MustBool.top ())
    | MustBeEqual _ -> MustBool (MustBool.top ())
    | Priority _ -> Int (ID.top ())
    | EvalInt _ -> Int (ID.top ())
    | EvalLength _ -> Int (ID.top ())
    | BlobSize _ -> Int (ID.top ())
    | CurrentThreadId -> Varinfo (VI.top ())
    | HeapVar -> Varinfo (VI.top ())
    | EvalStr _ -> Str (SD.top ())
    | PrintFullState -> Unit
    | IterPrevVars _ -> Unit
    | IterVars _ -> Unit
    | TheAnswerToLifeUniverseAndEverything -> Unit
    | PartAccess _ -> PartAccessResult (PartAccessResult.top ())

  let pretty () (type a) (state: a result) =
    match state with
    | Int n ->  ID.pretty () n
    | Str s ->  SD.pretty () s
    | LvalSet n ->  LS.pretty () n
    | ExprSet n ->  ES.pretty () n
    | TypeSet n -> TS.pretty () n
    | Varinfo n -> VI.pretty () n
    | MustBool n -> text (string_of_bool n)
    | MayBool n -> text (string_of_bool n)
    | PartAccessResult n -> PartAccessResult.pretty () n
    | Unit -> text "()"

  let short w (type a) (state: a result) =
    match state with
    | Int n ->  ID.short w n
    | Str s ->  SD.short w s
    | LvalSet n ->  LS.short w n
    | ExprSet n ->  ES.short w n
    | TypeSet n -> TS.short w n
    | Varinfo n -> VI.short w n
    | MustBool n -> string_of_bool n
    | MayBool n -> string_of_bool n
    | PartAccessResult n -> PartAccessResult.short w n
    | Unit -> "()"

  let join (type a) (x: a result) (y: a result): a result =
    match x, y with
    | Int x, Int y -> Int (ID.join x y)
    | LvalSet x, LvalSet y -> LvalSet (LS.join x y)
    | ExprSet x, ExprSet y -> ExprSet (ES.join x y)
    | TypeSet x, TypeSet y -> TypeSet (TS.join x y)
    | Varinfo x, Varinfo y -> Varinfo (VI.join x y)
    | MustBool x, MustBool y -> MustBool (MustBool.join x y)
    | MayBool x, MayBool y -> MayBool (MayBool.join x y)
    | PartAccessResult x, PartAccessResult y -> PartAccessResult (PartAccessResult.join x y)
    | Unit, Unit -> Unit
    | Str x, Str y -> Str (SD.join x y)

  let meet (type a) (x: a result) (y: a result): a result =
    match x, y with
    | Int x, Int y -> Int (ID.meet x y)
    | LvalSet x, LvalSet y -> LvalSet (LS.meet x y)
    | ExprSet x, ExprSet y -> ExprSet (ES.meet x y)
    | TypeSet x, TypeSet y -> TypeSet (TS.meet x y)
    | Varinfo x, Varinfo y -> Varinfo (VI.meet x y)
    | MustBool x, MustBool y -> MustBool (MustBool.meet x y)
    | MayBool x, MayBool y -> MayBool (MayBool.meet x y)
    | PartAccessResult x, PartAccessResult y -> PartAccessResult (PartAccessResult.meet x y)
    | Unit, Unit -> Unit
    | Str x, Str y -> Str (SD.meet x y)
end
