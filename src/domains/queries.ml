(** Structures for the querying subsystem. *)

open Deriving.Cil
open Pretty

module GU = Goblintutil
module ID = IntDomain.FlatPureIntegers
module LS = SetDomain.ToppedSet (Lval.CilLval) (struct let topname = "All" end)
module TS = SetDomain.ToppedSet (Basetype.CilType) (struct let topname = "All" end)
module PS = SetDomain.ToppedSet (Exp.LockingPattern) (struct let topname = "All" end)
module LPS = SetDomain.ToppedSet (Printable.Prod (Lval.CilLval) (Lval.CilLval)) (struct let topname = "All" end)

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

type iterprevvar = int -> (MyCFG.node * Obj.t * int) -> MyARG.inline_edge -> unit
let iterprevvar_to_yojson _ = `Null
type itervar = int -> unit
let itervar_to_yojson _ = `Null

type t = EqualSet of exp
       | MayPointTo of exp
       | ReachableFrom of exp
       | ReachableUkTypes of exp
       | Regions of exp
       | MayEscape of varinfo
       | Priority of string
       | MayBePublic of {global: varinfo; write: bool} (* old behavior with write=false *)
       | MayBePublicWithout of {global: varinfo; write: bool; without_mutex: PreValueDomain.Addr.t}
       | MustBeProtectedBy of {mutex: PreValueDomain.Addr.t; global: varinfo; write: bool}
       | CurrentLockset
       | MustBeAtomic
       | MustBeSingleThreaded
       | MustBeUniqueThread
       | CurrentThreadId
       | MayBeThreadReturn
       | EvalFunvar of exp
       | EvalInt of exp
       | EvalStr of exp
       | EvalLength of exp (* length of an array or string *)
       | BlobSize of exp (* size of a dynamically allocated `Blob pointed to by exp *)
       | PrintFullState
       | CondVars of exp
       | PartAccess of {exp: exp; var_opt: varinfo option; write: bool}
       | IterPrevVars of iterprevvar
       | IterVars of itervar
       | MustBeEqual of exp * exp (* are two expression known to must-equal ? *)
       | MayBeEqual of exp * exp (* may two expressions be equal? *)
       | MayBeLess of exp * exp (* may exp1 < exp2 ? *)
       | TheAnswerToLifeUniverseAndEverything
       | HeapVar
       | IsHeapVar of varinfo
[@@deriving to_yojson]

module PartAccessResult = Access.PartAccessResult

type result = [
  | `Top
  | `Int of ID.t
  | `Str of string
  | `LvalSet of LS.t
  | `ExprSet of ES.t
  | `ExpTriples of PS.t
  | `TypeSet of TS.t
  | `Varinfo of VI.t
  | `MustBool of bool  (* true \leq false *)
  | `MayBool of bool   (* false \leq true *)
  | `PartAccessResult of PartAccessResult.t
  | `Bot
] [@@deriving ord, to_yojson]

type ask = t -> result

module Result: Lattice.S with type t = result =
struct
  include Printable.Std
  type t = result [@@deriving ord, to_yojson]

  let name () = "query result domain"

  let bot () = `Bot
  let is_bot x = x = `Bot
  let bot_name = "Bottom"
  let top () = `Top
  let is_top x = x = `Top
  let top_name = "Unknown"

  let equal x y =
    match (x, y) with
    | (`Top, `Top) -> true
    | (`Bot, `Bot) -> true
    | (`Int x, `Int y) -> ID.equal x y
    | (`LvalSet x, `LvalSet y) -> LS.equal x y
    | (`ExprSet x, `ExprSet y) -> ES.equal x y
    | (`ExpTriples x, `ExpTriples y) -> PS.equal x y
    | (`TypeSet x, `TypeSet y) -> TS.equal x y
    | (`Varinfo x, `Varinfo y) -> VI.equal x y
    | (`MustBool x, `MustBool y) -> Bool.equal x y
    | (`MayBool x, `MayBool y) -> Bool.equal x y
    | (`PartAccessResult x, `PartAccessResult y) -> PartAccessResult.equal x y
    | _ -> false

  let hash (x:t) =
    match x with
    | `Int n -> ID.hash n
    | `LvalSet n -> LS.hash n
    | `ExprSet n -> ES.hash n
    | `ExpTriples n -> PS.hash n
    | `TypeSet n -> TS.hash n
    | `Varinfo n -> VI.hash n
    | `PartAccessResult n -> PartAccessResult.hash n
    (* `MustBool and `MayBool should work by the following *)
    | _ -> Hashtbl.hash x

  let pretty () state =
    match state with
    | `Int n ->  ID.pretty () n
    | `Str s ->  text s
    | `LvalSet n ->  LS.pretty () n
    | `ExprSet n ->  ES.pretty () n
    | `ExpTriples n ->  PS.pretty () n
    | `TypeSet n -> TS.pretty () n
    | `Varinfo n -> VI.pretty () n
    | `MustBool n -> text (string_of_bool n)
    | `MayBool n -> text (string_of_bool n)
    | `PartAccessResult n -> PartAccessResult.pretty () n
    | `Bot -> text bot_name
    | `Top -> text top_name

  let show state =
    match state with
    | `Int n ->  ID.show n
    | `Str s ->  s
    | `LvalSet n ->  LS.show n
    | `ExprSet n ->  ES.show n
    | `ExpTriples n ->  PS.show n
    | `TypeSet n -> TS.show n
    | `Varinfo n -> VI.show n
    | `MustBool n -> string_of_bool n
    | `MayBool n -> string_of_bool n
    | `PartAccessResult n -> PartAccessResult.show n
    | `Bot -> bot_name
    | `Top -> top_name

  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let leq x y =
    match (x,y) with
    | (_, `Top) -> true
    | (`Top, _) -> false
    | (`Bot, _) -> true
    | (_, `Bot) -> false
    | (`Int x, `Int y) -> ID.leq x y
    | (`LvalSet x, `LvalSet y) -> LS.leq x y
    | (`ExprSet x, `ExprSet y) -> ES.leq x y
    | (`ExpTriples x, `ExpTriples y) -> PS.leq x y
    | (`TypeSet x, `TypeSet y) -> TS.leq x y
    | (`Varinfo x, `Varinfo y) -> VI.leq x y
    (* TODO: should these be more like IntDomain.Booleans? *)
    | (`MustBool x, `MustBool y) -> x == y || x
    | (`MayBool x, `MayBool y) -> x == y || y
    | (`PartAccessResult x, `PartAccessResult y) -> PartAccessResult.leq x y
    | _ -> false

  let join x y =
    try match (x,y) with
      | (`Top, _)
      | (_, `Top) -> `Top
      | (`Bot, x)
      | (x, `Bot) -> x
      | (`Int x, `Int y) -> `Int (ID.join x y)
      | (`LvalSet x, `LvalSet y) -> `LvalSet (LS.join x y)
      | (`ExprSet x, `ExprSet y) -> `ExprSet (ES.join x y)
      | (`ExpTriples x, `ExpTriples y) -> `ExpTriples (PS.join x y)
      | (`TypeSet x, `TypeSet y) -> `TypeSet (TS.join x y)
      | (`Varinfo x, `Varinfo y) -> `Varinfo (VI.join x y)
      | (`MustBool x, `MustBool y) -> `MustBool (x && y)
      | (`MayBool x, `MayBool y) -> `MayBool (x || y)
      | (`PartAccessResult x, `PartAccessResult y) -> `PartAccessResult (PartAccessResult.join x y)
      | _ -> `Top
    with IntDomain.Unknown -> `Top

  let meet x y =
    try match (x,y) with
      | (`Bot, _)
      | (_, `Bot) -> `Bot
      | (`Top, x)
      | (x, `Top) -> x
      | (`Int x, `Int y) -> `Int (ID.meet x y)
      | (`LvalSet x, `LvalSet y) -> `LvalSet (LS.meet x y)
      | (`ExprSet x, `ExprSet y) -> `ExprSet (ES.meet x y)
      | (`ExpTriples x, `ExpTriples y) -> `ExpTriples (PS.meet x y)
      | (`TypeSet x, `TypeSet y) -> `TypeSet (TS.meet x y)
      | (`Varinfo x, `Varinfo y) -> `Varinfo (VI.meet x y)
      | (`MustBool x, `MustBool y) -> `MustBool (x || y)
      | (`MayBool x, `MayBool y) -> `MayBool (x && y)
      | (`PartAccessResult x, `PartAccessResult y) -> `PartAccessResult (PartAccessResult.meet x y)
      | _ -> `Bot
    with IntDomain.Error -> `Bot

  let widen x y =
    try match (x,y) with
      | (`Top, _)
      | (_, `Top) -> `Top
      | (`Bot, x)
      | (x, `Bot) -> x
      | (`Int x, `Int y) -> `Int (ID.widen x y)
      | (`LvalSet x, `LvalSet y) -> `LvalSet (LS.widen x y)
      | (`ExprSet x, `ExprSet y) -> `ExprSet (ES.widen x y)
      | (`ExpTriples x, `ExpTriples y) -> `ExpTriples (PS.widen x y)
      | (`TypeSet x, `TypeSet y) -> `TypeSet (TS.widen x y)
      | (`Varinfo x, `Varinfo y) -> `Varinfo (VI.widen x y)
      | (`MustBool x, `MustBool y) -> `MustBool (x && y)
      | (`MayBool x, `MayBool y) -> `MustBool (x || y)
      | (`PartAccessResult x, `PartAccessResult y) -> `PartAccessResult (PartAccessResult.widen x y)
      | _ -> `Top
    with IntDomain.Unknown -> `Top

  let narrow x y =
    match (x,y) with
    | (`Int x, `Int y) -> `Int (ID.narrow x y)
    | (`LvalSet x, `LvalSet y) -> `LvalSet (LS.narrow x y)
    | (`ExprSet x, `ExprSet y) -> `ExprSet (ES.narrow x y)
    | (`ExpTriples x, `ExpTriples y) -> `ExpTriples (PS.narrow x y)
    | (`TypeSet x, `TypeSet y) -> `TypeSet (TS.narrow x y)
    | (`Varinfo x, `Varinfo y) -> `Varinfo (VI.narrow x y)
    | (`MustBool x, `MustBool y) -> `MustBool (x || y)
    | (`MayBool x, `MayBool y) -> `MayBool (x && y)
    | (`PartAccessResult x, `PartAccessResult y) -> `PartAccessResult (PartAccessResult.narrow x y)
    | (x,_) -> x

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>%s\n</data>\n</value>\n" (Goblintutil.escape (show x))
end
