(** The lattice signature and simple functors for building lattices. *)

module Pretty = GoblintCil.Pretty
module GU = Goblintutil

(* module type Rel =
sig
  type t
  type relation = Less | Equal | Greater | Uncomparable
  val rel : t -> t -> relation
  val in_rel : t -> relation -> t -> bool
end *)

(* partial order: elements might not be comparable and no bot/top -> join etc. might fail with exception Uncomparable *)
exception Uncomparable
module type PO =
sig
  include Printable.S
  val leq: t -> t -> bool
  val join: t -> t -> t
  val meet: t -> t -> t
  val widen: t -> t -> t (** [widen x y] assumes [leq x y]. Solvers guarantee this by calling [widen old (join old new)]. *)

  val narrow: t -> t -> t

  (** If [leq x y = false], then [pretty_diff () (x, y)] should explain why. *)
  val pretty_diff: unit -> (t * t) -> Pretty.doc
end

(* complete lattice *)
module type S =
sig
  include PO
  val bot: unit -> t
  val is_bot: t -> bool
  val top: unit -> t
  val is_top: t -> bool
end

exception TopValue
(** Exception raised by a topless lattice in place of a top value.
    Surrounding lattice functors may handle this on their own. *)

exception BotValue
(** Exception raised by a bottomless lattice in place of a bottom value.
    Surrounding lattice functors may handle this on their own. *)

exception Unsupported of string
let unsupported x = raise (Unsupported x)

exception Invalid_widen of Pretty.doc

let () = Printexc.register_printer (function
    | Invalid_widen doc ->
      Some (Pretty.sprint ~width:max_int (Pretty.dprintf "Lattice.Invalid_widen(%a)" Pretty.insert doc))
    | _ -> None (* for other exceptions *)
  )

let assert_valid_widen ~leq ~pretty_diff x y =
  if not (leq x y) then
    raise (Invalid_widen (pretty_diff () (x, y)))

module UnitConf (N: Printable.Name) =
struct
  include Printable.UnitConf (N)
  let leq _ _ = true
  let join _ _ = ()
  let widen = join
  let meet _ _ = ()

  let narrow = meet
  let top () = ()
  let is_top _ = true
  let bot () = ()
  let is_bot _ = true

  let pretty_diff () _ = Pretty.text "UnitConf: impossible"
end
module Unit = UnitConf (struct let name = "()" end)


module NoBotTop =
struct
  let top () = raise TopValue
  let is_top _ = false
  let bot () = raise BotValue
  let is_bot _ = false
end


module Fake (Base: Printable.S) =
struct
  include Base
  let leq = equal
  let join x y =
    if equal x y then x else raise (Unsupported "fake join")
  let widen = join
  let meet x y =
    if equal x y then x else raise (Unsupported "fake meet")
  let narrow = meet
  include NoBotTop

  let pretty_diff () (x,y) =
    Pretty.dprintf "%s: %a not equal %a" (Base.name ()) pretty x pretty y
end

module type PD =
sig
  include Printable.S
  val dummy: t
end

module FakeSingleton (Base: PD) =
struct
  include Base
  let leq x y = true
  let join x y = x
  let widen = join
  let meet x y = x
  let narrow = meet
  let top () = Base.dummy
  let bot () = Base.dummy
  let is_top _ = true
  let is_bot _ = true

  let pretty_diff () _ = Pretty.text "FakeSingleton: impossible"
end

module Reverse (Base: S) =
struct
  include Base
  (* include StdCousot (* this isn't good *) *)
  let widen = Base.meet
  let narrow = Base.join
  let bot = Base.top
  let is_bot = Base.is_top
  let top = Base.bot
  let is_top = Base.is_bot
  let leq x y = Base.leq y x
  let join x y = Base.meet x y
  let meet x y = Base.join x y
  let name () = "Reversed (" ^ name () ^ ")"
  let pretty_diff () (x,y) =
    Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml = Base.printXml

  let arbitrary = Base.arbitrary
end

(* HAS SIDE-EFFECTS ---- PLEASE INSTANCIATE ONLY ONCE!!! *)
module HConsed (Base:S) =
struct
  include Printable.HConsed (Base)
  let lift_f2 f x y = f (unlift x) (unlift y)
  let narrow x y = if x.BatHashcons.tag == y.BatHashcons.tag then x else lift (lift_f2 Base.narrow x y)
  let widen x y = if x.BatHashcons.tag == y.BatHashcons.tag then x else lift (lift_f2 Base.widen x y)
  let meet x y = if x.BatHashcons.tag == y.BatHashcons.tag then x else lift (lift_f2 Base.meet x y)
  let join x y = if x.BatHashcons.tag == y.BatHashcons.tag then x else lift (lift_f2 Base.join x y)
  let leq x y = (x.BatHashcons.tag == y.BatHashcons.tag) || lift_f2 Base.leq x y
  let is_top = lift_f Base.is_top
  let is_bot = lift_f Base.is_bot
  let top () = lift (Base.top ())
  let bot () = lift (Base.bot ())

  let pretty_diff () (x,y) = Base.pretty_diff () (x.BatHashcons.obj,y.BatHashcons.obj)
end

module HashCached (M: S) =
struct
  include Printable.HashCached (M)

  let leq = lift_f2 M.leq
  let join = lift_f2' M.join
  let meet = lift_f2' M.meet
  let widen = lift_f2' M.widen
  let narrow = lift_f2' M.narrow
  let bot () = lift @@ M.bot ()
  let is_bot = lift_f M.is_bot
  let top () = lift @@ M.top ()
  let is_top = lift_f M.is_top

  let pretty_diff () ((x:t),(y:t)): Pretty.doc = M.pretty_diff () (unlift x, unlift y)
end

module Flat (Base: Printable.S) (N: Printable.LiftingNames) =
struct
  include Printable.Lift (Base) (N)
  let bot () = `Bot
  let is_bot x = x = `Bot
  let top () = `Top
  let is_top x = x = `Top

  let leq (x:t) (y:t) =
    match (x,y) with
    | (_, `Top) -> true
    | (`Top, _) -> false
    | (`Bot, _) -> true
    | (_, `Bot) -> false
    | (`Lifted x, `Lifted y) -> Base.equal x y

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    if leq x y then Pretty.text "No Changes" else
      Pretty.dprintf "%a instead of %a" pretty x pretty y

  let join x y =
    match (x,y) with
    | (`Top, _) -> `Top
    | (_, `Top) -> `Top
    | (`Bot, x) -> x
    | (x, `Bot) -> x
    | (`Lifted x, `Lifted y) when Base.equal x y -> `Lifted x
    | _ -> `Top

  let widen = join

  let meet x y =
    match (x,y) with
    | (`Bot, _) -> `Bot
    | (_, `Bot) -> `Bot
    | (`Top, x) -> x
    | (x, `Top) -> x
    | (`Lifted x, `Lifted y) when Base.equal x y -> `Lifted x
    | _ -> `Bot

  let narrow = meet

end


module Lift (Base: S) (N: Printable.LiftingNames) =
struct
  include Printable.Lift (Base) (N)

  let bot () = `Bot
  let is_bot x = x = `Bot
  let top () = `Top
  let is_top x = x = `Top

  let leq x y =
    match (x,y) with
    | (_, `Top) -> true
    | (`Top, _) -> false
    | (`Bot, _) -> true
    | (_, `Bot) -> false
    | (`Lifted x, `Lifted y) -> Base.leq x y

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    match (x,y) with
    | (`Lifted x, `Lifted y) -> Base.pretty_diff () (x,y)
    | _ -> if leq x y then Pretty.text "No Changes" else
        Pretty.dprintf "%a instead of %a" pretty x pretty y

  let join x y =
    match (x,y) with
    | (`Top, _) -> `Top
    | (_, `Top) -> `Top
    | (`Bot, x) -> x
    | (x, `Bot) -> x
    | (`Lifted x, `Lifted y) -> `Lifted (Base.join x y)

  let meet x y =
    match (x,y) with
    | (`Bot, _) -> `Bot
    | (_, `Bot) -> `Bot
    | (`Top, x) -> x
    | (x, `Top) -> x
    | (`Lifted x, `Lifted y) -> `Lifted (Base.meet x y)

  let widen x y =
    match (x,y) with
    | (`Lifted x, `Lifted y) -> `Lifted (Base.widen x y)
    | _ -> y

  let narrow x y =
    match (x,y) with
    | (`Lifted x, `Lifted y) -> `Lifted (Base.narrow x y)
    | _ -> x
end

module LiftPO (Base: PO) (N: Printable.LiftingNames) =
struct
  include Printable.Lift (Base) (N)

  let bot () = `Bot
  let is_bot x = x = `Bot
  let top () = `Top
  let is_top x = x = `Top

  let leq x y =
    match (x,y) with
    | (_, `Top) -> true
    | (`Top, _) -> false
    | (`Bot, _) -> true
    | (_, `Bot) -> false
    | (`Lifted x, `Lifted y) -> Base.leq x y

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    match (x,y) with
    | (`Lifted x, `Lifted y) -> Base.pretty_diff () (x,y)
    | _ -> if leq x y then Pretty.text "No Changes" else
        Pretty.dprintf "%a instead of %a" pretty x pretty y

  let join x y =
    match (x,y) with
    | (`Top, _) -> `Top
    | (_, `Top) -> `Top
    | (`Bot, x) -> x
    | (x, `Bot) -> x
    | (`Lifted x, `Lifted y) ->
      try `Lifted (Base.join x y)
      with Uncomparable -> `Top

  let meet x y =
    match (x,y) with
    | (`Bot, _) -> `Bot
    | (_, `Bot) -> `Bot
    | (`Top, x) -> x
    | (x, `Top) -> x
    | (`Lifted x, `Lifted y) ->
      try `Lifted (Base.meet x y)
      with Uncomparable -> `Bot

  let widen x y =
    match (x,y) with
    | (`Lifted x, `Lifted y) ->
      (try `Lifted (Base.widen x y)
      with Uncomparable -> `Top)
    | _ -> y

  let narrow x y =
    match (x,y) with
    | (`Lifted x, `Lifted y) ->
      (try `Lifted (Base.narrow x y)
      with Uncomparable -> `Bot)
    | _ -> x
end

module Lift2 (Base1: S) (Base2: S) (N: Printable.LiftingNames) =
struct
  include Printable.Lift2 (Base1) (Base2) (N)

  let bot () = `Bot
  let is_bot x = x = `Bot
  let top () = `Top
  let is_top x = x = `Top

  let leq x y =
    match (x,y) with
    | (_, `Top) -> true
    | (`Top, _) -> false
    | (`Bot, _) -> true
    | (_, `Bot) -> false
    | (`Lifted1 x, `Lifted1 y) -> Base1.leq x y
    | (`Lifted2 x, `Lifted2 y) -> Base2.leq x y
    | _ -> false

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    match x, y with
    | `Lifted1 x, `Lifted1 y -> Base1.pretty_diff () (x, y)
    | `Lifted2 x, `Lifted2 y -> Base2.pretty_diff () (x, y)
    | _ when leq x y -> Pretty.text "No Changes"
    | _ -> Pretty.dprintf "%a instead of %a" pretty x pretty y

  let join x y =
    match (x,y) with
    | (`Top, _) -> `Top
    | (_, `Top) -> `Top
    | (`Bot, x) -> x
    | (x, `Bot) -> x
    | (`Lifted1 x, `Lifted1 y) -> begin
        try `Lifted1 (Base1.join x y)
        with Unsupported _ -> `Top
      end
    | (`Lifted2 x, `Lifted2 y) -> begin
        try `Lifted2 (Base2.join x y)
        with Unsupported _ -> `Top
      end
    | _ -> `Top

  let meet x y =
    match (x,y) with
    | (`Bot, _) -> `Bot
    | (_, `Bot) -> `Bot
    | (`Top, x) -> x
    | (x, `Top) -> x
    | (`Lifted1 x, `Lifted1 y) -> begin
        try `Lifted1 (Base1.meet x y)
        with Unsupported _ -> `Bot
      end
    | (`Lifted2 x, `Lifted2 y) -> begin
        try `Lifted2 (Base2.meet x y)
        with Unsupported _ -> `Bot
      end
    | _ -> `Bot

  let widen x y =
    match (x,y) with
    | (`Lifted1 x, `Lifted1 y) -> `Lifted1 (Base1.widen x y)
    | (`Lifted2 x, `Lifted2 y) -> `Lifted2 (Base2.widen x y)
    | _ -> y

  let narrow x y =
    match (x,y) with
    | (`Lifted1 x, `Lifted1 y) -> `Lifted1 (Base1.narrow x y)
    | (`Lifted2 x, `Lifted2 y) -> `Lifted2 (Base2.narrow x y)
    | _ -> x

end

module ProdConf (C: Printable.ProdConfiguration) (Base1: S) (Base2: S) =
struct
  include Printable.ProdConf (C) (Base1) (Base2)

  let bot () = (Base1.bot (), Base2.bot ())
  let is_bot (x1,x2) = Base1.is_bot x1 && Base2.is_bot x2
  let top () = (Base1.top (), Base2.top ())
  let is_top (x1,x2) = Base1.is_top x1 && Base2.is_top x2

  let leq (x1,x2) (y1,y2) = Base1.leq x1 y1 && Base2.leq x2 y2

  let pretty_diff () ((x1,x2:t),(y1,y2:t)): Pretty.doc =
    if Base1.leq x1 y1 then
      Base2.pretty_diff () (x2,y2)
    else
      Base1.pretty_diff () (x1,y1)

  let op_scheme op1 op2 (x1,x2) (y1,y2): t = (op1 x1 y1, op2 x2 y2)
  let join = op_scheme Base1.join Base2.join
  let meet = op_scheme Base1.meet Base2.meet
  let narrow = op_scheme Base1.narrow Base2.narrow
  let widen = op_scheme Base1.widen Base2.widen

end


module Prod = ProdConf (struct let expand_fst = true let expand_snd = true end)
module ProdSimple = ProdConf (struct let expand_fst = false let expand_snd = false end)

module Prod3 (Base1: S) (Base2: S) (Base3: S) =
struct
  include Printable.Prod3 (Base1) (Base2) (Base3)

  let bot () = (Base1.bot (), Base2.bot (), Base3.bot ())
  let is_bot (x1,x2,x3) = Base1.is_bot x1 && Base2.is_bot x2 && Base3.is_bot x3
  let top () = (Base1.top (), Base2.top (), Base3.top ())
  let is_top (x1,x2,x3) = Base1.is_top x1 && Base2.is_top x2 && Base3.is_top x3

  let leq (x1,x2,x3) (y1,y2,y3) = Base1.leq x1 y1 && Base2.leq x2 y2 && Base3.leq x3 y3

  let pretty_diff () ((x1,x2,x3:t),(y1,y2,y3:t)): Pretty.doc =
    if not (Base1.leq x1 y1) then
      Base1.pretty_diff () (x1,y1)
    else if not (Base2.leq x2 y2) then
      Base2.pretty_diff () (x2,y2)
    else
      Base3.pretty_diff () (x3,y3)

  let op_scheme op1 op2 op3 (x1,x2,x3) (y1,y2,y3): t = (op1 x1 y1, op2 x2 y2, op3 x3 y3)
  let join = op_scheme Base1.join Base2.join Base3.join
  let meet = op_scheme Base1.meet Base2.meet Base3.meet
  let widen = op_scheme Base1.widen Base2.widen Base3.widen
  let narrow = op_scheme Base1.narrow Base2.narrow Base3.narrow
end

module LiftBot (Base : S) =
struct
  include Printable.LiftBot (Base)

  let bot () = `Bot
  let is_bot x = x = `Bot
  let top () = `Lifted (Base.top ())
  let is_top x =
    match x with
    | `Lifted x -> Base.is_top x
    | `Bot -> false

  let leq x y =
    match (x,y) with
    | (`Bot, _) -> true
    | (_, `Bot) -> false
    | (`Lifted x, `Lifted y) -> Base.leq x y

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    match x, y with
    | `Lifted x, `Lifted y -> Base.pretty_diff () (x, y)
    | _ -> Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let join x y =
    match (x,y) with
    | (`Bot, x) -> x
    | (x, `Bot) -> x
    | (`Lifted x, `Lifted y) -> `Lifted (Base.join x y)

  let meet x y =
    match (x,y) with
    | (`Bot, _) -> `Bot
    | (_, `Bot) -> `Bot
    | (`Lifted x, `Lifted y) -> `Lifted (Base.meet x y)

  let widen x y =
    match (x,y) with
    | (`Lifted x, `Lifted y) -> `Lifted (Base.widen x y)
    | _ -> y

  let narrow x y =
    match (x,y) with
    | (`Lifted x, `Lifted y) -> `Lifted (Base.narrow x y)
    | _ -> x
end

module LiftTop (Base : S) =
struct
  include Printable.LiftTop (Base)

  let top () = `Top
  let is_top x = x = `Top
  let bot () = `Lifted (Base.bot ())
  let is_bot x =
    match x with
    | `Lifted x -> Base.is_bot x
    | `Top -> false

  let leq x y =
    match (x,y) with
    | (_, `Top) -> true
    | (`Top, _) -> false
    | (`Lifted x, `Lifted y) -> Base.leq x y

  let join x y =
    match (x,y) with
    | (`Top, x) -> `Top
    | (x, `Top) -> `Top
    | (`Lifted x, `Lifted y) -> `Lifted (Base.join x y)

  let meet x y =
    match (x,y) with
    | (`Top, x) -> x
    | (x, `Top) -> x
    | (`Lifted x, `Lifted y) -> `Lifted (Base.meet x y)

  let widen x y =
    match (x,y) with
    | (`Lifted x, `Lifted y) -> `Lifted (Base.widen x y)
    | _ -> y

  let narrow x y =
    match (x,y) with
    | (`Lifted x, `Lifted y) -> `Lifted (Base.narrow x y)
    | _ -> x

  let pretty_diff () (x,y) =
    match (x,y) with
    | `Lifted x, `Lifted y -> Base.pretty_diff () (x,y)
    | _ -> Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module Liszt (Base: S) =
struct
  include Printable.Liszt (Base)
  let bot () = raise (Unsupported "bot?")
  let is_top _ = false
  let top () = raise (Unsupported "top?")
  let is_bot _ = false

  let leq =
    let f acc x y = Base.leq x y && acc in
    List.fold_left2 f true

  let join = List.map2 Base.join
  let widen = join
  let meet = List.map2 Base.meet
  let narrow = meet

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "%a not leq %a" pretty x pretty y
end

module type Num = sig val x : unit -> int end
module ProdList (Base: S) (N: Num) =
struct
  include Printable.Liszt (Base)

  let bot () = BatList.make (N.x ()) (Base.bot ())
  let is_bot = List.for_all Base.is_bot
  let top () = BatList.make (N.x ()) (Base.top ())
  let is_top = List.for_all Base.is_top

  let leq =
    let f acc x y = Base.leq x y && acc in
    List.fold_left2 f true

  let join = List.map2 Base.join
  let widen = List.map2 Base.widen
  let meet = List.map2 Base.meet
  let narrow = List.map2 Base.narrow

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "%a not leq %a" pretty x pretty y
end

module Chain (P: Printable.ChainParams) =
struct
  include Printable.Std
  include Printable.Chain (P)

  let bot () = 0
  let is_bot x = x = 0
  let top () = P.n () - 1
  let is_top x = x = P.n () - 1

  let leq x y = x <= y
  let join x y = max x y
  let widen = join
  let meet x y = min x y
  let narrow = meet

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "%a not leq %a" pretty x pretty y
end
