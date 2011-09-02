module type S =
sig
  include Printable.S
  val leq: t -> t -> bool
  val join: t -> t -> t
  val meet: t -> t -> t
  val bot: unit -> t
  val is_bot: t -> bool
  val top: unit -> t
  val is_top: t -> bool
  val widen: t -> t -> t
  val narrow: t -> t -> t
end

module StdCousot = 
struct
  let widen x y = y
  let narrow x y = x
end

exception TopValue
exception BotValue
exception Unsupported of string
let unsupported x = raise (Unsupported x)

module Unit = 
struct
  include Printable.Unit
  include StdCousot
  let leq _ _ = true
  let join _ _ = ()
  let meet _ _ = ()
  let top () = ()
  let is_top _ = true
  let bot () = ()
  let is_bot _ = true
end



module Fake (Base: Printable.S) = 
struct 
  include Base
  include StdCousot
  let leq = equal
  let join x y = 
    if equal x y then x else raise (Unsupported "fake join")
  let meet x y = 
    if equal x y then x else raise (Unsupported "fake meet")
  let top () = raise (Unsupported "fake top")
  let is_top _ = false
  let bot () = raise (Unsupported "fake bot")
  let is_bot _ = false
end

module type PD = 
sig
  include Printable.S
  val dummy: t
end

module UnsafeFake (Base: PD) =
struct
  include Fake (Base)
  let join x y = x
  let meet x y = x
  let top () = Base.dummy
  let bot () = Base.dummy
  let is_top _ = false 
  let is_bot _ = false
end

module Reverse (Base: S) =
struct
  include Base
  include StdCousot (* this isn't good *)  
  let bot = Base.top
  let top = Base.bot
  let leq x y = Base.leq y x
  let join = Base.meet
  let meet = Base.join
end

module Flat (Base: Printable.S) (N: Printable.LiftingNames) = 
struct 
  include Printable.Lift (Base) (N)
  include StdCousot

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

  let meet x y = 
    match (x,y) with 
      | (`Bot, _) -> `Bot
      | (_, `Bot) -> `Bot
      | (`Top, x) -> x
      | (x, `Top) -> x
      | (`Lifted x, `Lifted y) when Base.equal x y -> `Lifted x
      | _ -> `Bot
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
    if leq x y then Pretty.text "No Changes" else
    Pretty.dprintf "%a instead of %a" pretty x pretty y

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
    if leq x y then Pretty.text "No Changes" else
    Pretty.dprintf "%a instead of %a" pretty x pretty y

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
end
                               
  
module Liszt (Base: S) = 
struct
  include Printable.Liszt (Base)
  include StdCousot
  let bot () = raise (Unsupported "bot?") 
  let is_top _ = false
  let top () = raise (Unsupported "top?")
  let is_bot _ = false

  let rec leq = 
    let f acc x y = Base.leq x y && acc in
      List.fold_left2 f 

  let join = List.map2 Base.join
  let meet = List.map2 Base.meet
end

module Chain (P: Printable.ChainParams) = 
struct
  include Printable.Std
  include Printable.Chain (P)
  include StdCousot
  let bot () = 0
  let is_bot x = x = 0
  let top () = P.n - 1
  let is_top x = x = P.n - 1

  let leq x y = x <= y
  let join x y = max x y
  let meet x y = min x y
end
