open Ppxlib
open Ppx_easy_deriving

module LeqArg: Product.Reduce.Conjunctive.S =
struct
  let name = "leq"
end

module LeqDeriver = Deriver.Make (Product.Reduce2.Make (Product.Reduce.Conjunctive.Make (LeqArg)))
let leq_deriving = LeqDeriver.register ()


module JoinArg: Product.Map2.S =
struct
  let name = "join"
end

module JoinDeriver = Deriver.Make (Product.Map2.Make (JoinArg))
let join_deriving = JoinDeriver.register ()


module MeetArg: Product.Map2.S =
struct
  let name = "meet"
end

module MeetDeriver = Deriver.Make (Product.Map2.Make (MeetArg))
let meet_deriving = MeetDeriver.register ()


module WidenArg: Product.Map2.S =
struct
  let name = "widen"
end

module WidenDeriver = Deriver.Make (Product.Map2.Make (WidenArg))
let widen_deriving = WidenDeriver.register ()


module NarrowArg: Product.Map2.S =
struct
  let name = "narrow"
end

module NarrowDeriver = Deriver.Make (Product.Map2.Make (NarrowArg))
let narrow_deriving = NarrowDeriver.register ()


module BotArg: Product.Create.S =
struct
  let name = "bot"
  let typ ~loc _ = [%type: unit]
end

module BotDeriver = Deriver.Make (Product.Create.Make (BotArg))
let bot_deriving = BotDeriver.register ()


module IsBotArg: Product.Reduce.Conjunctive.S =
struct
  let name = "is_bot"
end

module IsBotDeriver = Deriver.Make (Product.Reduce1.Make (Product.Reduce.Conjunctive.Make (IsBotArg)))
let is_bot_deriving = IsBotDeriver.register ()


module TopArg: Product.Create.S =
struct
  let name = "top"
  let typ ~loc _ = [%type: unit]
end

module TopDeriver = Deriver.Make (Product.Create.Make (TopArg))
let top_deriving = TopDeriver.register ()


module IsTopArg: Product.Reduce.Conjunctive.S =
struct
  let name = "is_top"
end

module IsTopDeriver = Deriver.Make (Product.Reduce1.Make (Product.Reduce.Conjunctive.Make (IsTopArg)))
let is_top_deriving = IsTopDeriver.register ()


let _ = Ppxlib.Deriving.add_alias "lattice" [
    leq_deriving;
    join_deriving;
    meet_deriving;
    widen_deriving;
    narrow_deriving;
    bot_deriving;
    is_bot_deriving;
    top_deriving;
    is_top_deriving;
  ]
