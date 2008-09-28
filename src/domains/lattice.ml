(* * Copyright (c) 2005-2007, *     * University of Tartu *     * Vesal Vojdani <vesal.vojdani@gmail.com> *     * Kalmer Apinis <kalmera@ut.ee> *     * Jaak Randmets <jaak.ra@gmail.com> *     * Toomas Römer <toomasr@gmail.com> * All rights reserved.  * * Redistribution and use in source and binary forms, with or without modification, * are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 * 
 *     * Neither the name of the University of Tartu nor the names of its
 *       contributors may be used to endorse or promote products derived from
 *       this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

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

  let leq x y =
    match (x,y) with
      | (_, `Top) -> true
      | (`Top, _) -> false
      | (`Bot, _) -> true
      | (_, `Bot) -> false
      | (`Lifted x, `Lifted y) -> Base.equal x y

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
  include StdCousot

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
end

module Lift2 (Base1: S) (Base2: S) (N: Printable.LiftingNames) = 
struct 
  include Printable.Lift2 (Base1) (Base2) (N)
  include StdCousot

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
end

module ProdConf (C: Printable.ProdConfiguration) (Base1: S) (Base2: S) =
struct
  include Printable.ProdConf (C) (Base1) (Base2)
  include StdCousot

  let bot () = (Base1.bot (), Base2.bot ())
  let is_bot (x1,x2) = Base1.is_bot x1 && Base2.is_bot x2
  let top () = (Base1.top (), Base2.top ())
  let is_top (x1,x2) = Base1.is_top x1 && Base2.is_top x2

  let leq (x1,x2) (y1,y2) = Base1.leq x1 y1 && Base2.leq x2 y2

  let op_scheme op1 op2 (x1,x2) (y1,y2): t = (op1 x1 y1, op2 x2 y2)
  let join = op_scheme Base1.join Base2.join
  let meet = op_scheme Base1.meet Base2.meet
end


module Prod = ProdConf (struct
                          let expand_fst = true
                          let expand_snd = true
                        end)

module Prod3 (Base1: S) (Base2: S) (Base3: S) =
struct
  include Printable.Prod3 (Base1) (Base2) (Base3)
  include StdCousot

  let bot () = (Base1.bot (), Base2.bot (), Base3.bot ())
  let is_bot (x1,x2,x3) = Base1.is_bot x1 && Base2.is_bot x2 && Base3.is_bot x3
  let top () = (Base1.top (), Base2.top (), Base3.top ())
  let is_top (x1,x2,x3) = Base1.is_top x1 && Base2.is_top x2 && Base3.is_top x3

  let leq (x1,x2,x3) (y1,y2,y3) = Base1.leq x1 y1 && Base2.leq x2 y2 && Base3.leq x3 y3

  let op_scheme op1 op2 op3 (x1,x2,x3) (y1,y2,y3): t = (op1 x1 y1, op2 x2 y2, op3 x3 y3)
  let join = op_scheme Base1.join Base2.join Base3.join
  let meet = op_scheme Base1.meet Base2.meet Base3.meet
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
