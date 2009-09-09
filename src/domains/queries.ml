(* 
 * Copyright (c) 2005-2007,
 *     * University of Tartu
 *     * Vesal Vojdani <vesal.vojdani@gmail.com>
 *     * Kalmer Apinis <kalmera@ut.ee>
 *     * Jaak Randmets <jaak.ra@gmail.com>
 *     * Toomas Römer <toomasr@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
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
 
include Cil
include Pretty 

module ID = IntDomain.Integers

type t = ExpEq of Cil.exp * Cil.exp
       | TheAnswerToLifeUniverseAndEverything

module Result: Lattice.S with type t = [
    | `Top
    | `Int of ID.t
    | `Bot
    ] = 
struct 
  type t = [
    | `Top
    | `Int of ID.t
    | `Bot
    ]

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
      | _ -> false

  let hash (x:t) =
    match x with
      | `Int n -> ID.hash n
      | _ -> Hashtbl.hash x

  let compare x y = 
    let constr_to_int x = match x with
        | `Bot -> 0
        | `Int _ -> 1
        | `Address _ -> 3
        | `Struct _ -> 5
        | `Union _ -> 6
        | `Array _ -> 7
        | `Blob _ -> 9
        | `Top -> 100
    in match x,y with
      | `Int x, `Int y -> ID.compare x y
      | _ -> Pervasives.compare (constr_to_int x) (constr_to_int y)

  let pretty_f _ () state = 
    match state with
      | `Int n ->  ID.pretty () n
      | `Bot -> text bot_name
      | `Top -> text top_name

  let short w state = 
    match state with
      | `Int n ->  ID.short w n
      | `Bot -> bot_name
      | `Top -> top_name

  let rec isSimple x = 
    match x with
      | `Int n ->  ID.isSimple n
      | _ -> true

  let toXML_f _ state =
    match state with
      | `Int n -> ID.toXML n
      | `Bot -> Xml.Element ("Leaf", ["text",bot_name], [])
      | `Top -> Xml.Element ("Leaf", ["text",top_name], [])

  let pretty () x = pretty_f short () x
  let toXML s = toXML_f short s

  let leq x y =
    match (x,y) with
      | (_, `Top) -> true
      | (`Top, _) -> false
      | (`Bot, _) -> true
      | (_, `Bot) -> false
      | (`Int x, `Int y) -> ID.leq x y
      | _ -> false

  let join x y = 
    match (x,y) with 
      | (`Top, _) -> `Top
      | (_, `Top) -> `Top
      | (`Bot, x) -> x
      | (x, `Bot) -> x
      | (`Int x, `Int y) -> `Int (ID.join x y)
      | _ -> `Top

  let meet x y = 
    match (x,y) with 
      | (`Bot, _) -> `Bot
      | (_, `Bot) -> `Bot
      | (`Top, x) -> x
      | (x, `Top) -> x
      | (`Int x, `Int y) -> `Int (ID.meet x y)
      | _ -> `Bot

  let widen x y =
    match (x,y) with 
      | (`Top, _) -> `Top
      | (_, `Top) -> `Top
      | (`Bot, x) -> x
      | (x, `Bot) -> x
      | (`Int x, `Int y) -> `Int (ID.widen x y)
      | _ -> `Top    

  let narrow x y =
    match (x,y) with 
      | (`Int x, `Int y) -> `Int (ID.narrow x y)
      | (x,_) -> x
end