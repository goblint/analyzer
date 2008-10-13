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

open Pretty

module type S = 
sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
  val compare: t -> t -> int
  val short: int -> t -> string
  val isSimple: t -> bool
  val pretty: unit -> t -> doc
  val toXML : t -> Xml.xml
  (* These two let's us reuse the short function, and allows some overriding
   * possibilities. *)
  val pretty_f: (int -> t -> string) -> unit -> t -> doc
  val toXML_f : (int -> t -> string) -> t -> Xml.xml
  (* This is for debugging *)
  val name: unit -> string
end

module Std =
struct 
  let equal = Util.equals
  let hash = Hashtbl.hash
  let compare = Pervasives.compare
  let classify _ = 0
  let class_name _ = "None"
  let name () = "std"
end

module Blank = 
struct
  include Std
  let pretty () _ = text "Output not supported"
  let short _ _ = "Output not supported"
  let toXML x = Xml.Element ("Leaf", [("text", "Output not supported")], [])
  let isSimple _ = true
  let pretty_f _ = pretty
  let toXML_f _ = toXML
  let name () = "blank"
end

module Unit = 
struct
  type t = unit
  include Std
  let pretty () _ = text "()"
  let short _ _ = "()"
  let toXML x = Xml.Element ("Leaf", [("text", "()")], [])
  let isSimple _ = true
  let pretty_f _ = pretty
  let toXML_f _ = toXML
  let name () = "blank"
end

module type LiftingNames =
sig
  val bot_name: string
  val top_name: string
end

module DefaultNames = 
struct
  let bot_name = "bot"
  let top_name = "top"
end

module Lift (Base: S) (N: LiftingNames) =
struct
  type t = [`Bot | `Lifted of Base.t | `Top]
  include Std
  include N

  let equal x y = 
    match (x, y) with
      | (`Top, `Top) -> true
      | (`Bot, `Bot) -> true
      | (`Lifted x, `Lifted y) -> Base.equal x y
      | _ -> false

  let short w state = 
    match state with
      | `Lifted n ->  Base.short w n
      | `Bot -> bot_name
      | `Top -> top_name

  let isSimple x = 
    match x with
      | `Lifted n -> Base.isSimple n
      | _ -> true

  let pretty_f _ () (state:t) = 
    match state with
      | `Lifted n ->  Base.pretty () n
      | `Bot -> text bot_name
      | `Top -> text top_name

  let toXML_f _ (state:t) =
    match state with
      | `Lifted n -> Base.toXML n
      | `Bot -> Xml.Element ("Leaf", ["text",bot_name], [])
      | `Top -> Xml.Element ("Leaf", ["text",top_name], [])

  let toXML m = toXML_f short m

  let pretty () x = pretty_f short () x
  let name () = "lifted " ^ Base.name ()
end

module Either (Base1: S) (Base2: S) =
struct
  type t = [`Left of Base1.t | `Right of Base2.t]
  include Std

  let equal x y = 
    match (x, y) with
      | (`Left x, `Left y) -> Base1.equal x y
      | (`Right x, `Right y) -> Base2.equal x y
      | _ -> false

  let pretty_f _ () (state:t) = 
    match state with
      | `Left n ->  Base1.pretty () n
      | `Right n ->  Base2.pretty () n

  let short w state = 
    match state with
      | `Left n ->  Base1.short w n
      | `Right n ->  Base2.short w n

  let isSimple x = 
    match x with
      | `Left n ->  Base1.isSimple n
      | `Right n ->  Base2.isSimple n

  let toXML_f _ (state:t) =
    match state with
      | `Left n -> Base1.toXML n
      | `Right n -> Base2.toXML n

  let toXML m = toXML_f short m

  let pretty () x = pretty_f short () x
  let name () = "either " ^ Base1.name () ^ " and " ^ Base2.name ()
end

module Lift2 (Base1: S) (Base2: S) (N: LiftingNames) =
struct
  type t = [`Bot | `Lifted1 of Base1.t | `Lifted2 of Base2.t | `Top]
  include Std
  include N

  let equal x y = 
    match (x, y) with
      | (`Top, `Top) -> true
      | (`Bot, `Bot) -> true
      | (`Lifted1 x, `Lifted1 y) -> Base1.equal x y
      | (`Lifted2 x, `Lifted2 y) -> Base2.equal x y
      | _ -> false

  let pretty_f _ () (state:t) = 
    match state with
      | `Lifted1 n ->  Base1.pretty () n
      | `Lifted2 n ->  Base2.pretty () n
      | `Bot -> text bot_name
      | `Top -> text top_name

  let short w state = 
    match state with
      | `Lifted1 n ->  Base1.short w n
      | `Lifted2 n ->  Base2.short w n
      | `Bot -> bot_name
      | `Top -> top_name

  let isSimple x = 
    match x with
      | `Lifted1 n ->  Base1.isSimple n
      | `Lifted2 n ->  Base2.isSimple n
      | _ -> true

  let toXML_f _ (state:t) =
    match state with
      | `Lifted1 n -> Base1.toXML n
      | `Lifted2 n -> Base2.toXML n
      | `Bot -> Xml.Element ("Leaf", ["text",bot_name], [])
      | `Top -> Xml.Element ("Leaf", ["text",top_name], [])

  let toXML m = toXML_f short m

  let pretty () x = pretty_f short () x
  let name () = "lifted " ^ Base1.name () ^ " and " ^ Base2.name ()
end

module type ProdConfiguration =
sig
  val expand_fst: bool
  val expand_snd: bool 
end

module ProdConf (C: ProdConfiguration) (Base1: S) (Base2: S)=
struct
  include C

  type t = Base1.t * Base2.t

  include Std
  
  let equal (x1,x2) (y1,y2) = Base1.equal x1 y1 && Base2.equal x2 y2

  let compare (x1,x2) (y1,y2) = 
    match Base1.compare x1 y1, Base2.compare x2 y2 with
      | (-1, _) -> -1
      | ( 1, _) ->  1
      | ( 0,-1) -> -1
      | ( 0, 1) ->  1
      | ( 0, 0) ->  0
      | _       -> failwith "is this possible?"

  let short w (x,y) = 
    let first  = ref "" in
    let second = ref "" in
      first  := Base1.short (w - 4 - 6 (* chars for 2.*) ) x;
      second := Base2.short (w - 4 - String.length !first) y;
    "(" ^ !first ^ ", " ^ !second ^ ")"

  let isSimple (x,y) = Base1.isSimple x && Base2.isSimple y

  let name () = Base1.name () ^ " * " ^ Base2.name ()

  let pretty_f sf () (x,y) = 
    if expand_fst || expand_snd then
      text "("
      ++ (if expand_fst then Base1.pretty () x else text (Base1.short 60 x))
      ++ text ", "
      ++ (if expand_snd then Base2.pretty () y else text (Base2.short 60 y))
      ++ text ")"
    else
      text (sf Goblintutil.summary_length (x,y))
  
  let pretty () x = pretty_f short () x

  let toXML_f sf ((x, y) as st) =
    let esc = Goblintutil.escape in
    let nodes = match expand_fst,expand_snd with
        | (true, false) -> [Base1.toXML x]
        | (false, true) -> [Base2.toXML y]
        | (true, true) -> [Base1.toXML x; Base2.toXML y]
        | _ -> []
    in
    let node_leaf = if nodes = [] then "Leaf" else "Node" in
      Xml.Element (node_leaf, [("text", esc (sf Goblintutil.summary_length st))], nodes)

  let toXML m = toXML_f short m
end

module Prod = ProdConf (struct let expand_fst = true let expand_snd = true end)
module ProdSimple = ProdConf (struct let expand_fst = false let expand_snd = false end)

module Prod3 (Base1: S) (Base2: S) (Base3: S) =
struct 
  type t = Base1.t * Base2.t * Base3.t
  include Std
  let equal (x1,x2,x3) (y1,y2,y3) = 
    Base1.equal x1 y1 && Base2.equal x2 y2 && Base3.equal x3 y3
  let short w (x,y,z) = 
    let first = ref "" in
    let second= ref "" in
    let third = ref "" in
      first  := Base1.short (w-6- 12 (* chars for 2.&3.*) ) x;
      second := Base2.short (w-6- 6 - String.length !first) y;
      third  := Base3.short (w-6- String.length !first - String.length !second) z;
      "(" ^ !first ^ ", " ^ !second ^ ", " ^ !third ^ ")"

  let pretty_f _ () (x,y,z) = 
    text "(" ++ 
      Base1.pretty () x 
    ++ text ", " ++ 
      Base2.pretty () y 
    ++ text ", " ++ 
      Base3.pretty () z 
    ++ text ")"

  let isSimple (x,y,z) = Base1.isSimple x && Base2.isSimple y && Base3.isSimple z
  let toXML_f sf ((x,y,z) as st) =
    let esc = Goblintutil.escape in
      Xml.Element ("Node", [("text", esc (sf Goblintutil.summary_length st))],
                   [Base1.toXML x; Base2.toXML y; Base3.toXML z])

  let toXML m = toXML_f short m

  let pretty () x = pretty_f short () x
  let name () = Base1.name () ^ " * " ^ Base2.name () ^ " * " ^ Base3.name ()
end

module Liszt (Base: S) =
struct
  type t = Base.t list
  include Std
  let equal x y = try List.for_all2 Base.equal x y with Invalid_argument _ -> false

  let short _ x = 
    let elems = List.map (Base.short max_int) x in
      "[" ^ (String.concat ", " elems) ^ "]"

  let pretty_f sf () x = text (sf max_int x)
  let isSimple _ = true

  let toXML_f sf x = 
    let esc = Goblintutil.escape in
      match x with
        | (y::_) when not (Base.isSimple y) ->
            let elems = List.map Base.toXML x in
              Xml.Element ("Node", [("text", esc (sf max_int x))], elems)
        | _ -> Xml.Element ("Leaf", [("text", esc (sf max_int x))], [])

  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = Base.name () ^ " list"
end

module type ChainParams = sig
  val n: int
  val names: int -> string
end

module Chain (P: ChainParams): S with type t = int = 
struct
  type t = int
  include Std

  let short _ x = P.names x
  let pretty_f f () x = text (f max_int x)
  let toXML_f f x = Xml.Element ("Leaf", ["text",f 80 x], [])
  let isSimple _ = true

  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
end

(** Concatenates a list of strings that 
   fit in the given character constraint *)
let get_short_list begin_str end_str w list =
    let continues = "..." in
    (* Maximal space for short description *)
    let usable_length = 
      w-String.length continues 
       -String.length begin_str 
       -String.length end_str in
    (* Structure elements separator *)
    let separator = ", " in
    let separator_length = String.length separator in 
    (* List of elements, that are in our character boundaries*)
    let str_list_w_size = List.map (fun a -> (a,String.length a)) list in
    let to_length_pair alst (b,bb) = 
      match alst with
	  []         -> [b,bb]
	| (a,aa)::tl -> (b,aa+bb+separator_length)::(a,aa)::tl in
    let str_list_sum_size_rev = List.fold_left to_length_pair [] str_list_w_size in
   
    let cut_str_pair_list_rev = 
      List.filter (fun (a,s) -> s<=usable_length) str_list_sum_size_rev in

    let cut_str_list_rev = List.map fst cut_str_pair_list_rev in

    let cut_str_list = 
      if ((List.length cut_str_list_rev) < (List.length list)) then
	 List.rev (continues::cut_str_list_rev) 
      else
	 List.rev cut_str_list_rev in  	

    let str = String.concat separator cut_str_list in
      begin_str ^ str ^ end_str
