(** Some things are not quite lattices ... *)

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
  val pretty_diff: unit -> (t * t) -> Pretty.doc
  val toXML : t -> Xml.xml
  (* These two lets us reuse the short function, and allows some overriding
   * possibilities. *)
  val pretty_f: (int -> t -> string) -> unit -> t -> doc
  val toXML_f : (int -> t -> string) -> t -> Xml.xml
  (* This is for debugging *)
  val name: unit -> string
end

module Std =
struct 
(*  let equal = Util.equals
  let hash = Hashtbl.hash*)
  let compare = Pervasives.compare
  let classify _ = 0
  let class_name _ = "None"
  let name () = "std"
  let trace_enabled = false
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
  let pretty_diff () (x,y) = dprintf "Unsupported"
end

module PrintSimple (P: sig 
                      type t'
                      val short: int -> t' -> string 
                      val name: unit -> string
                    end) =
struct
  let isSimple _ = true
  let pretty_f sf () x = text (sf max_int x)
  let toXML_f sf st =
    let esc = Goblintutil.escape in
    let l = Goblintutil.summary_length in
    let summary = esc (sf l st) in
      Xml.Element ("Leaf", ["text", summary], [])
  let pretty () x = pretty_f P.short () x
  let toXML m = toXML_f P.short m
  let pretty_diff () (x,y) = 
    dprintf "%s: %a not leq %a" (P.name ()) pretty x pretty y
end


module type Name = sig val name: string end
module UnitConf (N: Name) = 
struct
  type t = unit
  include Std
  let hash () = 7134679
  let equal _ _ = true
  let pretty () _ = text N.name
  let short _ _ = N.name
  let toXML x = Xml.Element ("Leaf", [("text", N.name)], [])
  let isSimple _ = true
  let pretty_f _ = pretty
  let toXML_f _ = toXML
  let name () = "Unit"
  let pretty_diff () (x,y) = 
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end
module Unit = UnitConf (struct let name = "()" end)

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

(* HAS SIDE-EFFECTS ---- PLEASE INSTANCIATE ONLY ONCE!!! *)
module HConsed (Base:S) =
struct
  module HC = BatHashcons.MakeTable (Base)
  let htable = HC.create 100000
   
  type t = Base.t BatHashcons.hobj
  let unlift x = x.BatHashcons.obj
  let lift = HC.hashcons htable
  let lift_f f (x:Base.t BatHashcons.hobj) = f (x.BatHashcons.obj)
  let name () = "HConsed "^Base.name ()
  let hash x = x.BatHashcons.hcode
  let equal x y = x.BatHashcons.tag = y.BatHashcons.tag
  let compare x y =  Pervasives.compare x.BatHashcons.tag y.BatHashcons.tag
  let short w = lift_f (Base.short w) 
  let pretty_f sf () = lift_f (Base.pretty_f (fun w x -> sf w (lift x)) ())
  let pretty = pretty_f short
  let toXML_f sf = lift_f (Base.toXML_f (fun w x -> sf w (lift x)))
  let toXML = toXML_f short 
  let isSimple = lift_f Base.isSimple  
  let pretty_diff () (x,y) = Base.pretty_diff () (x.BatHashcons.obj,y.BatHashcons.obj)
end

module Lift (Base: S) (N: LiftingNames) =
struct
  type t = [`Bot | `Lifted of Base.t | `Top]
  include Std
  include N

  let hash = function 
    | `Top -> 4627833
    | `Bot -> -30385673
    | `Lifted x -> Base.hash x * 13

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
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module Either (Base1: S) (Base2: S) =
struct
  type t = [`Left of Base1.t | `Right of Base2.t]
  include Std

  let hash state = 
    match state with
      | `Left n ->  Base1.hash n
      | `Right n ->  133 * Base2.hash n

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
  let name () = "either " ^ Base1.name () ^ " or " ^ Base2.name ()
  let pretty_diff () (x,y) = 
    match (x,y) with
      | `Left x, `Left y ->  Base1.pretty_diff () (x,y)
      | `Right x, `Right y ->  Base2.pretty_diff () (x,y)
      | _ -> Pretty.dprintf "%a not leq %a" pretty x pretty y
end

module Option (Base: S) (N: Name) = Either (Base) (UnitConf (N))

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

  let hash state = 
    match state with
      | `Lifted1 n -> Base1.hash n
      | `Lifted2 n -> 77 * Base2.hash n
      | `Bot -> 13432255
      | `Top -> -33434577

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
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
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
  
  let hash (x,y) = Base1.hash x + Base2.hash y * 17
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
  let pretty_diff () ((x1,x2:t),(y1,y2:t)): Pretty.doc = 
    if Base1.equal x1 y1 then
      Base2.pretty_diff () (x2,y2)
    else 
      Base1.pretty_diff () (x1,y1)
end

module Prod = ProdConf (struct let expand_fst = true let expand_snd = true end)
module ProdSimple = ProdConf (struct let expand_fst = false let expand_snd = false end)

module Prod3 (Base1: S) (Base2: S) (Base3: S) =
struct 
  type t = Base1.t * Base2.t * Base3.t
  include Std
  let hash (x,y,z) = Base1.hash x + Base2.hash y * 17 + Base3.hash z * 33
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
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module Liszt (Base: S) =
struct
  type t = Base.t list
  include Std
  let equal x y = try List.for_all2 Base.equal x y with Invalid_argument _ -> false
  let hash = List.fold_left (fun xs x -> xs + Base.hash x) 996699
  
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
  let pretty_diff () ((x:t),(y:t)): Pretty.doc = 
    Pretty.dprintf "%a not leq %a" pretty x pretty y
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
  let hash x = x-5284
  let equal (x:int) (y:int) = x=y

  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let pretty_diff () ((x:t),(y:t)): Pretty.doc = 
    Pretty.dprintf "%a not leq %a" pretty x pretty y
end

module LiftBot (Base : S) =
struct
  type t = [`Bot | `Lifted of Base.t ]
  include Std
    
  let lift x = `Lifted x
    
  let equal x y = 
    match (x, y) with
      | (`Bot, `Bot) -> true
      | (`Lifted x, `Lifted y) -> Base.equal x y
      | _ -> false

  let hash = function 
    | `Bot -> 56613454
    | `Lifted n -> Base.hash n
      
  let short w state = 
    match state with
      | `Lifted n ->  Base.short w n
      | `Bot -> "bot of " ^ (Base.name ())

  let isSimple x = 
    match x with
      | `Lifted n -> Base.isSimple n
      | _ -> true

  let pretty_f _ () (state:t) = 
    match state with
      | `Lifted n ->  Base.pretty () n
      | `Bot -> text ("bot of " ^ (Base.name ()))

  let toXML_f _ (state:t) =
    match state with
      | `Lifted n -> Base.toXML n
      | `Bot -> Xml.Element ("Leaf", ["text","bot of " ^ (Base.name ())], [])

  let toXML m = toXML_f short m

  let pretty () x = pretty_f short () x
  let name () = "bottom or " ^ Base.name ()
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module LiftTop (Base : S) =
struct
  type t = [`Top | `Lifted of Base.t ]
  include Std
    
  let lift x = `Lifted x
    
  let equal x y = 
    match (x, y) with
      | (`Top, `Top) -> true
      | (`Lifted x, `Lifted y) -> Base.equal x y
      | _ -> false

  let hash = function 
    | `Top -> 7890
    | `Lifted n -> Base.hash n

  let short w state = 
    match state with
      | `Lifted n ->  Base.short w n
      | `Top -> "top of " ^ (Base.name ())

  let isSimple x = 
    match x with
      | `Lifted n -> Base.isSimple n
      | _ -> true

  let pretty_f _ () (state:t) = 
    match state with
      | `Lifted n ->  Base.pretty () n
      | `Top -> text ("top of " ^ (Base.name ()))

  let toXML_f _ (state:t) =
    match state with
      | `Lifted n -> Base.toXML n
      | `Top -> Xml.Element ("Leaf", ["text","top of " ^ (Base.name ())], [])

  let toXML m = toXML_f short m

  let pretty () x = pretty_f short () x
  let name () = "top or " ^ Base.name ()
  let pretty_diff () (x,y) = 
    match (x,y) with
      | `Lifted x, `Lifted y -> Base.pretty_diff () (x,y)
      | _ -> dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end


module Strings = 
struct
  type t = string
  include Std
  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) (y:t) = x=y
  let pretty () n = text n
  let short _ n = n
  let toXML x = Xml.Element ("Leaf", [("text", x)], [])
  let isSimple _ = true
  let pretty_f _ = pretty
  let toXML_f _ = toXML
  let name () = "String"
  let pretty_diff () (x,y) = 
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
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
