(** Some things are not quite lattices ... *)

open Pretty

type json = Yojson.Safe.json
let json_to_yojson x = x

module type S =
sig
  val name: string (* for debugging *)
  type t
  (* generated *)
  val show : t -> string
  val to_yojson : t -> json
  (* val to_xml : t -> xml *)
  (* val t_to_protobuf : Protobuf.Encoder.t -> t -> unit *)
  (* val diff : t -> t -> t *)

  (* hand-written (for now) fast xml output *)
  val printXml : 'a BatInnerIO.output -> t -> unit

  val equal: t -> t -> bool
  val hash: t -> int
  val compare: t -> t -> int
end

module Std =
struct
  (*  let equal = Util.equals
      let hash = Hashtbl.hash*)
  let compare = Pervasives.compare
  let classify _ = 0
  let class_name _ = "None"
  let name = "std"
  let trace_enabled = false
end

module Blank =
struct
  include Std
  let show = "Output not supported"
  let name = "blank"
  let diff x y = x
  let printXml f _ = BatPrintf.fprintf f "<value>\n<data>\nOutput not supported!\n</data>\n</value>\n"
end

module PrintSimple (P: sig
    type t'
    val short: t' -> string
    val name: string
  end) =
struct
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (P.short x))
end

module type Name = sig val name: string end
module UnitConf (N: Name) =
struct
  type t = unit [@@deriving yojson]
  include Std
  let hash () = 7134679
  let equal _ _ = true
  let show _ = N.name
  let name = "Unit"
  let printXml f () = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape N.name)
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
  let name = "HConsed "^Base.name
  let hash x = x.BatHashcons.hcode
  let equal x y = x.BatHashcons.tag = y.BatHashcons.tag
  let compare x y =  Pervasives.compare x.BatHashcons.tag y.BatHashcons.tag
  let short w = lift_f Base.show
  let to_yojson = lift_f (Base.to_yojson)
  let printXml f x = Base.printXml f x.BatHashcons.obj
end

module Lift (Base: S) (N: LiftingNames) =
struct
  type t = [`Bot | `Lifted of Base.t | `Top] [@@deriving to_yojson]
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

  let show state =
    match state with
    | `Lifted n ->  Base.show n
    | `Bot -> bot_name
    | `Top -> top_name

  let name = "lifted " ^ Base.name
  let printXml f = function
    | `Bot      -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape N.top_name)
    | `Top      -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape N.top_name)
    | `Lifted x -> Base.printXml f x
end

module Either (Base1: S) (Base2: S) =
struct
  type t = [`Left of Base1.t | `Right of Base2.t] [@@deriving to_yojson]
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

  let show state =
    match state with
    | `Left n ->  Base1.show n
    | `Right n ->  Base2.show n
  let name = "either " ^ Base1.name ^ " or " ^ Base2.name
  let printXml f = function
    | `Left x  -> BatPrintf.fprintf f "<value><map>\n<key>\nLeft\n</key>\n%a</map>\n</value>\n" Base1.printXml x
    | `Right x -> BatPrintf.fprintf f "<value><map>\n<key>\nRight\n</key>\n%a</map>\n</value>\n" Base2.printXml x
end

module Option (Base: S) (N: Name) = Either (Base) (UnitConf (N))

module Lift2 (Base1: S) (Base2: S) (N: LiftingNames) =
struct
  type t = [`Bot | `Lifted1 of Base1.t | `Lifted2 of Base2.t | `Top] [@@deriving to_yojson]
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

  let show state =
    match state with
    | `Lifted1 n ->  Base1.show n
    | `Lifted2 n ->  Base2.show n
    | `Bot -> bot_name
    | `Top -> top_name

  let name = "lifted " ^ Base1.name ^ " and " ^ Base2.name
  let printXml f = function
    | `Bot       -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" N.top_name
    | `Top       -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" N.top_name
    | `Lifted1 x -> BatPrintf.fprintf f "<value>\n<map>\n<key>\nLifted1\n</key>\n%a</map>\n</value>\n" Base1.printXml x
    | `Lifted2 x -> BatPrintf.fprintf f "<value>\n<map>\n<key>\nLifted2\n</key>\n%a</map>\n</value>\n" Base2.printXml x
end

module type ProdConfiguration =
sig
  val expand_fst: bool
  val expand_snd: bool
end

module ProdConf (C: ProdConfiguration) (Base1: S) (Base2: S)=
struct
  include C

  type t = Base1.t * Base2.t [@@deriving to_yojson]

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

  let show (x,y) =
    let first  = ref "" in
    let second = ref "" in
    first  := Base1.show x;
    second := Base2.show y;
    "(" ^ !first ^ ", " ^ !second ^ ")"

  let name = Base1.name ^ " * " ^ Base2.name

  let printXml f (x,y) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape Base1.name) Base1.printXml x (Goblintutil.escape Base2.name) Base2.printXml y
end

module Prod = ProdConf (struct let expand_fst = true let expand_snd = true end)
module ProdSimple = ProdConf (struct let expand_fst = false let expand_snd = false end)

module Prod3 (Base1: S) (Base2: S) (Base3: S) =
struct
  type t = Base1.t * Base2.t * Base3.t [@@deriving to_yojson]
  include Std
  let hash (x,y,z) = Base1.hash x + Base2.hash y * 17 + Base3.hash z * 33
  let equal (x1,x2,x3) (y1,y2,y3) =
    Base1.equal x1 y1 && Base2.equal x2 y2 && Base3.equal x3 y3
  let short w (x,y,z) =
    let first = ref "" in
    let second= ref "" in
    let third = ref "" in
    first  := Base1.show x;
    second := Base2.show y;
    third  := Base3.show z;
    "(" ^ !first ^ ", " ^ !second ^ ", " ^ !third ^ ")"

  let printXml f (x,y,z) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape Base1.name) Base1.printXml x (Goblintutil.escape Base2.name) Base2.printXml y (Goblintutil.escape Base3.name) Base3.printXml z

  let name = Base1.name ^ " * " ^ Base2.name ^ " * " ^ Base3.name
end

module Liszt (Base: S) =
struct
  type t = Base.t list [@@deriving to_yojson]
  include Std
  let equal x y = try List.for_all2 Base.equal x y with Invalid_argument _ -> false
  let hash = List.fold_left (fun xs x -> xs + Base.hash x) 996699

  let show x =
    let elems = List.map Base.show x in
    "[" ^ (String.concat ", " elems) ^ "]"

  let name = Base.name ^ " list"
  let printXml f xs =
    let rec loop n = function
      | [] -> ()
      | x::xs ->
        BatPrintf.fprintf f "<key>%d</key>\n%a\n" n Base.printXml x;
        loop (n+1) xs
    in
    BatPrintf.fprintf f "<value>\n<map>\n";
    loop 0 xs;
    BatPrintf.fprintf f "</map>\n</value>\n"
end

module type ChainParams = sig
  val n: int
  val names: int -> string
end

module Chain (P: ChainParams): S with type t = int =
struct
  type t = int [@@deriving yojson]
  include Std

  let show x = P.names x
  let hash x = x-5284
  let equal (x:int) (y:int) = x=y

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%d\n</data>\n</value>\n" x
end

module LiftBot (Base : S) =
struct
  type t = [`Bot | `Lifted of Base.t ] [@@deriving to_yojson]
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

  let show state =
    match state with
    | `Lifted n ->  Base.show n
    | `Bot -> "bot of " ^ Base.name

  let name = "bottom or " ^ Base.name
  let printXml f = function
    | `Bot -> BatPrintf.fprintf f "<value>\n<data>\nbottom\n</data>\n</value>\n"
    | `Lifted n -> Base.printXml f n
end

module LiftTop (Base : S) =
struct
  type t = [`Top | `Lifted of Base.t ] [@@deriving to_yojson]
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

  let show state =
    match state with
    | `Lifted n ->  Base.show n
    | `Top -> "top of " ^ Base.name

  let name = "top or " ^ Base.name

  let printXml f = function
    | `Top -> BatPrintf.fprintf f "<value>\n<data>\ntop\n</data>\n</value>\n"
    | `Lifted n -> Base.printXml f n
end


module Strings =
struct
  type t = string [@@deriving to_yojson]
  include Std
  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) (y:t) = x=y
  let pretty () n = text n
  let show n = n
  let name = "String"
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" x
end

let show_list a b xs =
  a ^ String.concat ", " xs ^ b