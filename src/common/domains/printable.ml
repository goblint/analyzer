(** Signature for comparable and outputtable elements.
    Functors for common printables. *)

module Pretty = GoblintCil.Pretty
open Pretty

module type S =
sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
  val compare: t -> t -> int
  val show: t -> string
  val pretty: unit -> t -> doc
  (* These two lets us reuse the short function, and allows some overriding
   * possibilities. *)
  val printXml : 'a BatInnerIO.output -> t -> unit
  (* This is for debugging *)
  val name: unit -> string
  val to_yojson : t -> Yojson.Safe.t

  val tag: t -> int (** Unique ID, given by HConsed, for context identification in witness *)

  val arbitrary: unit -> t QCheck.arbitrary

  (* For hashconsing together with incremental we need to re-hashcons old values.
   * For HashconsLifter.D this is done on any lattice operation, so we can replace x with `join bot x` to hashcons it again and get a new tag for it.
   * For HashconsLifter.C we call hashcons only in `context` which is in Analyses.Spec but not in Analyses.GlobConstrSys, i.e. not visible to the solver. *)
  (* The default for functors should pass the call to their argument modules, except for HConsed below where we want to have the side-effect and return a value with the updated tag. *)
  val relift: t -> t
end

module Empty: S =
struct
  type t = | [@@deriving eq, ord, hash]
  let show (x: t) = match x with _ -> .
  let pretty () (x: t) = match x with _ -> .
  let printXml _ (x: t) = match x with _ -> .
  let name () = "empty"
  let to_yojson (x: t) = match x with _ -> .
  let tag (x: t) = match x with _ -> .
  let arbitrary () = failwith "Printable.Empty.arbitrary"
  let relift (x: t) = match x with _ -> .
end

(** Default dummy definitions.

    Include as the first thing to avoid these overriding actual definitions. *)
module Std =
struct
  let tag _ = failwith "Std: no tag"
  let arbitrary () = failwith "no arbitrary"
end

(** Default dummy definitions for leaf types: primitive and CIL types,
    which don't contain inner types that require relifting. *)
module StdLeaf =
struct
  include Std

  let relift x = x
end


module type Showable =
sig
  type t
  val show: t -> string
end

module SimpleShow (P: Showable) =
struct
  let pretty () x = text (P.show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (P.show x))
  let to_yojson x = `String (P.show x)
end

module type Prettyable =
sig
  type t
  val pretty: unit -> t -> doc
end

module SimplePretty (P: Prettyable) =
struct
  let show x = GobPretty.sprint P.pretty x
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end

module type Formatable =
sig
  type t
  val pp: Format.formatter -> t -> unit
end

module SimpleFormat (P: Formatable) =
struct
  let show x = GobFormat.asprint P.pp x
  let pretty () x = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end


module type Name = sig val name: string end
module UnitConf (N: Name) =
struct
  type t = unit [@@deriving eq, ord, hash]
  include StdLeaf
  let pretty () _ = text N.name
  let show _ = N.name
  let name () = "Unit"
  let printXml f () = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape N.name)
  let to_yojson () = `String N.name
  let arbitrary () = QCheck.unit
end
module Unit = UnitConf (struct let name = "()" end)

(* HAS SIDE-EFFECTS ---- PLEASE INSTANCIATE ONLY ONCE!!! *)
module HConsed (Base:S) =
struct
  include Std (* for default invariant, tag, ... *)

  module HC = BatHashcons.MakeTable (Base)
  let htable = HC.create 100000

  type t = Base.t BatHashcons.hobj
  let unlift x = x.BatHashcons.obj
  let lift = HC.hashcons htable
  let lift_f f (x:Base.t BatHashcons.hobj) = f (x.BatHashcons.obj)

  let show = lift_f Base.show
  let pretty () = lift_f (Base.pretty ())

  (* Debug printing with tags *)
  (* let pretty () x = Pretty.dprintf "%a[%d,%d]" Base.pretty x.BatHashcons.obj x.BatHashcons.tag x.BatHashcons.hcode
     let show x = (Base.show x.BatHashcons.obj) ^ "[" ^ string_of_int x.BatHashcons.tag ^ "," ^ string_of_int x.BatHashcons.hcode ^ "]" *)

  let relift x = let y = Base.relift x.BatHashcons.obj in HC.hashcons htable y
  let name () = "HConsed "^Base.name ()
  let hash x = x.BatHashcons.hcode
  let tag x = x.BatHashcons.tag
  let compare x y =  Stdlib.compare x.BatHashcons.tag y.BatHashcons.tag
  let to_yojson = lift_f (Base.to_yojson)
  let printXml f x = Base.printXml f x.BatHashcons.obj

  let equal_debug x y = (* This debug version checks if we call hashcons enough to have up-to-date tags. Comment out the equal below to use this. This will be even slower than with hashcons disabled! *)
    if x.BatHashcons.tag = y.BatHashcons.tag then ( (* x.BatHashcons.obj == y.BatHashcons.obj || *)
      if not (Base.equal x.BatHashcons.obj y.BatHashcons.obj) then
        Logs.error "tags are equal but values are not for %a and %a" pretty x pretty y;
      assert (Base.equal x.BatHashcons.obj y.BatHashcons.obj);
      true
    ) else (
      if Base.equal x.BatHashcons.obj y.BatHashcons.obj then
        Logs.error "tags are not equal but values are for %a and %a" pretty x pretty y;
      assert (not (Base.equal x.BatHashcons.obj y.BatHashcons.obj));
      false
    )
  let equal x y = x.BatHashcons.tag = y.BatHashcons.tag
  (* let equal = equal_debug *)
  let arbitrary () = QCheck.map ~rev:unlift lift (Base.arbitrary ())
end

module HashCached (Base: S) =
struct
  module LazyHash = LazyEval.Make (struct type t = Base.t type result = int let eval = Base.hash end)

  let name () = "HashCached " ^ Base.name ()

  type t =
    {
      m: Base.t;
      lazy_hash: LazyHash.t;
    }

  let lift m = {m; lazy_hash = LazyHash.make m}
  let unlift {m; _} = m
  let relift x = lift @@ Base.relift x.m

  let lift_f f x = f (unlift x)
  let lift_f' f x = lift @@ lift_f f x
  let lift_f2 f x y = f (unlift x) (unlift y)
  let lift_f2' f x y = lift @@ lift_f2 f x y

  let equal = lift_f2 Base.equal
  let compare = lift_f2 Base.compare
  let hash x = LazyHash.force x.lazy_hash
  let show = lift_f Base.show

  let pretty () = lift_f (Base.pretty ())

  let printXml f = lift_f (Base.printXml f)
  let to_yojson = lift_f (Base.to_yojson)

  let arbitrary () = QCheck.map ~rev:unlift lift (Base.arbitrary ())

  let tag = lift_f Base.tag
end


module type PrefixNameConf =
sig
  val expand: bool
end

module PrefixName (Conf: PrefixNameConf) (Base: S): S with type t = Base.t =
struct
  include Base

  let pretty () x =
    if Conf.expand then
      Pretty.dprintf "%s:%a" (Base.name ()) Base.pretty x
    else
      Base.pretty () x

  let show x =
    if Conf.expand then
      Base.name () ^ ":" ^ Base.show x
    else
      Base.show x

  let printXml f x =
    if Conf.expand then
      BatPrintf.fprintf f "<value><map>\n<key>\n%s\n</key>\n%a</map>\n</value>\n" (Base.name ()) Base.printXml x
    else
      Base.printXml f x

  let to_yojson x =
    if Conf.expand then
      `Assoc [(Base.name (), Base.to_yojson x)]
    else
      Base.to_yojson x
end


module type LiftConf =
sig
  val bot_name: string
  val top_name: string
  val expand1: bool
end

module DefaultConf =
struct
  let bot_name = "bot"
  let top_name = "top"
  let expand1 = true
  let expand2 = true
  let expand3 = true
end

module LiftConf (Conf: LiftConf) (Base: S) =
struct
  open struct
    module Base = PrefixName (struct let expand = Conf.expand1 end) (Base)
  end

  type t = [`Bot | `Lifted of Base.t | `Top] [@@deriving eq, ord, hash]
  include Std
  open Conf

  let lift x = `Lifted x

  let show state =
    match state with
    | `Lifted n ->  Base.show n
    | `Bot -> bot_name
    | `Top -> top_name

  let pretty () (state:t) =
    match state with
    | `Lifted n ->  Base.pretty () n
    | `Bot -> text bot_name
    | `Top -> text top_name

  let name () = "lifted " ^ Base.name ()
  let printXml f = function
    | `Bot      -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape bot_name)
    | `Top      -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape top_name)
    | `Lifted x -> Base.printXml f x

  let to_yojson = function
    | `Bot -> `String bot_name
    | `Top -> `String top_name
    | `Lifted x -> Base.to_yojson x

  let relift x = match x with
    | `Bot |`Top -> x
    | `Lifted v -> `Lifted (Base.relift v)

  let arbitrary () =
    let open QCheck.Iter in
    let shrink = function
      | `Lifted x -> (return `Bot) <+> (GobQCheck.shrink (Base.arbitrary ()) x >|= lift)
      | `Bot -> empty
      | `Top -> GobQCheck.Iter.of_arbitrary ~n:20 (Base.arbitrary ()) >|= lift
    in
    QCheck.frequency ~shrink ~print:show [
      20, QCheck.map lift (Base.arbitrary ());
      1, QCheck.always `Bot;
      1, QCheck.always `Top
    ] (* S TODO: decide frequencies *)
end

module type EitherConf =
sig
  val expand1: bool
  val expand2: bool
end

module EitherConf (Conf: EitherConf) (Base1: S) (Base2: S) =
struct
  open struct
    module Base1 = PrefixName (struct let expand = Conf.expand1 end) (Base1)
    module Base2 = PrefixName (struct let expand = Conf.expand2 end) (Base2)
  end

  type t = [`Left of Base1.t | `Right of Base2.t] [@@deriving eq, ord, hash]
  include Std

  let pretty () (state:t) =
    match state with
    | `Left n -> Base1.pretty () n
    | `Right n -> Base2.pretty () n

  let show state =
    match state with
    | `Left n -> Base1.show n
    | `Right n -> Base2.show n

  let name () = "either " ^ Base1.name () ^ " or " ^ Base2.name ()
  let printXml f = function
    | `Left x -> Base1.printXml f x
    | `Right x -> Base2.printXml f x

  let to_yojson = function
    | `Left x -> Base1.to_yojson x
    | `Right x -> Base2.to_yojson x

  let relift = function
    | `Left x -> `Left (Base1.relift x)
    | `Right x -> `Right (Base2.relift x)
end

module Either = EitherConf (DefaultConf)

module type Either3Conf =
sig
  include EitherConf
  val expand3: bool
end

module Either3Conf (Conf: Either3Conf) (Base1: S) (Base2: S) (Base3: S) =
struct
  open struct
    module Base1 = PrefixName (struct let expand = Conf.expand1 end) (Base1)
    module Base2 = PrefixName (struct let expand = Conf.expand2 end) (Base2)
    module Base3 = PrefixName (struct let expand = Conf.expand3 end) (Base3)
  end

  type t = [`Left of Base1.t | `Middle of Base2.t | `Right of Base3.t] [@@deriving eq, ord, hash]
  include Std

  let pretty () (state:t) =
    match state with
    | `Left n -> Base1.pretty () n
    | `Middle n -> Base2.pretty () n
    | `Right n -> Base3.pretty () n

  let show state =
    match state with
    | `Left n -> Base1.show n
    | `Middle n -> Base2.show n
    | `Right n -> Base3.show n

  let name () = "either " ^ Base1.name () ^ " or " ^ Base2.name () ^ " or " ^ Base3.name ()
  let printXml f = function
    | `Left x  -> Base1.printXml f x
    | `Middle x  -> Base2.printXml f x
    | `Right x -> Base3.printXml f x

  let to_yojson = function
    | `Left x -> Base1.to_yojson x
    | `Middle x -> Base2.to_yojson x
    | `Right x -> Base3.to_yojson x

  let relift = function
    | `Left x -> `Left (Base1.relift x)
    | `Middle x -> `Middle (Base2.relift x)
    | `Right x -> `Right (Base3.relift x)
end

module Either3 = Either3Conf (DefaultConf)

module Option (Base: S) (N: Name) =
struct
  type t = Base.t option [@@deriving eq, ord, hash]
  include Std

  let pretty () (state:t) =
    match state with
    | None -> text N.name
    | Some n -> Base.pretty () n

  let show state =
    match state with
    | None -> N.name
    | Some n -> Base.show n

  let name () = Base.name () ^ " option"
  let printXml f = function
    | None -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape N.name)
    | Some x -> Base.printXml f x

  let to_yojson = function
    | None -> `String N.name
    | Some x -> Base.to_yojson x

  let relift = Option.map Base.relift
end

module type Lift2Conf =
sig
  include LiftConf
  val expand2: bool
end

module Lift2Conf (Conf: Lift2Conf) (Base1: S) (Base2: S) =
struct
  open struct
    module Base1 = PrefixName (struct let expand = Conf.expand1 end) (Base1)
    module Base2 = PrefixName (struct let expand = Conf.expand2 end) (Base2)
  end

  type t = [`Bot | `Lifted1 of Base1.t | `Lifted2 of Base2.t | `Top] [@@deriving eq, ord, hash]
  include Std
  open Conf

  let pretty () (state:t) =
    match state with
    | `Lifted1 n ->  Base1.pretty () n
    | `Lifted2 n ->  Base2.pretty () n
    | `Bot -> text bot_name
    | `Top -> text top_name

  let show state =
    match state with
    | `Lifted1 n ->  Base1.show n
    | `Lifted2 n ->  Base2.show n
    | `Bot -> bot_name
    | `Top -> top_name

  let relift x = match x with
    | `Lifted1 n -> `Lifted1 (Base1.relift n)
    | `Lifted2 n -> `Lifted2 (Base2.relift n)
    | `Bot | `Top -> x

  let name () = "lifted " ^ Base1.name () ^ " and " ^ Base2.name ()
  let printXml f = function
    | `Bot       -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" bot_name
    | `Top       -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" top_name
    | `Lifted1 x -> Base1.printXml f x
    | `Lifted2 x -> Base2.printXml f x

  let to_yojson = function
    | `Bot -> `String bot_name
    | `Top -> `String top_name
    | `Lifted1 x -> Base1.to_yojson x
    | `Lifted2 x -> Base2.to_yojson x
end

module type ProdConfiguration =
sig
  val expand_fst: bool
  val expand_snd: bool
end

module ProdConf (C: ProdConfiguration) (Base1: S) (Base2: S)=
struct
  include C

  type t = Base1.t * Base2.t [@@deriving eq, ord, hash, relift]

  include Std

  let show (x,y) =
    (* TODO: remove ref *)
    let first  = ref "" in
    let second = ref "" in
    first  := Base1.show x;
    second := Base2.show y;
    "(" ^ !first ^ ", " ^ !second ^ ")"

  let name () = Base1.name () ^ " * " ^ Base2.name ()

  let pretty () (x,y) =
    if expand_fst || expand_snd then
      text "("
      ++ text (Base1.name ())
      ++ text ":"
      ++ align
      ++ (if expand_fst then Base1.pretty () x else text (Base1.show x))
      ++ unalign
      ++ text ", "
      ++ text (Base2.name ())
      ++ text ":"
      ++ align
      ++ (if expand_snd then Base2.pretty () y else text (Base2.show y))
      ++ unalign
      ++ text ")"
    else
      text (show (x,y))

  let printXml f (x,y) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (XmlUtil.escape (Base1.name ())) Base1.printXml x (XmlUtil.escape (Base2.name ())) Base2.printXml y

  let to_yojson (x, y) =
    `Assoc [ (Base1.name (), Base1.to_yojson x); (Base2.name (), Base2.to_yojson y) ]

  let arbitrary () = QCheck.pair (Base1.arbitrary ()) (Base2.arbitrary ())
end

module Prod = ProdConf (struct let expand_fst = true let expand_snd = true end)
module ProdSimple = ProdConf (struct let expand_fst = false let expand_snd = false end)

module Prod3 (Base1: S) (Base2: S) (Base3: S) =
struct
  type t = Base1.t * Base2.t * Base3.t [@@deriving eq, ord, hash, relift]
  include Std

  let show (x,y,z) =
    (* TODO: remove ref *)
    let first = ref "" in
    let second= ref "" in
    let third = ref "" in
    first  := Base1.show x;
    second := Base2.show y;
    third  := Base3.show z;
    "(" ^ !first ^ ", " ^ !second ^ ", " ^ !third ^ ")"

  let pretty () (x,y,z) =
    text "("
    ++ text (Base1.name ())
    ++ text ":"
    ++ align
    ++ Base1.pretty () x
    ++ unalign
    ++ text ", "
    ++ text (Base2.name ())
    ++ text ":"
    ++ align
    ++ Base2.pretty () y
    ++ unalign
    ++ text ", "
    ++ text (Base3.name ())
    ++ text ":"
    ++ align
    ++ Base3.pretty () z
    ++ unalign
    ++ text ")"

  let printXml f (x,y,z) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (XmlUtil.escape (Base1.name ())) Base1.printXml x (XmlUtil.escape (Base2.name ())) Base2.printXml y (XmlUtil.escape (Base3.name ())) Base3.printXml z

  let to_yojson (x, y, z) =
    `Assoc [ (Base1.name (), Base1.to_yojson x); (Base2.name (), Base2.to_yojson y); (Base3.name (), Base3.to_yojson z) ]

  let name () = Base1.name () ^ " * " ^ Base2.name () ^ " * " ^ Base3.name ()

  let arbitrary () = QCheck.triple (Base1.arbitrary ()) (Base2.arbitrary ()) (Base3.arbitrary ())
end

module PQueue (Base: S) =
struct
  type t = Base.t BatDeque.dq
  include Std

  let show x = "[" ^ (BatDeque.fold_right (fun a acc -> Base.show a ^ ", " ^ acc) x "]")

  let pretty () x = text (show x)
  let name () = Base.name () ^ "queue"

  let relift x = BatDeque.map Base.relift x

  let printXml f xs =
    let rec loop n q =
      match BatDeque.front q with
      | None -> ()
      | Some (x, xs) -> (BatPrintf.fprintf f "<key>%d</key>\n%a\n" n Base.printXml x;
                         loop (n+1) (xs))
    in
    BatPrintf.fprintf f "<value>\n<map>\n";
    loop 0 xs;
    BatPrintf.fprintf f "</map>\n</value>\n"

  let to_yojson q = `List (BatDeque.to_list @@ BatDeque.map (Base.to_yojson) q)
  let hash q = BatDeque.fold_left (fun acc x -> (acc + 71) * (Base.hash x)) 11 q
  let equal q1 q2 = BatDeque.eq ~eq:Base.equal q1 q2
  let compare q1 q2 =
    match BatDeque.front q1, BatDeque.front q2 with
    | None, None -> 0
    | None, Some(_, _) -> -1
    | Some(_, _), None -> 1
    | Some(a1, q1'), Some(a2, q2') ->
      let c = Base.compare a1 a2 in
      if c <> 0 then c
      else compare q1' q2'
end

module Liszt (Base: S) =
struct
  type t = Base.t list [@@deriving eq, ord, hash, to_yojson]
  include Std

  let show x =
    let elems = List.map Base.show x in
    "[" ^ (String.concat ", " elems) ^ "]"

  let pretty () x = text (show x)

  let relift x = List.map Base.relift x

  let name () = Base.name () ^ " list"
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
  val n: unit -> int
  val names: int -> string
end

module Chain (P: ChainParams): S with type t = int =
struct
  type t = int [@@deriving eq, ord, hash]
  include StdLeaf
  let name () = "chain"

  let show x = P.names x
  let pretty () x = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (P.names x)
  let to_yojson x = `String (P.names x)

  let arbitrary () = QCheck.int_range 0 (P.n () - 1)
end

module LiftBot (Base : S) =
struct
  type t = [`Bot | `Lifted of Base.t ] [@@deriving eq, ord, hash]
  include Std

  let lift x = `Lifted x

  let show state =
    match state with
    | `Lifted n ->  Base.show n
    | `Bot -> "bot of " ^ (Base.name ())

  let pretty () (state:t) =
    match state with
    | `Lifted n ->  Base.pretty () n
    | `Bot -> text ("bot of " ^ (Base.name ()))

  let name () = "bottom or " ^ Base.name ()
  let printXml f = function
    | `Bot -> BatPrintf.fprintf f "<value>\n<data>\nbottom\n</data>\n</value>\n"
    | `Lifted n -> Base.printXml f n

  let to_yojson = function
    | `Bot -> `String "⊥"
    | `Lifted n -> Base.to_yojson n

  let relift = function
    | `Bot -> `Bot
    | `Lifted n -> `Lifted (Base.relift n)
end

module LiftTop (Base : S) =
struct
  type t = [`Top | `Lifted of Base.t ] [@@deriving eq, ord, hash]
  include Std

  let lift x = `Lifted x

  let show state =
    match state with
    | `Lifted n ->  Base.show n
    | `Top -> "top of " ^ (Base.name ())

  let pretty () (state:t) =
    match state with
    | `Lifted n ->  Base.pretty () n
    | `Top -> text ("top of " ^ (Base.name ()))

  let name () = "top or " ^ Base.name ()

  let printXml f = function
    | `Top -> BatPrintf.fprintf f "<value>\n<data>\ntop\n</data>\n</value>\n"
    | `Lifted n -> Base.printXml f n

  let to_yojson = function
    | `Top -> `String "⊤"
    | `Lifted n -> Base.to_yojson n

  let relift = function
    | `Top -> `Top
    | `Lifted n -> `Lifted (Base.relift n)

  let arbitrary () =
    let open QCheck.Iter in
    let shrink = function
      | `Lifted x -> GobQCheck.shrink (Base.arbitrary ()) x >|= lift
      | `Top -> GobQCheck.Iter.of_arbitrary ~n:20 (Base.arbitrary ()) >|= lift
    in
    QCheck.frequency ~shrink ~print:show [
      20, QCheck.map lift (Base.arbitrary ());
      1, QCheck.always `Top
    ] (* S TODO: decide frequencies *)
end


module Strings =
struct
  type t = string [@@deriving eq, ord, hash, to_yojson]
  include StdLeaf
  let pretty () n = text n
  let show n = n
  let name () = "String"
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" x
end


module type FailwithMessage =
sig
  val message: string
end

module Failwith (Message: FailwithMessage): S =
struct
  type t = |

  let name () = "failwith"
  let equal _ _ = failwith Message.message
  let compare _ _ = failwith Message.message
  let hash _ = failwith Message.message
  let tag _ = failwith Message.message

  let show _ = failwith Message.message
  let pretty _ _ = failwith Message.message
  let printXml _ _ = failwith Message.message
  let to_yojson _ = failwith Message.message

  let arbitrary _ = failwith Message.message
  let relift _ = failwith Message.message
end


(** Concatenates a list of strings that
    fit in the given character constraint *)
let get_short_list begin_str end_str list =
  let continues = "..." in
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

  let cut_str_pair_list_rev = str_list_sum_size_rev in

  let cut_str_list_rev = List.map fst cut_str_pair_list_rev in

  let cut_str_list =
    if List.compare_lengths cut_str_list_rev list < 0 then
      List.rev (continues::cut_str_list_rev)
    else
      List.rev cut_str_list_rev in

  let str = String.concat separator cut_str_list in
  begin_str ^ str ^ end_str


module Yojson =
struct
  include StdLeaf
  type t = Yojson.Safe.t [@@deriving eq]
  let name () = "yojson"

  let compare = Stdlib.compare
  let hash = Hashtbl.hash

  let pretty = GobYojson.pretty

  include SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )

  let to_yojson x = x (* override SimplePretty *)
end

module Z: S with type t = Z.t =
struct
  include StdLeaf
  include GobZ
  let name () = "Z"

  include SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end
