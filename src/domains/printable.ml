(** Some things are not quite lattices ... *)

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
  (* The default for this should be identity, except for HConsed below where we want to have the side-effect and return a value with the updated tag. *)
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


module Std =
struct
  (*  let equal = Util.equals
      let hash = Hashtbl.hash*)
  let name () = "std"

  (* start MapDomain.Groupable *)
  type group = |
  let show_group (x: group) = match x with _ -> .
  let to_group _ = None
  let trace_enabled = false
  (* end MapDomain.Groupable *)

  let tag _ = failwith "Std: no tag"
  let arbitrary () = failwith "no arbitrary"
  let relift x = x
end

module Blank =
struct
  include Std
  let pretty () _ = text "Output not supported"
  let show _ = "Output not supported"
  let name () = "blank"
  let printXml f _ = BatPrintf.fprintf f "<value>\n<data>\nOutput not supported!\n</data>\n</value>\n"
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
  let show x = Pretty.sprint ~width:max_int (P.pretty () x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end


module type Name = sig val name: string end
module UnitConf (N: Name) =
struct
  type t = unit [@@deriving eq, ord, hash]
  include Std
  let pretty () _ = text N.name
  let show _ = N.name
  let name () = "Unit"
  let printXml f () = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape N.name)
  let to_yojson () = `String N.name
  let arbitrary () = QCheck.unit
  let relift x = x
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
  include Std (* for default invariant, tag, ... *)

  module HC = BatHashcons.MakeTable (Base)
  let htable = HC.create 100000

  type t = Base.t BatHashcons.hobj
  let unlift x = x.BatHashcons.obj
  let lift = HC.hashcons htable
  let lift_f f (x:Base.t BatHashcons.hobj) = f (x.BatHashcons.obj)
  let relift x = let y = Base.relift x.BatHashcons.obj in HC.hashcons htable y
  let name () = "HConsed "^Base.name ()
  let hash x = x.BatHashcons.hcode
  let tag x = x.BatHashcons.tag
  let compare x y =  Stdlib.compare x.BatHashcons.tag y.BatHashcons.tag
  let show = lift_f Base.show
  let to_yojson = lift_f (Base.to_yojson)
  let pretty () = lift_f (Base.pretty ())
  let printXml f x = Base.printXml f x.BatHashcons.obj

  let equal_debug x y = (* This debug version checks if we call hashcons enough to have up-to-date tags. Comment out the equal below to use this. This will be even slower than with hashcons disabled! *)
    if x.BatHashcons.tag = y.BatHashcons.tag then ( (* x.BatHashcons.obj == y.BatHashcons.obj || *)
      if not (Base.equal x.BatHashcons.obj y.BatHashcons.obj) then
        Logs.error "tags are equal but values are not for %a and %a\n" pretty x pretty y;
      assert (Base.equal x.BatHashcons.obj y.BatHashcons.obj);
      true
    ) else (
      if Base.equal x.BatHashcons.obj y.BatHashcons.obj then
        Logs.error "tags are not equal but values are for %a and %a\n" pretty x pretty y;
      assert (not (Base.equal x.BatHashcons.obj y.BatHashcons.obj));
      false
    )
  let equal x y = x.BatHashcons.tag = y.BatHashcons.tag
  (* let equal = equal_debug *)
  let arbitrary () = QCheck.map ~rev:unlift lift (Base.arbitrary ())
end

module HashCached (M: S) =
struct
  module LazyHash = LazyEval.Make (struct type t = M.t type result = int let eval = M.hash end)

  let name () = "HashCached " ^ M.name ()

  type t =
    {
      m: M.t;
      lazy_hash: LazyHash.t;
    }

  let lift m = {m; lazy_hash = LazyHash.make m}
  let unlift {m; _} = m

  let lift_f f x = f (unlift x)
  let lift_f' f x = lift @@ lift_f f x
  let lift_f2 f x y = f (unlift x) (unlift y)
  let lift_f2' f x y = lift @@ lift_f2 f x y

  let equal = lift_f2 M.equal
  let compare = lift_f2 M.compare
  let hash x = LazyHash.force x.lazy_hash
  let show = lift_f M.show

  let pretty () = lift_f (M.pretty ())

  let printXml f = lift_f (M.printXml f)
  let to_yojson = lift_f (M.to_yojson)

  let arbitrary () = QCheck.map ~rev:unlift lift (M.arbitrary ())

  let tag = lift_f M.tag
end

module Lift (Base: S) (N: LiftingNames) =
struct
  type t = [`Bot | `Lifted of Base.t | `Top] [@@deriving eq, ord, hash]
  include Std
  include N

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
    | `Bot      -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape N.bot_name)
    | `Top      -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape N.top_name)
    | `Lifted x -> Base.printXml f x

  let to_yojson = function
    | `Bot -> `String N.bot_name
    | `Top -> `String N.top_name
    | `Lifted x -> Base.to_yojson x

  let relift x = match x with
    | `Bot |`Top -> x
    | `Lifted v -> `Lifted (Base.relift v)

  let arbitrary () =
    let open QCheck.Iter in
    let shrink = function
      | `Lifted x -> (return `Bot) <+> (MyCheck.shrink (Base.arbitrary ()) x >|= lift)
      | `Bot -> empty
      | `Top -> MyCheck.Iter.of_arbitrary ~n:20 (Base.arbitrary ()) >|= lift
    in
    QCheck.frequency ~shrink ~print:show [
      20, QCheck.map lift (Base.arbitrary ());
      1, QCheck.always `Bot;
      1, QCheck.always `Top
    ] (* S TODO: decide frequencies *)
end

module Either (Base1: S) (Base2: S) =
struct
  type t = [`Left of Base1.t | `Right of Base2.t] [@@deriving eq, ord, hash]
  include Std

  let pretty () (state:t) =
    match state with
    | `Left n ->  Base1.pretty () n
    | `Right n ->  Base2.pretty () n

  let show state =
    match state with
    | `Left n ->  Base1.show n
    | `Right n ->  Base2.show n

  let name () = "either " ^ Base1.name () ^ " or " ^ Base2.name ()
  let printXml f = function
    | `Left x  -> BatPrintf.fprintf f "<value><map>\n<key>\nLeft\n</key>\n%a</map>\n</value>\n" Base1.printXml x
    | `Right x -> BatPrintf.fprintf f "<value><map>\n<key>\nRight\n</key>\n%a</map>\n</value>\n" Base2.printXml x

  let to_yojson = function
    | `Left x -> `Assoc [ Base1.name (), Base1.to_yojson x ]
    | `Right x -> `Assoc [ Base2.name (), Base2.to_yojson x ]

  let relift = function
    | `Left x -> `Left (Base1.relift x)
    | `Right x -> `Right (Base2.relift x)
end

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

module Lift2 (Base1: S) (Base2: S) (N: LiftingNames) =
struct
  type t = [`Bot | `Lifted1 of Base1.t | `Lifted2 of Base2.t | `Top] [@@deriving eq, ord, hash]
  include Std
  include N

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
    | `Bot       -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" N.bot_name
    | `Top       -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" N.top_name
    | `Lifted1 x -> BatPrintf.fprintf f "<value>\n<map>\n<key>\nLifted1\n</key>\n%a</map>\n</value>\n" Base1.printXml x
    | `Lifted2 x -> BatPrintf.fprintf f "<value>\n<map>\n<key>\nLifted2\n</key>\n%a</map>\n</value>\n" Base2.printXml x

  let to_yojson = function
    | `Bot -> `String N.bot_name
    | `Top -> `String N.top_name
    | `Lifted1 x -> `Assoc [ Base1.name (), Base1.to_yojson x ]
    | `Lifted2 x -> `Assoc [ Base2.name (), Base2.to_yojson x ]
end

module type ProdConfiguration =
sig
  val expand_fst: bool
  val expand_snd: bool
end

module ProdConf (C: ProdConfiguration) (Base1: S) (Base2: S)=
struct
  include C

  type t = Base1.t * Base2.t [@@deriving eq, ord, hash]

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
      ++ (if expand_fst then Base1.pretty () x else text (Base1.show x))
      ++ text ", "
      ++ (if expand_snd then Base2.pretty () y else text (Base2.show y))
      ++ text ")"
    else
      text (show (x,y))

  let printXml f (x,y) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (XmlUtil.escape (Base1.name ())) Base1.printXml x (XmlUtil.escape (Base2.name ())) Base2.printXml y

  let to_yojson (x, y) =
    `Assoc [ (Base1.name (), Base1.to_yojson x); (Base2.name (), Base2.to_yojson y) ]

  let arbitrary () = QCheck.pair (Base1.arbitrary ()) (Base2.arbitrary ())

  let relift (x,y) = (Base1.relift x, Base2.relift y)
end

module Prod = ProdConf (struct let expand_fst = true let expand_snd = true end)
module ProdSimple = ProdConf (struct let expand_fst = false let expand_snd = false end)

module Prod3 (Base1: S) (Base2: S) (Base3: S) =
struct
  type t = Base1.t * Base2.t * Base3.t [@@deriving eq, ord, hash]
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
    text "(" ++
    Base1.pretty () x
    ++ text ", " ++
    Base2.pretty () y
    ++ text ", " ++
    Base3.pretty () z
    ++ text ")"

  let printXml f (x,y,z) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (XmlUtil.escape (Base1.name ())) Base1.printXml x (XmlUtil.escape (Base2.name ())) Base2.printXml y (XmlUtil.escape (Base3.name ())) Base3.printXml z

  let to_yojson (x, y, z) =
    `Assoc [ (Base1.name (), Base1.to_yojson x); (Base2.name (), Base2.to_yojson y); (Base3.name (), Base3.to_yojson z) ]

  let name () = Base1.name () ^ " * " ^ Base2.name () ^ " * " ^ Base3.name ()

  let relift (x,y,z) = (Base1.relift x, Base2.relift y, Base3.relift z)
  let arbitrary () = QCheck.triple (Base1.arbitrary ()) (Base2.arbitrary ()) (Base3.arbitrary ())
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

  let common_prefix x y =
    let rec helper acc x y =
      match x,y with
      | x::xs, y::ys when Base.equal x y-> helper (x::acc) xs ys
      | _ -> acc
    in
    List.rev (helper [] x y)

  let common_suffix x y = List.rev (common_prefix (List.rev x) (List.rev y))
end

module type ChainParams = sig
  val n: unit -> int
  val names: int -> string
end

module Chain (P: ChainParams): S with type t = int =
struct
  type t = int [@@deriving eq, ord, hash]
  include Std

  let show x = P.names x
  let pretty () x = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (P.names x)
  let to_yojson x = `String (P.names x)

  let arbitrary () = QCheck.int_range 0 (P.n () - 1)
  let relift x = x
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
      | `Lifted x -> MyCheck.shrink (Base.arbitrary ()) x >|= lift
      | `Top -> MyCheck.Iter.of_arbitrary ~n:20 (Base.arbitrary ()) >|= lift
    in
    QCheck.frequency ~shrink ~print:show [
      20, QCheck.map lift (Base.arbitrary ());
      1, QCheck.always `Top
    ] (* S TODO: decide frequencies *)
end


module Strings =
struct
  type t = string [@@deriving eq, ord, hash, to_yojson]
  include Std
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
