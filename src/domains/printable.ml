(** Some things are not quite lattices ... *)

open Pretty

type json = Yojson.Safe.t
let json_to_yojson x = x

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
  (* These two lets us reuse the short function, and allows some overriding
   * possibilities. *)
  val pretty_f: (int -> t -> string) -> unit -> t -> doc
  val printXml : 'a BatInnerIO.output -> t -> unit
  (* This is for debugging *)
  val name: unit -> string
  val to_yojson : t -> json

  val invariant: Invariant.context -> t -> Invariant.t
  val tag: t -> int (** Unique ID, given by HConsed, for context identification in witness *)

  val arbitrary: unit -> t QCheck.arbitrary

  (* For hashconsing together with incremental we need to re-hashcons old values.
   * For HashconsLifter.D this is done on any lattice operation, so we can replace x with `join bot x` to hashcons it again and get a new tag for it.
   * For HashconsLifter.C we call hashcons only in `context` which is in Analyses.Spec but not in Analyses.GlobConstrSys, i.e. not visible to the solver. *)
  (* The default for this should be identity, except for HConsed below where we want to have the side-effect and return a value with the updated tag. *)
  val relift: t -> t
end


module Std =
struct
  (*  let equal = Util.equals
      let hash = Hashtbl.hash*)
  let name () = "std"

  (* start MapDomain.Groupable *)
  type group = unit
  let show_group () = "None"
  let to_group = None
  let trace_enabled = false
  (* end MapDomain.Groupable *)

  let invariant _ _ = Invariant.none
  let tag _ = failwith "Std: no tag"
  let arbitrary () = failwith "no arbitrary"
  let relift x = x
end

(* Only include where data is guaranteed to be non-cyclic *)
module StdPolyCompare =
struct
  include Std
  let compare = compare (* Careful, does not terminate on cyclic data *)
end

module Blank =
struct
  include Std
  let pretty () _ = text "Output not supported"
  let short _ _ = "Output not supported"
  let isSimple _ = true
  let pretty_f _ = pretty
  let name () = "blank"
  let pretty_diff () (x,y) = dprintf "Unsupported"
  let printXml f _ = BatPrintf.fprintf f "<value>\n<data>\nOutput not supported!\n</data>\n</value>\n"
end

(* Only include where data is guaranteed to be non-cyclic *)
module BlankPolyCompare =
struct
  include Blank
  let compare = compare (* Careful, does not terminate on cyclic data *)
end

module PrintSimple (P: sig
    type t'
    val short: int -> t' -> string
    val name: unit -> string
  end) =
struct
  let isSimple _ = true
  let pretty_f sf () x = text (sf max_int x)
  let pretty () x = pretty_f P.short () x
  let pretty_diff () (x,y) =
    dprintf "%s: %a not leq %a" (P.name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (P.short 800 x))
end


module type Name = sig val name: string end
module UnitConf (N: Name) =
struct
  type t = unit [@@deriving yojson]
  include Std
  let compare _ _ = 0
  let hash () = 7134679
  let equal _ _ = true
  let pretty () _ = text N.name
  let short _ _ = N.name
  let isSimple _ = true
  let pretty_f _ = pretty
  let name () = "Unit"
  let pretty_diff () (x,y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f () = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape N.name)

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
  let equal x y = x.BatHashcons.tag = y.BatHashcons.tag
  let compare x y =  Stdlib.compare x.BatHashcons.tag y.BatHashcons.tag
  let short w = lift_f (Base.short w)
  let to_yojson = lift_f (Base.to_yojson)
  let pretty_f sf () = lift_f (Base.pretty_f (fun w x -> sf w (lift x)) ())
  let pretty = pretty_f short
  let isSimple = lift_f Base.isSimple
  let pretty_diff () (x,y) = Base.pretty_diff () (x.BatHashcons.obj,y.BatHashcons.obj)
  let printXml f x = Base.printXml f x.BatHashcons.obj

  let invariant c = lift_f (Base.invariant c)
  let equal_debug x y = (* This debug version checks if we call hashcons enough to have up-to-date tags. Comment out the equal below to use this. This will be even slower than with hashcons disabled! *)
    if x.BatHashcons.tag = y.BatHashcons.tag then ( (* x.BatHashcons.obj == y.BatHashcons.obj || *)
      if not (Base.equal x.BatHashcons.obj y.BatHashcons.obj) then
        ignore @@ Pretty.printf "tags are equal but values are not for %a and %a\n" pretty x pretty y;
      assert (Base.equal x.BatHashcons.obj y.BatHashcons.obj);
      true
    ) else (
      if Base.equal x.BatHashcons.obj y.BatHashcons.obj then
        ignore @@ Pretty.printf "tags are not equal but values are for %a and %a\n" pretty x pretty y;
      assert (not (Base.equal x.BatHashcons.obj y.BatHashcons.obj));
      false
    )
  let equal x y = x.BatHashcons.tag = y.BatHashcons.tag
  (* let equal = equal_debug *)
  let arbitrary () = QCheck.map ~rev:unlift lift (Base.arbitrary ())
end

module HashCached (M: S) =
struct
  let name () = "HashCached " ^ M.name ()

  type t =
    {
      m: M.t;
      lazy_hash: int Lazy.t;
    }

  let lift m = {m; lazy_hash = lazy (M.hash m)}
  let unlift {m; _} = m

  let lift_f f x = f (unlift x)
  let lift_f' f x = lift @@ lift_f f x
  let lift_f2 f x y = f (unlift x) (unlift y)
  let lift_f2' f x y = lift @@ lift_f2 f x y

  let equal = lift_f2 M.equal
  let compare = lift_f2 M.compare
  let hash x = Lazy.force x.lazy_hash
  let short w = lift_f (M.short w)
  let isSimple = lift_f M.isSimple

  let pretty_f short () = lift_f (M.pretty_f (fun w x -> short w (lift x)) ())

  let pretty () x = pretty_f short () x

  let pretty_diff () ((x:t),(y:t)): Pretty.doc = M.pretty_diff () (unlift x, unlift y)
  let printXml f = lift_f (M.printXml f)

  let to_yojson = lift_f (M.to_yojson)

  let arbitrary () = QCheck.map ~rev:unlift lift (M.arbitrary ())

  let tag = lift_f M.tag
  let invariant c = lift_f (M.invariant c)
end

module Lift (Base: S) (N: LiftingNames) =
struct
  type t = [`Bot | `Lifted of Base.t | `Top] [@@deriving to_yojson]
  include Std
  include N

  let lift x = `Lifted x

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

  let compare x y =
    match (x, y) with
    | (`Top, `Top) -> 0
    | (`Bot, `Bot) -> 0
    | (`Top, _) -> 1
    | (`Bot, _) -> -1
    | (_, `Top) -> -1
    | (_, `Bot) -> 1
    | (`Lifted x, `Lifted y) -> Base.compare x y
    | _ -> raise @@ invalid_arg "Invalid argument for Lift.compare"

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

  let pretty () x = pretty_f short () x
  let name () = "lifted " ^ Base.name ()
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f = function
    | `Bot      -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape N.bot_name)
    | `Top      -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape N.top_name)
    | `Lifted x -> Base.printXml f x

  let invariant c = function
    | `Lifted x -> Base.invariant c x
    | `Top | `Bot -> Invariant.none

  let arbitrary () =
    let open QCheck.Iter in
    let shrink = function
      | `Lifted x -> (return `Bot) <+> (MyCheck.shrink (Base.arbitrary ()) x >|= lift)
      | `Bot -> empty
      | `Top -> MyCheck.Iter.of_arbitrary ~n:20 (Base.arbitrary ()) >|= lift
    in
    QCheck.frequency ~shrink ~print:(short 10000) [ (* S TODO: better way to define printer? *)
      20, QCheck.map lift (Base.arbitrary ());
      1, QCheck.always `Bot;
      1, QCheck.always `Top
    ] (* S TODO: decide frequencies *)
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

  let compare x y = if equal x y then 0 else
    match (x, y) with
    | (`Left _), (`Right _) -> -1
    | (`Right _), (`Left _) -> 1
    | (`Right x), (`Right y) -> Base2.compare x y
    | (`Left x), (`Left y) -> Base1.compare x y
    | _, _ -> raise @@ Invalid_argument "Invalid argument for Either.compare"

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

  let pretty () x = pretty_f short () x
  let name () = "either " ^ Base1.name () ^ " or " ^ Base2.name ()
  let pretty_diff () (x,y) =
    match (x,y) with
    | `Left x, `Left y ->  Base1.pretty_diff () (x,y)
    | `Right x, `Right y ->  Base2.pretty_diff () (x,y)
    | _ -> Pretty.dprintf "%a not leq %a" pretty x pretty y
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

  let compare x y =
    let order x = match x with
      | `Top -> 0
      | `Bot -> 1
      | `Lifted1 _ -> 2
      | `Lifted2 _ -> 3
    in
    if equal x y
      then 0
      else
        let compareOrder = compare (order x) (order y) in
        if compareOrder != 0
          then compareOrder
          else
           match (x, y) with
            | (`Lifted1 a, `Lifted1 b) -> Base1.compare a b
            | (`Lifted2 x, `Lifted2 y) -> Base2.compare x y
            | _, _ -> raise @@ Failure "compare Lift2 failed"

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

  let pretty () x = pretty_f short () x
  let name () = "lifted " ^ Base1.name () ^ " and " ^ Base2.name ()
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f = function
    | `Bot       -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" N.bot_name
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
    | (x, _) when x < 0 -> -1
    | (x, _) when x > 0->  1
    | ( 0, x) when x < 0 -> -1
    | ( 0, x) when x > 0 ->  1
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

  let printXml f (x,y) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (Base1.name ())) Base1.printXml x (Goblintutil.escape (Base2.name ())) Base2.printXml y

  let pretty_diff () ((x1,x2:t),(y1,y2:t)): Pretty.doc =
    if Base1.equal x1 y1 then
      Base2.pretty_diff () (x2,y2)
    else
      Base1.pretty_diff () (x1,y1)

  let invariant c (x, y) = Invariant.(Base1.invariant c x && Base2.invariant c y)
  let arbitrary () = QCheck.pair (Base1.arbitrary ()) (Base2.arbitrary ())

  let relift (x,y) = (Base1.relift x, Base2.relift y)
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
  let compare (x1,x2,x3) (y1,y2,y3) =
    let comp1 = Base1.compare x1 y1 in
    if comp1 <> 0
      then comp1
      else let comp2 = Base2.compare x2 y2 in
      if comp2 <> 0
      then comp2
      else Base3.compare x3 y3

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

  let printXml f (x,y,z) =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (Base1.name ())) Base1.printXml x (Goblintutil.escape (Base2.name ())) Base2.printXml y (Goblintutil.escape (Base3.name ())) Base3.printXml z

  let pretty () x = pretty_f short () x
  let name () = Base1.name () ^ " * " ^ Base2.name () ^ " * " ^ Base3.name ()
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let invariant c (x, y, z) = Invariant.(Base1.invariant c x && Base2.invariant c y && Base3.invariant c z)
  let arbitrary () = QCheck.triple (Base1.arbitrary ()) (Base2.arbitrary ()) (Base3.arbitrary ())
end

module Liszt (Base: S) =
struct
  type t = Base.t list [@@deriving to_yojson]
  include Std
  let equal x y = try List.for_all2 Base.equal x y with Invalid_argument _ -> false
  let compare x y = BatList.compare Base.compare x y
  let hash = List.fold_left (fun xs x -> xs + Base.hash x) 996699

  let short _ x =
    let elems = List.map (Base.short max_int) x in
    "[" ^ (String.concat ", " elems) ^ "]"

  let pretty_f sf () x = text (sf max_int x)
  let isSimple _ = true

  let pretty () x = pretty_f short () x
  let name () = Base.name () ^ " list"
  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "%a not leq %a" pretty x pretty y
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
  let compare x y = x-y

  let short _ x = P.names x
  let pretty_f f () x = text (f max_int x)
  let isSimple _ = true
  let hash x = x-5284
  let equal (x:int) (y:int) = x=y
  let pretty () x = pretty_f short () x
  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "%a not leq %a" pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (P.names x)

  let arbitrary () = QCheck.int_range 0 (P.n - 1)
  let relift x = x
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

  let compare x y =
    match x,y with
    | `Bot,`Bot -> 0
    | (`Bot, _) -> -1
    | (_, `Bot) -> 1
    | (`Lifted x, `Lifted y) -> Base.compare x y

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

  let pretty () x = pretty_f short () x
  let name () = "bottom or " ^ Base.name ()
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
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

  let compare x y =
    match (x, y) with
    | `Top, `Top -> 0
    | `Top, _ -> 1
    | _, `Top -> -1
    | `Lifted x, `Lifted y -> Base.compare x y

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

  let pretty () x = pretty_f short () x
  let name () = "top or " ^ Base.name ()
  let pretty_diff () (x,y) =
    match (x,y) with
    | `Lifted x, `Lifted y -> Base.pretty_diff () (x,y)
    | _ -> dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let printXml f = function
    | `Top -> BatPrintf.fprintf f "<value>\n<data>\ntop\n</data>\n</value>\n"
    | `Lifted n -> Base.printXml f n

  let arbitrary () =
    let open QCheck.Iter in
    let shrink = function
      | `Lifted x -> MyCheck.shrink (Base.arbitrary ()) x >|= lift
      | `Top -> MyCheck.Iter.of_arbitrary ~n:20 (Base.arbitrary ()) >|= lift
    in
    QCheck.frequency ~shrink ~print:(short 10000) [ (* S TODO: better way to define printer? *)
      20, QCheck.map lift (Base.arbitrary ());
      1, QCheck.always `Top
    ] (* S TODO: decide frequencies *)
end


module Strings =
struct
  type t = string [@@deriving to_yojson]
  include StdPolyCompare
  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) (y:t) = x=y
  let pretty () n = text n
  let short _ n = n
  let isSimple _ = true
  let pretty_f _ = pretty
  let name () = "String"
  let pretty_diff () (x,y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" x
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
