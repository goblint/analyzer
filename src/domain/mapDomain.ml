(** Map domains. *)

module Pretty = GoblintCil.Pretty
open Pretty

module type PS =
sig
  include Printable.S
  type key
  (** The type of the map keys. *)

  type value
  (** The type of the values. *)

  val add: key -> value -> t -> t
  val remove: key -> t -> t
  val find: key -> t -> value
  val find_opt: key -> t -> value option
  val mem: key -> t -> bool
  val iter: (key -> value -> unit) -> t -> unit
  val map: (value -> value) -> t -> t
  val filter: (key -> value -> bool) -> t -> t
  val mapi: (key -> value -> value) -> t -> t
  val fold: (key -> value -> 'a -> 'a) -> t -> 'a -> 'a

  val add_list: (key * value) list -> t -> t
  val add_list_set: key list -> value -> t -> t
  val add_list_fun: key list -> (key -> value) -> t -> t

  val for_all: (key -> value -> bool) -> t -> bool
  val map2: (value -> value -> value) -> t -> t -> t
  val long_map2: (value -> value -> value) -> t -> t -> t
  val merge : (key -> value option -> value option -> value option) -> t -> t -> t (* TODO: unused, remove? *)

  val cardinal: t -> int
  val choose: t -> key * value
  val singleton: key -> value -> t
  val empty: unit -> t
  val is_empty: t -> bool
  val exists: (key -> value -> bool) -> t -> bool
  val bindings: t -> (key * value) list
end

module type S =
sig
  include PS
  include Lattice.S with type t := t

  val widen_with_fct: (value -> value -> value) -> t -> t -> t
  (* Widen using a custom widening function for value rather than the default one for value *)
  val join_with_fct: (value -> value -> value) -> t -> t -> t
  (* Join using a custom join function for value rather than the default one for value *)
  val leq_with_fct: (value -> value -> bool) -> t -> t -> bool
  (* Leq test using a custom leq function for value rather than the default one provided for value *)
end

(** Subsignature of {!S}, which is sufficient for {!Print}. *)
module type Bindings =
sig
  type t
  type key
  type value
  val fold: (key -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: (key -> value -> unit) -> t -> unit
end

(** Reusable output definitions for maps. *)
module Print (D: Printable.S) (R: Printable.S) (M: Bindings with type key = D.t and type value = R.t) =
struct
  let pretty () map =
    let doc = M.fold (fun k v acc ->
        acc ++ dprintf "%a ->@?@[%a@]\n" D.pretty k R.pretty v
      ) map nil
    in
    if doc = Pretty.nil then
      text "{}"
    else
      dprintf "@[{\n  @[%a@]}@]" Pretty.insert doc

  let show map = GobPretty.sprint pretty map

  let printXml f map =
    BatPrintf.fprintf f "<value>\n<map>\n";
    M.iter (fun k v ->
        BatPrintf.fprintf f "<key>\n%s</key>\n%a" (XmlUtil.escape (D.show k)) R.printXml v
      ) map;
    BatPrintf.fprintf f "</map>\n</value>\n"

  let to_yojson map =
    let l = M.fold (fun k v acc ->
        (D.show k, R.to_yojson v) :: acc
      ) map []
    in
    `Assoc l
end

module type Groupable =
sig
  include Printable.S
  type group (* use [@@deriving show { with_path = false }] *)
  val compare_group: group -> group -> int
  val show_group: group -> string
  val to_group: t -> group
end

(** Reusable output definitions for maps with key grouping. *)
module PrintGroupable (D: Groupable) (R: Printable.S) (M: Bindings with type key = D.t and type value = R.t) =
struct
  include Print (D) (R) (M)

  module Group =
  struct
    type t = D.group
    let compare = D.compare_group
  end

  module GroupMap = Map.Make (Group)

  let pretty () mapping =
    let groups =
      M.fold (fun k v acc ->
          GroupMap.update (D.to_group k) (fun doc ->
              let doc = Option.value doc ~default:Pretty.nil in
              let doc' = doc ++ dprintf "%a ->@?  @[%a@]\n" D.pretty k R.pretty v in
              Some doc'
            ) acc
        ) mapping GroupMap.empty
    in
    let pretty_groups () = GroupMap.fold (fun group doc acc ->
        acc ++ dprintf "@[%s {\n  @[%a@]}@]\n" (D.show_group group) Pretty.insert doc
      ) groups nil in
    dprintf "@[{\n  @[%t@]}@]" pretty_groups

  let show map = GobPretty.sprint pretty map

  (* TODO: groups in XML, JSON? *)
end

module PMap (Domain: Printable.S) (Range: Lattice.S) : PS with
  type key = Domain.t and
  type value = Range.t =
struct
  module M = Map.Make (Domain)

  include Printable.Std
  include M
  type key = Domain.t
  type value = Range.t
  type t = Range.t M.t (* key -> value  mapping *)

  let name () = "map"

  (* And one less brainy definition *)
  let for_all2 = M.equal
  let equal x y = x == y || for_all2 Range.equal x y
  let compare x y = if equal x y then 0 else M.compare Range.compare x y
  let hash xs = fold (fun k v a -> a + (Domain.hash k * Range.hash v)) xs 0

  let empty () = M.empty


  let add_list keyvalues m =
    List.fold_left (fun acc (key,value) -> add key value acc) m keyvalues

  let add_list_set keys value m =
    List.fold_left (fun acc key -> add key value acc) m keys

  let add_list_fun keys f m =
    List.fold_left (fun acc key -> add key (f key) acc) m keys

  let long_map2 op =
    let f k v1 v2 =
      match v1, v2 with
      | Some v1, Some v2 -> Some (op v1 v2)
      | Some _, _ -> v1
      | _, Some _ -> v2
      | _ -> None
    in
    M.merge f

  let map2 op =
    (* Similar to the previous, except we ignore elements that only occur in one
     * of the mappings, so we start from an empty map *)
    let f k v1 v2 =
      match v1, v2 with
      | Some v1, Some v2 -> Some (op v1 v2)
      | _ -> None
    in
    M.merge f

  include Print (Domain) (Range) (
    struct
      type nonrec t = t
      type nonrec key = key
      type nonrec value = value
      let fold = fold
      let iter = iter
    end
    )

  (* uncomment to easily check pretty's grouping during a normal run, e.g. ./regtest 01 01: *)
  (* let add k v m = let _ = Pretty.printf "%a\n" pretty m in M.add k v m *)

  let arbitrary () = QCheck.always M.empty (* S TODO: non-empty map *)

  let relift m =
    M.fold (fun k v acc ->
        M.add (Domain.relift k) (Range.relift v) acc
      ) m M.empty
end

(* TODO: why is HashCached.hash significantly slower as a functor compared to being inlined into PMap? *)
module HashCached (M: S) : S with
  type key = M.key and
  type value = M.value =
struct
  include Lattice.HashCached (M)

  type key = M.key
  type value = M.value

  let add k v = lift_f' (M.add k v)
  let remove k = lift_f' (M.remove k)
  let find k = lift_f (M.find k)
  let find_opt k = lift_f (M.find_opt k)
  let mem k = lift_f (M.mem k)
  let iter f = lift_f (M.iter f)
  let map f = lift_f' (M.map f)
  let mapi f = lift_f' (M.mapi f)
  let fold f x a = M.fold f (unlift x) a
  let filter f = lift_f' (M.filter f)
  let merge f = lift_f2' (M.merge f)
  let for_all f = lift_f (M.for_all f)

  let cardinal = lift_f M.cardinal
  let choose = lift_f M.choose
  let singleton k v = lift @@ M.singleton k v
  let empty () = lift @@ M.empty ()
  let is_empty = lift_f M.is_empty
  let exists p = lift_f (M.exists p)
  let bindings = lift_f M.bindings


  let add_list keyvalues = lift_f' (M.add_list keyvalues)

  let add_list_set keys value = lift_f' (M.add_list_set keys value)

  let add_list_fun keys f = lift_f' (M.add_list_fun keys f)

  let long_map2 op = lift_f2' (M.long_map2 op)

  let map2 op = lift_f2' (M.map2 op)

  let leq_with_fct f = lift_f2 (M.leq_with_fct f)
  let join_with_fct f = lift_f2' (M.join_with_fct f)
  let widen_with_fct f = lift_f2' (M.widen_with_fct f)

  let relift = lift_f' M.relift
end

(* TODO: this is very slow because every add/remove in a fold-loop relifts *)
(* TODO: currently hardcoded to assume_idempotent *)
module HConsed (M: S) : S with
  type key = M.key and
  type value = M.value =
struct
  include Lattice.HConsed (M) (struct let assume_idempotent = false end)

  type key = M.key
  type value = M.value

  let lift_f' f x = lift @@ lift_f f x
  let lift_f2' f x y = lift @@ lift_f2 f x y

  let add k v = lift_f' (M.add k v)
  let remove k = lift_f' (M.remove k)
  let find k = lift_f (M.find k)
  let find_opt k = lift_f (M.find_opt k)
  let mem k = lift_f (M.mem k)
  let iter f = lift_f (M.iter f)
  let map f = lift_f' (M.map f)
  let mapi f = lift_f' (M.mapi f)
  let fold f x a = M.fold f (unlift x) a
  let filter f = lift_f' (M.filter f)
  let merge f = lift_f2' (M.merge f)
  let for_all f = lift_f (M.for_all f)

  let cardinal = lift_f M.cardinal
  let choose = lift_f M.choose
  let singleton k v = lift @@ M.singleton k v
  let empty () = lift @@ M.empty ()
  let is_empty = lift_f M.is_empty
  let exists p = lift_f (M.exists p)
  let bindings = lift_f M.bindings


  let add_list keyvalues = lift_f' (M.add_list keyvalues)

  let add_list_set keys value = lift_f' (M.add_list_set keys value)

  let add_list_fun keys f = lift_f' (M.add_list_fun keys f)

  let long_map2 op = lift_f2' (M.long_map2 op)

  let map2 op = lift_f2' (M.map2 op)

  let leq_with_fct f = lift_f2 (M.leq_with_fct f)
  let join_with_fct f = lift_f2' (M.join_with_fct f)
  let widen_with_fct f = lift_f2' (M.widen_with_fct f)
end

module Timed (M: S) : S with
  type key = M.key and
  type value = M.value =
struct
  let time str f arg = Timing.wrap (M.name ()) (Timing.wrap str f) arg

  (* Printable.S *)
  type t = M.t

  let equal x y = time "equal" (M.equal x) y
  let compare x y = time "compare" (M.compare x) y
  let hash x = time "hash" M.hash x
  let tag x = time "tag" M.tag x
  (* TODO: time these also? *)
  let name = M.name
  let to_yojson = M.to_yojson
  let show = M.show
  let pretty = M.pretty
  let pretty_diff = M.pretty_diff
  let printXml = M.printXml
  let arbitrary = M.arbitrary

  (* Lattice.S *)
  let top () = time "top" M.top ()
  let is_top x = time "is_top" M.is_top x
  let bot () = time "bot" M.bot ()
  let is_bot x = time "is_bot" M.is_bot x
  let leq x y = time "leq" (M.leq x) y
  let join x y = time "join" (M.join x) y
  let meet x y = time "meet" (M.meet x) y
  let widen x y = time "widen" (M.widen x) y
  let narrow x y = time "narrow" (M.narrow x) y

  (* MapDomain.S *)
  type key = M.key
  type value = M.value

  let add k v x = time "add" (M.add k v) x
  let remove k x = time "remove" (M.remove k) x
  let find k x = time "find" (M.find k) x
  let find_opt k x = time "find_opt" (M.find_opt k) x
  let mem k x = time "mem" (M.mem k) x
  let iter f x = time "iter" (M.iter f) x
  let map f x = time "map" (M.map f) x
  let mapi f x = time "mapi" (M.mapi f) x
  let fold f x a = time "fold" (M.fold f x) a
  let filter f x = time "filter" (M.filter f) x
  let merge f x y = time "merge" (M.merge f x) y
  let for_all f x = time "for_all" (M.for_all f) x

  let cardinal x = time "cardinal" M.cardinal x
  let choose x = time "choose" M.choose x
  let singleton k v = time "singleton" (M.singleton k) v
  let empty () = time "empty" M.empty ()
  let is_empty x = time "is_empty" M.is_empty x
  let exists p x = time "exists" (M.exists p) x
  let bindings x = time "bindings" M.bindings x


  let add_list xs x = time "add_list" (M.add_list xs) x
  let add_list_set ks v x = time "add_list_set" (M.add_list_set ks v) x
  let add_list_fun ks f x = time "add_list_fun" (M.add_list_fun ks f) x

  let long_map2 f x y = time "long_map2" (M.long_map2 f x) y

  let map2 f x y = time "map2" (M.map2 f x) y

  let leq_with_fct f x y = time "leq_with_fct" (M.leq_with_fct f x) y
  let join_with_fct f x y = time "join_with_fct" (M.join_with_fct f x) y
  let widen_with_fct f x y = time "widen_with_fct" (M.widen_with_fct f x) y

  let relift x = M.relift x
end

module MapBot (Domain: Printable.S) (Range: Lattice.S) : S with
  type key = Domain.t and
  type value = Range.t =
struct
  include PMap (Domain) (Range)

  let leq_with_fct f m1 m2 =
    (* For each key-value in m1, the same key must be in m2 with a geq value: *)
    let p key value =
      try f value (find key m2) with Not_found -> false
    in
    m1 == m2 || for_all p m1

  let leq = leq_with_fct Range.leq

  let find x m = try find x m with | Not_found -> Range.bot ()
  let top () = Lattice.unsupported "partial map top"
  let bot () = empty ()
  let is_top _ = false
  let is_bot = is_empty

  let pretty_diff () ((m1:t),(m2:t)): Pretty.doc =
    let diff_key k v acc_opt =
      match find k m2 with
      | v2 when not (Range.leq v v2) ->
        let acc = BatOption.map_default (fun acc -> acc ++ line) Pretty.nil acc_opt in
        Some (acc ++ dprintf "Map: %a =@?@[%a@]" Domain.pretty k Range.pretty_diff (v, v2))
      | exception Lattice.BotValue ->
        let acc = BatOption.map_default (fun acc -> acc ++ line) Pretty.nil acc_opt in
        Some (acc ++ dprintf "Map: %a =@?@[%a not leq bot@]" Domain.pretty k Range.pretty v)
      | v2 -> acc_opt
    in
    match fold diff_key m1 None with
    | Some w -> w
    | None -> Pretty.dprintf "No binding grew."

  let meet m1 m2 = if m1 == m2 then m1 else map2 Range.meet m1 m2

  let join_with_fct f m1 m2 =
    if m1 == m2 then m1 else long_map2 f m1 m2
  let join = join_with_fct Range.join

  let widen_with_fct f =  long_map2 f
  let widen  = widen_with_fct Range.widen


  let narrow = map2 Range.narrow
end

module MapTop (Domain: Printable.S) (Range: Lattice.S) : S with
  type key = Domain.t and
  type value = Range.t =
struct
  include PMap (Domain) (Range)

  let leq_with_fct f m1 m2 = (* TODO use merge or sth faster? *)
    (* For each key-value in m2, the same key must be in m1 with a leq value: *)
    let p key value =
      try f (find key m1) value with Not_found -> false
    in
    m1 == m2 || for_all p m2

  let leq = leq_with_fct Range.leq

  let find x m = try find x m with | Not_found -> Range.top ()
  let top () = empty ()
  let bot () = Lattice.unsupported "partial map bot"
  let is_top = is_empty
  let is_bot _ = false

  (* let cleanup m = fold (fun k v m -> if Range.is_top v then remove k m else m) m m *)
  let meet m1 m2 = if m1 == m2 then m1 else long_map2 Range.meet m1 m2

  let join_with_fct f m1 m2 =
    if m1 == m2 then m1 else map2 f m1 m2

  let join = join_with_fct Range.join

  let widen_with_fct f = map2 f
  let widen = widen_with_fct Range.widen

  let narrow = long_map2 Range.narrow

  let pretty_diff () ((m1:t),(m2:t)): Pretty.doc =
    let diff_key k v acc_opt =
      match find k m1 with
      | v1 when not (Range.leq v1 v) ->
        let acc = BatOption.map_default (fun acc -> acc ++ line) Pretty.nil acc_opt in
        Some (acc ++ dprintf "Map: %a =@?@[%a@]" Domain.pretty k Range.pretty_diff (v1, v))
      | exception Lattice.TopValue ->
        let acc = BatOption.map_default (fun acc -> acc ++ line) Pretty.nil acc_opt in
        Some (acc ++ dprintf "Map: %a =@?@[top not leq %a@]" Domain.pretty k Range.pretty v)
      | v1 -> acc_opt
    in
    match fold diff_key m2 None with
    | Some w -> w
    | None -> Pretty.dprintf "No binding grew."
end

exception Fn_over_All of string

module LiftTop (Range: Lattice.S) (M: S with type value = Range.t): S with
  type key = M.key and
  type value = Range.t =
struct
  include Lattice.LiftTop (M)

  type key   = M.key
  type value = M.value

  let add k v = function
    | `Top -> `Top
    | `Lifted x -> `Lifted (M.add k v x)

  let remove k = function
    | `Top -> `Top
    | `Lifted x -> `Lifted (M.remove k x)

  let find k = function
    | `Top -> Range.top ()
    | `Lifted x -> M.find k x

  let find_opt k = function
    | `Top -> Some (Range.top ())
    | `Lifted x -> M.find_opt k x

  let mem k = function
    | `Top -> true
    | `Lifted x -> M.mem k x

  let map f = function
    | `Top -> `Top
    | `Lifted x -> `Lifted (M.map f x)

  let add_list xs = function
    | `Top -> `Top
    | `Lifted x -> `Lifted (M.add_list xs x)

  let add_list_set ks v = function
    | `Top -> `Top
    | `Lifted x -> `Lifted (M.add_list_set ks v x)

  let add_list_fun ks f = function
    | `Top -> `Top
    | `Lifted x -> `Lifted (M.add_list_fun ks f x)

  let map2 f x y =
    match x, y with
    | `Lifted x, `Lifted y -> `Lifted (M.map2 f x y)
    | _ -> raise (Fn_over_All "map2")

  let long_map2 f x y =
    match x, y with
    | `Lifted x, `Lifted y -> `Lifted (M.long_map2 f x y)
    | _ -> raise (Fn_over_All "long_map2")

  let for_all f = function
    | `Top -> raise (Fn_over_All "for_all")
    | `Lifted x -> M.for_all f x

  let iter f = function
    | `Top -> raise (Fn_over_All "iter")
    | `Lifted x -> M.iter f x

  let fold f x a =
    match x with
    | `Top -> raise (Fn_over_All "fold")
    | `Lifted x -> M.fold f x a

  let filter f x =
    match x with
    | `Top -> raise (Fn_over_All "filter")
    | `Lifted x -> `Lifted (M.filter f x)

  let merge f x y  =
    match x, y with
    | `Lifted x, `Lifted y -> `Lifted (M.merge f x y)
    | _ -> raise (Fn_over_All "merge")

  let leq_with_fct f x y =
    match (x,y) with
    | (_, `Top) -> true
    | (`Top, _) -> false
    | (`Lifted x, `Lifted y) -> M.leq_with_fct f x y

  let join_with_fct f x y =
    match (x,y) with
    | (`Top, x) -> `Top
    | (x, `Top) -> `Top
    | (`Lifted x, `Lifted y) -> `Lifted (M.join_with_fct f x y)

  let widen_with_fct f x y =
    match (x,y) with
    | (`Lifted x, `Lifted y) -> `Lifted (M.widen_with_fct f x y)
    | _ -> y

  let cardinal = function
    | `Top -> raise (Fn_over_All "cardinal")
    | `Lifted x -> M.cardinal x

  let choose = function
    | `Top -> raise (Fn_over_All "choose")
    | `Lifted x -> M.choose x

  let singleton k v = `Lifted (M.singleton k v)
  let empty () = `Lifted (M.empty ())
  let is_empty = function
    | `Top -> false
    | `Lifted x -> M.is_empty x
  let exists f = function
    | `Top -> raise (Fn_over_All "exists")
    | `Lifted x -> M.exists f x
  let bindings = function
    | `Top -> raise (Fn_over_All "bindings")
    | `Lifted x -> M.bindings x
  let mapi f = function
    | `Top -> `Top
    | `Lifted x -> `Lifted (M.mapi f x)
end

module MapBot_LiftTop (Domain: Printable.S) (Range: Lattice.S) : S with
  type key = Domain.t and
  type value = Range.t =
struct
  module M = MapBot (Domain) (Range)
  include LiftTop (Range) (M)
end


module LiftBot (Range: Lattice.S) (M: S with type value = Range.t): S with
  type key = M.key and
  type value = Range.t =
struct
  include Lattice.LiftBot (M)

  type key   = M.key
  type value = M.value

  let add k v = function
    | `Bot -> `Bot
    | `Lifted x -> `Lifted (M.add k v x)

  let remove k = function
    | `Bot -> `Bot
    | `Lifted x -> `Lifted (M.remove k x)

  let find k = function
    | `Bot -> Range.bot ()
    | `Lifted x -> M.find k x

  let find_opt k = function
    | `Bot -> Some (Range.bot ())
    | `Lifted x -> M.find_opt k x

  let mem k = function
    | `Bot -> false
    | `Lifted x -> M.mem k x

  let map f = function
    | `Bot -> `Bot
    | `Lifted x -> `Lifted (M.map f x)

  let add_list xs = function
    | `Bot -> `Bot
    | `Lifted x -> `Lifted (M.add_list xs x)

  let add_list_set ks v = function
    | `Bot -> `Bot
    | `Lifted x -> `Lifted (M.add_list_set ks v x)

  let add_list_fun ks f = function
    | `Bot -> `Bot
    | `Lifted x -> `Lifted (M.add_list_fun ks f x)

  let map2 f x y =
    match x, y with
    | `Lifted x, `Lifted y -> `Lifted (M.map2 f x y)
    | _ -> raise (Fn_over_All "map2")

  let long_map2 f x y =
    match x, y with
    | `Lifted x, `Lifted y -> `Lifted (M.long_map2 f x y)
    | _ -> raise (Fn_over_All "long_map2")

  let for_all f = function
    | `Bot -> raise (Fn_over_All "for_all")
    | `Lifted x -> M.for_all f x

  let iter f = function
    | `Bot -> raise (Fn_over_All "iter")
    | `Lifted x -> M.iter f x

  let fold f x a =
    match x with
    | `Bot -> raise (Fn_over_All "fold")
    | `Lifted x -> M.fold f x a

  let filter f x =
    match x with
    | `Bot -> raise (Fn_over_All "filter")
    | `Lifted x -> `Lifted (M.filter f x)

  let merge f x y  =
    match x, y with
    | `Lifted x, `Lifted y -> `Lifted (M.merge f x y)
    | _ -> raise (Fn_over_All "merge")

  let join_with_fct f x y =
    match (x,y) with
    | (`Bot, x) -> x
    | (x, `Bot) -> x
    | (`Lifted x, `Lifted y) -> `Lifted (M.join_with_fct f x y)

  let widen_with_fct f x y =
    match (x,y) with
    | (`Lifted x, `Lifted y) -> `Lifted(M.widen_with_fct f x y)
    | _ -> y

  let leq_with_fct f x y =
    match (x,y) with
    | (`Bot, _) -> true
    | (_, `Bot) -> false
    | (`Lifted x, `Lifted y) -> M.leq_with_fct f x y

  let cardinal = function
    | `Bot -> raise (Fn_over_All "cardinal")
    | `Lifted x -> M.cardinal x

  let choose = function
    | `Bot -> raise (Fn_over_All "choose")
    | `Lifted x -> M.choose x

  let singleton k v = `Lifted (M.singleton k v)
  let empty () = `Lifted (M.empty ())
  let is_empty = function
    | `Bot -> false
    | `Lifted x -> M.is_empty x
  let exists f = function
    | `Bot -> raise (Fn_over_All "exists")
    | `Lifted x -> M.exists f x
  let bindings = function
    | `Bot -> raise (Fn_over_All "bindings")
    | `Lifted x -> M.bindings x
  let mapi f = function
    | `Bot -> `Bot
    | `Lifted x -> `Lifted (M.mapi f x)
end

module MapTop_LiftBot (Domain: Printable.S) (Range: Lattice.S): S with
  type key = Domain.t and
  type value = Range.t =
struct
  module M = MapTop (Domain) (Range)
  include LiftBot (Range) (M)
end

(** Map abstracted by a single (joined) key. *)
module Joined (E: Lattice.S) (R: Lattice.S): S with type key = E.t and type value = R.t =
struct
  type key = E.t
  type value = R.t
  include Lattice.Prod (E) (R)

  let singleton e r = (e, r)
  let exists p (e, r) = p e r
  let for_all p (e, r) = p e r
  let mem e (e', _) = E.leq e e'
  let choose er = er
  let bindings er = [er]
  let remove e ((e', _) as er) =
    if E.leq e' e then
      (E.bot (), R.bot ())
    else
      er
  let map f (e, r) = (e, f r)
  let mapi f (e, r) = (e, f e r)
  let map2 f (e, r) (e', r') = (E.meet e e', f r r')
  let long_map2 f (e, r) (e', r') = (E.join e e', f r r')
  let merge f m1 m2 = failwith "MapDomain.Joined.merge" (* TODO: ? *)
  let fold f (e, r) a = f e r a
  let empty () = (E.bot (), R.bot ())
  let add e r (e', r') = (E.join e e', R.join r r')
  let is_empty (e, _) = E.is_bot e
  let iter f (e, r) = f e r
  let cardinal er =
    if is_empty er then
      0
    else
      1
  let find e (e', r) =
    if E.leq e e' then
      r
    else
      raise Not_found
  let find_opt e (e', r) =
    if E.leq e e' then
      Some r
    else
      None
  let filter p s = failwith "MapDomain.Joined.filter"
  let add_list ers m = List.fold_left (fun acc (e, r) ->
      add e r acc
    ) m ers
  let add_list_set es r m = List.fold_left (fun acc e ->
      add e r acc
    ) m es
  let add_list_fun es f m = List.fold_left (fun acc e ->
      add e (f e) acc
    ) m es

  let leq_with_fct _ _ _ = failwith "MapDomain.Joined.leq_with_fct"
  let join_with_fct _ _ _ = failwith "MapDomain.Joined.join_with_fct"
  let widen_with_fct _ _ _ = failwith "MapDomain.Joined.widen_with_fct"
end
