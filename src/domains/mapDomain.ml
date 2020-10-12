(** Specification and functors for maps. *)

open Pretty
module ME = Messages
module GU = Goblintutil

module type S =
sig
  include Lattice.S
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
  (*  val mapi: (key -> value -> value) -> t -> t*)
  val fold: (key -> value -> 'a -> 'a) -> t -> 'a -> 'a

  val add_list: (key * value) list -> t -> t
  val add_list_set: key list -> value -> t -> t
  val add_list_fun: key list -> (key -> value) -> t -> t
  val filter_class: int -> t -> t

  val for_all: (key -> value -> bool) -> t -> bool
  val map2: (value -> value -> value) -> t -> t -> t
  val long_map2: (value -> value -> value) -> t -> t -> t
  val merge : (key -> value option -> value option -> value option) -> t -> t -> t
  (*  val fold2: (key -> value -> value -> 'a -> 'a) -> t -> t -> 'a -> 'a*)

  val widen_with_fct: (value -> value -> value) -> t -> t -> t
  (* Widen using a custom widening function for value rather than the default one for value *)
  val join_with_fct: (value -> value -> value) -> t -> t -> t
  (* Join using a custom join function for value rather than the default one for value *)
  val leq_with_fct: (value -> value -> bool) -> t -> t -> bool
  (* Leq test using a custom leq function for value rather than the default one provided for value *)
end

module type Groupable =
sig
  include Printable.S
  val classify: t -> int
  val class_name: int -> string
  val trace_enabled: bool
end

module StripClasses (G: Groupable) =
struct
  include G
  let classify x = 0
end

(* Just a global hack for tracing individual variables. *)

module PMap (Domain: Groupable) (Range: Lattice.S) =
struct
  module M = Deriving.Map.Make (Domain)

  include Printable.Std
  type key = Domain.t
  type value = Range.t
  type t = Range.t M.t [@@deriving to_yojson] (* key -> value  mapping *)
  let trace_enabled = Domain.trace_enabled

  (* And some braindead definitions, because I would want to do
   * include Map.Make (Domain) with type t = Range.t t *)
  let add = M.add
  let remove = M.remove
  let find = M.find
  let find_opt = M.find_opt
  let mem = M.mem
  let iter = M.iter
  let map = M.map
  let mapi = M.mapi
  let fold = M.fold
  let filter = M.filter
  (* And one less brainy definition *)
  let for_all2 = M.equal
  let equal = for_all2 Range.equal
  let compare x y = if equal x y then 0 else M.compare Range.compare x y
  let merge = M.merge
  let for_all = M.for_all
  let find_first = M.find_first
  let hash xs = fold (fun k v a -> a + (Domain.hash k * Range.hash v)) xs 0


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

  let short _ x = "mapping"
  let isSimple _ = false

  let pretty_f short () mapping =
    let groups =
      let group_fold key itm gps =
        let cl = Domain.classify key in
        match gps with
        | (a,n) when cl <>  n -> ((cl,(M.add key itm M.empty))::a, cl)
        | (a,_) -> ((fst (List.hd a),(M.add key itm (snd (List.hd a))))::(List.tl a),cl) in
      List.rev (fst (fold group_fold mapping ([],min_int)))
    in
    let f key st dok =
      if ME.tracing && trace_enabled && !ME.tracevars <> [] &&
         not (List.mem (Domain.short 80 key) !ME.tracevars) then
        dok
      else
        dok ++ (if Range.isSimple st then dprintf "%a -> %a\n" else
                  dprintf "%a -> \n  @[%a@]\n") Domain.pretty key Range.pretty st
    in
    let group_name a () = text (Domain.class_name a) in
    let pretty_group  map () = fold f map nil in
    let pretty_groups rest map =
      match (fst map) with
      | 0 ->  rest ++ pretty_group (snd map) ()
      | a -> rest ++ dprintf "@[%t {\n  @[%t@]}@]\n" (group_name a) (pretty_group (snd map)) in
    let content () = List.fold_left pretty_groups nil groups in
    dprintf "@[%s {\n  @[%t@]}@]" (short 60 mapping) content

  let pretty () x = pretty_f short () x

  let filter_class g m =
    fold (fun key value acc -> if Domain.classify key = g then add key value acc else acc) m M.empty

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "PMap: %a not leq %a" pretty x pretty y
  let printXml f xs =
    let print_one k v =
      BatPrintf.fprintf f "<key>\n%s</key>\n%a" (Goblintutil.escape (Domain.short 800 k)) Range.printXml v
    in
    BatPrintf.fprintf f "<value>\n<map>\n";
    iter print_one xs;
    BatPrintf.fprintf f "</map>\n</value>\n"

  let arbitrary () = QCheck.always M.empty (* S TODO: non-empty map *)
end


module MapBot (Domain: Groupable) (Range: Lattice.S) (*: S with
  type key = Domain.t and
type value = Range.t and
type t = Range.t Map.Make(Domain).t *) =
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
  let bot () = M.empty
  let is_top _ = false
  let is_bot = M.is_empty

  let pretty_diff () ((m1:t),(m2:t)): Pretty.doc =
    let p key value =
      not (try Range.leq value (find key m2) with Not_found -> false)
    in
    let report key v1 v2 =
      Pretty.dprintf "Map: %a =@?@[%a@]"
        Domain.pretty key Range.pretty_diff (v1,v2)
    in
    let diff_key k v = function
      | None   when p k v -> Some (report k v (find k m2))
      | Some w when p k v -> Some (w++Pretty.line++report k v (find k m2))
      | x -> x
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

module MapTop (Domain: Groupable) (Range: Lattice.S): S with
  type key = Domain.t and
type value = Range.t and
type t = Range.t Map.Make(Domain).t =
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
  let top () = M.empty
  let bot () = Lattice.unsupported "partial map bot"
  let is_top = M.is_empty
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
    let p key value =
      not (try Range.leq value (find key m2) with Not_found -> true)
    in
    let report key v1 v2 =
      Pretty.dprintf "Map: %a =@?@[%a@]"
        Domain.pretty key Range.pretty_diff (v1,v2)
    in
    let diff_key k v = function
      | None   when p k v -> Some (report k v (find k m2))
      | Some w when p k v -> Some (w++Pretty.line++report k v (find k m2))
      | x -> x
    in
    match fold diff_key m1 None with
    | Some w -> w
    | None -> Pretty.dprintf "No binding grew."
end

exception Fn_over_All of string

module MapBot_LiftTop (Domain: Groupable) (Range: Lattice.S) (* : S with
  type key = Domain.t and
type value = Range.t *) =
struct
  module M = MapBot (Domain) (Range)
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

  let filter_class i = function
    | `Top -> `Top
    | `Lifted x -> `Lifted (M.filter_class i x)

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

end

module MapTop_LiftBot (Domain: Groupable) (Range: Lattice.S): S with
  type key = Domain.t and
type value = Range.t =
struct
  module M = MapTop (Domain) (Range)
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

  let filter_class i = function
    | `Bot -> `Bot
    | `Lifted x -> `Lifted (M.filter_class i x)

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
end
