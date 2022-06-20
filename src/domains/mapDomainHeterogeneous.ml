(** Specification and functors for maps. *)

open Pretty
module ME = Messages
module GU = Goblintutil

type 'a mt = (module Lattice.S with type t = 'a)

module type PS =
sig
  include Printable.S
  type 'a key
  (** The type of the map keys. *)

  type 'a value
  (** The type of the values. *)

  type binding
  type mapper = { map : 'a. 'a key -> 'a value -> 'a value }
  type mapper2 = { map2 : 'a. 'a key -> 'a value -> 'a value -> 'a value }
  type merger = { merge : 'a. 'a key -> 'a value option -> 'a value option -> 'a value option }

  val add: 'a key -> 'a value -> t -> t
  val remove: 'a key -> t -> t
  val find: 'a key -> t -> 'a value
  val find_opt: 'a key -> t -> 'a value option
  val mem: 'a key -> t -> bool
  val iter: (binding -> unit) -> t -> unit
  val map: mapper -> t -> t
  val filter: (binding -> bool) -> t -> t
  val fold: (binding -> 'a -> 'a) -> t -> 'a -> 'a

  val add_list: binding list -> t -> t
  val add_list_set: 'a key list -> 'a value -> t -> t

  val for_all: (binding -> bool) -> t -> bool
  val map2: mapper2 -> t -> t -> t
  val long_map2: mapper2 -> t -> t -> t
  val merge : merger -> t -> t -> t

  val cardinal: t -> int
  val choose: t -> binding
  val singleton: 'a key -> 'a value -> t
  val empty: unit -> t
  val is_empty: t -> bool
  val exists: (binding -> bool) -> t -> bool
  val bindings: t -> binding list
end

module type S =
sig
  include PS
  include Lattice.S with type t := t

  type equality_comparer = { equal : 'a. 'a key -> 'a value -> 'a value -> bool }

  val widen_with_fct: mapper2 -> t -> t -> t
  (* Widen using a custom widening function for value rather than the default one for value *)
  val join_with_fct: mapper2 -> t -> t -> t
  (* Join using a custom join function for value rather than the default one for value *)
  val leq_with_fct: equality_comparer -> t -> t -> bool
  (* Leq test using a custom leq function for value rather than the default one provided for value *)
end

module Map
  (* : sig
     include PS' with type 'a value = 'a

     val token : (module Lattice.S with type t = 'a) -> 'a key
     end *)
=
struct
  module M = Hmap.Make(struct type 'a t = 'a mt * string end)

  include Printable.Std
  type 'a key = 'a M.key
  type 'a value = 'a
  type binding = M.binding = B : 'a key * 'a value -> binding
  type mapper = M.mapper = { map : 'a. 'a key -> 'a value -> 'a value }
  type mapper2 = { map2 : 'a. 'a key -> 'a value -> 'a value -> 'a value }
  type merger = M.merger = { merge : 'a. 'a key -> 'a value option -> 'a value option -> 'a value option }
  type t = M.t (* key -> value  mapping *)

  let token (m : (module Lattice.S with type t = 'a)) id = (M.Key.create (m, id))

  (* And some braindead definitions, because I would want to do
   * include Map.Make (Domain) with type t = Range.t t *)
  let add = M.add
  let remove = M.rem
  let find_opt = M.find
  let find k m = match find_opt k m with Some x -> x | _ -> raise Not_found
  let mem = M.mem
  let iter = M.iter
  let map = M.map
  let fold = M.fold
  let filter = M.filter
  (* And one less brainy definition *)
  let for_all2 = M.equal
  let equal x y = x == y || for_all2 { equal = fun (type a) k a b -> let module R = (val (fst (M.Key.info k) : a mt)) in R.equal a b } x y
  let compare x y = if equal x y then 0 else M.compare { compare = fun (type a) k a b -> let module R = (val (fst (M.Key.info k) : a mt)) in R.compare a b } x y
  let merge = M.merge
  let for_all = M.for_all
  let hash xs = fold (fun (B(k,v)) a -> let module R = (val (fst (M.Key.info k))) in a + (Hashtbl.hash k * R.hash v)) xs 0

  let cardinal = M.cardinal
  let choose = M.get_any_binding
  let singleton = M.singleton
  let empty () = M.empty
  let is_empty = M.is_empty
  let exists = M.exists
  let bindings = M.bindings


  let add_list keyvalues m =
    List.fold_left (fun acc (B(key,value)) -> add key value acc) m keyvalues

  let add_list_set keys value m =
    List.fold_left (fun acc key -> add key value acc) m keys

  let add_list_fun keys f m =
    List.fold_left (fun acc key -> add key (f key) acc) m keys

  let long_map2 op =
    let f (type a) k (v1 : a option) v2 =
      match v1, v2 with
      | Some v1, Some v2 -> Some (op.map2 k v1 v2)
      | Some _, _ -> v1
      | _, Some _ -> v2
      | _ -> None
    in
    M.merge { merge = f }

  let map2 op =
    (* Similar to the previous, except we ignore elements that only occur in one
     * of the mappings, so we start from an empty map *)
    let f k v1 v2 =
      match v1, v2 with
      | Some v1, Some v2 -> Some (op.map2 k v1 v2)
      | _ -> None
    in
    M.merge { merge = f }

  let show x = "mapping"

  let pretty () mapping =
    let text () = Pretty.text in
    let f (B(key, value) : binding) dok =
      if ME.tracing && trace_enabled && !ME.tracevars <> [] &&
         not (List.mem (snd (M.Key.info key)) !ME.tracevars) then
        dok
      else
        dok ++ dprintf "%a ->@?  @[%a@]\n" text (snd (M.Key.info key)) (let module R = (val (fst (M.Key.info key))) in R.pretty) value
    in
    let pretty_group () map = fold f map nil in
    let content () = pretty_group () mapping in
    dprintf "@[%s {\n  @[%t@]}@]" (show mapping) content
  let printXml _ = failwith "not supported; keys cannot be serialized xml"
  let to_yojson _ = failwith "not supported; keys cannot be serialized json"

  let printXml f xs =
    let print_one (B (k, v)) =
      BatPrintf.fprintf f "<key>\n%s</key>\n%a" (XmlUtil.escape (snd (M.Key.info k))) (let module R = (val (fst (M.Key.info k))) in R.printXml) v
    in
    BatPrintf.fprintf f "<value>\n<map>\n";
    iter print_one xs;
    BatPrintf.fprintf f "</map>\n</value>\n"

  let to_yojson xs =
    let f (B (k, v)) = (snd (M.Key.info k), (let module R = (val (fst (M.Key.info k))) in R.to_yojson v)) in
    `Assoc (xs |> M.bindings |> List.map f)

  let arbitrary () = QCheck.always M.empty (* S TODO: non-empty map *)
end


module MapSetJmp = struct
  include Map

  let bot_map = ref (empty ())

  let token (m : (module Lattice.S with type t = 'a)) bot id =
    let token = token m id in
    bot_map := add token bot !bot_map;
    token

  type equality_comparer = M.equality_comparer = { equal: 'a. 'a key -> 'a value -> 'a value -> bool }

  let leq_with_fct f m1 m2 = (* TODO use merge or sth faster? *)
    (* For each key-value in m1, the same key must be in m2 with a geq value: *)
    let p (B(key, value)) =
      try f.equal key value (find key m2) with Not_found -> false
    in
    m1 == m2 || for_all p m1

  let leq a b =
    let result = leq_with_fct { equal = (fun (type a) k a b -> let module R = (val (fst (M.Key.info k) : a mt)) in R.leq a b) } a b in
    if Messages.tracing && not (M.is_empty a && M.is_empty b) then Messages.tracel "hetmap" "%s\n%a\n%a\n%b\n" "leq" pretty a pretty b result;
    result


  let find (type a) k m = try find k m with | Not_found -> let module R = (val (fst (M.Key.info k) : a mt)) in R.top ()
  let top () = empty ()
  let bot () = !bot_map
  let is_top _ = Lattice.unsupported "MapSetJmp.is_top"
  let is_bot _ = Lattice.unsupported "MapSetJmp.is_bot"

  (* let cleanup m = fold (fun k v m -> if Range.is_top v then remove k m else m) m m *)

  let logable str f a b =
    let result = f a b in
    if Messages.tracing && not (M.is_empty a && M.is_empty b) then Messages.tracel "hetmap" "%s\n%a\n%a\n%a\n" str pretty a pretty b pretty result;
    result

  let meet = logable "meet" (fun m1 m2 -> if m1 == m2 then m1 else long_map2 { map2 = fun (type a) k a b -> let module R = (val (fst (M.Key.info k) : a mt)) in R.meet a b } m1 m2)

  let join_with_fct f = logable "join" (fun m1 m2 -> if m1 == m2 then m1 else long_map2 f m1 m2)
  let join = join_with_fct { map2 = fun (type a) k a b -> let module R = (val (fst (M.Key.info k) : a mt)) in R.join a b }

  let widen_with_fct f = logable "widen" (long_map2 f)
  let widen = widen_with_fct { map2 = fun (type a) k a b -> let module R = (val (fst (M.Key.info k) : a mt)) in R.widen a b }
  let narrow = logable "narrow" (long_map2 { map2 = fun (type a) k a b -> let module R = (val (fst (M.Key.info k) : a mt)) in R.narrow a b })

  let pretty_diff () ((m1:t),(m2:t)): Pretty.doc =
    let p (type a) key value =
      let module R = (val (fst (M.Key.info key) : a mt)) in
      not (try R.leq value (find key m2) with Not_found -> false)
    in
    let report (type a) key v1 v2 =
      let module R = (val (fst (M.Key.info key) : a mt)) in
      Pretty.dprintf "Map: %a =@?@[%a@]"
        (fun () -> text) (snd (M.Key.info key)) R.pretty_diff (v1,v2)
    in
    let diff_key (B(k, v)) = function
      | None   when p k v -> Some (report k v (find k m2))
      | Some w when p k v -> Some (w++Pretty.line++report k v (find k m2))
      | x -> x
    in
    match fold diff_key m1 None with
    | Some w -> w
    | None -> Pretty.dprintf "No binding grew."
end
