open Cil
open Pretty
module OMap = Map (* save Ocaml's Map before overwriting it with BatMap *)
open Batteries

module M = Messages


exception Unknown
exception Error

(* signature for map entries *)
module type S =
sig
  include Lattice.S
  type k = Lval.CilLval.t (* key *)
  type s (* state is defined by Impl *)
  type r (* record *)

  (* printing *)
  val string_of: t -> string
  val string_of_key: k -> string
  val string_of_record: r -> string

  (* constructing *)
  val make: k -> location list -> s -> t

  (* manipulation *)
  val map: (r -> r) -> t -> t
  val filter: (r -> bool) -> t -> t
  val union: t -> t -> t
  val set_key: k -> t -> t
  val set_state: s -> t -> t
  val remove_state: s -> t -> t

  (* deconstructing *)
  val split: t -> r Set.t * r Set.t
  val map': (r -> 'a) -> t -> 'a Set.t * 'a Set.t
  val filter': (r -> bool) -> t -> r Set.t * r Set.t
  val length: t -> int * int

  (* predicates *)
  val must: (r -> bool) -> t -> bool
  val may: (r -> bool) -> t -> bool
  (* properties of records *)
  val key: r -> k
  val loc: r -> location list
  val edit_loc: (location list -> location list) -> r -> r
  val state: r -> s
  val in_state: s -> r -> bool

  (* special variables *)
  val get_record: t -> r option
  (* val make_record: k -> location list -> s -> r *)
  val make_var: k -> t
  val from_tuple: r Set.t * r Set.t -> t

  (* aliasing *)
  val is_alias: t -> bool
  val get_alias: t -> k
  val make_alias: k -> t
end

module Value (Impl: sig
    type s (* state *)
    val name: string
    val var_state: s
    val string_of_state: s -> string
  end) : S with type s = Impl.s =
struct
  type k = Lval.CilLval.t
  type s = Impl.s
  type r = { key: k; loc: location list; state: s }
  type t = r Set.t * r Set.t (* must, may *)

  include Printable.Std
  include Lattice.StdCousot

  (* special variable used for indirection *)
  let alias_var = Cil.makeVarinfo false "@alias" Cil.voidType, `NoOffset
  (* alias structure: x[0].key=alias_var, y[0].key=linked_var *)
  let is_alias (x,y) = x<>Set.empty && (Set.choose x).key=alias_var
  let get_alias (x,y) = (Set.choose y).key

  (* Printing *)
  let string_of_key k = Lval.CilLval.short 80 k
  let string_of_loc xs = String.concat ", " (List.map (fun x -> string_of_int x.line) xs)
  let string_of_record r = Impl.string_of_state r.state^" ("^string_of_loc r.loc^")"
  let string_of (x,y) =
    if is_alias (x,y) then
      "alias for "^string_of_key @@ get_alias (x,y)
    else let z = Set.diff y x in
      "{ "^String.concat ", " (List.map string_of_record (Set.elements x))^" }, "^
      "{ "^String.concat ", " (List.map string_of_record (Set.elements z))^" }"
  let short i x = string_of x
  include Printable.PrintSimple (struct
    type t' = t
    let name () = Impl.name
    let short = short
  end)

  (* Printable.S *)
  let equal = Util.equals
  let hash = Hashtbl.hash
  (* Lattice.S must be implemented to be used as Range for MapDomain *)
  (* let leq x y = equal y (join x y) *)
  let leq  (a,b) (c,d) = Set.subset c a && Set.subset b d
  let join (a,b) (c,d) = (* M.report ("JOIN\tx: " ^ (string_of (a,b)) ^ "\n\ty: " ^ (string_of (c,d))); *)
    let r = Set.intersect a c, Set.union b d in
    (* M.report @@ "result: "^string_of r; *)
    r
  let meet x y = M.report ("MEET\tx: " ^ (string_of x) ^ "\n\ty: " ^ (string_of y)); x
  (* top/bot are handled by MapDomain, only bot () gets called *)
  let top ()   = Set.empty, Set.empty
  let is_top x = x=top ()
  let bot ()   = raise Unknown (* called in MapDomain.MapBot(K)(V).find *)
  let is_bot x = false

  (* constructing & manipulation *)
  let make_record k l s = { key=k; loc=l; state=s }
  let make k l s = let v = Set.singleton (make_record k l s) in v,v
  let map f (x,y)  = Set.map f x, Set.map f y
  let filter p (x,y) = Set.filter p x, Set.filter p y (* retains top *)
  let union (a,b) (c,d) = Set.union a c, Set.union b d
  let set_key k v = map (fun x -> {x with key=k}) v (* changes key for all elements *)
  let set_state s v = map (fun x -> {x with state=s}) v
  let remove_state s v = filter (fun x -> x.state<>s) v
  let locs ?p:(p=const true) v = filter p v |> map (fun x -> x.loc) |> snd |> Set.elements

  (* deconstructing *)
  let split = identity
  let length (x,y) = Set.cardinal x, Set.cardinal y
  let map' = map
  let filter' = filter

  (* predicates *)
  let must   p (x,y) = Set.exists p x || not (Set.is_empty y) && Set.for_all p y
  let may    p (x,y) = Set.exists p y || is_top (x,y)

  (* properties of records *)
  let key r = r.key
  let loc r = r.loc
  let edit_loc f r = {r with loc=f r.loc}
  let state r = r.state
  let in_state s r = r.state = s

  (* special variables *)
  let get_record (x,y) = if Set.is_empty x then None else Some (Set.choose x)
  let make_var_record k = make_record k [] Impl.var_state
  let make_var_set k = Set.singleton (make_var_record k)
  let make_var k = make_var_set k, make_var_set k
  let make_alias k = make_var_set alias_var, make_var_set k
  let from_tuple = identity
end


module Domain (V: S) =
struct
  module K = Lval.CilLval
  module V = V
  module MD = MapDomain.MapBot (Lval.CilLval) (V)
  include MD
  (* Used to access additional functions of Map.
  Can't use BatMap because type is not compatible with MD.
  Also avoids dependencies for other files using the following functions. *)
  module MDMap = OMap.Make (Lval.CilLval) (* why does OMap.Make (K) not work? *)

  (* Map functions *)
  (* find that resolves aliases *)
  let find' k m = let v = find k m in if V.is_alias v then find (V.get_alias v) m else v
  let find_option k m = if mem k m then Some(find' k m) else None
  let get_alias k m = (* target: returns Some k' if k links to k' *)
    if mem k m && V.is_alias (find k m) then Some (V.get_alias (find k m)) else None
  let get_aliased k m = (* sources: get list of keys that link to k *)
    (* iter (fun k' (x,y) -> if V.is_alias (x,y) then print_endline ("alias "^V.string_of_key k'^" -> "^V.string_of_key (Set.choose y).key)) m; *)
    (* TODO V.get_alias v=k somehow leads to Out_of_memory... *)
    filter (fun k' v -> V.is_alias v && V.string_of_key (V.get_alias v)=V.string_of_key k) m |> MDMap.bindings |> List.map fst
  let get_aliases k m = (* get list of all other keys that have the same pointee *)
    match get_alias k m with
    | Some k' -> [k] (* k links to k' *)
    | None -> get_aliased k m (* k' that link to k *)
  let alias a b m = (* link a to b *)
    (* if b is already an alias, follow it... *)
    let b' = get_alias b m |? b in
    (* add an entry for key a, that points to b' *)
    add a (V.make_alias b') m
  let remove' k m = (* fixes keys that link to k before removing it *)
    if mem k m && not (V.is_alias (find k m)) then (* k might be aliased *)
      let v = find k m in
      match get_aliased k m with
      | [] -> remove k m (* nothing links to k *)
      | k'::xs -> let m = add k' v m in (* set k' to v, link xs to k', finally remove k *)
          (* List.map (fun x -> x.vname) (k'::xs) |> String.concat ", " |> print_endline; *)
          List.fold_left (fun m x -> alias x k' m) m xs |> remove k
    else remove k m (* k not in m or an alias *)
  let add' k v m =
    remove' k m (* fixes keys that might have linked to k *)
    |> add k v (* set new value *)
  let change k v m = (* if k is an alias, replace its pointee *)
    add (get_alias k m |? k) v m

  (* special variables *)
  let get_record k m = Option.bind (find_option k m) V.get_record
  let edit_record k f m =
    let v = find_option k m |? V.make_var k in
    add k (V.map f v) m
  let get_value k m = find_option k m |> Option.map_default V.split (Set.empty,Set.empty)
  let extend_value k v' m =
    let v = V.from_tuple v' in
    if mem k m then
      add k (V.union (find k m) v) m
    else
      add k v m
  let union (a,b) (c,d) = Set.union a c, Set.union b d
  let is_special_var k = String.get (V.string_of_key k) 0 = '@'
  let without_special_vars m = filter (fun k v -> not @@ is_special_var k) m

  (* functions needed for enter & combine *)
  (* only keep globals, aliases to them and special veriables *)
  let only_globals m = filter (fun k v ->  (fst k).vglob || V.is_alias v && (fst (V.get_alias v)).vglob || is_special_var k) m
  (* adds all the bindings from m2 to m1 (overwrites!) *)
  let add_all m1 m2 = add_list (MDMap.bindings m2) m1

  (* callstack for locations *)
  let callstack_var = Cil.makeVarinfo false "@callstack" Cil.voidType, `NoOffset
  let callstack m = get_record callstack_var m |> Option.map_default V.loc []
  let string_of_callstack m = " [call stack: "^String.concat ", " (List.map (fun x -> string_of_int x.line) (callstack m))^"]"
  let edit_callstack f m = edit_record callstack_var (V.edit_loc f) m


  (* predicates *)
  let must k p m = mem k m && V.must p (find' k m)
  let may  k p m = mem k m && V.may  p (find' k m)
  let is_may k m = mem k m && let x,y = V.length (find' k m) in x=0 && y>0

  let filter_values p m = (* filters all values in the map and flattens result *)
    let flatten_sets = List.fold_left Set.union Set.empty in
    without_special_vars m
    |> filter (fun k v -> V.may p v && not (V.is_alias v))
    |> MDMap.bindings |> List.map (fun (k,v) -> V.filter' p v)
    |> List.split |> (fun (x,y) -> flatten_sets x, flatten_sets y)
  let filter_records k p m = (* filters both sets of k *)
    if mem k m then V.filter' p (find' k m) else Set.empty, Set.empty

  let unknown k m = add' k (V.top ()) m
  let is_unknown k m = if mem k m then V.is_top (find' k m) else false

  (* printing *)
  let string_of_state k m = if not (mem k m) then "?" else V.string_of (find' k m)
  let string_of_key k = V.string_of_key k
  let string_of_keys rs = Set.map (V.string_of_key % V.key) rs |> Set.elements |> String.concat ", "
  let string_of_entry k m = string_of_key k ^ ": " ^ string_of_state k m
  let string_of_map m = List.map (fun (k,v) -> string_of_entry k m) (MDMap.bindings m)

  let warn ?may:(may=false) ?loc:(loc=[!Tracing.current_loc]) msg =
    M.report ~loc:(List.last loc) (if may then "{yellow}MAYBE "^msg else "{YELLOW}"^msg)

  (* getting keys from Cil Lvals *)
  let sprint f x = Pretty.sprint 80 (f () x)

  let key_from_lval lval = match lval with (* try to get a Lval.CilLval from Cil.Lval *)
    | Var v1, o1 -> v1, Lval.CilLval.of_ciloffs o1
    | Mem Lval(Var v1, o1), o2 -> v1, Lval.CilLval.of_ciloffs (addOffset o1 o2)
    (* | Mem exp, o1 -> failwith "not implemented yet" (* TODO use query_lv *) *)
    | _ -> Cil.makeVarinfo false ("?"^sprint d_exp (Lval lval)) Cil.voidType, `NoOffset (* TODO *)

  let keys_from_lval lval ask = (* use MayPointTo query to get all possible pointees of &lval *)
    (* print_query_lv ctx.ask (AddrOf lval); *)
    let query_lv ask exp = match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) -> Queries.LS.elements l
      | _ -> []
    in
    let exp = AddrOf lval in
    let xs = query_lv ask exp in (* MayPointTo -> LValSet *)
    M.debug @@ "MayPointTo "^sprint d_exp exp^" = ["
      ^String.concat ", " (List.map string_of_key xs)^"]";
    xs
end
