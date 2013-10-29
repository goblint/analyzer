open Cil
open Pretty
module OMap = Map (* save Ocaml's Map before overwriting it with BatMap *)
open Batteries

module M = Messages


exception Unknown
exception Error

module Val =
struct
  module T =
  struct
    type state = string
    type record = { key: Lval.CilLval.t; loc: location list; state: state }
    type t' = record Set.t * record Set.t (* must, may *)
  end

  include Printable.Std
  include Lattice.StdCousot
  include T
  type t = t'

  (* special variable used for indirection *)
  let alias_var = Cil.makeVarinfo false "@alias" Cil.voidType, `NoOffset
  (* alias structure: x[0].key=alias_var, y[0].key=linked_var *)
  let is_alias (x,y) = x<>Set.empty && (Set.choose x).key=alias_var
  let get_alias (x,y) = (Set.choose y).key

  (* Printing *)
  let string_of_key k = Lval.CilLval.short 80 k
  let string_of_record r =
    let loc xs = String.concat ", " (List.map (fun x -> string_of_int x.line) xs) in
    r.state^" ("^loc r.loc^")"
  let string_of (x,y) =
    let z = Set.diff y x in
    "{ "^String.concat ", " (List.map string_of_record (Set.elements x))^" }, "^
    "{ "^String.concat ", " (List.map string_of_record (Set.elements z))^" }"
  let short i x = string_of x
  include Printable.PrintSimple (struct
    type t' = t
    let name () = "Spec value"
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

  (* transforms May-Sets of length 1 to Must. NOTE: this should only be done if the original set had more than one element! *)
  (* let maybe_must = function May xs when Set.cardinal xs = 1 -> Must (Set.choose xs) | x -> x *)
  (* let may = function Must x -> May (Set.singleton x) | xs -> xs *)
  (* let records = function Must x -> (Set.singleton x) | May xs -> xs *)
  (* let list_of_records = function Must x -> [x] | May xs -> List.of_enum (Set.enum xs) *)
  (* let vnames x = String.concat ", " (List.map (fun r -> string_of_key r.var) (list_of_records x)) *)

  (* creation & manipulation *)
  let make_set k l s = Set.singleton { key=k; loc=l; state=s }
  let make k l s = let v = make_set k l s in v,v
  let make_var_set k = make_set k [] ""
  let make_alias k = make_var_set alias_var, make_var_set k
  let map f (x,y)  = Set.map f x, Set.map f y
  let filter p (x,y) = Set.filter p x, Set.filter p y (* retains top *)
  let union (a,b) (c,d) = Set.union a c, Set.union b d
  let change_key v k = map (fun x -> {x with key=k}) v (* changes key for all elements *)
  let change_state v s = map (fun x -> {x with state=s}) v
  let remove_state v s = filter (fun x -> x.state<>s) v
  let locs ?p:(p=const true) v = filter p v |> map (fun x -> x.loc) |> snd |> Set.elements

  (* predicates *)
  let must   p (x,y) = Set.exists p x || not (Set.is_empty y) && Set.for_all p y
  let may    p (x,y) = Set.exists p y || is_top (x,y)
  let length   (x,y) = Set.cardinal x, Set.cardinal y

  (* properties of records *)
  let in_state s r = r.state = s
end


module Dom =
struct
  module K = Lval.CilLval
  module V = Val
  module MD = MapDomain.MapBot (Lval.CilLval) (Val)
  include MD
  (* Used to access additional functions of Map.
  Can't use BatMap because type is not compatible with MD.
  Also avoids dependencies for other files using the following functions. *)
  module MDMap = OMap.Make (Lval.CilLval) (* why does OMap.Make (K) not work? *)
  open V.T

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

  (* used for special variables *)
  let get_record k m =
    if mem k m then
      let x,y = find k m in
      if Set.is_empty x then None
      else Some (Set.choose x)
    else None
  let add_record k r m =
    let x = Set.singleton r in
    add k (x,x) m
  let get_value k m =
    if mem k m then find k m
    else Set.empty, Set.empty
  let extend_value k v m =
    if mem k m then
      add k (V.union (find k m) v) m
    else
      add k v m
  let without_special_vars m = filter (fun k v -> String.get (V.string_of_key k) 0 <> '@') m

  (* helper functions *)
  let filter_values p m = (* filters all values in the map and flattens result *)
    let flatten_sets = List.fold_left Set.union Set.empty in
    without_special_vars m
    |> filter (fun k v -> V.may p v && not (V.is_alias v))
    |> MDMap.bindings |> List.map (fun (k,v) -> V.filter p v)
    |> List.split |> (fun (x,y) -> flatten_sets x, flatten_sets y)
  let filter_records k p m = (* filters both sets of k *)
    if mem k m then V.filter p (find' k m) else Set.empty, Set.empty

  let may  k p m = mem k m && V.may  p (find' k m)
  let must k p m = mem k m && V.must p (find' k m)

  let warn ?may:(may=false) ?loc:(loc=[!Tracing.current_loc]) msg =
    M.report ~loc:(List.last loc) (if may then "{yellow}MAYBE "^msg else "{YELLOW}"^msg)

  let unknown k m = add' k (V.top ()) m
  let is_unknown k m = if mem k m then V.is_top (find' k m) else false


  (* domain specific *)
  (* let find_records k m = if mem k m then V.records (find k m) else Set.empty *)
  let goto k loc state m = add' k (V.make k loc state) m
  let may_goto k loc state m = let v = V.join (find' k m) (V.make k loc state) in add' k v m
  let is_may k m = mem k m && let x,y = V.length (find' k m) in x=0 && y>0
  let in_state     k s m = must k (V.in_state s) m
  let may_in_state k s m = may  k (V.in_state s) m
  let get_states k m = if not (mem k m) then [] else find' k m |> V.map (fun x -> x.state) |> snd |> Set.elements

  let string_of_state k m = if not (mem k m) then "?" else V.string_of (find' k m)
  let string_of_key k = K.short 80 k
  let string_of_keys rs = Set.map (fun r -> V.string_of_key r.key) rs |> Set.elements |> String.concat ", "
  let string_of_entry k m = string_of_key k ^ ": " ^ string_of_state k m
  let string_of_map m = List.map (fun (k,v) -> string_of_entry k m) (MDMap.bindings m)
end
