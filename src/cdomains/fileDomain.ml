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
    type loc = location list
    type mode = Read | Write
    type state = Open of string*mode | Closed | Error
    type record = { key: Lval.CilLval.t; loc: loc; state: state }
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
    let loc xs = String.concat ", " (List.map (fun r -> string_of_int r.line) xs) in
    let mode r = match r with Read -> "Read" | Write -> "Write" in
    match r.state with
    | Open(filename, m) -> "open("^filename^", "^mode m^") ("^loc r.loc^")"
    | Closed -> "closed ("^loc r.loc^")"
    | Error  -> "error ("^loc r.loc^")"
  let string_of (x,y) =
    if is_alias (x,y) then
      "alias for "^string_of_key @@ get_alias (x,y)
    else let z = Set.diff y x in
      "{ "^String.concat ", " (List.map string_of_record (Set.elements x))^" }, "^
      "{ "^String.concat ", " (List.map string_of_record (Set.elements z))^" }"
  let short i v = string_of v
  include Printable.PrintSimple (struct
    type t' = t
    let name () = "File handles"
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

  (* creation & manipulation *)
  let make_set k l s = Set.singleton { key=k; loc=l; state=s }
  let make_var_set k = make_set k [] Closed
  let make_alias k = make_var_set alias_var, make_var_set k
  let map f (x,y)  = Set.map f x, Set.map f y
  let filter p (x,y) = Set.filter p x, Set.filter p y (* retains top *)
  let union (a,b) (c,d) = Set.union a c, Set.union b d
  let change_key v k = map (fun x -> {x with key=k}) v (* changes key for all elements *)

  (* predicates *)
  let must   p (x,y) = Set.exists p x || not (Set.is_empty y) && Set.for_all p y
  let may    p (x,y) = Set.exists p y || is_top (x,y)

  (* properties of records (e.g. used by Dom.report) *)
  let opened   r = r.state <> Closed && r.state <> Error
  let closed   r = r.state = Closed
  let writable r = match r.state with Open((_,Write)) -> true | _ -> false
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

  let must k p m = if mem k m then V.must p (find' k m) else false
  let may  k p m = if mem k m then V.may  p (find' k m) else false

  let warn ?may:(may=false) ?loc:(loc=[!Tracing.current_loc]) msg =
    M.report ~loc:(List.last loc) (if may then "{yellow}MAYBE "^msg else "{YELLOW}"^msg)

  let unknown k m = add' k (V.top ()) m
  let is_unknown k m = if mem k m then V.is_top (find' k m) else false


  (* domain specific *)
  (* returns a tuple (thunk, result) *)
  let report_ ?neg:(neg=false) k p msg m =
    let f ?may:(may=false) msg =
      let f () = warn ~may:may msg in
      f, if may then `May true else `Must true in
    let mf = (fun () -> ()), `Must false in
    if mem k m then
      let p = if neg then not % p else p in
      let v = find' k m in
      if V.must p v then f msg (* must *)
      else if V.may p v then f ~may:true msg (* may *)
      else mf (* none *)
    else if neg then f msg else mf

  let report ?neg:(neg=false) k p msg m = (fst (report_ ~neg:neg k p msg m)) () (* evaluate thunk *)

  let reports k xs m =
    let uncurry (neg, p, msg) = report_ ~neg:neg k p msg m in
    let f result x = if snd (uncurry x) = result then Some (fst (uncurry x)) else None in
    let must_true = BatList.filter_map (f (`Must true)) xs in
    let may_true  = BatList.filter_map (f (`May true)) xs in
    (* output first must and first may *)
    if List.length must_true > 0 then (List.hd must_true) ();
    if List.length may_true  > 0 then (List.hd may_true) ()

  let fopen k loc filename mode m =
    if is_unknown k m then m else
    let mode = match String.lowercase mode with "r" -> Read | _ -> Write in
    let x = V.make_set k loc (Open(filename, mode)) in
    add' k (x,x) m
  let fclose k loc m =
    if is_unknown k m then m else
    let x = V.make_set k loc Closed in
    change k (x,x) m
  let error k m =
    if is_unknown k m then m else
    let loc = if mem k m then let v = Set.choose (snd (find' k m)) in v.loc else [] in
    let x = V.make_set k loc Error in
    change k (x,x) m
  let success k m =
    if is_unknown k m then m else
    let v = find' k m in
    let x,y = V.filter V.opened v in
    let v = if x = Set.empty && Set.cardinal y = 1 then y,y else x,y in
    change k v m
end
