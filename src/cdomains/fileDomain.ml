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
    type record = { var: varinfo; loc: loc; state: state }
    type t' = record Set.t * record Set.t (* must, may *)
  end

  include Printable.Std
  include Lattice.StdCousot
  include T
  type t = t'

  (* special variable used for indirection *)
  let alias_var = Cil.makeVarinfo false "@alias" Cil.voidType
  let var_set var = Set.singleton {var=var; loc=[]; state=Closed}
  (* alias structure: x[0].var=alias_var, y[0].var=linked_var *)
  let is_alias (x,y) = x<>Set.empty && (Set.choose x).var=alias_var
  let get_alias (x,y) = (Set.choose y).var
  let make_alias var = var_set alias_var, var_set var

  let string_of_record r =
    let loc xs = String.concat ", " (List.map (fun r -> string_of_int r.line) xs) in
    let mode r = match r with Read -> "Read" | Write -> "Write" in
    match r.state with
    | Open(filename, m) -> "open("^filename^", "^(mode m)^") ("^(loc r.loc)^")"
    | Closed -> "closed ("^(loc r.loc)^")"
    | Error  -> "error ("^(loc r.loc)^")"

  let string_of (x,y) =
    let z = Set.diff y x in
    "{ "^(String.concat ", " (List.map string_of_record (Set.elements x)))^" }, "^
    "{ "^(String.concat ", " (List.map string_of_record (Set.elements z)))^" }"

  let short i v = string_of v

  include Printable.PrintSimple (struct
    type t' = t
    let name () = "File handles"
    let short = short
  end)

  let create v l s = { var=v; loc=l; state=s }
  let map f (x,y)  = Set.map f x, Set.map f y
  let rebind x var = map (fun x -> {x with var=var}) x

  let equal = Util.equals
  (* let leq x y = equal y (join x y) *)
  let hash = Hashtbl.hash
  let leq  (a,b) (c,d) = Set.subset c a && Set.subset b d
  let join (a,b) (c,d) = (* M.report ("JOIN\tx: " ^ (string_of (a,b)) ^ "\n\ty: " ^ (string_of (c,d))); *)
    let r = Set.intersect a c, Set.union b d in
    (* M.report ("result: "^(string_of r)); *)
    r
  let meet x y = M.report ("MEET\tx: " ^ (string_of x) ^ "\n\ty: " ^ (string_of y)); x
  (* top/bot are handled by MapDomain, only bot () gets called *)
  let top ()   = Set.empty, Set.empty
  let is_top x = x=top ()
  let bot ()   = raise Unknown (* called in MapDomain.MapBot(K)(V).find *)
  let is_bot x = false

  (* properties for records (e.g. used by Dom.report) *)
  let opened   r = r.state <> Closed && r.state <> Error
  let closed   r = r.state = Closed
  let writable r = match r.state with Open((_,Write)) -> true | _ -> false

  (* predicates *)
  let filter p (x,y) = Set.filter p x, Set.filter p y (* retains top *)
  let must   p (x,y) = Set.exists p x || not (Set.is_empty y) && Set.for_all p y
  let may    p (x,y) = Set.exists p y || is_top (x,y)

  (* set operations *)
  let union (a,b) (c,d) = Set.union a c, Set.union b d
end

module Dom =
struct
  module K = Basetype.Variables
  module V = Val
  module MD = MapDomain.MapBot (Basetype.Variables) (Val)
  include MD
  (* Used to access additional functions of Map.
  Can't use BatMap because type is not compatible with MD.
  Also avoids dependencies for other files using the following functions. *)
  module MDMap = OMap.Make (Basetype.Variables) (* why does OMap.Make (K) not work? *)
  open V.T

  (* other Map functions *)
  (* val bindings : 'a t -> (key * 'a) list
  Return the list of all bindings of the given map. The returned list is sorted in increasing order with respect to the ordering Ord.compare, where Ord is the argument given to Map.Make. *)
  (* own map functions *)
  (* find that resolves aliases *)
  let find' k m = let v = find k m in if V.is_alias v then find (V.get_alias v) m else v
  let find_option k m = if mem k m then Some(find' k m) else None
  let add' k v m =
    if mem k m then
      let v' = find k m in
      if V.is_alias v' then (* previous value was an alias *)
        add (V.get_alias v') v m (* replace its pointee *)
      else add k v m
    else add k v m
  let alias a b m =
    let v = find b m in
    (* if b is already an alias, follow it... *)
    let b' = if V.is_alias v then V.get_alias v else b in
    (* now the entry for a should be an alias pointing to b' *)
    add a (V.make_alias b') m

  (* domain specific *)
  let filter_values p m =
    let filtered_map = filter (fun k v -> V.may p v) m in
    let bindings = MDMap.bindings filtered_map in
    let values = List.map (fun (k,v) -> V.filter p v) bindings in
    let xs, ys = List.split values in
    let flatten_set xs = List.fold_left (fun x y -> Set.union x y) Set.empty xs in
    flatten_set xs, flatten_set ys
  let filter_records k p m = if mem k m then let v = find' k m in V.filter p v else Set.empty, Set.empty

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

  let must k p m = if mem k m then V.must p (find' k m) else false
  let may  k p m = if mem k m then V.may  p (find' k m) else false

  (* returns a tuple (thunk, result) *)
  let report_ ?neg:(neg=false) k p msg m =
    let f ?may:(may=false) s =
      let f () = Messages.report (if may then ("MAYBE "^s) else s) in
      if may then f, `May true else f, `Must true in
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

  let unknown k m = add' k (Set.empty, Set.empty) m
  let is_unknown k m = if mem k m then V.is_top (find' k m) else false

  let fopen k loc filename mode m =
    if is_unknown k m then m else
    let mode = match String.lowercase mode with "r" -> Read | _ -> Write in
    let x = Set.singleton (V.create k loc (Open(filename, mode))) in
    add' k (x,x) m
  let fclose k loc m =
    if is_unknown k m then m else
    let x = Set.singleton (V.create k loc Closed) in
    add' k (x,x) m
  let error k m =
    if is_unknown k m then m else
    let loc = if mem k m then let v = Set.choose (snd (find' k m)) in v.loc else [] in
    let x = Set.singleton (V.create k loc Error) in
    add' k (x,x) m
  let success k m =
    if is_unknown k m then m else
    let v = find' k m in
    let x,y = V.filter V.opened v in
    let v = if x = Set.empty && Set.cardinal y = 1 then y,y else x,y in
    add' k v m

end
