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
    type state = Open of string*mode | Close
    type record = { var: varinfo; loc: loc; state: state }
    type t' = record Set.t * record Set.t (* mustOpen, mayOpen *)
  end

  include Printable.Std
  include Lattice.StdCousot
  include T
  type t = t'

  let toStringRecord x =
    let loc xs = String.concat ", " (List.map (fun x -> string_of_int x.line) xs) in
    let mode x = match x with Read -> "Read" | Write -> "Write" in
    match x.state with
    | Open(filename, m) -> "open("^filename^", "^(mode m)^") ("^(loc x.loc)^")"
    | Close -> "closed ("^(loc x.loc)^")"

  let toString (x,y) =
    let z = Set.diff y x in
    "{ "^(String.concat ", " (List.map toStringRecord (Set.elements x)))^" }, "^
    "{ "^(String.concat ", " (List.map toStringRecord (Set.elements z)))^" }"
    (* IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) xs *)

  let short i x = toString x

  include Printable.PrintSimple (struct
    type t' = t
    let name () = "File pointers"
    let short = short
  end)

  let create v l s = { var=v; loc=l; state=s }
  let map f (x,y) = Set.map f x, Set.map f y
  let rebind x var = map (fun x -> {x with var=var}) x
  (* let may = function Must x -> May (Set.singleton x) | xs -> xs *)
  (* let records = function Must x -> (Set.singleton x) | May xs -> xs *)
  (* let recordsList (x,y) = List.of_enum (Set.enum x), List.of_enum (Set.enum y) *)
  (* let vnames x = String.concat ", " (List.map (fun x -> x.var.vname) (recordsList x)) *)

  let equal = Util.equals
  (* let leq x y = equal y (join x y) *)
  let leq (a,b) (c,d) = Set.subset c a && Set.subset b d
  let hash = Hashtbl.hash
  let join (a,b) (c,d) = (* M.report ("JOIN\tx: " ^ (toString x) ^ "\n\ty: " ^ (toString y)); *)
    let r = Set.intersect a c, Set.union b d in
    (* M.report ("result: "^(toString r)); *)
    r
  let meet x y = M.report ("MEET\tx: " ^ (toString x) ^ "\n\ty: " ^ (toString y)); x
  (* top/bot are handled by MapDomain, only bot () gets called *)
  let top ()   = Set.empty, Set.empty
  let is_top x = x=top ()
  let bot ()   = raise Unknown (* called in MapDomain.MapBot(K)(V).find *)
  let is_bot x = false

  (* properties used by FileUses.report *)
  let opened x = x.state <> Close
  let closed x = x.state = Close
  let writable x = match x.state with Open((_,Write)) -> true | _ -> false

  (* predicates *)
  let filter p (x,y) = Set.filter p x, Set.filter p y (* retains top *)
  let must   p (x,y) = Set.exists p x || not (Set.is_empty y) && Set.for_all p y
  let may    p (x,y) = Set.exists p y || is_top (x,y)

  (* set operations *)
  let union (a,b) (c,d) = Set.union a c, Set.union b d
end

module FileUses  =
struct
  module K = Basetype.Variables
  module V = Val
  module MD = MapDomain.MapBot (Basetype.Variables) (Val)
  include MD
  (* don't use BatMap to avoid dependencies for other files using the following functions *)
  module M = OMap.Make (Basetype.Variables) (* why does OMap.Make (K) not work? *)
  open V.T

  (* other map functions *)
  (* val bindings : 'a t -> (key * 'a) list
  Return the list of all bindings of the given map. The returned list is sorted in increasing order with respect to the ordering Ord.compare, where Ord is the argument given to Map.Make. *)
  (* own map functions *)
  let findOption k m = if mem k m then Some(find k m) else None

  (* domain specific *)
  (* let predicate ?may:(may=false) v p = match v with Must x -> p x | May xs -> if may then Set.exists p xs else Set.for_all p xs && Set.cardinal xs > 1 *)
  (* let filterMap ?may:(may=false) p m = filter (fun k v -> predicate ~may:may v p) m (* this is OCaml's Map.filter which corresponds to BatMap.filteri *) *)
  (* let filterMap p m = filter (fun k v -> V.must p v) m *)
  let filterValues p m =
    let filteredMap = filter (fun k v -> V.may p v) m in
    let bindings = M.bindings filteredMap in
    let values = List.map (fun (k,v) -> V.filter p v) bindings in
    let xs, ys = List.split values in
    let flattenSet xs = List.fold_left (fun x y -> Set.union x y) Set.empty xs in
    flattenSet xs, flattenSet ys
  let filterRecords p k m = if mem k m then let v = find k m in V.filter p v else Set.empty, Set.empty

  let getRecord k m =
    if mem k m then
      let x,y = find k m in
      if Set.is_empty x then None
      else Some (Set.choose x)
    else None
  let addRecord k r m =
    let x = Set.singleton r in
    add k (x,x) m
  let getValue k m =
    if mem k m then find k m
    else Set.empty, Set.empty
  let extendValue k v m =
    if mem k m then
      add k (V.union (find k m) v) m
    else
      add k v m


  (* let checkMay var p m = if mem var m then let x,y = find var m in Set.exists p x, Set.exists p y else (false, false) *)
  (* not used anymore -> remove? *)
  (* let check var p m = if mem var m then V.must p (find var m) else false *)
  (* let opened var m = check var V.opened m *)
  (* let closed var m = check var V.closed m *)
  (* let writable var m = check var V.writable m *)
  let must p k m = if mem k m then V.must p (find k m) else false
  let may  p k m = if mem k m then V.may  p (find k m) else false

  (* returns a tuple (thunk, result) *)
  let report_ ?neg:(neg=false) var p msg m =
    let f ?may:(may=false) s =
      let f () = Messages.report (if may then ("MAYBE "^s) else s) in
      if may then f, `May true else f, `Must true in
    let mf = (fun () -> ()), `Must false in
    if mem var m then
      let p = if neg then not % p else p in
      let v = find var m in
      if V.must p v then f msg (* must *)
      else if V.may p v then f ~may:true msg (* may *)
      else mf (* none *)
    else if neg then f msg else mf

  let report ?neg:(neg=false) var p msg m = (fst (report_ ~neg:neg var p msg m)) () (* evaluate thunk *)

  let reports m var xs =
    let uncurry (neg, p, msg) = report_ ~neg:neg var p msg m in
    let f result x = if snd (uncurry x) = result then Some (fst (uncurry x)) else None in
    let must_true = BatList.filter_map (f (`Must true)) xs in
    let may_true  = BatList.filter_map (f (`May true)) xs in
    (* output first must and first may *)
    if List.length must_true > 0 then (List.hd must_true) ();
    if List.length may_true  > 0 then (List.hd may_true) ()

  let unknown k m = add k (Set.empty, Set.empty) m
  let is_unknown k m = if mem k m then V.is_top (find k m) else false

  let fopen var loc filename mode m =
    if is_unknown var m then m else
    let mode = match String.lowercase mode with "r" -> Read | _ -> Write in
    let x = Set.singleton (V.create var loc (Open(filename, mode))) in
    add var (x,x) m
  let fclose var loc m =
    if is_unknown var m then m else
    let x = Set.singleton (V.create var loc Close) in
    add var (x,x) m

end
