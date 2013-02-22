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
    type t' = Must of record | May of record Set.t
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

  let toString = function
    | Must x -> "Must "^(toStringRecord x)
    | May xs -> "May "^(String.concat ", " (List.map toStringRecord (List.of_enum (Set.enum xs))))
    (* IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) xs *)

  let short i x = toString x

  include Printable.PrintSimple (struct
    type t' = t
    let name () = "File pointers"
    let short = short
  end) 

  let create v l s = { var=v; loc=l; state=s }
  let map f = function Must x -> Must (f x) | May xs -> May (Set.map f xs)
  let rebind x var = map (fun x -> {x with var=var}) x
  let may = function Must x -> May (Set.singleton x) | xs -> xs
  let records = function Must x -> (Set.singleton x) | May xs -> xs
  let recordsList = function Must x -> [x] | May xs -> List.of_enum (Set.enum xs)
  let vnames x = String.concat ", " (List.map (fun x -> x.var.vname) (recordsList x))

  let equal = Util.equals
  (* let leq x y = true *)
  (* let leq x y = equal y (join x y) *)
  let leq x y = Set.subset (records x) (records y)
  let hash = Hashtbl.hash
  let join x y = (* M.report ("JOIN\tx: " ^ (toString x) ^ "\n\ty: " ^ (toString y)); *)
    let r = May (Set.union (records x) (records y)) in
    (* M.report ("result: "^(toString r)); *)
    r
  let meet x y = M.report ("MEET\tx: " ^ (toString x) ^ "\n\ty: " ^ (toString y)); x
  (* top/bot are handled by MapDomain, only bot () gets called *)
  let top () = raise Unknown
  let is_top x = false 
  let bot () = May(Set.empty) (* called in MapDomain.MapBot(K)(V).find *)
  let is_bot x = x=bot ()
  
  (* properties used by FileUses.report *)
  let opened x = x.state <> Close
  let closed x = x.state = Close
  let writable x = match x.state with Open((_,Write)) -> true | _ -> false
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
  let bindings m = M.bindings m
  (* own map functions *)
  let findOption k m = if mem k m then Some(find k m) else None

  (* domain specific *)
  let predicate ?may:(may=false) v p = match v with Must x -> p x | May xs -> if may then Set.exists p xs else Set.for_all p xs
  let filterMap ?may:(may=false) p m = filter (fun k v -> predicate ~may:may v p) m (* this is OCaml's Map.filter which corresponds to BatMap.filteri *)
  let filterValues ?may:(may=false) p m = List.concat (
    List.map (fun (k,v) -> List.filter p (V.recordsList v)) (* can't use BatMap.values *)
    (M.bindings (filterMap ~may:may p m)))

  let check m var p = if mem var m then predicate (find var m) p else false
  let opened m var = check m var (fun x -> x.state <> Close)
  let closed m var = check m var (fun x -> x.state = Close)
  let writable m var = check m var (fun x -> match x.state with Open((_,Write)) -> true | _ -> false)

  (* returns a tuple (thunk, result) *)
  let report_ ?neg:(neg=false) m var p msg =
    let f ?may:(may=false) s =
      let f () = Messages.report (if may then ("might be "^s) else s) in
      if may then f, `May true else f, `Must true in
    let mf = (fun () -> ()), `Must false in
    if mem var m then
      let v = find var m in
      let p = if neg then not % p else p in
      match v with
        | Must x -> if p x then f msg else mf
        | May xs -> if Set.for_all p xs then f msg
                    else if Set.exists p xs then f ~may:true msg
                    else mf
    else if neg then f msg else mf

  let report ?neg:(neg=false) m var p msg = (fst (report_ ~neg:neg m var p msg)) () (* evaluate thunk *)

  let reports xs =
    let uncurry (neg, m, var, p, msg) = report_ ~neg:neg m var p msg in
    let f result x = if snd (uncurry x) = result then Some (fst (uncurry x)) else None in
    let must_true = BatList.filter_map (f (`Must true)) xs in
    let may_true  = BatList.filter_map (f (`May true)) xs in
    (* output first must and first may *)
    if List.length must_true > 0 then (List.hd must_true) ();
    if List.length may_true  > 0 then (List.hd may_true) ()

  let fopen m var loc filename mode =
    let mode = match String.lowercase mode with "r" -> Read | _ -> Write in
    add var (Must(V.create var loc (Open(filename, mode)))) m
  let fclose m var loc = add var (Must(V.create var loc Close)) m

  let may m var = add var (V.may (find var m)) m

end