open Cil
open Pretty
open BatPervasives

module M = Messages


exception Unknown
exception Error

module Val = 
struct
  module T =
  struct
    (* assign Top on any pointer modification *)
    type loc = Loc of (location list) | Bot | Top
    type mode = Read | Write
    type state = Open of string*mode | Close
    type record = { var: varinfo; loc: loc; state: state }
    type t' = Must of record | May of record list
  end

  include Printable.Std
  include Lattice.StdCousot
  include T
  type t = t'
  
  let toStringRecord x =
    let loc x = match x with
      | Loc(loc) -> String.concat ", " (List.map (fun x -> string_of_int x.line) loc)
      | Bot -> "Bot" | Top -> "Top" in
    let mode x = match x with Read -> "Read" | Write -> "Write" in
    match x.state with
    | Open(filename, m) -> "open("^filename^", "^(mode m)^") ("^(loc x.loc)^")"
    | Close -> "closed ("^(loc x.loc)^")"

  let toString = function
    | Must x -> "Must "^(toStringRecord x)
    | May xs -> "May "^(String.concat ", " (List.map toStringRecord xs))

  let short i x = toString x

  include Printable.PrintSimple (struct
    type t' = t
    let name () = "File pointers"
    let short = short
  end) 

  let create v l s = { var=v; loc=l; state=s }
  let may = function Must x -> May [x] | xs -> xs
  let recordList = function Must x -> [x] | May xs -> xs

  (* let equal = Util.equals *)
  let equal x y = let xs = recordList x in let ys = recordList y in
    List.for_all (fun x -> List.mem x ys) xs && List.for_all (fun y -> List.mem y xs) ys
  let hash = Hashtbl.hash
  let join x y = M.report ("JOIN\tx: " ^ (toString x) ^ "\n\ty: " ^ (toString y));
    (* Out_of_memory:
    unique runs out because it keeps the latest element -> ordering of list changes -> no fixpoint reached
    unique_cmp doesn't because it keeps the first
    -> better use Set *)
    let r = May (BatList.unique_cmp ((recordList x)@(recordList y))) in
    (* let r = May ((recordList x)@(recordList y)) in *)
    M.report ("result: "^(toString r));
    r
  let leq x y = equal y (join x y)
  let meet x y = M.report ("MEET\tx: " ^ (toString x) ^ "\n\ty: " ^ (toString y)); x
  let top () = raise Unknown
  let is_top x = (* x.loc = Top *)false 
  let bot () = raise Error
  let is_bot x = (* x.loc = Bot *)false
  
  (* let dummy () = { var=(Cil.makeVarinfo false "dummy" Cil.voidType); loc=Bot; state=Close } *)

  (* properties used by FileUses.report *)
  let opened x = x.state <> Close
  let closed x = x.state = Close
  let writable x = match x.state with Open((_,Write)) -> true | _ -> false
end

module FileUses  = 
struct 
(*   module VarSet = SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All Variables" end)
  include Lattice.Prod (VarSet) (VarSet) (* Base1: open file handles, Base2: closed file handles *) *)

  (* include Printable.Std *)
  (* include Lattice.StdCousot *)

  module K = Basetype.Variables
  module V = Val
  module MD = MapDomain.MapBot (Basetype.Variables) (Val)
  include MD
  module M = Map.Make (Basetype.Variables) (* why does Map.Make (K) not work? *)
  open V.T

  (* other map functions *)
  (* val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  filter p m returns the map with all the bindings in m that satisfy predicate p. *)
  let filter p m = M.filter p m
  (* val bindings : 'a t -> (key * 'a) list
  Return the list of all bindings of the given map. The returned list is sorted in increasing order with respect to the ordering Ord.compare, where Ord is the argument given to Map.Make. *)
  let bindings m = M.bindings m
  (* own map functions *)
  let findOption k m = if mem k m then Some(find k m) else None

  (* domain specific *)
  let predicate ?may:(may=false) v p = match v with Must x -> p x | May xs -> if may then List.exists p xs else List.for_all p xs
  let filterOnVal ?may:(may=false) p m = M.filter (fun k v -> predicate ~may:may v p) m
  let filterVars ?may:(may=false) p m = List.map (fun (k,v) -> k) (M.bindings (filterOnVal ~may:may p m))

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
      let p = if neg then not -| p else p in
      match v with
        | Must x -> if p x then f msg else mf
        | May xs -> if List.for_all p xs then f msg
                    else if List.exists p xs then f ~may:true msg
                    else mf
    else if neg then f msg else mf

  let report ?neg:(neg=false) m var p msg = (fst (report_ ~neg:neg m var p msg)) () (* evaluate thunk *)

  let reports xs =
    let uncurry (neg, m, var, p, msg) = report_ ~neg:neg m var p msg in
(*     let f x = uncurry x = `Must true in
    ignore(List.exists f xs) (* stops after first `Must true. like if .. else if .. else ..*) *)
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

(*   let toXML_f sf x = 
    match toXML x with
      | Xml.Element (node, [text, _], elems) -> 
          let summary = "File Uses: " ^ sf Goblintutil.summary_length x in
            Xml.Element (node, [text, summary], elems)
      | x -> x
      
  let toXML s  = toXML_f short s *)
end