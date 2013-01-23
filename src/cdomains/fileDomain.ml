open Cil
open Pretty

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
    | Open(filename, m) -> "open "^filename^" "^(mode m)^" ("^(loc x.loc)^")"
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

  let equal = Util.equals
  let hash = Hashtbl.hash
  let leq x y = true
  let join x y = M.report ("JOIN\tx: " ^ (toString x) ^ "\n\ty: " ^ (toString x)); x
  let meet x y = M.report ("MEET\tx: " ^ (toString x) ^ "\n\ty: " ^ (toString x)); x
  let top () = raise Unknown
  let is_top x = (* x.loc = Top *)false 
  let bot () = raise Error
  let is_bot x = (* x.loc = Bot *)false

  let create v l s = { var=v; loc=l; state=s }
  let dummy () = { var=(Cil.makeVarinfo false "dummy" Cil.voidType); loc=Bot; state=Close }
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
  include V.T

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
  let predicate v p = match v with Must x -> p x | May xs -> List.for_all p xs
  let filterOnVal p m = M.filter (fun k v -> predicate v p) m
  let filterVars p m = List.map (fun (k,v) -> k) (M.bindings (filterOnVal p m))

  let check m var p = if mem var m then predicate (find var m) p else false
  let opened m var = check m var (fun x -> x.state <> Close)
  let closed m var = check m var (fun x -> x.state = Close)
  let writable m var = check m var (fun x -> match x.state with Open((_,Write)) -> true | _ -> false)

  let fopen m var loc filename mode =
    let mode = match String.lowercase mode with "r" -> Read | _ -> Write in
    add var (Must(V.create var loc (Open(filename, mode)))) m
  let fclose m var loc = add var (Must(V.create var loc Close)) m

  let mayVal = function Must x -> May [x] | xs -> xs
  let may m var = add var (mayVal (find var m)) m

(*   let toXML_f sf x = 
    match toXML x with
      | Xml.Element (node, [text, _], elems) -> 
          let summary = "File Uses: " ^ sf Goblintutil.summary_length x in
            Xml.Element (node, [text, summary], elems)
      | x -> x
      
  let toXML s  = toXML_f short s *)
end