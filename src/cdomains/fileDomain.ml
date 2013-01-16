open Cil
open Pretty

module M = Messages


exception Unknown
exception Error

module Val = 
struct
  include Printable.Std
  include Lattice.StdCousot
  (* assign Top on any pointer modification *)
  type loc = Loc of (location list) | Bot | Top
  type mode = Read | Write
  type state = Open of string*mode | Close
  type cert = Must | May
  type t = varinfo * loc * state * cert

  include Printable.PrintSimple (struct
    type t' = t
    let name () = "File pointers"
    let short i x = "bla" (* why isn't this used? *)
  end) 

  let equal = Util.equals
  let hash = Hashtbl.hash
  let leq x y = true
  let join x y = M.report "JOIN"; x
  let meet x y = M.report "MEET"; x
  let top () = raise Unknown
  let is_top x = match x with (_, Top, _, _) -> true | _ -> false
  let bot () = raise Error
  let is_bot x = match x with (_, Bot, _, _) -> true | _ -> false

  let toString (v,l,s,c) =
    let loc x = match x with
      | Loc(loc) -> String.concat ", " (List.map (fun x -> string_of_int x.line) loc)
      | Bot -> "Bot" | Top -> "Top" in
    let mode x = match x with Read -> "Read" | Write -> "Write" in
    let mustmay x = match x with Must -> "Must" | May -> "May" in
    match s with
    | Open(filename, m) -> "open "^filename^(mode m)^(loc l)^(mustmay c)
    | Close -> "closed "^(loc l)^(mustmay c)
  let short i x = toString x

  let dummy () = ((Cil.makeVarinfo false "dummy" Cil.voidType), Bot, Close, Must)
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

  (* val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  filter p m returns the map with all the bindings in m that satisfy predicate p. *)
  let filter p m = M.filter p m

  (* val bindings : 'a t -> (key * 'a) list
  Return the list of all bindings of the given map. The returned list is sorted in increasing order with respect to the ordering Ord.compare, where Ord is the argument given to Map.Make. *)
  let bindings m = M.bindings m

  let predicate v p = let v,l,s,c = v in p v l s c
  let filterOnVal p m = M.filter (fun k v -> predicate v p) m
  let filterVars p m = List.map (fun (k,v) -> let v,l,s,c = v in v) (M.bindings (filterOnVal p m))

  let check m var p = if mem var m then predicate (find var m) p else false
  let opened m var = check m var (fun v l s c -> match s with V.Open(_) -> true | _ -> false)
  let closed m var = check m var (fun v l s c -> match s with V.Close -> true | _ -> false)
  let writable m var = check m var (fun v l s c -> match s with V.Open((_,V.Write)) -> true | _ -> false)

  let fopen m var loc filename mode cert =
    let mode = match String.lowercase mode with "r" -> V.Read | _ -> V.Write in
    add var (var, loc, (V.Open(filename, mode)), cert) m
  let fclose m var loc cert = add var (var, loc, V.Close, cert) m

(*   let toXML_f sf x = 
    match toXML x with
      | Xml.Element (node, [text, _], elems) -> 
          let summary = "File Uses: " ^ sf Goblintutil.summary_length x in
            Xml.Element (node, [text, summary], elems)
      | x -> x
      
  let toXML s  = toXML_f short s *)
end