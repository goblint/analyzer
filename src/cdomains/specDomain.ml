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
    type record = { var: Lval.CilLval.t; loc: location list; state: state }
    type t' = Must of record | May of record Set.t
  end

  include Printable.Std
  include Lattice.StdCousot
  include T
  type t = t'

  (* Printing *)
  let string_of_key k = Lval.CilLval.short 80 k
  let string_of_record r =
    let loc xs = String.concat ", " (List.map (fun x -> string_of_int x.line) xs) in
    r.state^" ("^(loc r.loc)^")"
  let string_of = function
    | Must x -> "Must "^(string_of_record x)
    | May xs -> "May "^(String.concat ", " (List.map string_of_record (List.of_enum (Set.enum xs))))
  let short i x = string_of x
  include Printable.PrintSimple (struct
    type t' = t
    let name () = "Spec value"
    let short = short
  end)

  let make v l s = { var=v; loc=l; state=s }
  let map f = function Must x -> Must (f x) | May xs -> May (Set.map f xs)
  let rebind x var = map (fun x -> {x with var=var}) x
  let change_state x state = map (fun x -> {x with state=state}) x
  (* transforms May-Sets of length 1 to Must. NOTE: this should only be done if the original set had more than one element! *)
  let maybe_must = function May xs when Set.cardinal xs = 1 -> Must (Set.choose xs) | x -> x
  let remove_state x state = match x with May xs -> maybe_must @@ May (Set.filter (fun x -> x.state<>state) xs) | x -> x
  let may = function Must x -> May (Set.singleton x) | xs -> xs (* TODO diff. semantic of May with one elem. and more elem.! *)
  let records = function Must x -> (Set.singleton x) | May xs -> xs
  let list_of_records = function Must x -> [x] | May xs -> List.of_enum (Set.enum xs)
  let vnames x = String.concat ", " (List.map (fun r -> string_of_key r.var) (list_of_records x))
  let locs ?p:(p=const true) x = List.map (fun x -> x.loc) (List.filter p (list_of_records x))

  (* Printable.S *)
  let equal = Util.equals
  let hash = Hashtbl.hash
  (* Lattice.S must be implemented to be used as Range for MapDomain *)
  (* let leq x y = equal y (join x y) *)
  let leq x y = Set.subset (records x) (records y)
  let join x y = (* M.report ("JOIN\tx: " ^ (string_of x) ^ "\n\ty: " ^ (string_of y)); *)
    May (Set.union (records x) (records y))
  let meet x y = M.report ("MEET\tx: " ^ (string_of x) ^ "\n\ty: " ^ (string_of y)); x
    (* May (Set.intersection (records x) (records y)) *)
  (* top/bot are handled by MapDomain, only bot () gets called *)
  let top () = raise Unknown
  let is_top x = false
  let bot () = May(Set.empty) (* called in MapDomain.MapBot(K)(V).find *)
  let is_bot x = x=bot ()
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
  let find_option k m = if mem k m then Some(find k m) else None

  (* domain specific *)
  let find_records k m = if mem k m then V.records (find k m) else Set.empty
  let goto var loc state m = add var (Must(V.make var loc state)) m
  let may_goto var loc state m = add var (May(Set.add (V.make var loc state) (find_records var m))) m
  let is_may k m = mem k m && match find k m with May _ -> true | Must _ -> false
  let may k p m = mem k m && Set.exists p (V.records (find k m))
  let must k p m = mem k m && let xs = V.records (find k m) in Set.for_all p xs && not (is_may k m && Set.cardinal xs = 1) (* TODO semantics of May with length 1? *)
  let in_state k state m = must k (fun x -> x.state = state) m
  let may_in_state k state m = may k (fun x -> x.state = state) m
  let get_states k m = if not (mem k m) then [] else List.map (fun x -> x.state) (V.list_of_records (find k m))

  let string_of_state k m = if not (mem k m) then "?" else match find k m with
    | Must x -> x.state
    | xs -> "["^String.concat ", " (List.map (fun x -> x.state) (V.list_of_records xs))^"]"
  let string_of_key k = K.short 80 k
  let string_of_entry k m = string_of_key k ^ ": " ^ string_of_state k m
  let string_of_map m = List.map (fun (k,v) -> string_of_entry k m) (MDMap.bindings m)
end
