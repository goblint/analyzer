(** Minimal signature for hashtables. *)

module type H =
sig
  type key
  type 'a t
  val create: int -> 'a t
  val clear: 'a t -> unit
  val copy: 'a t -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val remove: 'a t -> key -> unit
  val find: 'a t -> key -> 'a
  val find_default: 'a t -> key -> 'a -> 'a
  val find_all: 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length: 'a t -> int
end

module Printable (Domain: Printable.S) (Range: Printable.S) =
struct
  include Printable.Std
  module M = Hashtbl.Make (Domain)
  (* How can I just include this one and set the type 'a t = Range.t M.t???
   * I will just include them manually for now! *)
  type t = Range.t M.t
  type key = Domain.t
  type value = Range.t
  let create = M.create
  let clear = M.clear
  let find = M.find
  let find_all = M.find_all
  let copy = M.copy
  let add = M.add
  let remove = M.remove
  let replace = M.replace
  let mem = M.mem
  let iter = M.iter
  let fold = M.fold
  let length = M.length

  let equal x y =
    let forall2 f x y =
      let ch k v t = t && try f (find x k) v with Not_found -> false in
      fold ch y true
    in length x = length y && forall2 Range.equal x y
  let hash xs = fold (fun k v xs -> xs lxor (Domain.hash k) lxor (Range.hash v)) xs 0
  let show x = "mapping"


  open Pretty
  let pretty () mapping =
    let f key st dok =
      dok ++ dprintf "%a ->@?  @[%a@]\n" Domain.pretty key Range.pretty st
    in
    let content () = fold f mapping nil in
    let defline () = dprintf "OTHERS -> Not available\n" in
    dprintf "@[Mapping {\n  @[%t%t@]}@]" content defline

  let printXml f xs =
    let print_one k v =
      BatPrintf.fprintf f "<key>\n%a</key>\n%a" Domain.printXml k Range.printXml v
    in
    BatPrintf.fprintf f "<value>\n<set>\n";
    iter print_one xs;
    BatPrintf.fprintf f "</set>\n</value>\n"
end
