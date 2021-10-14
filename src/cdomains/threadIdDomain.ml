open Cil

module type S =
sig
  include Printable.S
  include MapDomain.Groupable with type t := t

  val threadinit: varinfo -> multiple:bool -> t
  val to_varinfo: t -> varinfo
  val is_main: t -> bool
  val is_unique: t -> bool

  (** Overapproximates whether the first TID can be involved in the creation fo the second TID*)
  val may_create: t -> t -> bool

  (** Is the first TID a must parent of the second thread. Always false if the first TID is not unique *)
  val is_must_parent: t -> t -> bool

  type marshal
  val init: marshal option -> unit
  val finalize: unit -> marshal

  (** How the created varinfos should be namend. Returned strings should not contain [Cil.location]s, but may contain node ids. *)
  val name_varinfo: t -> string
end

module type Stateless =
sig
  include S

  val threadenter: Node.t -> varinfo -> t
end

module type Stateful =
sig
  include S

  module D: Lattice.S

  val threadenter: t * D.t -> Node.t -> varinfo -> t
  val threadspawn: D.t -> Node.t -> varinfo -> D.t
end


(** Type to represent an abstract thread ID. *)
module FunLoc: Stateless =
struct
  module M = struct
    include Printable.Prod (CilType.Varinfo) (Printable.Option (Node) (struct let name = "no location" end))

    (* Defines how varinfos representing a FunLoc are named.
    The varinfo-name contains node ids, but not their location (for compatibility with incremental analysis) *)
    let name_varinfo = function
      | (f, Some n) -> f.vname ^ "@" ^ Node.show n
      | (f, None) -> f.vname
  end
  include M

  let show = function
    | (f, Some n) -> f.vname ^ "@" ^ (CilType.Location.show  (Node.location n))
    | (f, None) -> f.vname

  include Printable.PrintSimple (
    struct
      type nonrec t = t
      let show = show
    end
  )

  let threadinit v ~multiple: t = (v, None)
  let threadenter l v: t = (v, Some l)

  let is_main = function
    | ({vname = "main"; _}, None) -> true
    | _ -> false

  let is_unique _ = false (* TODO: should this consider main unique? *)
  let may_create _ _ = true
  let is_must_parent _ _ = false

  module VarinfoMap = RichVarinfo.Make (RichVarinfo.EmptyDescription (M))
  let to_varinfo = VarinfoMap.to_varinfo
  type marshal = VarinfoMap.marshal
  let init m = VarinfoMap.unmarshal m
  let finalize () = VarinfoMap.marshal ()
end


module Unit (Base: Stateless): Stateful =
struct
  include Base

  module D = Lattice.Unit

  let threadenter _ = threadenter
  let threadspawn () _ _ = ()
end

module History (Base: Stateless): Stateful =
struct
  module P =
  struct
    include Printable.Liszt (Base)
    (* Prefix is stored in reversed order (main is last) since prepending is more efficient. *)
    let name () = "prefix"
  end
  module S =
  struct
    include SetDomain.Make (Base)
    let name () = "set"
  end
  module M = struct
    include Printable.Prod (P) (S)
    (* Varinfos for histories are named using a string representation based on node ids,
     not locations, for compatibilty with incremental analysis.*)
    let name_varinfo ((l, s): t): string =
     let list_name = String.concat "," (List.map Base.name_varinfo l) in
     let set_name = String.concat "," (List.map Base.name_varinfo (S.elements s)) in
     list_name ^ ", {" ^ set_name ^ "}"
  end
  include M

  module D =
  struct
    include S
    let name () = "created"
  end

  let is_unique (_, s) =
    S.is_empty s

  let is_must_parent (p,s) (p',s') =
    if not (S.is_empty s) then
      false
    else
      let cdef_ancestor = P.common_suffix p p' in
      P.equal p cdef_ancestor

  let may_create (p,s) (p',s') =
    S.subset (S.union (S.of_list p) s) (S.union (S.of_list p') s')

  let compose ((p, s) as current) n =
    if BatList.mem_cmp Base.compare n p then (
      (* TODO: can be optimized by implementing some kind of partition_while function *)
      let s' = S.of_list (BatList.take_while (fun m -> not (Base.equal n m)) p) in
      let p' = List.tl (BatList.drop_while (fun m -> not (Base.equal n m)) p) in
      (p', S.add n (S.union s s'))
    )
    else if is_unique current then
      (n :: p, s)
    else
      (p, S.add n s)

  let threadinit v ~multiple =
    let base_tid = Base.threadinit v ~multiple in
    if multiple then
      ([], S.singleton base_tid)
    else
      ([base_tid], S.empty ())

  let threadenter ((p, _ ) as current, cs) (n: Node.t) v =
    let n = Base.threadenter n v in
    let ((p', s') as composed) = compose current n in
    if is_unique composed && S.mem n cs then
      (p, S.singleton n)
    else
      composed

  let threadspawn cs l v =
    S.add (Base.threadenter l v) cs

  module VarinfoMap = RichVarinfo.Make (RichVarinfo.EmptyDescription(M))
  let to_varinfo = VarinfoMap.to_varinfo

  let is_main = function
    | ([fl], s) when S.is_empty s && Base.is_main fl -> true
    | _ -> false

  type marshal = VarinfoMap.marshal
  let finalize () = VarinfoMap.marshal ()
  let init m = VarinfoMap.unmarshal m
end

module ThreadLiftNames = struct
  let bot_name = "Bot Threads"
  let top_name = "Top Threads"
end
module Lift (Thread: S) =
struct
  include Lattice.Flat (Thread) (ThreadLiftNames)
  let name () = "Thread"
end

(* Since the thread ID module is extensively used statically, it cannot be dynamically switched via an option. *)
(* TODO: make dynamically switchable? using flag-configured delegating module (like array domains)? *)

(* Old thread IDs *)
(* module Thread = Unit (FunLoc) *)

(* Thread IDs with prefix-set history *)
module Thread = History (FunLoc)


module ThreadLifted = Lift (Thread)
