open Cil

module type S =
sig
  include Printable.S
  include MapDomain.Groupable with type t := t

  val threadinit: varinfo -> multiple:bool -> t
  val to_varinfo: t -> varinfo
  val is_main: t -> bool
  val is_unique: t -> bool
end

module type Stateless =
sig
  include S

  val threadenter: location -> varinfo -> t
end

module type Stateful =
sig
  include S

  module D: Lattice.S

  val threadenter: t * D.t -> location -> varinfo -> t
  val threadspawn: D.t -> location -> varinfo -> D.t
end


(** Type to represent an abstract thread ID. *)
module FunLoc: Stateless =
struct
  module M = Printable.Prod (CilType.Varinfo) (Printable.Option (CilType.Location) (struct let name = "no location" end))
  include M

  let show = function
    | (f, Some l) -> f.vname ^ "@" ^ CilType.Location.show l
    | (f, None) -> f.vname

  include Printable.PrintSimple (
    struct
      type nonrec t = t
      let show = show
    end
  )

  let threadinit v ~multiple: t = (v, None)
  let threadenter l v: t = (v, Some l)

  let to_varinfo: t -> varinfo =
    let module RichVarinfoM = RichVarinfo.Make (M) in
    RichVarinfoM.map ~name:show ~size:113

  let is_main = function
    | ({vname = "main"; _}, None) -> true
    | _ -> false

  let is_unique _ = false (* TODO: should this consider main unique? *)
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
  module M = Printable.Prod (P) (S)
  include M

  module D =
  struct
    include S
    let name () = "created"
  end

  let is_unique (_, s) =
    S.is_empty s

  (** Find the latest definite common ancestor of two tids *)
  let cdef_ancestor (p,s) (p',s') =
    (P.common_suffix p p', S.empty ())

  (** Overapproximation of whether (p,s) can be involved in the creation of (p',s') *)
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

  let threadenter ((p, _ ) as current, cs) l v =
    let n = Base.threadenter l v in
    let ((p', s') as composed) = compose current n in
    if is_unique composed && S.mem n cs then
      (p, S.singleton n)
    else
      composed

  let threadspawn cs l v =
    S.add (Base.threadenter l v) cs

  let to_varinfo: t -> varinfo =
    let module RichVarinfoM = RichVarinfo.Make (M) in
    RichVarinfoM.map ~name:show ~size:113

  let is_main = function
    | ([fl], s) when S.is_empty s && Base.is_main fl -> true
    | _ -> false
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
