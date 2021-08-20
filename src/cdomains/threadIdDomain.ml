open Cil

module type S =
sig
  include Printable.S
  include MapDomain.Groupable with type t := t

  module D: Lattice.S

  val start_thread: varinfo -> t
  val spawn_thread: t * D.t -> location -> varinfo -> t
  val spawned_thread: D.t -> location -> varinfo -> D.t
  val to_varinfo: t -> varinfo
  val is_main: t -> bool
end

module type Stateless =
sig
  include S
  val spawn_thread': location -> varinfo -> t
end

(** Type to represent an abstract thread ID. *)
module FunLoc: Stateless =
struct
  module M = Printable.Prod (CilType.Varinfo) (Printable.Option (CilType.Location) (struct let name = "no location" end))
  include M

  module D = Lattice.Unit

  let show = function
    | (f, Some l) -> f.vname ^ "@" ^ CilType.Location.show l
    | (f, None) -> f.vname

  include Printable.PrintSimple (
    struct
      type nonrec t = t
      let show = show
    end
  )

  let start_thread v: t = (v, None)
  let spawn_thread' l v: t = (v, Some l)
  let spawn_thread _ l v: t = spawn_thread' l v

  let spawned_thread () _ _ = ()

  let to_varinfo: t -> varinfo =
    let module RichVarinfoM = RichVarinfo.Make (M) in
    RichVarinfoM.map ~name:show ~size:113

  let is_main = function
    | ({vname = "main"; _}, None) -> true
    | _ -> false
end

module History (Base: Stateless): S =
struct
  module P =
  struct
    include Printable.Liszt (Base)
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

  let compose ((p, s) as current) n =
    if S.mem n s then
      current
    else if BatList.mem_cmp Base.compare n p then (
      let new_loop = n :: BatList.take_while (fun m -> not (Base.equal n m)) p in
      let new_pref = List.tl (BatList.drop_while (fun m -> not (Base.equal n m)) p) in
      (new_pref, S.of_list new_loop)
    )
    else if S.is_empty s then
      (n :: p, s) (* reversed storage is more efficient *)
    else
      failwith "what now"

  let start_thread v = ([Base.start_thread v], S.empty ())
  let spawn_thread ((p, _ ) as current, cs) l v =
    let n = Base.spawn_thread' l v in
    let ((p', s') as composed) = compose current n in
    if S.is_empty s' && S.mem n cs then
      (p, S.singleton n)
    else
      composed

  let spawned_thread cs l v =
    S.add (Base.spawn_thread' l v) cs

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

module Thread = History (FunLoc) (* TODO: make dynamically switchable? *)
module ThreadLifted = Lift (Thread)
