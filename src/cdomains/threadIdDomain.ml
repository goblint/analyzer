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

(** Type to represent an abstract thread ID. *)
module FunLoc: S with type t = varinfo * location option =
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
  let spawn_thread _ l v: t = (v, Some l)

  let spawned_thread () _ _ = ()

  let to_varinfo: t -> varinfo =
    let module RichVarinfoM = RichVarinfo.Make (M) in
    RichVarinfoM.map ~name:show ~size:113

  let is_main = function
    | ({vname = "main"; _}, None) -> true
    | _ -> false
end

module FunLocHistory: S =
struct
  module P =
  struct
    include Printable.Liszt (FunLoc)
    let name () = "prefix"
  end
  module S =
  struct
    include SetDomain.Make (FunLoc)
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
    else if BatList.mem_cmp FunLoc.compare n p then (
      let new_loop = n :: BatList.take_while (fun m -> not (FunLoc.equal n m)) p in
      let new_pref = List.tl (BatList.drop_while (fun m -> not (FunLoc.equal n m)) p) in
      (new_pref, S.of_list new_loop)
    )
    else if S.is_empty s then
      (n :: p, s) (* reversed storage is more efficient *)
    else
      failwith "what now"

  let start_thread v = ([(v, None)], S.empty ())
  let spawn_thread ((p, _ ) as current, cs) l v =
    let n = (v, Some l) in
    let ((p', s') as composed) = compose current n in
    if S.is_empty s' && S.mem n cs then
      (p, S.singleton n)
    else
      composed

  let spawned_thread cs l v =
    S.add (v, Some l) cs

  let to_varinfo: t -> varinfo =
    let module RichVarinfoM = RichVarinfo.Make (M) in
    RichVarinfoM.map ~name:show ~size:113

  let is_main = function
    | ([fl], s) when S.is_empty s && FunLoc.is_main fl -> true
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

module Thread = FunLocHistory (* TODO: make dynamically switchable? *)
module ThreadLifted = Lift (Thread)
