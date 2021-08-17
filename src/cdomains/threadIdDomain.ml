open Cil

module type S =
sig
  include Printable.S
  include MapDomain.Groupable with type t := t

  val start_thread: varinfo -> t
  val spawn_thread: t -> location -> varinfo -> t
  val to_varinfo: t -> varinfo
  val is_main: t -> bool
end

(** Type to represent an abstract thread ID. *)
module FunLoc: S with type t = varinfo * location option =
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

  let start_thread v: t = (v, None)
  let spawn_thread _ l v: t = (v, Some l)

  let to_varinfo: t -> varinfo =
    let module RichVarinfoM = RichVarinfo.Make (M) in
    RichVarinfoM.map ~name:show ~size:113

  let is_main = function
    | ({vname = "main"; _}, None) -> true
    | _ -> false
end

module FunLocHistory: S =
struct
  module M = Printable.Liszt (FunLoc)
  include M

  let start_thread v = [(v, None)]
  let spawn_thread current l v = (v, Some l) :: current (* reversed storage is more efficient *)

  let to_varinfo: t -> varinfo =
    let module RichVarinfoM = RichVarinfo.Make (M) in
    RichVarinfoM.map ~name:show ~size:113

  let is_main = function
    | [fl] when FunLoc.is_main fl -> true
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
