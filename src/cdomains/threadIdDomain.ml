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
module FunLoc: S =
struct
  module M = Printable.Prod (CilType.Varinfo) (Printable.Option (CilType.Location) (struct let name = "no location" end))
  include M

  let start_thread v: t = (v, None)
  let spawn_thread _ l v: t = (v, Some l)

  let to_varinfo: t -> varinfo =
    let module RichVarinfoM = RichVarinfo.Make (M) in
    let name = function
      | (f, Some l) -> f.vname ^ "@" ^ CilType.Location.show l
      | (f, None) -> f.vname
    in
    RichVarinfoM.map ~name ~size:113

  let is_main = function
    | ({vname = "main"; _}, None) -> true
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

module Thread = FunLoc (* TODO: make dynamically switchable? *)
module ThreadLifted = Lift (Thread)
