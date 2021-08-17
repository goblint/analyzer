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
  include Basetype.Variables

  let thread_hash = Hashtbl.create 113

  let get_thread_var (f: varinfo) loc =
    try Hashtbl.find thread_hash (f,loc)
    with Not_found ->
      let name =
        match loc with
        | None -> f.vname
        | Some l -> f.vname ^ "@" ^ CilType.Location.show l
      in
      let newvar = Goblintutil.create_var (makeGlobalVar name voidType) in
      Hashtbl.add thread_hash (f,loc) newvar;
      newvar

  let start_thread v: t = get_thread_var v None
  let spawn_thread _ l v: t = get_thread_var v (Some l)

  let to_varinfo x = x
  let is_main = function
    | {vname = "main"; _} -> true
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
