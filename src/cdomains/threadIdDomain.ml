open Cil

module type S =
sig
  include Printable.S with type t = varinfo (* TODO: remove varinfo constraint *)
  include MapDomain.Groupable with type t := t

  val spawn_thread: ?loc:location -> varinfo -> t
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

  let spawn_thread ?loc v: t = get_thread_var v loc
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
