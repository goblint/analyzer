open Cil

(** Type to represent an abstract thread ID. *)
module Thread = struct
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
  let spawn_thread l v: t = get_thread_var v (Some l)
end

module ThreadLiftNames = struct
  let bot_name = "Bot Threads"
  let top_name = "Top Threads"
end
module ThreadLifted =
struct
  include Lattice.Flat (Thread) (ThreadLiftNames)
  let name () = "Thread"
end
