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


module ThreadSet = SetDomain.ToppedSet (Thread) (struct let topname = "All Threads" end)

module CreatedThreadSet = ThreadSet

module ThreadCreation =
struct
  module UNames = struct
    let truename  = "repeated"
    let falsename = "unique"
  end
  module Uniqueness = IntDomain.MakeBooleans (UNames)
  module ParentThreadSet =
  struct
    include ThreadSet
    let name () = "parents"
  end
  module DirtyExitNames =
  struct
    let truename = "dirty exit"
    let falsename = "clean exit"
  end

  (* A thread exits cleanly iff it joined all threads it started, and they also all exit cleanly *)
  module DirtyExit = IntDomain.MakeBooleans (DirtyExitNames)
  include Lattice.Prod3 (Uniqueness) (ParentThreadSet) (DirtyExit)
end


module ThreadStringSet = SetDomain.ToppedSet (Printable.Strings) (struct let topname = "All Threads" end)
