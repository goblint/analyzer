open Cil

module type S =
sig
  include Lattice.S
  val is_multi: t -> bool
  val is_bad: t -> bool
  val get_single: unit -> t
  val get_multi: unit -> t
  val get_main:  unit -> t
  val switch: t -> t -> bool
end

module Trivial = struct
  module TrivialNames = struct
    let truename = "Multithreaded"
    let falsename = "Singlethreaded"
  end
  include IntDomain.MakeBooleans (TrivialNames)

  let is_multi x = x
  let is_bad   x = x
  let get_single () = false
  let get_multi () = true
  let get_main  () = true
  let switch x y = x <> y
end

module Simple = struct
  module SimpleNames = struct
    let n = 3
    let names = function
      | 0 -> "Singlethreaded"
      | 1 -> "Main Thread"
      | 2 -> "Some Threads"
      | _ -> "WHAT??"
  end
  include Lattice.Chain (SimpleNames)
  let is_multi x = x > 0
  let is_bad   x = x > 1
  let get_multi () = 2
  let get_main  () = 1
  let get_single () = 0
  let switch x y = match x,y with
    | 0,0 -> false
    | 0,_ -> true
    | _,0 -> true
    | _   -> false
  let name () = "MT mode"
end

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
        | Some l -> f.vname ^ "@" ^ Basetype.ProgLines.short 80 l
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

(** The basic thread domain that distinguishes singlethreaded mode, a single
  * thread ID, and otherwise goes to top. *)
module SimpleThreadDomain = struct

  include Lattice.ProdSimple (Simple) (ThreadLifted)
  let is_multi (x,_) = x > 0
  let is_bad   (x,_) = x > 1
  let get_multi () = (2, ThreadLifted.top ())
  let make_main (x,y) = (Simple.join 1 x, y)
  let spawn_thread l v = (2, `Lifted (Thread.spawn_thread l v))
  let start_single v : t = (0, `Lifted (Thread.start_thread v))
  let start_main   v : t = (2, `Lifted (Thread.start_thread v))
  let start_multi  v : t = (2, `Lifted (Thread.start_thread v))
  let switch (x,z) (y,_) = (Simple.switch x y, z)


  let short w (x,y) =
    let tid = ThreadLifted.short w y in
    if x > 1 then tid else tid ^ "!" (* ! means unique *)
  let pretty () x = pretty_f short () x
  let same_tid x y =
    match x,y with
    | (_, `Lifted x), (_, `Lifted y) -> Thread.equal x y
    | _ -> false
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
  include Lattice.ProdSimple (Uniqueness) (ThreadSet)
end


module ThreadStringSet = SetDomain.ToppedSet (Printable.Strings) (struct let topname = "All Threads" end)
