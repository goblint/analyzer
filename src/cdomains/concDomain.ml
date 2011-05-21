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

module TrivialNames = struct
  let truename = "Multithreaded" 
  let falsename = "Singlethreaded"
end

module Trivial = struct 
  include IntDomain.MakeBooleans (TrivialNames)
  let is_multi x = x
  let is_bad   x = x
  let get_single () = false
  let get_multi () = true
  let get_main  () = true
  let switch x y = x <> y
end

module SimpleNames = struct
  let n = 3
  let names = function
    | 0 -> "Singlethreaded"
    | 1 -> "Main Thread"
    | 2 -> "Some Threads"
    | _ -> "WHAT??"
end

module Simple = struct
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
end

module ThreadCJNames = struct
  let truename = "created" 
  let falsename = "joined"
end

module ThreadCJState = struct
  include IntDomain.MakeBooleans (ThreadCJNames)
end

module ThreadLiftNames = struct
  let bot_name = "zero" 
  let top_name = "many/many"
end

module ThreadState = struct
  include Lattice.Flat (ThreadCJState) (ThreadLiftNames)
end

module Variables = MapDomain.StripClasses (Basetype.Variables)

(** Single vector-like domain (tid -> state) for thread states. *)
module ThreadVector = struct
  include MapDomain.MapBot (Variables) (ThreadState)
  
  let zero = ThreadState.bot()
  let many_many = ThreadState.top()
  
  let created = `Lifted true
  let joined = `Lifted false
  
  let create_thread v t =
    let o = (find t v) in
    let n = if o == zero then created else many_many in
    add t n (remove t v)
    
  let join_thread v t =
    let o = (find t v) in
    let n = if o == created then joined else many_many in
    add t n (remove t v)
  
  (** Returns whether the given thread has been
      created in this lattice value. *)  
  let is_created v t =
    fold (fun _ value l -> l or value == created) v false
    
end

(**
  Double-thread vector. Maps thread id into vector of thread id's
  that the thread creates and joins. *)
module ThreadsVector = struct
  include MapDomain.MapBot (Variables) (ThreadVector)
  
  let create_thread v t1 t2 =
    let o = (find t1 v) in
    let n = ThreadVector.create_thread o t2 in
    add t1 n (remove t1 v)
  
  let join_thread v t1 t2 =
    let o = (find t1 v) in
    let n = ThreadVector.join_thread o t2 in
    add t1 n (remove t1 v)
  
  (*FIXME stubs*)
  let is_unique_created v t = true
  
  let is_singleton v t = true
  
  (*
  let creation_path v t = creation_path_visited v t []
  
  let creation_path_visited v t vs*)
  
  (** Returns all threads that create the give thread. *)
  let creators v t =
    fold (fun creator tv creators ->
      if ThreadVector.is_created tv t then t :: creators else creators) v []

  (** Returns (Some creator) when the thread t has a single creator.
      Othwerwise returns None. *)
  let creating_parent v t =
    match creators v t with
      | [creator] -> Some creator
      | _         -> None

  (** Checks whether path contains the given thread. *)
  let rec contains t path =
    match path with
      | thread :: rest -> t == thread or contains t rest
      | []             -> false

  (*FIXME stub*)
  let is_main t = true

  (** Finds path from main to given thread taking
      into account the suffix path which is also used for
      detecting cyclic path. *)
  let rec creating_path_vs v t path =
    match creating_parent v t with
      | Some creator -> if contains creator path then None
                        else if is_main creator then Some [creator]
                        else creating_path_vs v creator (creator :: path)
      | None         -> None

  (** Wrapper around creating_path_vs,
      starts with empty suffix path. *)
  let creating_path v t =
    creating_path_vs v t []

end

(** Helper structure to store the current thread id in the
    thread analysis domain below. *)
module ThreadIdSet = SetDomain.Make (Basetype.Variables)

(** Thread analysis domain. Embeds the current thread id
    into the domain value. *)
module ThreadDomain = struct
  include Lattice.Prod (ThreadIdSet) (ThreadsVector)
  
  (** Entry state for the created thread. *)
  let make_entry d t = (ThreadIdSet.singleton t, snd d)
  
  (** State after creating a thread. *)
  let create_thread d t1 t2 =
    (fst d, ThreadsVector.create_thread (snd d) t1 t2)
 
  (** State after joining a thread. *) 
  let join_thread d t1 t2 =
    (fst d, ThreadsVector.join_thread (snd d) t1 t2)
    
  (** Retrieves the current thread component. *)  
  let current_thread d = fst d
  
  (** Returns true if thread t has been created only once. *)
  let is_unique_created d t = ThreadsVector.is_unique_created (snd d)
  
end
