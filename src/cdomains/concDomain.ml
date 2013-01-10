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

(** Value names for thread state lattices below. *)
module ThreadCJNames = struct
  let truename = "created" 
  let falsename = "joined"
end

(** Base values for thread state. Bottom (zero)
    and top (many) are added later. *)
module ThreadCJState = struct
  include IntDomain.MakeBooleans (ThreadCJNames)
end

(** Value names for bottom (zero) and top (many). *)
module ThreadLiftNames = struct
  let bot_name = "zero" 
  let top_name = "many"
end

(** Rhomb lattice for thread states. *)
module ThreadState = struct
  include Lattice.Flat (ThreadCJState) (ThreadLiftNames)
end

(** Strips grouping (see Groupable) information. Makes
    threads state printout less verbose (aux thread variable
    for the main thread is defined as global variable). *)
module Variables = MapDomain.StripClasses (Basetype.Variables)

(** Single vector-like domain (tid -> state) for thread states. *)
module ThreadVector = struct
  include MapDomain.MapBot (Variables) (ThreadState)
  
  let zero = ThreadState.bot()
  let many_many = ThreadState.top()
  
  let created = `Lifted true
  let joined = `Lifted false
  
  (** Helper method for thread creation. Used by
      the transfer function for creation edges. *)
  let create_thread v t =
    let o = (find t v) in
    let n = if o == zero then created else many_many in
    add t n (remove t v)
    
  (** Helper method for thread join. Used by
      the transfer function for join edges. *)    
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
  
  (** Helper method for thread creation. Used by
      the transfer function for creation edges. *)
  let create_thread v t1 t2 =
    let o = (find t1 v) in
    let n = ThreadVector.create_thread o t2 in
    add t1 n (remove t1 v)
  
  (** Helper method for thread join. Used by
      the transfer function for join edges. *)  
  let join_thread v t1 t2 =
    let o = (find t1 v) in
    let n = ThreadVector.join_thread o t2 in
    add t1 n (remove t1 v)
  
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

  (** FIXME stub. Identifies the main thread. Needs to be
      consistent with the main thread id definition in thread.ml. *)
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
    match creating_path_vs v t [] with
      | Some path -> Some (List.rev path)
      | None      -> None
    
  (** Implementation of unique predicate. Tries to find
      the unique creation path from main to this thread and
      returns whether is exists. *)
  let is_unique v t =
    match creating_path v t with
      | Some _ -> true
      | None   -> false

  (** Returns true if the given thread has not been
      created in the threads vector. *)
  let is_not_created v t =
    None == creating_parent v t
  
  (** Helper method to calculate last common prefix element for the given two list
      and aux element t which is returned in the case that the lists do
      not have common prefix. *)
  let rec lcpe l1 l2 t =
    if l1 == [] then t
    else if l2 == [] then t
    else if List.hd l1 == List.hd l2 then lcpe (List.tl l1) (List.tl l2) (List.hd l1)
    else t
  
  (** Helper method to calculate LCA for two paths represented
      as lists. Assumes that first element of both is main thread. *)  
  let lcap l1 l2 =
    lcpe (List.tl l1) (List.tl l2) (List.hd l1)
    
  (** Calculates least common anchestor (LCA) per given two threads and the vector. *)
  let lca v t1 t2 =
    let path1 = creating_path v t1 in
    let path2 = creating_path v t2 in
    match path1 with
      | Some p1 -> begin
                     match path2 with
                       | Some p2 -> lcap p1 p2
                       | None    -> None
                     end
      | None -> None  

  (** FIXME stub. Returns true when there is full join path from
      t1 to t2 (t2 is joined with respect to t1). *)
  let is_join_path v t1 t2 = true

  (** Returns whether the Case B holds in the current vector
      for given two threads. *)
  let is_case_b_in_value v t1 t2 =
    let t_lca = lca v t1 t2 in
    match t_lca with
      | Some t -> is_unique v t
                  && (
                    (is_join_path v t t1 && is_unique v t2)
                    or (is_join_path v t t2 && is_unique v t1)
                  ) 
      | None   -> false

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
  
  (** Transfer function. State after creating a thread. *)
  let create_thread d t1 t2 =
    (fst d, ThreadsVector.create_thread (snd d) t1 t2)
 
  (** Transfer function. State after joining a thread. *) 
  let join_thread d t1 t2 =
    (fst d, ThreadsVector.join_thread (snd d) t1 t2)
    
  (** Retrieves the current thread component. *)  
  let current_thread d = fst d
  
  (** Returns true if thread t has been created only once. *)
  let is_unique d t = ThreadsVector.is_unique (snd d)
  
  (** Given two domain values representing two program
      points including current thread value, returns whether
      the Case A holds between them. *)
  let is_case_a d1 d2 =
    ThreadsVector.is_not_created (snd d2) (fst d1)
    or ThreadsVector.is_not_created (snd d1) (fst d2)
  
  (** Given two domain values in two program points,
      returns whether the Case B holds between them. *)  
  let is_case_b d1 d2 =
    ThreadsVector.is_case_b_in_value (snd d1) (fst d1) (fst d2)
    or ThreadsVector.is_case_b_in_value (snd d2) (fst d1) (fst d2)
  
  (** For two program points, check whether the points
      are non-parallel by checking whether Case A or Case B applies. *)  
  let is_non_parallel d1 d2 =
    is_case_a d1 d2
    or is_case_b d1 d2
  
end

module ThreadStringSet = 
struct 
  include SetDomain.ToppedSet (Printable.Strings) (struct let topname = "All Threads" end)

  let toXML_f sf x = 
    match toXML x with
      | Xml.Element (node, [text, _], elems) -> 
          let summary = "Thread: " ^ sf Goblintutil.summary_length x in
            Xml.Element (node, [text, summary], elems)
      | x -> x
      
  let toXML s  = toXML_f short s
end
