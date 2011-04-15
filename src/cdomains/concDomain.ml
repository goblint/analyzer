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

(* Thread state where state is chain. *)

module ThreadStateNames = struct
  exception InvalidStateValue
  let n = 4
  
  let zero = 0
  let joined = 1
  let created = 2
  let many_many = 3
  
  let names = function
    | 0 -> "zero"
    | 1 -> "joined"
    | 2 -> "created"
    | 3 -> "many/many"
    | _ -> raise InvalidStateValue
end

module ThreadState = struct
  include Lattice.Chain (ThreadStateNames)
end

module ThreadDomain = struct
  include MapDomain.MapBot (Basetype.Variables) (ThreadState)
  
  module V = ThreadStateNames
  
  let create_thread t m =
    let o = (find t m) in
    let n = if o == V.zero then V.created else V.many_many in
    add t n (remove t m)
  
  let join_thread t m =
    let o = (find t m) in
    let n = if o == V.created then V.joined else V.many_many in
    add t n (remove t m)
    
end

(* Alternative domain where the thread state is a rhomb lattice. *)

module ThreadRhombNames = struct
  let truename = "created" 
  let falsename = "joined"
end

module ThreadRhombCJState = struct
  include IntDomain.MakeBooleans (ThreadRhombNames)
end

module ThreadRhombLiftNames = struct
  let bot_name = "zero" 
  let top_name = "many/many"
end

module ThreadRhombState = struct
  include Lattice.Flat (ThreadRhombCJState) (ThreadRhombLiftNames)
end

module ThreadRhombDomain = struct
  include MapDomain.MapBot (Basetype.Variables) (ThreadRhombState)
  
  let zero = ThreadRhombState.bot()
  let many_many = ThreadRhombState.top()
  
  let created = `Lifted true
  let joined = `Lifted false
  
  let create_thread t m =
    let o = (find t m) in
    let n = if o == zero then created else many_many in
    add t n (remove t m)
    
  let join_thread t m =
    let o = (find t m) in
    let n = if o == created then joined else many_many in
    add t n (remove t m)
    
end

module Glob = 
struct
  module Var = Basetype.Variables
  module Val = ThreadState
end
