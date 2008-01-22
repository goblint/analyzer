module TrivialNames = struct
  let truename = "Multithreaded" 
  let falsename = "Singlethreaded"
end

module Trivial = struct 
  include IntDomain.MakeBooleans (TrivialNames)
  let is_multi x = x
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
  let get_multi () = 2
  let get_main  () = 1
  let switch x y = match x,y with
    | 0,0 -> false
    | 0,_ -> true
    | _,0 -> true
    | _   -> false
end
