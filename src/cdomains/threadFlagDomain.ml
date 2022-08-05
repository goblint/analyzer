module type S =
sig
  include Lattice.S
  val is_multi: t -> bool
  val is_not_main: t -> bool
  val get_single: unit -> t
  val get_multi: unit -> t
  val get_main:  unit -> t
end

(* unused *)
module Trivial: S =
struct
  module TrivialNames =
  struct
    let truename = "Multithreaded"
    let falsename = "Singlethreaded"
  end
  include IntDomain.MakeBooleans (TrivialNames)

  let is_multi x = x
  let is_not_main x = x
  let get_single () = false
  let get_multi () = true
  let get_main  () = true
end

(** Thread flag which distinguishes main thread as unique. *)
module Simple: S =
struct
  module SimpleNames =
  struct
    let n () = 3
    let names = function
      | 0 -> "Singlethreaded"
      | 1 -> "Multithreaded (main)"
      | 2 -> "Multithreaded (other)"
      | _ -> "WHAT??"
  end
  include Lattice.Chain (SimpleNames)
  let is_multi x = x > 0
  let is_not_main x = x > 1
  let get_multi () = 2
  let get_main  () = 1
  let get_single () = 0
  let name () = "MT mode"
end
