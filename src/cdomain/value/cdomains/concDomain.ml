(** Domains for thread sets and their uniqueness. *)

module ThreadSet =
struct
  include SetDomain.Make (ThreadIdDomain.Thread)

  let is_top = mem UnknownThread

  let top () = singleton UnknownThread

  let merge uop cop x y =
    match is_top x, is_top y with
    | true, true -> uop x y
    | false, true -> x
    | true, false -> y
    | false, false -> cop x y

  let meet x y = merge join meet x y

  let narrow x y = merge (fun x y -> widen x (join x y)) narrow x y

end
module MustThreadSet = SetDomain.Reverse(ThreadSet)

module CreatedThreadSet = ThreadSet

module ThreadCreation =
struct
  module UNames = struct
    let name = "unique"
    let true_name  = "repeated"
    let false_name = "unique"
  end
  module Uniqueness = BoolDomain.MakeMayBool (UNames)
  module ParentThreadSet =
  struct
    include ThreadSet
    let name () = "parents"
  end
  module DirtyExitNames =
  struct
    let name = "exit"
    let true_name = "dirty exit"
    let false_name = "clean exit"
  end

  (* A thread exits cleanly iff it joined all threads it started, and they also all exit cleanly *)
  module DirtyExit = BoolDomain.MakeMayBool (DirtyExitNames)
  include Lattice.Prod3 (Uniqueness) (ParentThreadSet) (DirtyExit)
end


module ThreadStringSet = SetDomain.ToppedSet (Printable.Strings) (struct let topname = "All Threads" end)
