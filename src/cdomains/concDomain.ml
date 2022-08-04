module ThreadSet = SetDomain.ToppedSet (ThreadIdDomain.Thread) (struct let topname = "All Threads" end)
module MustThreadSet = SetDomain.Reverse(ThreadSet)

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
