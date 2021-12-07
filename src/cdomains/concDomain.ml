module LiftHoarePO (Base: ThreadIdDomain.Stateful) =
struct
  include Base

  let join x y =
    if leq x y then
      y
    else if leq y x then
      x
    else
      failwith "join"
  let widen = join
  let meet x y =
    if leq x y then
      x
    else if leq y x then
      y
    else
      failwith "meet"
  let narrow = meet
  let pretty_diff () (x, y) = failwith "pretty_diff"
end

module ThreadSetBase = HoareDomain.Set_LiftTop (Lattice.LiftPO (LiftHoarePO (ThreadIdDomain.Thread)) (Printable.DefaultNames)) (struct let topname = "All Threads" end)

module ThreadSet =
struct
  include ThreadSetBase

  let of_list' ts: t =
    ts
    |> List.map (fun t -> `Lifted t)
    |> of_list

  let elements' (ts: t) =
    ts
    |> elements
    |> List.map (function
        | `Lifted t -> t
        | _ -> failwith "elements"
      )

  let add' t ts = add (`Lifted t) ts

  let choose' ts = match choose ts with
    | `Lifted t -> t
    | _ -> failwith "choose"

  let remove' t ts = remove (`Lifted t) ts

  let mem' t ts = mem (`Lifted t) ts
  let singleton' t = singleton (`Lifted t)

  let exists' p ts = exists (function
      | `Lifted t -> p t
      | _ -> false
    ) ts
end

module MustThreadSet =
struct
  include SetDomain.Reverse (ThreadSetBase)

  let of_list' ts: t =
    ts
    |> List.map (fun t -> `Lifted t)
    |> of_list

  let elements' (ts: t) =
    ts
    |> elements
    |> List.map (function
        | `Lifted t -> t
        | _ -> failwith "elements"
      )

  let add' t ts = add (`Lifted t) ts
end

(* module ThreadSet = SetDomain.ToppedSet (ThreadIdDomain.Thread) (struct let topname = "All Threads" end) *)
(* module MustThreadSet = SetDomain.Reverse(ThreadSetBase) *)

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
