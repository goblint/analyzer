(** Must active and have passed pthreadOnce calls ([pthreadOnce]). *)

open GoblintCil
open Analyses
module LF = LibraryFunctions
module Mval = PreValueDomain.Mval

module Spec =
struct
  module Onces = struct
    include SetDomain.ToppedSet (Mval) (struct let topname = "All onces" end)
    let name () = "mayOnces"
  end

  module ActiveOnces = struct
    include Lattice.Reverse (Onces)
    let name () = "active"
  end

  module SeenOnces = struct
    include Lattice.Reverse (Onces)
    let name () = "seen"
  end

  include Analyses.IdentitySpec

  let name () = "pthreadOnce"
  module D = Lattice.Prod (ActiveOnces) (SeenOnces)
  include Analyses.ValueContexts(D)

  let startstate v = (Onces.empty (), Onces.empty ())

  let possible_mvals (a: Queries.ask) barrier =
    Queries.AD.to_mval (a.f (Queries.MayPointTo barrier))

  let event man (e: Events.t) oman : D.t =
    (* Since the sets we track are must sets, it is not problenatic if different mvals refer to the same concrete thing, we are only imprecise in this case. *)
    match e with
    | Events.EnterOnce { once_control; ran } ->
      let (active, seen) = man.local in
      let ask = Analyses.ask_of_man man in
      let possible_vinfos = possible_mvals ask once_control in
      if not ran then
        (let unseen = List.filter (fun mval -> not (Onces.mem mval seen) && not (Onces.mem mval active)) possible_vinfos in
         match unseen with
         | [] -> raise Deadcode
         | [(v,_) as mval] when not (ask.f (Queries.IsMultiple v)) -> (Onces.add mval active, seen)
         | _ :: _ -> man.local)
      else
        (match possible_vinfos with
         | [(v,_) as mval] when not (ask.f (Queries.IsMultiple v)) -> (Onces.add mval active, seen)
         | _ -> man.local)
    | Events.LeaveOnce { once_control } ->
      (let (active, seen) = man.local in
       let ask = Analyses.ask_of_man man in
       let active' = Onces.diff active (Onces.of_list (possible_mvals ask once_control)) in
       let seen' = match possible_mvals ask once_control with
         | [(v,_) as mval] when not (ask.f (Queries.IsMultiple v)) -> Onces.add mval seen
         | _ -> seen
       in
       (active', seen'))
    | _ -> man.local

  let access man _ = man.local

  module A =
  struct
    include D
    let name () = "onces"
    let may_race (a1, s1) (a2, s2) =
      (* Following the paper, we check the commented thing below, but it simplifies to three disjointness checks and no unions *)
      (* (Onces.disjoint a1 (Onces.union a2 s2))) && (Onces.disjoint a2 (Onces.union a1 s1))) *)
      Onces.disjoint a1 a2 && Onces.disjoint a1 s2 && Onces.disjoint a2 s1
    let should_print (a1, s1) = not (Onces.is_empty a1) || not (Onces.is_empty s1)
  end


  let threadenter man ~multiple lval f args =
    let (_, seen) = man.local in
    [Onces.empty (), seen]

  let threadspawn man ~multiple lval f args fman = man.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
