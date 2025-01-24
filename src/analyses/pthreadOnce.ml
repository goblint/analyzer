(** Must active and have passed pthreadOnce calls ([pthreadOnce]). *)

open GoblintCil
open Analyses
module LF = LibraryFunctions

module Spec =
struct
  module Onces = struct
    include SetDomain.ToppedSet (CilType.Varinfo) (struct let topname = "All onces" end)
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

  include Analyses.DefaultSpec

  let name () = "pthreadOnce"
  module D = Lattice.Prod (ActiveOnces) (SeenOnces)
  include Analyses.ValueContexts(D)

  (* transfer functions *)
  let assign man (lval:lval) (rval:exp) : D.t =
    man.local

  let branch man (exp:exp) (tv:bool) : D.t =
    man.local

  let body man (f:fundec) : D.t =
    man.local

  let return man (exp:exp option) (f:fundec) : D.t =
    man.local

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [man.local, man.local]

  let combine_env man lval fexp f args fc au f_ask =
    au

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    man.local

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    man.local

  let startstate v = (Onces.empty (), Onces.empty ())

  let possible_vinfos (a: Queries.ask) barrier =
    Queries.AD.to_var_may (a.f (Queries.MayPointTo barrier))

  let event man (e: Events.t) oman : D.t =
    match e with
    | Events.EnterOnce { once_control; tf } when tf ->
      (let (active, seen) = man.local in
       let ask = Analyses.ask_of_man man in
       let possible_vinfos = possible_vinfos ask once_control in
       let unseen = List.filter (fun v -> not (Onces.mem v seen) && not (Onces.mem v active)) possible_vinfos in
       match unseen with
       | [] -> raise Deadcode
       | [v] -> (Onces.add v active, seen)
       | _ :: _ -> man.local)
    | Events.EnterOnce { once_control; tf } ->
      (let (active, seen) = man.local in
       let ask = Analyses.ask_of_man man in
       match possible_vinfos ask once_control with
       | [v] -> (Onces.add v active, seen)
       | _ -> man.local)
    | Events.LeaveOnce { once_control } ->
      (let (active, seen) = man.local in
       let ask = Analyses.ask_of_man man in
       let active' = Onces.diff active (Onces.of_list (possible_vinfos ask once_control)) in
       let seen' = match possible_vinfos ask once_control with
         | [v] -> Onces.add v seen
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
      (Onces.is_empty (Onces.inter a1 (Onces.union a2 s2))) && (Onces.is_empty (Onces.inter a2 (Onces.union a1 s1)))
    let should_print f = true
  end


  let threadenter man ~multiple lval f args =
    let (_, seen) = man.local in
    [Onces.empty (), seen]

  let threadspawn man ~multiple lval f args fman = man.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
