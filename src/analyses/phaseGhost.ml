(** Analysis for checking whether ghost globals are only accessed by one unique thread
    and have known lower and upper bounds ([phaseGhost]). *)

open Analyses
open GoblintCil

module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet

module Const =
struct
  include Lattice.Flat (IntOps.BigIntOps)
  let name () = "ghost-constant"
end

module Bounded =
struct
  include BoolDomain.MustBool
  let name () = "bounded"
end

module Spec =
struct
  include IdentitySpec

  (* TODO: Do we allow multiple ghosts per thread? *)

  let name () = "phaseGhost"

  module D = MapDomain.MapBot (Basetype.Variables) (Const)
  include ValueContexts (D)
  module P = UnitP

  module V = VarinfoV
  module G =
  struct
    include Lattice.Prod (TIDs) (Bounded)
    let tids = fst
    let bounded = snd
    let create_tids tids = (tids, Bounded.bot ())
    let create_bounded bounded = (TIDs.bot (), bounded)
  end

  let initial_ghost_values () =
    List.fold_left (fun acc -> function
        | GVar (v, initinfo, _) when YamlWitness.VarSet.mem v !(YamlWitness.ghostVars) ->
          begin match initinfo.init with
            | Some (SingleInit exp) ->
              begin match Cil.getInteger (Cil.constFold true exp) with
                | Some z -> D.add v (`Lifted z) acc
                | None -> acc
              end
            | None when Cil.isIntegralType v.vtype ->
              D.add v (`Lifted Z.zero) acc
            | _ ->
              acc
          end
        | _ ->
          acc
      ) (D.bot ()) !Cilfacade.current_file.globals

  let startstate _ = initial_ghost_values ()
  let exitstate _ = initial_ghost_values ()

  let tids_of_current_thread man =
    match man.ask Queries.CurrentThreadId with
    | `Lifted tid when TID.is_unique tid -> TIDs.singleton tid
    | _ -> TIDs.top ()

  (* This local constant folding intentionally disregards writes from other threads.
     It is only for the phaseGhost checker itself. *)
  (* This information must **not** be used to refine other analyses or returned by any query,
     because it is unsound in the presence of other threads interfering. By the same token, it must
     be not used to raise Deadcode in branch. *)
  let rec eval_const state e =
    match Cil.stripCasts e with
    | Const _ ->
      Cil.getInteger (Cil.constFold true e)
    | Lval (Var var, NoOffset) when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
      begin match D.find_opt var state with
        | Some (`Lifted z) -> Some z
        | _ -> None
      end
    | UnOp (Neg, e, _) ->
      Option.map Z.neg (eval_const state e)
    | BinOp (PlusA, e1, e2, _)
    | BinOp (IndexPI, e1, e2, _)
    | BinOp (PlusPI, e1, e2, _) ->
      Option.bind (eval_const state e1) (fun z1 ->
          Option.map (Z.add z1) (eval_const state e2)
        )
    | BinOp (MinusA, e1, e2, _) ->
      Option.bind (eval_const state e1) (fun z1 ->
          Option.map (Z.sub z1) (eval_const state e2)
        )
    | BinOp (Mult, e1, e2, _) ->
      Option.bind (eval_const state e1) (fun z1 ->
          Option.map (Z.mul z1) (eval_const state e2)
        )
    | _ ->
      None

  let is_bounded_update man lval rval =
    match eval_const man.local rval with
    | Some _ -> true
    | _ ->
      match man.ask (Queries.EvalInt rval) with
      | `Lifted value ->
        let module ID = IntDomain.IntDomTuple in
        not (ID.is_top_of (ID.ikind value) value)
        && Option.is_some (ID.minimal value)
        && Option.is_some (ID.maximal value)
      | _ -> false

  let event man e oman =
    match e with
    | Events.Access {ad; kind ; _ }  when kind = AccessKind.Read ->
      (* Reads are fine *)
      man.local
    | Events.Access {ad; _; } ->
      (* TODO: This is very involved to iterate over everything here *)
      let tids = tids_of_current_thread man in
      Queries.AD.iter (function
          | Queries.AD.Addr.Addr (var, _) when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
            man.sideg var (G.create_tids tids)
          | _ ->
            ()
        ) ad;
      man.local
    | _ ->
      man.local

  let assign man lval rval =
    if !AnalysisState.global_initialization then
      man.local
    else
      match lval with
      | Var var, NoOffset when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
        let bounded = is_bounded_update man lval rval in
        let local =
          match bounded, eval_const man.local rval with
          | true, Some z -> D.add var (`Lifted z) man.local
          | _ -> D.add var (Const.top ()) man.local
        in
        man.sideg var (G.create_bounded bounded);
        local
      | _ ->
        man.local

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.IsPhaseGhost var when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
      let unique_owner tids =
        if TIDs.is_top tids then
          false
        else
          match TIDs.elements tids with
          | [] ->
            true
          | [tid] when TID.is_unique tid ->
            true
          | _ ->
            false
      in
      let global = man.global var in
      G.bounded global && unique_owner (G.tids global)
    | Queries.Owner var when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
      let tidset = G.tids (man.global var) in
      begin match TIDs.elements tidset with
        | [] ->
          `Bot
        | [tid] when TID.is_unique tid ->
          `Lifted tid
        | _ ->
          `Top
      end
    | Queries.WarnGlobal g ->
      let g: V.t = Obj.obj g in
      let global = man.global g in
      let tidset = G.tids global in
      if TIDs.is_top tidset then
        (M.warn_noloc ~category:Witness "phaseGhost: global %a is accessed by a non-unique or unknown thread id" CilType.Varinfo.pretty g;)
      else
        (match TIDs.elements tidset with
         | [tid] when TID.is_unique tid ->
           if G.bounded global then
             M.info_noloc ~category:Witness "phaseGhost: global %a is only accessed by unique thread %a and has known lower and upper bounds" CilType.Varinfo.pretty g TID.pretty tid
           else
             M.warn_noloc ~category:Witness "phaseGhost: global %a is only accessed by unique thread %a, but does not have known lower and upper bounds" CilType.Varinfo.pretty g TID.pretty tid
         | _ ->
           M.warn_noloc ~category:Witness "phaseGhost: global %a is accessed by multiple unique threads: %a" CilType.Varinfo.pretty g TIDs.pretty tidset)
    | _ ->
      Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["access"; "threadid"] (module Spec : MCPSpec)
