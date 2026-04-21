(** Analysis for checking whether ghost globals are only accessed by one unique thread ([phaseGhostSplit]). *)

open Analyses
open GoblintCil

module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet

module Const =
struct
  include Lattice.Flat (IntOps.BigIntOps)
  let name () = "ghost-constant"
end

module Spec =
struct
  include IdentitySpec

  let name () = "phaseGhostSplit"

  module D = MapDomain.MapBot (Basetype.Variables) (Const)
  include ValueContexts (D)
  module P = IdentityP (D)

  module V = VarinfoV
  module G = Lattice.Chain (struct let n () = 99 let names i = string_of_int(i) end)

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

  let increment_constant lval rval =
    let is_same_lval e =
      match Cil.stripCasts e with
      | Lval lval' -> CilType.Lval.equal lval lval'
      | _ -> false
    in
    let is_one_constant e =
      match Cil.getInteger (Cil.constFold true e) with
      | Some k -> Z.equal k Z.one
      | None -> false
    in
    match Cil.stripCasts rval with
    | BinOp (PlusA, e1, e2, _) ->
      if is_same_lval e1 then
        is_one_constant e2
      else if is_same_lval e2 then
        is_one_constant e1
      else
        false
    | _ ->
      false

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

  let is_increment_by_one state lval rval =
    increment_constant lval rval ||
    match eval_const state (Lval lval), eval_const state rval with
    | Some old_value, Some new_value ->
      Z.equal new_value (Z.succ old_value)
    | _ ->
      false




  let sync man reason =
    if !AnalysisState.global_initialization then
      man.local
    else
      let may_be_advanced_here var =
        match man.ask (Queries.Owner var), man.ask Queries.CurrentThreadId with
        | `Bot, _
        | _, `Bot ->
          false
        | `Lifted owner, `Lifted tid ->
          if TID.equal owner tid then
            false
          else
            let created_threads = man.ask Queries.CreatedThreads in
            let owner_possibly_started =
              not (MHP.definitely_not_started (tid, created_threads) owner)
            in
            let owner_not_must_joined =
              not (ConcDomain.FiniteMustThreadSet.mem_lifted owner (man.ask Queries.MustJoinedThreads))
            in
            let below_max =
              match D.find var man.local with
              | `Lifted z ->
                let max = man.global var in
                Z.to_int z < max
              | _ -> failwith "invariant"
            in
            owner_possibly_started && owner_not_must_joined && below_max
        | `Lifted _, `Top -> true
        | `Top, `Lifted _
        | `Top, `Top ->
          failwith "assumption about ghost owner violated"
      in
      let rec handle_vars m = function
        | []  -> man.split m []
        | var :: vars ->
          if may_be_advanced_here var then
            (let v' = (match (D.find var m) with
                 | `Lifted x -> Z.succ x
                 | _ -> failwith "assumption")
             in
             let advanced = D.add var (`Lifted v') m in
             handle_vars advanced vars;
             handle_vars m vars)
          else
            handle_vars m vars
      in
      YamlWitness.VarSet.iter (fun var ->
          let owner = man.ask (Queries.Owner var) in
          let may_advance = may_be_advanced_here var in
          M.warn ~category:Witness "phaseGhostSplit: ghost %a has owner %a and may %s be advanced here"
            CilType.Varinfo.pretty var ThreadIdDomain.ThreadLifted.pretty owner
            (if may_advance then "" else " not ")
        ) !(YamlWitness.ghostVars);
      handle_vars man.local (YamlWitness.VarSet.elements !(YamlWitness.ghostVars));
      raise Deadcode


  let assign man lval rval =
    if !AnalysisState.global_initialization then
      man.local
    else
      match lval with
      | Var var, NoOffset when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
        (match eval_const man.local (Lval lval) with
         | Some z ->
           (let v = Z.succ z in
            let i = Z.to_int v in
            man.sideg var i;
            M.warn ~category:Witness "phaseGhostSplit: ghost %a has max %i" CilType.Varinfo.pretty var i;
            D.add var (`Lifted v) man.local)
         | None -> failwith "Failed to evaluate ghost to constant")
      | _ ->
        man.local
end

let _ =
  MCP.register_analysis ~dep:["access"; "threadid"; "phaseGhost"] (module Spec : MCPSpec)
