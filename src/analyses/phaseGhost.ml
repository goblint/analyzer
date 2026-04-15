(** Analysis for checking whether ghost globals are only accessed by one unique thread ([phaseGhost]). *)

open Analyses
open GoblintCil

module TID = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet

module Const =
struct
  include Lattice.Flat (IntOps.BigIntOps)
  let name () = "ghost-constant"
end

module IncByOne =
struct
  include BoolDomain.MustBool
  let name () = "increment-by-one"
end

module Spec =
struct
  include IdentitySpec

  let name () = "phaseGhost"

  module D = MapDomain.MapBot (Basetype.Variables) (Const)
  include ValueContexts (D)
  module P = IdentityP (D)

  module V = VarinfoV
  module G =
  struct
    include Lattice.Prod (TIDs) (IncByOne)
    let tids = fst
    let inc_by_one = snd
    let create_tids tids = (tids, IncByOne.bot ())
    let create_inc_by_one inc_by_one = (TIDs.bot (), inc_by_one)
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

  let event man e oman =
    match e with
    | Events.Access {ad; _} ->
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
        let inc_by_one = is_increment_by_one man.local lval rval in
        let local =
          match inc_by_one, eval_const man.local rval with
          | true, Some z -> D.add var (`Lifted z) man.local
          | _ -> D.add var (Const.top ()) man.local
        in
        man.sideg var (G.create_inc_by_one inc_by_one);
        local
      | _ ->
        man.local

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.WarnGlobal g ->
      let g: V.t = Obj.obj g in
      let (tidset, inc_by_one) = man.global g in
      if TIDs.is_top tidset then
        M.warn_noloc ~category:Witness "phaseGhost: global %a is accessed by a non-unique or unknown thread id" CilType.Varinfo.pretty g
      else
        (match TIDs.elements tidset with
         | [tid] ->
           if inc_by_one then
             M.info_noloc ~category:Witness "phaseGhost: global %a is only accessed by unique thread %a and is only ever increased by one" CilType.Varinfo.pretty g TID.pretty tid
           else
             M.warn_noloc ~category:Witness "phaseGhost: global %a is only accessed by unique thread %a, but is not only ever increased by one" CilType.Varinfo.pretty g TID.pretty tid
         | _ ->
           M.warn_noloc ~category:Witness "phaseGhost: global %a is accessed by multiple unique threads: %a" CilType.Varinfo.pretty g TIDs.pretty tidset)
    | _ ->
      Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["access"; "threadid"] (module Spec : MCPSpec)
