(** Analysis for checking whether ghost globals are only accessed by one unique thread ([phaseGhost]). *)

open Analyses
open GoblintCil

module TID = ThreadIdDomain.Thread
module ID = IntDomain.IntDomTuple

module TIDs = ConcDomain.ThreadSet

module IncByOne =
struct
  include BoolDomain.MustBool
  let name () = "increment-by-one"
end

module Spec =
struct
  module D = Lattice.Unit
  include IdentityUnitContextsSpec

  let name () = "phaseGhost"

  module V = VarinfoV
  module G =
  struct
    include Lattice.Prod (TIDs) (IncByOne)
    let tids = fst
    let inc_by_one = snd
    let create_tids tids = (tids, IncByOne.bot ())
    let create_inc_by_one inc_by_one = (TIDs.bot (), inc_by_one)
  end

  let startstate _ = ()
  let exitstate _ = ()

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
    let is_positive_constant e =
      match Cil.getInteger (Cil.constFold true e) with
      | Some k -> Z.gt k Z.zero
      | None -> false
    in
    match Cil.stripCasts rval with
    | BinOp (PlusA, e1, e2, _) ->
      if is_same_lval e1 then
        is_positive_constant e2
      else if is_same_lval e2 then
        is_positive_constant e1
      else
        false
    | _ ->
      false

  let is_increment_by_one man lval rval =
    if increment_constant lval rval then
      true
    else
      let old_value = man.ask (Queries.EvalInt (Lval lval)) in
      let new_value = man.ask (Queries.EvalInt rval) in
      match old_value, new_value with
      | `Lifted old_value, `Lifted new_value ->
        let ik = Cilfacade.get_ikind_exp (Lval lval) in
        let expected_new_value = ID.add old_value (ID.of_int ik Z.one) in
        ID.equal expected_new_value new_value
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
    begin match lval with
      | Var var, NoOffset when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
        man.sideg var (G.create_inc_by_one (is_increment_by_one man lval rval))
      | _ ->
        ()
    end;
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
           M.warn_noloc ~category:Witness "phaseGhost: global %a is accessed by multiple unique threads: %a" CilType.Varinfo.pretty g TIDs.pretty tidset
        )
    | _ ->
      Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["access"; "threadid"] (module Spec : MCPSpec)
