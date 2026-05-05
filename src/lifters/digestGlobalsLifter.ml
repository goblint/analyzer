(** Lifts a [Spec] with the context gas variable. The gas variable limits the number of context-sensitively analyzed function calls in a call stack.
    For every function call the gas is reduced. If the gas is zero, the remaining function calls are analyzed without context-information *)

open Batteries
open GoblintCil
open Analyses
open GobConfig

module M = Messages

module Lifter (S:Spec)
  : Spec with module D = S.D
          and module C = S.C
          (* and module G = S.G *)
=
struct
  include S

  module DigestSet = SetDomain.Make(S.P)
  module G = struct
    include Lattice.Lift2 (S.G) (DigestSet)

    let to_global g = `Lifted1 g
    let to_digest_set g = `Lifted2 g

    let global (g: t) = match g with
      | `Lifted1 g -> g
      | `Bot -> G.bot ()
      | `Top
      | `Lifted2 _ -> failwith "Expected Lifted1"

    let digest_set (g: t) = match g with
      | `Lifted2 g -> g
      | `Bot -> DigestSet.bot ()
      | `Top
      | `Lifted1 _ -> failwith "Expected Lifted2"
  end

  module VarWithDigest = Printable.Prod (S.V) (S.P)
  module V = struct
    include Printable.Either (S.V) (VarWithDigest)
    let is_write_only = function
      | `Left var -> S.V.is_write_only var
      | `Right (var, _digest) -> S.V.is_write_only var

    let get_var = function
      | `Left var
      | `Right (var, _) -> var

    let var v = `Left v
    let var_with_digest (v, digest) : t = `Right (v, digest)

    (* Does not matter, not used in lifters above *)
    let use_digest _ = failwith "use_digest not implemented for DigestGlobalLifter.V"
  end


  let conv man : (S.D.t, S.G.t, S.C.t, S.V.t) Analyses.man =
    (* TODO: This should look up use_digest, and potentially look up variables with all possible digests that are compatible with the current digest *)
    let global (v : S.V.t) : S.G.t =
      if S.V.use_digest v then
        (* TODO: Should actually read from all compatible digests *)
        (* TODO: There be a need for some analysis to read the value for the current digest *)

        let digests = man.global (V.var v) in
        let digests = G.digest_set digests in

        let current_digest = P.of_elt man.local in

        let read_and_add_digest d acc =
          if true then (* TODO: Use [compatible man current_digest digest as check *)
            let g = man.global (V.var_with_digest (v, d)) in
            let g = G.global g in
            S.G.join acc g
          else
            acc
        in
        DigestSet.fold read_and_add_digest digests (S.G.bot ())
      else
        G.global (man.global (V.var v))
    in
    let sideg (v: S.V.t) (g: S.G.t) : unit =
      let g = G.to_global g in
      if S.V.use_digest v then
        let digest = P.of_elt man.local in
        man.sideg (V.var v) (G.to_digest_set (DigestSet.singleton digest));
        man.sideg (V.var_with_digest (v, digest)) g
      else
        man.sideg (V.var v) g
    in
    {
      man with
      global;
      sideg
    }

  let context man f d = S.context (conv man) f d

  let sync man reason = S.sync (conv man) reason
  let query man (type a) (q : a Queries.t) : a Queries.result =
    let conv_v v =
      let v : V.t = Obj.obj v in
      let v = V.get_var v in
      Obj.repr v
    in
    match (q : a Queries.t) with
    | Queries.WarnGlobal v ->
      let v = conv_v v in
      S.query (conv man) (WarnGlobal v)
    | Queries.InvariantGlobal v ->
      let v = conv_v v in
      S.query (conv man) (InvariantGlobal v)
    | Queries.YamlEntryGlobal (v, t) ->
      let v = conv_v v in
      S.query (conv man) (YamlEntryGlobal (v, t))
    | Queries.IterSysVars (vq, vqf) ->
      let vqf (v (*: S.V.t as Obj.t*)) =
        let v : S.V.t = Obj.obj v in
        let v = if S.V.use_digest v then
            let digest = S.P.of_elt man.local in
            V.var_with_digest (v, digest)
          else
            V.var v
        in
        let v = Obj.repr v in
        vqf v
      in
      S.query (conv man) (Queries.IterSysVars (vq, vqf))
    | _ ->
      S.query (conv man) q

  let assign man l e = S.assign (conv man) l e

  let vdecl man v = S.vdecl (conv man) v

  let branch man e b = S.branch (conv man) e b

  let body man f = S.body (conv man) f

  let return man exp f = S.return (conv man) exp f

  let asm man = S.asm (conv man)

  let skip man = S.skip (conv man)

  let special man lv v args = S.special (conv man) lv v args

  let enter man lv f args = S.enter (conv man) lv f args

  let combine_assign man lv f_exp f args c fd f_a =
    S.combine_assign (conv man) lv f_exp f args c fd f_a

  let combine_env man lv f_exp f args c fd f_a =
    S.combine_env (conv man) lv f_exp f args c fd f_a

  let paths_as_set man = S.paths_as_set (conv man)

  let threadenter man ~multiple lv v args = S.threadenter (conv man) ~multiple lv v args

  let threadspawn man ~multiple lv v args f_man = S.threadspawn (conv man) ~multiple lv v args (conv f_man)

  let event man event oman = S.event (conv man) event (conv oman)

  let compatible man p p' = S.compatible (conv man) p p'
end
