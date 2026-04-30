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
          and module G = S.G
=
struct
  include S

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
    let var_with_digest (v, digest) = `Right (var, digest)

    (* Does not matter, not used in lifters above *)
    let use_digest _ = failwith "use_digest not implemented for DigestGlobalLifter.V"
  end


  let conv man : (S.D.t, S.G.t, S.C.t, S.V.t) Analyses.man =
    (* TODO: This should look up use_digest, and potentially look up variables with all possible digests that are compatible with the current digest *)
    let global (v : S.V.t) : S.G.t = man.global (V.var v) in

    (* TODO: This should look up use_digest, and potentially add the current digest *)
    let sideg (v: S.V.t) (g: S.G.t) : unit = man.sideg (V.var v)  g in
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
