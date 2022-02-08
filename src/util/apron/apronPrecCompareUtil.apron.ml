open PrecCompareUtil
open ApronDomain
open RelationPrecCompareUtil

(* Currently serialization of Apron results only works for octagons. *)
module OctagonD = ApronDomain.AD2Complete (ApronDomain.OctagonManager)
module Util =
functor (D2: RelationDomain.RelD2) ->
struct
  include Util (MyNode) (D2)
  type marshal = D2.marshal RH.t
  type dump = marshal dump_gen
  type result = Dom.t RH.t result_gen

  let init () =
    Apron.Manager.set_deserialize OctagonManager.mgr

  let unmarshal (m: marshal): D2.t RH.t =
    RH.map (fun _ -> D2.unmarshal) m
end

include Util(OctagonD)
