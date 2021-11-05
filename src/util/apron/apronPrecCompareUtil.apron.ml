open PrecCompareUtil

(* Currently serialization of Apron results only works for octagons. *)
module Man = ApronDomain.OctagonManager
module D = ApronDomain.D2 (Man)

module Node =
struct
  include Node
  (* Override the name to "nodes", as plural fits better in the output format of PrePrivPrecCompare *)
  let name () = "nodes"
  let to_location n = Node.location n
end

module Util =
struct
  include Util (Node) (D)
  type marshal = (Man.mt Apron.Abstract0.t * string array) RH.t
  type dump = marshal dump_gen
  type result = Dom.t RH.t result_gen

  let init () =
    Apron.Manager.set_deserialize Man.mgr

  let unmarshal (m: marshal): Dom.t RH.t =
    RH.map (fun _ -> D.unmarshal) m
end
