open PrecCompareUtil

module MyNode =
struct
  include Node
  (* Override the name to "nodes", as plural fits better in the output format of PrePrivPrecCompare *)
  let name () = "nodes"
  let to_location n = Node.location n
end

(* Currently serialization of Apron results only works for octagons. *)
module type Util =
  functor (D2: RelationDomain.RelD2) ->
  sig
    include module type of struct include Util (MyNode) (D2) end
    type marshal
    type dump = marshal dump_gen
    type result = Dom.t RH.t result_gen

    val init: unit -> unit

    val unmarshal : marshal -> D2.t RH.t
  end

module DummyUtil : Util =
  functor (D2: RelationDomain.RelD2) ->
  struct
    include Util (MyNode) (D2)
    type marshal = D2.marshal RH.t
    type dump = marshal dump_gen
    type result = Dom.t RH.t result_gen

    let init () = ()

    let unmarshal (m : marshal) : D2.t RH.t = RH.map (fun _ -> D2.unmarshal) m
  end
