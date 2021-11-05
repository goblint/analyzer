open Prelude


(** A printable, where each element is related to one location.
 Multiple elements might be related to the same location. *)
module type LocalizedPrintable =
sig
  include Printable.S
  val to_location: t -> CilType.Location.t
end

type 'a dump_gen = {
  name: string;
  marshalled: 'a;
}

type 'a result_gen = {
  name: string;
  results: 'a;
}

module type R =
sig
  module Key: LocalizedPrintable
  module Dom: Lattice.S

  (** Module for the result hash map *)
  module RH : Hashtbl.S with type key = Key.t

  (** Type to which the result hashmap is actually convered before storing it to disc *)
  type marshal

  (** Wrapper of marhshal type, together with a name for the analysis + privatization combination *)
  type dump = marshal dump_gen

  (** Type of the actually usable result. To be obtained from the dump via the unmarshal function below  *)
  type result = Dom.t RH.t result_gen

  (** Maps the marshalled version of the result hashmap back to a usable version *)
  val unmarshal: marshal -> Dom.t RH.t
end

module type S =
sig
  include R
  val init: unit -> unit
end

module Util (Key: LocalizedPrintable) (Dom: Lattice.S) : R with module Key = Key and module Dom = Dom =
struct
  module Key = Key
  module Dom = Dom
  (* module for result hashmap *)
  module RH = Hashtbl.Make (Key)

  type marshal = Dom.t RH.t
  type dump = marshal dump_gen
  type result = Dom.t RH.t result_gen

  (* Leave loaded value untouched by default *)
  let unmarshal x = x
end

module LV =
struct
  include Printable.Prod (CilType.Location) (Basetype.Variables)
  let name () = "location variables"
  type marshal = t
  let pretty () (l, v) = Pretty.dprintf "%a %a" CilType.Location.pretty l Basetype.Variables.pretty v
  let to_location = fst
end

module LVUtil =
struct
  include Util (LV) (BaseDomain.VD)
  let init () =
    Cil.initCIL (); (* ValueDomain.Compound.leq depends on ptrdiffType initialization *)
end

module Node =
struct
  include Node
  let name () = "nodes"
  let to_location n = Node.location n
end

(* Currently serialization of Apron results only works for octagons. *)
module Man = ApronDomain.OctagonManager
module ApronD = ApronDomain.D2 (Man)

module ApronUtil =
struct
  include Util (Node) (ApronD)
  type marshal = (Man.mt Apron.Abstract0.t * string array) RH.t
  type dump = marshal dump_gen
  type result = Dom.t RH.t result_gen

  let init () =
    Apron.Manager.set_deserialize Man.mgr

  let unmarshal (m: marshal): Dom.t RH.t =
    RH.map (fun _ -> ApronD.unmarshal) m
end
