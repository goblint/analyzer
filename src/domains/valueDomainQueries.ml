(** Subset of queries used by the valuedomain, using a simpler representation. *)
open GoblintCil

module LS = SetDomain.ToppedSet (Lval.CilLval) (struct let topname = "All" end)

module ID =
struct
  module I = IntDomain.IntDomTuple
  include Lattice.Lift (I) (Printable.DefaultNames)

  let lift op x = `Lifted (op x)
  let unlift op x = match x with
    | `Lifted x -> op x
    | _ -> failwith "Queries.ID.unlift"
  let unlift_opt op x = match x with
    | `Lifted x -> op x
    | _ -> None
  let unlift_is op x = match x with
    | `Lifted x -> op x
    | _ -> false

  let bot_of = lift I.bot_of
  let top_of = lift I.top_of

  let of_int ik = lift (I.of_int ik)
  let of_bool ik = lift (I.of_bool ik)
  let of_interval ?(suppress_ovwarn=false) ik = lift (I.of_interval ~suppress_ovwarn ik)
  let of_excl_list ik = lift (I.of_excl_list ik)
  let of_congruence ik = lift (I.of_congruence ik)
  let starting ?(suppress_ovwarn=false) ik = lift (I.starting ~suppress_ovwarn ik)
  let ending ?(suppress_ovwarn=false) ik = lift (I.ending ~suppress_ovwarn ik)

  let to_int x = unlift_opt I.to_int x
  let to_bool x = unlift_opt I.to_bool x

  let is_top_of ik = unlift_is (I.is_top_of ik)

  let is_bot_ikind = function
    | `Bot -> false
    | `Lifted x -> I.is_bot x
    | `Top -> false
end

type t = {
  eval_int: exp -> ID.t;
  may_point_to: exp -> LS.t;
  is_multiple: varinfo -> bool;
}