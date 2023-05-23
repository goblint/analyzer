(** Queries within {!ValueDomain}. *)

open GoblintCil
open BoolDomain

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

type eval_int = exp -> ID.t
type may_point_to = exp -> LS.t
type is_multiple = varinfo -> bool

(** Subset of queries used by the valuedomain, using a simpler representation. *)
type t = {
  eval_int: eval_int;
  may_point_to: may_point_to;
  is_multiple: is_multiple;
}

let eval_int_binop (module Bool: Lattice.S with type t = bool) binop (eval_int: eval_int) e1 e2: Bool.t =
  let e = Cilfacade.makeBinOp binop e1 e2 in
  let i = eval_int e in
  if ID.is_bot i || ID.is_bot_ikind i then
    Bool.top () (* base returns bot for non-int results, consider unknown *)
  else
    match ID.to_bool i with
    | Some b -> b
    | None -> Bool.top ()

(** Backwards-compatibility for former [MustBeEqual] query. *)
let must_be_equal = eval_int_binop (module MustBool) Eq

(** Backwards-compatibility for former [MayBeEqual] query. *)
let may_be_equal = eval_int_binop (module MayBool) Eq

(** Backwards-compatibility for former [MayBeLess] query. *)
let may_be_less = eval_int_binop (module MayBool) Lt
