open GoblintCil
open GobApron

(** Symbolic (and fully syntactic) expression "lattice". *)
module MyExp =
struct
  include Lincons1Set

  let hash _ = failwith "TODO"
  let show _ = failwith "TODO"
  let pretty _ = failwith "TODO"
  let printXml _ = failwith "TODO"
  let name _ = failwith "TODO"
  let to_yojson _ = failwith "TODO"
  let tag _ = failwith "TODO"
  let arbitrary _ = failwith "TODO"
  let relift _ = failwith "TODO"
  let leq _ _ = failwith "ExpLat: leq" (* cannot say for sure, requires general entailment check *)
  let pretty_diff () _ = failwith "ExpLat: pretty_diff" (* irrelevant, no leq *)
  let join x y = failwith "TODO"
  let meet x y = failwith "TODO"
  let widen x y = y
  let narrow = meet
  let to_lincons1_set t : Lincons1Set.t = t
end


(** Lift {!ExpLat} such that join/meet folds don't introduce excessive [|| 0] or [&& 1] expressions. *)

module N =
struct
  include Printable.DefaultConf
  let bot_name = "false"
  let top_name = "true"
end

include Lattice.LiftConf (N) (MyExp)

let none = top ()
let of_lincons1_set = lift

let ( && ) = meet
let ( || ) = join

let to_lincons1_set t : Lincons1Set.t = MyExp.to_lincons1_set t

type context = {
  path: int option;
  lvals: Lval.Set.t;
}

let default_context = {
  path = None;
  lvals = Lval.Set.top ();
}
