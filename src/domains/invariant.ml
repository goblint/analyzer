open GoblintCil

(** Symbolic (and fully syntactic) expression "lattice". *)
module ExpLat =
struct
  include CilType.Exp

  let bot () = zero (* false *)
  let top () = one (* true *)
  let is_bot _ = failwith "ExpLat: is_bot" (* cannot say for sure, many contradictions exist *)
  let is_top _ = failwith "ExpLat: is_top" (* cannot say for sure, many tautologies exist *)

  let leq _ _ = failwith "ExpLat: leq" (* cannot say for sure, requires general entailment check *)
  let pp_diff ppf _ = failwith "ExpLat: pp_diff" (* irrelevant, no leq *)

  (* join and meet are not idempotent, commutative and associative,
     but it shouldn't be of issue since there's no leq either.
     Would need at least AC-rewriting, if not more, to ensure those symbolically *)
  let join x y = BinOp (LOr, x, y, intType)
  let meet x y = BinOp (LAnd, x, y, intType)
  let widen x y = y
  let narrow = meet
end

(** Lift {!ExpLat} such that join/meet folds don't introduce excessive [|| 0|] or [&& 1] expressions. *)

module N =
struct
  let bot_name = "false"
  let top_name = "true"
end

include Lattice.Lift (ExpLat) (N)

let none = top ()
let of_exp = lift

let ( && ) = meet
let ( || ) = join



type context = {
  path: int option;
  lval: lval option;
}

let default_context = {
  path = None;
  lval = None;
}
