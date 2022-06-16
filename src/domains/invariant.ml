open Cil

module ExpLat =
struct
  include CilType.Exp

  let bot () = zero (* false *)
  let top () = one (* true *)
  let is_bot _ = failwith "ExpLat: is_bot"
  let is_top _ = failwith "ExpLat: is_top"

  let leq _ _ = failwith "ExpLat: leq"
  let pretty_diff () _ = failwith "ExpLat: pretty_diff"

  let join x y = BinOp (LOr, x, y, intType)
  let meet x y = BinOp (LAnd, x, y, intType)
  let widen x y = y
  let narrow = meet
end

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
