open Cil

type t = exp option

type context = {
  path: int option;
  lval: lval option;
}

let none: t = None
let of_exp s: t = Some s

let combine op (i1:t) (i2:t): t =
  match i1, i2 with
  | Some i1, Some i2 -> Some (BinOp (op, i1, i2, intType))
  | Some i, None | None, Some i -> Some i
  | None, None -> None

let ( && ) = combine LAnd
let ( || ) = combine LOr