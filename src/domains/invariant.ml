type context = string

type t = string option

let none: t = None
let of_string s: t = Some s

let combine op (i1:t) (i2:t): t =
  match i1, i2 with
  | Some i1, Some i2 -> Some (i1 ^ " " ^ op ^ " " ^ i2)
  | Some i, None | None, Some i -> Some i
  | None, None -> None

let ( && ) = combine "&&"
let ( || ) = combine "||"