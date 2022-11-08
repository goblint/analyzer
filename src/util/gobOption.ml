let exists p = function
  | Some x -> p x
  | None -> false

let for_all p = function
  | Some x -> p x
  | None -> true


module Syntax =
struct
  let (let+) x f = Option.map f x
  let (and+) x y = match x, y with
    | Some x, Some y -> Some (x, y)
    | _, _ -> None
  let (let*) = Option.bind
  let (and*) = (and+)

  let (>>=) = Option.bind
end
