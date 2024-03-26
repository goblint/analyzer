let exists p = function
  | Some x -> p x
  | None -> false

let for_all p = function
  | Some x -> p x
  | None -> true

let map2 binop opt1 opt2 =
  match opt1, opt2 with
  | Some x1, Some x2 -> Some (binop x1 x2)
  | _ -> None

(** Open this to use applicative functor/monad syntax for {!option}. *)
module Syntax =
struct
  (* Applicative functor. *)
  let (let+) x f = Option.map f x
  let (and+) x y = match x, y with
    | Some x, Some y -> Some (x, y)
    | _, _ -> None

  (* Monad *)
  let (let*) = Option.bind
  let (and*) = (and+)

  let (>>=) = Option.bind
end
