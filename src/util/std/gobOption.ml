let concat_map f x = Option.bind x f

let exists p = function
  | Some x -> p x
  | None -> false

let for_all p = function
  | Some x -> p x
  | None -> true

let iter = Stdlib.Option.iter

let map2 binop opt1 opt2 =
  match opt1, opt2 with
  | Some x1, Some x2 -> Some (binop x1 x2)
  | _ -> None

let concat_map2 binop opt1 opt2 =
  match opt1, opt2 with
  | Some x1, Some x2 -> binop x1 x2
  | _ -> None

let exists2 p opt1 opt2 =
  match opt1, opt2 with
  | Some x1, Some x2 -> p x1 x2
  | _ -> false

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
