(** Open this to use applicative functor/monad syntax for {!result}. *)
module Syntax =
struct
  (* Applicative functor. *)
  let (let+) x f = Result.map f x
  let (and+) x y = match x, y with
    | Ok x, Ok y -> Ok (x, y)
    | Error e, _
    | _, Error e -> Error e

  (* Monad *)
  let (let*) = Result.bind
  let (and*) = (and+)

  let (>>=) = Result.bind
end
