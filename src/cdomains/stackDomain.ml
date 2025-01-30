(** Call stack domains. *)

module type S =
sig
  include Lattice.S
  val push: Basetype.Variables.t -> t -> t
end



module Dom1 =
struct
  let n = 3
  let rec times x = function 0 -> [] | n -> x::times x (n-1)
  let rec map2 f xs ys =
    match xs, ys with x::xs, y::ys -> (try f x y :: map2 f xs ys with Lattice.BotValue | Lattice.TopValue -> []) | _ -> []

  let rec fold_left2 f a b xs ys =
    match xs, ys with
    | [], _ | _, [] -> a
    | x::xs, y::ys ->
      try fold_left2 f (f a x y) b xs ys with
      | Lattice.BotValue | Lattice.TopValue -> b

  let rec take n xs =
    match n, xs with
    | _, [] -> []
    | 0, _  -> []
    | n, x::xs -> x :: take (n-1) xs

  module VarLat = Lattice.Fake (Basetype.Variables)

  module Var = Lattice.LiftConf (struct include Printable.DefaultConf let top_name="top" let bot_name="⊥" end) (VarLat)
  include Lattice.Liszt (Var)

  let top () : t = []
  let bot () : t = times (Var.bot ()) n

  let leq (x:t) (y:t) =
    if List.compare_lengths x y < 0 then false else
      let f acc x y = Var.leq x y && acc in
      fold_left2 f true false x y

  let join x y = map2 Var.join x y
  let meet (x:t) y = map2 Var.meet x y

  let push x (xs:t) = take n ((`Lifted x) :: xs)
end

module Dom2 =
struct
  module Var = Basetype.Variables
  include SetDomain.ToppedSet (Var) (struct let topname = "All functions" end)

  let push = add
end

module Loc = struct
  include Printable.Liszt (CilType.Location)
  let dummy = []
end
module Dom3 = struct
  include Lattice.FakeSingleton (Loc)
  let push x (xs:t): t = x :: xs
end
