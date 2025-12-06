module Syntax =
struct
  let (let@) f x = f x
  (** [let@ x = f in e] is [f @@ fun x -> e]. *)
end
