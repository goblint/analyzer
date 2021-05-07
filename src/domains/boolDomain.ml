(* TODO: implement lattice *)
module MayBool =
struct
  type t = bool
  let bot () = false
  let top () = true
  let equal = Bool.equal
  let compare = Bool.compare
  let leq x y = x == y || y
  let join = (||)
  let widen = (||)
  let meet = (&&)
  let narrow = (&&)
end

(* TODO: implement lattice *)
module MustBool =
struct
  type t = bool
  let bot () = true
  let top () = false
  let equal = Bool.equal
  let compare = Bool.compare
  let leq x y = x == y || x
  let join = (&&)
  let widen = (&&)
  let meet = (||)
  let narrow = (||)
end