module Bool =
struct
  include Basetype.RawBools
  (* type t = bool
  let equal = Bool.equal
  let compare = Bool.compare
  let relift x = x
  let arbitrary () = QCheck.bool *)

  let pp_diff ppf (x,y) = GoblintCil.Pretty.dprintf "%s: %a not leq %a" (name ()) pp x pp y ppf
end

module MayBool: Lattice.S with type t = bool =
struct
  include Bool
  let bot () = false
  let is_bot x = x = false
  let top () = true
  let is_top x = x = true
  let leq x y = x == y || y
  let join = (||)
  let widen = (||)
  let meet = (&&)
  let narrow = (&&)
end

module MustBool: Lattice.S with type t = bool =
struct
  include Bool
  let bot () = true
  let is_bot x = x = true
  let top () = false
  let is_top x = x = false
  let leq x y = x == y || x
  let join = (&&)
  let widen = (&&)
  let meet = (||)
  let narrow = (||)
end