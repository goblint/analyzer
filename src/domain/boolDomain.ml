(** Boolean domains. *)

module Bool =
struct
  include Printable.StdLeaf
  type t = bool [@@deriving eq, ord, hash]
  let name () = "bool"

  let show x = if x then "true" else "false"
  include Printable.SimpleShow (struct
      type nonrec t = t
      let show = show
    end)
  let to_yojson = [%to_yojson: bool] (* override to_yojson from SimpleShow *)

  let arbitrary () = QCheck.bool

  (* For Lattice.S *)
  let pretty_diff () (x,y) = GoblintCil.Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
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

module FlatBool: Lattice.S with type t = [`Bot | `Lifted of bool | `Top] =
  Lattice.FlatConf (struct
    include Printable.DefaultConf
    let top_name = "?"
    let bot_name = "-"
  end) (Bool)
