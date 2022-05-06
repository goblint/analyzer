module FloatDomTuple =
struct
type t = (float * float) option

let equal _ _ = false
let hash _ = 0
let compare _ _ = 0
let show _ = failwith "todo"
let pretty () (x: t) = failwith "todo"
let printXml _ (x: t) = failwith "todo"
let name () = failwith "todo"
let to_yojson (x: t) = failwith "todo"
let invariant _ (x: t) = failwith "todo"
let tag (x: t) = failwith "todo"
let arbitrary () = failwith "todo"
let relift (x: t) = failwith "todo"

let leq _ _ = false
let join _ _ = None
let meet _ _ = None
let widen _ _ = None (** [widen x y] assumes [leq x y]. Solvers guarantee this by calling [widen old (join old new)]. *)

let narrow _ _ = None

(** If [leq x y = false], then [pretty_diff () (x, y)] should explain why. *)
let pretty_diff () _ = failwith "todo"

let bot () = None
let is_bot _ = false
let top () = None
let is_top _ = false

let neg _ = None
let add _ _ = None
let sub _ _ = None
let mul _ _ = None
let div _ _ = None

let lt _ _ = None
let gt _ _ = None
let le _ _ = None
let ge _ _ = None
let eq _ _ = None
let ne _ _ = None
end
