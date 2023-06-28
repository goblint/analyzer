(** Domains for extraction of Pthread programs. *)

open Batteries

(** Thread ID *)
module Tid = IntDomain.Flattened

(** Context hash for function calls *)
module Ctx = IntDomain.Flattened

(** Set of predecessor nodes *)
module Pred = struct
  module Base = CilType.Location
  include SetDomain.Make (Base)

  let of_node = singleton % Node.location

  let of_current_node () = of_node @@ Option.get !MyCFG.current_node

  let string_of_elt (loc:Base.t) =
    let f i = (if i < 0 then "n" else "") ^ string_of_int (abs i) in
    f loc.line ^ "b" ^ f loc.byte
end

module D = struct
  include Printable.StdLeaf

  type t = { tid : Tid.t; pred : Pred.t; ctx : Ctx.t } [@@deriving eq, ord, hash, relift, to_yojson, lattice]

  (** printing *)
  let show x =
    Printf.sprintf
      "{ Tid=%s; pred=%s; ctx=%s }"
      (Tid.show x.tid)
      (Pred.show x.pred)
      (Ctx.show x.ctx)

  include Printable.SimpleShow (struct
      type nonrec t = t
      let show = show
    end)

  let name () = "pthread state"

  let make tid pred ctx = { tid; pred; ctx }
  let any_is_bot x = Tid.is_bot x.tid || Pred.is_bot x.pred

  let pretty_diff () (x,y) =
    if not (Tid.leq x.tid y.tid) then
      Tid.pretty_diff () (x.tid,y.tid)
    else if not (Pred.leq x.pred y.pred) then
      Pred.pretty_diff () (x.pred,y.pred)
    else
      Ctx.pretty_diff () (x.ctx, y.ctx)

end
