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

  type t = { tid : Tid.t; pred : Pred.t; ctx : Ctx.t } [@@deriving to_yojson, lattice]

  (** printing *)
  let show x =
    Printf.sprintf
      "{ Tid=%s; pred=%s; ctx=%s }"
      (Tid.show x.tid)
      (Pred.show x.pred)
      (Ctx.show x.ctx)

  include Printable.SimpleShow(struct type nonrec t = t let show = show end)

  let name () = "pthread state"

  (** let equal = Util.equals *)
  let equal x y =
    Tid.equal x.tid y.tid && Pred.equal x.pred y.pred && Ctx.equal x.ctx y.ctx


  (** compare all fields with correspoding compare operators *)
  let compare x y =
    List.fold_left
      (fun acc v -> if acc = 0 && v <> 0 then v else acc)
      0
      [ Tid.compare x.tid y.tid
      ; Pred.compare x.pred y.pred
      ; Ctx.compare x.ctx y.ctx
      ]


  (** let hash = Hashtbl.hash *)
  let hash x = Hashtbl.hash (Tid.hash x.tid, Pred.hash x.pred, Ctx.hash x.ctx)
  let make tid pred ctx = { tid; pred; ctx }
  let any_is_bot x = Tid.is_bot x.tid || Pred.is_bot x.pred

  let op_scheme op1 op2 op3 x y : t =
    { tid = op1 x.tid y.tid; pred = op2 x.pred y.pred; ctx = op3 x.ctx y.ctx }

  let pretty_diff () (x,y) =
    if not (Tid.leq x.tid y.tid) then
      Tid.pretty_diff () (x.tid,y.tid)
    else if not (Pred.leq x.pred y.pred) then
      Pred.pretty_diff () (x.pred,y.pred)
    else
      Ctx.pretty_diff () (x.ctx, y.ctx)

end
