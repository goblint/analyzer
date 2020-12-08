open Prelude

(** Thread ID *)
module Tid = IntDomain.Flattened

(** Priority *)
module Pri = IntDomain.Reverse (IntDomain.Lifted)

(** Context hash for function calls *)
module Ctx = IntDomain.Flattened

(** Set of predecessor nodes *)
module Pred = struct
  include SetDomain.Make (Basetype.ProgLocation)

  let of_node = singleton % MyCFG.getLoc

  let of_current_node () = of_node @@ Option.get !MyCFG.current_node

  let string_of_elt = Basetype.ProgLocation.short 99
end

type domain =
  { tid : Tid.t
  ; pri : Pri.t
  ; pred : Pred.t
  ; ctx : Ctx.t
  }
[@@deriving to_yojson]

module D = struct
  include Printable.Std

  type t = domain [@@deriving to_yojson]

  (** printing *)
  let short w x =
    Printf.sprintf
      "{ Tid=%s; pri=%s; pred=%s; ctx=%s }"
      (Tid.short 3 x.tid)
      (Pri.short 3 x.pri)
      (Pretty.sprint 200 (Pred.pretty () x.pred))
      (Ctx.short 50 x.ctx)


  include Printable.PrintSimple (struct
    type t' = t

    let name () = "pthread state"

    let short = short
  end)

  (** let equal = Util.equals *)
  let equal x y =
    Tid.equal x.tid y.tid
    && Pri.equal x.pri y.pri
    && Pred.equal x.pred y.pred
    && Ctx.equal x.ctx y.ctx


  (** compare all fields with correspoding compare operators *)
  let compare x y =
    List.fold_left
      (fun acc v -> if acc = 0 && v <> 0 then v else acc)
      0
      [ Tid.compare x.tid y.tid
      ; Pri.compare x.pri y.pri
      ; Pred.compare x.pred y.pred
      ; Ctx.compare x.ctx y.ctx
      ]


  (** let hash = Hashtbl.hash *)
  let hash x =
    Hashtbl.hash
      (Tid.hash x.tid, Pri.hash x.pri, Pred.hash x.pred, Ctx.hash x.ctx)


  let make tid pri pred ctx = { tid; pri; pred; ctx }

  let bot () =
    { tid = Tid.bot (); pri = Pri.bot (); pred = Pred.bot (); ctx = Ctx.bot () }


  let is_bot x = x = bot ()

  let any_is_bot x = Tid.is_bot x.tid || Pri.is_bot x.pri || Pred.is_bot x.pred

  let top () =
    { tid = Tid.top (); pri = Pri.top (); pred = Pred.top (); ctx = Ctx.top () }


  let is_top x =
    Tid.is_top x.tid
    && Pri.is_top x.pri
    && Pred.is_top x.pred
    && Ctx.is_top x.ctx


  let op_scheme op1 op2 op3 op4 x y : t =
    { tid = op1 x.tid y.tid
    ; pri = op2 x.pri y.pri
    ; pred = op3 x.pred y.pred
    ; ctx = op4 x.ctx y.ctx
    }


  let leq x y =
    Tid.leq x.tid y.tid
    && Pri.leq x.pri y.pri
    && Pred.leq x.pred y.pred
    && Ctx.leq x.ctx y.ctx


  let join x y = op_scheme Tid.join Pri.join Pred.join Ctx.join x y

  let widen = join

  let meet = op_scheme Tid.meet Pri.meet Pred.meet Ctx.meet

  let narrow = meet
end
