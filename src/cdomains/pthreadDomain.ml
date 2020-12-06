open Prelude

(** Process ID *)
module Pid = IntDomain.Flattened

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
  { pid : Pid.t
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
      "{ pid=%s; pri=%s; pred=%s; ctx=%s }"
      (Pid.short 3 x.pid)
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
    Pid.equal x.pid y.pid
    && Pri.equal x.pri y.pri
    && Pred.equal x.pred y.pred
    && Ctx.equal x.ctx y.ctx


  (** compare all fields with correspoding compare operators *)
  let compare x y =
    List.fold_left
      (fun acc v -> if acc = 0 && v <> 0 then v else acc)
      0
      [ Pid.compare x.pid y.pid
      ; Pri.compare x.pri y.pri
      ; Pred.compare x.pred y.pred
      ; Ctx.compare x.ctx y.ctx
      ]


  (** let hash = Hashtbl.hash *)
  let hash x =
    Hashtbl.hash
      (Pid.hash x.pid, Pri.hash x.pri, Pred.hash x.pred, Ctx.hash x.ctx)


  let make pid pri pred ctx = { pid; pri; pred; ctx }

  let bot () =
    { pid = Pid.bot (); pri = Pri.bot (); pred = Pred.bot (); ctx = Ctx.bot () }


  let is_bot x = x = bot ()

  let any_is_bot x = Pid.is_bot x.pid || Pri.is_bot x.pri || Pred.is_bot x.pred

  let top () =
    { pid = Pid.top (); pri = Pri.top (); pred = Pred.top (); ctx = Ctx.top () }


  let is_top x =
    Pid.is_top x.pid
    && Pri.is_top x.pri
    && Pred.is_top x.pred
    && Ctx.is_top x.ctx


  let op_scheme op1 op2 op3 op4 x y : t =
    { pid = op1 x.pid y.pid
    ; pri = op2 x.pri y.pri
    ; pred = op3 x.pred y.pred
    ; ctx = op4 x.ctx y.ctx
    }


  let leq x y =
    Pid.leq x.pid y.pid
    && Pri.leq x.pri y.pri
    && Pred.leq x.pred y.pred
    && Ctx.leq x.ctx y.ctx


  let join x y = op_scheme Pid.join Pri.join Pred.join Ctx.join x y

  let widen = join

  let meet = op_scheme Pid.meet Pri.meet Pred.meet Ctx.meet

  let narrow = meet
end
