open Prelude

(* Information for one task *)
(* Process ID *)
module Pid = IntDomain.Flattened
(* Priority *)
module Pri = IntDomain.Reverse (IntDomain.Lifted) (* TODO reverse? *)
(* Period *)
module Per = IntDomain.Flattened
(* Capacity *)
module Cap = IntDomain.Flattened

(* Information for all tasks *)
(* Partition mode *)
module Pmo = IntDomain.Flattened
(* Preemption lock *)
module PrE = IntDomain.Flattened
(* context hash for function calls *)
module Ctx = IntDomain.Flattened
(* predecessor nodes *)
module Pred = struct
  module Base = Basetype.ExtractLocation
  include SetDomain.Make (Base)
  let of_loc = singleton
  let of_node = of_loc % Node.location
  let of_current_node () = of_node @@ Option.get !MyCFG.current_node
  let string_of_elt = Basetype.ExtractLocation.show
end

(* define record type here so that fields are accessable outside of D *)
type process = { pid: Pid.t; pri: Pri.t; per: Per.t; cap: Cap.t; pmo: Pmo.t; pre: PrE.t; pred: Pred.t; ctx: Ctx.t } [@@deriving eq, ord, hash, to_yojson]

module D =
struct
  type t = process [@@deriving eq, ord, hash, to_yojson]
  include Printable.Std

  let name () = "ARINC state"

  (* printing *)
  let show x = Printf.sprintf "{ pid=%s; pri=%s; per=%s; cap=%s; pmo=%s; pre=%s; pred=%s; ctx=%s }" (Pid.show x.pid) (Pri.show x.pri) (Per.show x.per) (Cap.show x.cap) (Pmo.show x.pmo) (PrE.show x.pre) (Pretty.sprint ~width:200 (Pred.pretty () x.pred)) (Ctx.show x.ctx)
  include Printable.SimpleShow (struct
      type nonrec t = t
      let show = show
    end)

  (* modify fields *)
  let pid f d = { d with pid = f d.pid }
  let pri f d = { d with pri = f d.pri }
  let per f d = { d with per = f d.per }
  let cap f d = { d with cap = f d.cap }
  let pmo f d = { d with pmo = f d.pmo }
  let pre f d = { d with pre = f d.pre }
  let pred f d = { d with pred = f d.pred }

  let bot () = { pid = Pid.bot (); pri = Pri.bot (); per = Per.bot (); cap = Cap.bot (); pmo = Pmo.bot (); pre = PrE.bot (); pred = Pred.bot (); ctx = Ctx.bot () }
  let is_bot x = x = bot ()
  let is_bot1 x = Pid.is_bot x.pid || Pri.is_bot x.pri || Per.is_bot x.per || Cap.is_bot x.cap || Pmo.is_bot x.pmo || PrE.is_bot x.pre || Pred.is_bot x.pred
  let top () = { pid = Pid.top (); pri = Pri.top (); per = Per.top (); cap = Cap.top (); pmo = Pmo.top (); pre = PrE.top (); pred = Pred.top (); ctx = Ctx.top () }
  let is_top x = Pid.is_top x.pid && Pri.is_top x.pri && Per.is_top x.per && Cap.is_top x.cap && Pmo.is_top x.pmo && PrE.is_top x.pre && Pred.is_top x.pred && Ctx.is_top x.ctx

  let leq x y = Pid.leq x.pid y.pid && Pri.leq x.pri y.pri && Per.leq x.per y.per && Cap.leq x.cap y.cap && Pmo.leq x.pmo y.pmo && PrE.leq x.pre y.pre && Pred.leq x.pred y.pred && Ctx.leq x.ctx y.ctx
  let op_scheme op1 op2 op3 op4 op5 op6 op7 op8 x y: t = { pid = op1 x.pid y.pid; pri = op2 x.pri y.pri; per = op3 x.per y.per; cap = op4 x.cap y.cap; pmo = op5 x.pmo y.pmo; pre = op6 x.pre y.pre; pred = op7 x.pred y.pred; ctx = op8 x.ctx y.ctx }
  let join x y = let r = op_scheme Pid.join Pri.join Per.join Cap.join Pmo.join PrE.join Pred.join Ctx.join x y in
    (* let s x = if is_top x then "TOP" else if is_bot x then "BOT" else short 0 x in M.debug @@ "JOIN\t" ^ if equal x y then "EQUAL" else s x ^ "\n\t" ^ s y ^ "\n->\t" ^ s r; *)
    if Pred.cardinal r.pred > 5 then (Messages.debug "Pred.cardinal r.pred = %d with value %a" (Pred.cardinal r.pred) pretty r(* ; failwith "STOP" *));
    r
  let widen = join
  let meet = op_scheme Pid.meet Pri.meet Per.meet Cap.meet Pmo.meet PrE.meet Pred.meet Ctx.meet
  let narrow = meet

  let pretty_diff () (x,y) =
    Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end
