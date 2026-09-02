(** Lookahead widening.

    @see <https://doi.org/10.1007/11817963_41> Gopan, D., Reps, T. Lookahead Widening. *)

open Batteries
open Lattice
open Analyses

module Dom (Base: S) =
struct
  module Main =
  struct
    include Base
    let name () = "main"
  end
  module Pilot =
  struct
    include Base
    let name () = "pilot"
  end
  include Printable.Prod (Main) (Pilot)

  let bot () = (Main.bot (), Pilot.bot ())
  let is_bot (m, p) = Main.is_bot m
  let top () = (Main.top (), Pilot.top ())
  let is_top (m, p) = Main.is_top m && Pilot.is_top p

  let leq (m1, p1) (m2, p2) = Main.leq m1 m2 && (not (Main.equal m1 m2) || Pilot.leq p1 p2)

  let op_scheme mop pop (m1, p1) (m2, p2) = (mop m1 m2, pop p1 p2)
  let join x y = op_scheme Main.join Pilot.join x y
  let meet = op_scheme Main.meet Pilot.meet (** TODO: Might not be correct *)
  let widen ((m1, p1) as x) ((m2, p2) as y) =
    if leq y x then
      x
    else if Pilot.leq p2 p1 then
      (p2, p2)
    else
      op_scheme Main.join (fun p1 p2 ->
          if Pilot.leq p2 p1 then
            p1 (* ensure stable widening *) (* TODO: is this necessary for us? *)
          else
            Pilot.widen p1 p2
        ) x y
  let narrow = op_scheme Main.narrow Pilot.narrow (** TODO: Might not be correct *)

  let pretty_diff () ((m1, p1), (m2, p2)) =
    if Main.leq m1 m2 then
      Pilot.pretty_diff () (p1, p2)
    else
      Main.pretty_diff () (m1, m2)
end


module Lifter (S: Spec): Spec =
struct
  module D =
  struct
    include Dom (S.D)

    let printXml f (m, p) =
      BatPrintf.fprintf f "%a%a" S.D.printXml m S.D.printXml p
  end
  module G = S.G
  module C = S.C
  module V = S.V
  module P =
  struct
    include S.P
    let of_elt (x, _) = of_elt x
  end

  let name () = S.name () ^ " with lookahead widening"

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize

  let startstate v = (S.startstate v, S.startstate v)
  let exitstate  v = (S.exitstate  v, S.exitstate  v)
  let morphstate v (m, p) = (S.morphstate v m, S.morphstate v p)

  let convm (man: (D.t, G.t, C.t, V.t) man): (S.D.t, S.G.t, S.C.t, S.V.t) man =
    { man with local = fst man.local
             ; split = (fun d es -> man.split (d, snd man.local) es)
    }
  let convp (man: (D.t, G.t, C.t, V.t) man): (S.D.t, S.G.t, S.C.t, S.V.t) man =
    { man with local = snd man.local
             ; split = (fun d es -> man.split (fst man.local, d) es)
    }

  let context man fd (m, _) = S.context (convm man) fd m
  let startcontext () = S.startcontext ()

  let lift_fun (man: (D.t, G.t, C.t, V.t) man) g h =
    let main = h (g (convm man)) in
    if S.D.is_bot main then D.bot () else
      let@ () = GobRef.wrap AnalysisState.executing_speculative_computations true in
      (main, h (g (convp man)))
  let lift_fun' (man: (D.t, G.t, C.t, V.t) man) g h =
    let main = h (g (convm man)) in
    let@ () = GobRef.wrap AnalysisState.executing_speculative_computations true in
    (main, h (g (convp man)))
  let lift_fun2 (man: (D.t, G.t, C.t, V.t) man) g h1 h2 =
    let main = h1 (g (convm man)) in
    if S.D.is_bot main then D.bot () else
      let@ () = GobRef.wrap AnalysisState.executing_speculative_computations true in
      (main, h2 (g (convp man)))

  let sync man reason = lift_fun man S.sync   ((|>) reason)
  let query man (type a) (q: a Queries.t): a Queries.result = S.query (convm man) q
  let assign man lv e = lift_fun man S.assign ((|>) e % (|>) lv)
  let vdecl man v     = lift_fun man S.vdecl  ((|>) v)
  let branch man e tv = lift_fun man S.branch ((|>) tv % (|>) e)
  let body man f      = lift_fun man S.body   ((|>) f)
  let return man r f  = lift_fun man S.return ((|>) f % (|>) r)
  let asm man         = lift_fun man S.asm    identity
  let skip man        = lift_fun man S.skip identity
  let special man r f args = lift_fun man S.special ((|>) args % (|>) f % (|>) r)

  let enter man r f args =
    M.tracel "LA" "enter: %a" D.pretty man.local;
    let (l1, l2) = lift_fun' man S.enter ((|>) args % (|>) f % (|>) r) in
    M.tracel "LA" "enter l1: %a" (Pretty.d_list "\n" D.pretty) l1;
    M.tracel "LA" "enter l2: %a" (Pretty.d_list "\n" D.pretty) l2;
    List.map2 (fun (m1, m2) (p1, p2) -> ((m1, p1), (m2, p2))) l1 l2
  let combine_env man r fe f args fc es f_ask =
    lift_fun2 man S.combine_env (fun p -> p r fe f args fc (fst es) f_ask) (fun p -> p r fe f args fc (snd es) f_ask)
  let combine_assign man r fe f args fc es f_ask =
    lift_fun2 man S.combine_assign (fun p -> p r fe f args fc (fst es) f_ask) (fun p -> p r fe f args fc (snd es) f_ask)

  let threadenter man ~multiple lval f args =
    let (l1, l2) = lift_fun' man (S.threadenter ~multiple) ((|>) args % (|>) f % (|>) lval) in
    List.combine l1 l2
  let threadspawn man ~multiple lval f args fman =
    lift_fun2 man (S.threadspawn ~multiple) ((|>) (convm fman) % (|>) args % (|>) f % (|>) lval) ((|>) (convp fman) % (|>) args % (|>) f % (|>) lval)

  let paths_as_set man =
    let (l1, l2) = lift_fun' man S.paths_as_set Fun.id in
    List.combine l1 l2

  let event man e oman =
    lift_fun2 man S.event ((|>) (convm oman) % (|>) e) ((|>) (convp oman) % (|>) e)
end
