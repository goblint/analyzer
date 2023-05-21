(** Widening tokens are a generic and dynamic mechanism for delaying widening.
    All abstract elements carry a set of tokens, which analyses can add into.
    Lifted abstract elements are only widened if the token set does not increase,
    i.e. adding a widening token delays a widening.

    @see <http://www2.in.tum.de/bib/files/mihaila13widening.pdf> Mihaila, B., Sepp, A. & Simon, A. Widening as Abstract Domain. *)

(** Widening token. *)
module Token = Basetype.RawStrings (* Change to variant type if need other tokens than witness UUIDs. *)

(** Widening token set. *)
module TS = SetDomain.ToppedSet (Token) (struct let topname = "Top" end)

(** Reference to current {!add} implementation. Maintained by {!Lifter}. *)
let add_ref: (Token.t -> unit) ref = ref (fun _ ->
    if GobConfig.get_bool "ana.widen.tokens" then
      failwith "Unhandled widening token"
  )

(** Add widening token to local state. *)
let add t = !add_ref t


(** Widening tokens added to side effects.
    Maintained by {!Lifter} and {!MCP}. *)
let side_tokens: TS.t ref = ref (TS.bot ())

(** [with_side_token t f] adds widening token [t] to all side effects in [f]. *)
let with_side_token t f =
  let old_side_tokens = !side_tokens in
  side_tokens := TS.add t old_side_tokens;
  Fun.protect f ~finally:(fun () ->
      side_tokens := old_side_tokens
    )

(** [with_side_tokens ts f] adds widening tokens [ts] to all side effects in [f]. *)
let with_side_tokens ts f =
  let old_side_tokens = !side_tokens in
  side_tokens := TS.join ts old_side_tokens;
  Fun.protect f ~finally:(fun () ->
      side_tokens := old_side_tokens
    )


(** Widening tokens in current local state. Maintained by {!Lifter}. *)
let local_tokens: TS.t ref = ref (TS.bot ())

(** [with_local_side_tokens f] adds all widening tokens from local state to all side effects in [f]. *)
let with_local_side_tokens f =
  with_side_tokens !local_tokens f


open Batteries
open Analyses

(** Lift {!D} to carry widening tokens.
    All operations delegate to inner domain,
    except widening tokens are used to delay widenings. *)
module Dom (D: Lattice.S) =
struct
  include Lattice.Prod (D) (TS)
  let unlift (d, _) = d
  let lift d = (d, TS.bot ())

  (* Ignore tokens for identity.

     TD3 uses equal to check for fixpoint, not leq,
     so should we override this to ignore tokens to avoid potentially
     unnecessary extra work.

     Thus, this domain should not be used inside hashcons lifter,
     because it would prevent token sets changing. *)
  let equal (d1, t1) (d2, t2) = D.equal d1 d2
  let compare (d1, t1) (d2, t2) = D.compare d1 d2
  let hash (d, t) = D.hash d

  (* Ignore tokens for order. *)
  let leq (d1, t1) (d2, t2) = D.leq d1 d2
  let is_bot (d, t) = D.is_bot d
  (* TODO: need others? *)

  (* join also joins tokens *)

  let widen (d1, t1) (d2, t2) =
    let d' = if TS.is_empty (TS.diff t2 t1) then
        D.widen d1 d2
      else
        D.join d1 d2
    in
    (d', TS.join t1 t2)
end

(** Lift {!S} to carry widening tokens with both local and global states. *)
module Lifter (S: PostSpec): PostSpec =
struct
  module D =
  struct
    include Dom (S.D)

    let to_modular (d, ts) = S.D.to_modular d, ts
    let to_non_modular (d, ts) = S.D.to_non_modular d, ts

    let printXml f (d, t) =
      BatPrintf.fprintf f "\n%a<path><analysis name=\"tokens\">%a</analysis></path>" S.D.printXml d TS.printXml t
  end
  module G =
  struct
    include Dom (S.G)

    let printXml f (d, t) =
      BatPrintf.fprintf f "\n%a<analysis name=\"tokens\">%a</analysis>" S.G.printXml d TS.printXml t
  end
  module C = S.C
  module V = S.V

  let to_modular = D.to_modular
  let to_non_modular = D.to_non_modular

  let name () = S.name ()^" with widening tokens"

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize

  let should_join (x, _) (y, _) = S.should_join x y

  let startstate v = (S.startstate v, TS.bot ())
  let exitstate  v = (S.exitstate  v, TS.bot ())
  let morphstate v (d, t) = (S.morphstate v d, t)

  let context fd = S.context fd % D.unlift

  let conv (ctx: (D.t, G.t, C.t, V.t) ctx): (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
    { ctx with local = D.unlift ctx.local
             ; split = (fun d es -> ctx.split (d, snd ctx.local) es) (* Split keeps local widening tokens. *)
             ; global = (fun g -> G.unlift (ctx.global g))
             ; sideg = (fun v g -> ctx.sideg v (g, !side_tokens)) (* Using side_tokens for side effect. *)
    }

  let lift_fun ctx f g h =
    let new_tokens = ref (snd ctx.local) in (* New tokens not yet used during this transfer function, such that it is deterministic. *)
    let old_add = !add_ref in
    let old_local_tokens = !local_tokens in
    add_ref := (fun t -> new_tokens := TS.add t !new_tokens);
    local_tokens := snd ctx.local;
    let d =
      Fun.protect (fun () ->
          h (g (conv ctx))
        ) ~finally:(fun () ->
          local_tokens := old_local_tokens;
          add_ref := old_add
        )
    in
    (* If transfer function exits via exception, then new tokens are forgotten.
       There's nowhere to put them to potentially pass them to splits.
       Thus, this functor should not be used inside deadcode lifter. *)
    f d !new_tokens

  let lift' d ts = (d, ts)

  let paths_as_set ctx =
    let liftmap l ts = List.map (fun x -> (x, ts)) l in
    lift_fun ctx liftmap S.paths_as_set (Fun.id)

  let sync ctx reason = lift_fun ctx lift'   S.sync   ((|>) reason)

  let enter ctx r f args =
    let liftmap l ts = List.map (fun (x,y) -> (x, ts), (y, ts)) l in
    lift_fun ctx liftmap S.enter ((|>) args % (|>) f % (|>) r)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    lift_fun ctx Fun.const S.query (fun (x) -> x q)
  let assign ctx lv e = lift_fun ctx lift'   S.assign ((|>) e % (|>) lv)
  let vdecl ctx v     = lift_fun ctx lift'   S.vdecl  ((|>) v)
  let branch ctx e tv = lift_fun ctx lift'   S.branch ((|>) tv % (|>) e)
  let body ctx f      = lift_fun ctx lift'   S.body   ((|>) f)
  let return ctx r f  = lift_fun ctx lift'   S.return ((|>) f % (|>) r)
  let asm ctx         = lift_fun ctx lift'   S.asm    identity
  let skip ctx        = lift_fun ctx lift'   S.skip   identity
  let special ctx r f args       = lift_fun ctx lift' S.special ((|>) args % (|>) f % (|>) r)
  let combine_env ctx r fe f args fc es f_ask = lift_fun ctx lift' S.combine_env (fun p -> p r fe f args fc (D.unlift es) f_ask) (* TODO: use tokens from es *)
  let combine_assign ctx r fe f args fc es f_ask = lift_fun ctx lift' S.combine_assign (fun p -> p r fe f args fc (D.unlift es) f_ask) (* TODO: use tokens from es *)

  let threadenter ctx lval f args = lift_fun ctx (fun l ts -> List.map (Fun.flip lift' ts) l) S.threadenter ((|>) args % (|>) f % (|>) lval)
  let threadspawn ctx lval f args fctx = lift_fun ctx lift' S.threadspawn ((|>) (conv fctx) % (|>) args % (|>) f % (|>) lval)
  let event ctx e octx = lift_fun ctx lift' S.event ((|>) (conv octx) % (|>) e)
end
