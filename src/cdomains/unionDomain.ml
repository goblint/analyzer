(** Abstract domains for C unions. *)

open GoblintCil
open PreValueDomain

module type Arg =
sig
  include Lattice.S
  val try_meet: t -> t -> t (* May fail for unions *)

  val cast: ?torg:typ -> typ -> t -> t
end

module type S =
sig
  include Lattice.S
  type value
  val try_meet: t -> t -> t (* May fail for unions *)
  val invariant: value_invariant:(offset:Cil.offset -> lval:Cil.lval -> value -> Invariant.t) -> offset:Cil.offset -> lval:Cil.lval -> t -> Invariant.t
end

module Field =  Lattice.Flat (CilType.Fieldinfo) (struct
    let top_name = "Unknown field"
    let bot_name = "If you see this, you are special!"
  end)

module Simple (Values: Arg) =
struct
  include Lattice.Prod (Field) (Values)
  type value = Values.t

  let try_meet ((f: Field.t), (x: value)) (g, y) =
    match f, g with
    | `Bot, `Bot -> `Bot, Values.try_meet x y
    | _, `Bot
    | `Bot, _ -> raise NotMeetable
    | `Top, `Top -> `Top, Values.try_meet x y
    | `Top, _
    | _, `Top -> raise NotMeetable
    | `Lifted _, `Lifted _ when Field.equal f g -> f, Values.try_meet x y
    | `Lifted _, `Lifted _ -> raise NotMeetable

  let invariant ~value_invariant ~offset ~lval (lift_f, v) =
    match offset with
    (* invariants for all fields *)
    | Cil.NoOffset ->
      begin match lift_f with
        | `Lifted f ->
          let f_lval = Cil.addOffsetLval (Field (f, NoOffset)) lval in
          value_invariant ~offset ~lval:f_lval v
        | `Top
        | `Bot ->
          Invariant.none
      end
    (* invariant for one field *)
    | Field (f, offset) ->
      begin match lift_f with
        | `Lifted f' ->
          let v = Values.cast ~torg:f'.ftype f.ftype v in
          value_invariant ~offset ~lval v
        | `Top
        | `Bot ->
          Invariant.none
      end
    (* invariant for one index *)
    | Index (i, offset) ->
      Invariant.none
end
