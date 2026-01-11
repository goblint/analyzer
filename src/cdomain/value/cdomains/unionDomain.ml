(** Abstract domains for C unions. *)

open GoblintCil

module type Arg =
sig
  include Lattice.S
  val cast: kind:castkind -> ?torg:typ -> typ -> t -> t
end

module type S =
sig
  include Lattice.S
  type value
  val invariant: value_invariant:(offset:Cil.offset -> lval:Cil.lval -> value -> Invariant.t) -> offset:Cil.offset -> lval:Cil.lval -> t -> Invariant.t
end

module Field = struct
  include Lattice.FlatConf (struct
      include Printable.DefaultConf
      let top_name = "Unknown field"
      let bot_name = "If you see this, you are special!"
    end) (CilType.Fieldinfo)

  let meet f g =
    match meet f g with
    | `Bot -> raise Lattice.Uncomparable
    | m -> m
end

module Simple (Values: Arg) =
struct
  include Lattice.Prod (Field) (Values)
  type value = Values.t

  let meet (f, x) (g, y) =
    let field = Field.meet f g in
    let value = Values.meet x y in
    (field, value)

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
          let v = Values.cast ~kind:Internal ~torg:f'.ftype f.ftype v in (* TODO: proper castkind *)
          value_invariant ~offset ~lval v
        | `Top
        | `Bot ->
          Invariant.none
      end
    (* invariant for one index *)
    | Index (i, offset) ->
      Invariant.none
end
