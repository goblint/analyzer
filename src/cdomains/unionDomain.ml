open GoblintCil

module type Arg = Lattice.S

module type S =
sig
  include Lattice.S
  type value
  val invariant: value_invariant:(offset:Cil.offset -> Invariant.context1 -> value -> Invariant.t) -> offset:Cil.offset -> Invariant.context1 -> t -> Invariant.t
end

module Field =  Lattice.Flat (CilType.Fieldinfo) (struct
    let top_name = "Unknown field"
    let bot_name = "If you see this, you are special!"
  end)

module Simple (Values: Arg) =
struct
  include Lattice.Prod (Field) (Values)
  type value = Values.t

  let invariant ~value_invariant ~offset c (lift_f, v) =
    match offset with
    (* invariants for all fields *)
    | Cil.NoOffset ->
      let c_lval = BatOption.get c.Invariant.lval in
      begin match lift_f with
      | `Lifted f ->
        let f_lval = Cil.addOffsetLval (Field (f, NoOffset)) c_lval in
        let f_c = {c with lval=Some f_lval} in
        value_invariant ~offset f_c v
      | `Top
      | `Bot ->
        Invariant.none
      end
    (* invariant for one field *)
    | Field (f, offset) ->
      (* ignores lift_f and f because all fields are considered to have same value anyway *)
      value_invariant ~offset c v
    (* invariant for one index *)
    | Index (i, offset) ->
      Invariant.none
end
