
open GoblintCil

module type Arg =
sig
  include Lattice.S
  val cast: ?torg:typ -> typ -> t -> t
  val top_value: ?varAttr:attributes -> typ -> t
end

module type S =
sig
  include Lattice.S
  type value
  val get: fieldinfo -> t -> value
  val get_field_and_value: t -> fieldinfo option * value
  val map: (fieldinfo option -> value -> value) -> t -> t
  val fold: (fieldinfo option -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val smart_leq: leq_elem: (value -> value -> bool) -> t -> t -> bool
  val smart_join: join_elem:(value -> value -> value) -> t -> t -> t
  val smart_widen: widen_elem:(value -> value -> value) -> t -> t -> t
  val of_field: field:fieldinfo -> value:value -> t
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

  let get field (f, x) =
    if Field.equal f (`Lifted field) then
      x
    else
      Values.top ()

  let field_to_option = function
    | `Lifted f -> Some f
    | `Bot
    | `Top -> None

  let get_field_and_value (f, x) =
    let f = field_to_option f in
    f, x

  let fold g (f, x) acc =
    let fd = field_to_option f in
    g fd x acc

  let map g (f, x) =
    let fd = field_to_option f in
    f, (g fd x)

  let smart_leq ~leq_elem (f, x) (g, y) =
    Field.leq f g && leq_elem x y

  let join (f, x) (g, y) =
    match Field.join f g with
    | `Lifted f -> (`Lifted f, Values.join x y) (* f = g *)
    | x -> (x, Values.top ()) (* f <> g *)

  let smart_join ~join_elem (f, x) (g, y)  =
    match Field.join f g with
    | `Lifted f -> (`Lifted f, join_elem x y) (* f = g *)
    | x -> (x, Values.top ()) (* f <> g *)

  let widen (f, x) (g, y) =
    match Field.widen f g with
    | `Lifted f -> (`Lifted f, Values.widen x y) (* f = g *)
    | x -> (x, Values.top ()) (* f <> g *)

  let smart_widen ~widen_elem (f, x) (g, y)  =
    match Field.widen f g with
    | `Lifted f -> (`Lifted f, widen_elem x y) (* f = g *)
    | x -> (x, Values.top ()) (* f <> g *)

  let of_field ~field ~value : t =
    `Lifted field, value

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
