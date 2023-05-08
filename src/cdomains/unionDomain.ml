
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
  (** Return the field used in the last write of the union, and the value of that field *)

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

module SimpleOld (Values: Arg) =
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

module Map (Values: Arg) : S with type value = Values.t =
struct
  module Fieldinfo = struct
    include CilType.Fieldinfo
    include Printable.Std (* To make it Groupable *)
  end
  (* Use MapBot because the join should keep the entries contained in one of the operands *)
  module Map = MapDomain.MapBot_LiftTop (Fieldinfo) (Values)
  include Map
  type value = Values.t

  let get field m =
    match Map.find_opt field m with
    | Some v -> v
    | None -> Values.top ()

  let get_field_and_value m =
    if Map.is_empty m then
      None, Values.bot ()
    else if Map.is_top m then
      None, Values.top ()
    else if Map.cardinal m = 1 then
      let f, v = Map.choose m in
      Some f, v
    else
      None, Values.top ()

  let fold g (m: t) acc =
    if Map.is_top m then
      acc
    else
      Map.fold (fun fd v -> g (Some fd) v) m acc

  let map g m =
    if Map.is_top m then
      m
    else
      let map_elem fd v acc =
        let v = g (Some fd) v in
        Map.add fd v acc
      in
      Map.fold map_elem m (Map.empty ())

  let smart_leq ~leq_elem x y =
    if is_top y then
      true
    else if is_top x then
      false
    else
      let check_entry f v =
        let v' = Map.find_opt f y in
        BatOption.map_default (leq_elem v) false v'
      in
      Map.for_all check_entry x

  let join =
    Map.join

  (** Performs join or widen operation using per element merge function *)
  let merge ~merge_values x y =
    if is_top x || is_top y then
      x
    else
      let merge_entry f v v' =
        match v, v' with
        | None, None -> None
        | Some v, None
        | None, Some v -> Some v
        | Some v, Some v' -> Some (merge_values v v')
      in
      Map.merge merge_entry x y

  let smart_join ~join_elem =
    merge ~merge_values:join_elem

  let widen =
    Map.widen

  let smart_widen ~widen_elem  =
    merge ~merge_values:widen_elem

  let of_field ~field ~value : t =
    Map.singleton field value

  let invariant ~value_invariant ~offset ~lval m =
    (* TODO: Deduplicate with Simple.invariant *)

    (* Obtains the single field that was last written, if any. *)
    let field, v = get_field_and_value m in
    match offset with
    (* invariants for all fields *)
    | Cil.NoOffset ->
      begin match field with
        | Some f ->
          let f_lval = Cil.addOffsetLval (Field (f, NoOffset)) lval in
          value_invariant ~offset ~lval:f_lval v
        | None ->
          Invariant.none
      end
    (* invariant for one field *)
    | Field (f, offset) ->
      begin match field with
        | Some f' ->
          let v = Values.cast ~torg:f'.ftype f.ftype v in
          value_invariant ~offset ~lval v
        | None ->
          Invariant.none
      end
    (* invariant for one index *)
    | Index (i, offset) ->
      Invariant.none
end

module Simple = Map