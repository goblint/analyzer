open Cil

module type S =
sig
  include Lattice.S
  type value
  type field
  val get: t -> field -> value
  val replace: t -> field -> value -> t
  val fold: (field -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all_common_bindings: (value -> value -> bool) -> t -> t -> bool
  val map: (value -> value) -> t -> t
  val cardinal: t -> int
  val keys: t -> field list
  val widen_with_fct: (value -> value -> value) -> t -> t -> t
  val join_with_fct: (value -> value -> value) -> t -> t -> t
  val leq_with_fct: (value -> value -> bool) -> t -> t -> bool
end

module Simple (Val: Lattice.S) =
struct
  include Printable.Std
  module M = MapDomain.MapTop (Basetype.CilField) (Val)
  let name () = "simple structs"
  type t = M.t [@@deriving to_yojson]
  type field = fieldinfo
  type value = M.value

  (** Short summary for structs *)
  let short w mapping =
    let usable_length = w - 5 in
    let assoclist = M.fold (fun x y rest -> (x,y)::rest) mapping [] in
    let f (key, st) = Val.short usable_length st in
    let whole_str_list = List.rev_map f assoclist in
    Printable.get_short_list "<" ">" usable_length whole_str_list

  let for_all_common_bindings (pred: (value -> value -> bool)) (x:t) (y:t) =
    let pred_ok key value =
      try
        let other = M.find key y in
        pred value other
      with Not_found -> true
    in
    M.for_all pred_ok x

  let pretty () = M.pretty ()
  let replace s field value = M.add field value s
  let get s field = M.find field s
  let fold = M.fold
  let map = M.map
  let cardinal x = M.fold (fun _ _ -> succ) x 0
  let keys x = M.fold (fun k _ a -> k::a) x []

  (* Add these or the byte code will segfault ... *)
  let equal x y = M.equal x y
  let compare x y = M.compare x y
  let is_top x = M.is_top x
  let top () = M.top ()
  let is_bot x = M.is_bot x
  let bot () = M.bot ()
  let meet x y = M.meet x y
  let join x y = M.join x y
  let leq x y = M.leq x y
  let hash x = M.hash x
  let widen = M.widen
  let narrow = M.narrow
  let pretty_diff () (x,y) =
    Pretty.dprintf "{@[%a@] ...}" M.pretty_diff (x,y)
  let printXml f xs = M.printXml f xs
  let widen_with_fct = M.widen_with_fct
  let leq_with_fct = M.leq_with_fct
  let join_with_fct = M.join_with_fct

  let invariant c x =
    match c.Invariant.offset with
    (* invariants for all fields *)
    | NoOffset ->
      let c_lval = BatOption.get c.Invariant.lval in
      fold (fun f v acc ->
          let f_lval = Cil.addOffsetLval (Field (f, NoOffset)) c_lval in
          let f_c = {c with lval=Some f_lval} in
          let i = Val.invariant f_c v in
          Invariant.(acc && i)
        ) x Invariant.none
    (* invariant for one field *)
    | Field (f, offset) ->
      let f_c = {c with offset} in
      let v = get x f in
      Val.invariant f_c v
    (* invariant for one index *)
    | Index (i, offset) ->
      Invariant.none
end
