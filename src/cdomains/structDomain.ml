open Cil

module type S =
sig
  include Lattice.S
  type value
  type field
  val get: t -> field -> value
  val replace: t -> field -> value -> t
  val fold: (field -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val map: (value -> value) -> t -> t
end

module Simple (Val: Lattice.S) =
struct 
  include Printable.Std
  module M = MapDomain.MapTop (Basetype.CilField) (Val)
  let name () = "simple structs"
  type t = M.t
  type field = fieldinfo
  type value = M.value

  (** Short summary for structs *)
  let short w mapping = 
    let usable_length = w - 5 in
    let assoclist = M.fold (fun x y rest -> (x,y)::rest) mapping [] in
    let f (key, st) = Val.short usable_length st in
    let whole_str_list = List.rev_map f assoclist in
      Printable.get_short_list "<" ">" usable_length whole_str_list

  let toXML_f sf = M.toXML_f sf
  let pretty_f sf = M.pretty_f sf
  let toXML s = M.toXML_f short s
  let pretty () x = M.pretty_f short () x
  let replace s field value = M.add field value s
  let get s field = M.find field s
  let fold = M.fold
  let map = M.map
  
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
  let isSimple x = M.isSimple x
  let hash x = M.hash x
  let widen = M.widen
  let narrow = M.narrow
  let pretty_diff () (x,y) = 
    Pretty.dprintf "{@[%a@] ...}" M.pretty_diff (x,y)
  let printXml f xs = M.printXml f xs
end
