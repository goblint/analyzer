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

module type Relational =
sig
  include Lattice.S
  type value
  type field
  val add_variable_value_list: (Prelude.Ana.lhost * t) list -> t-> t
  val eval_assert_cil_exp: Cil.exp -> t -> t
  val get: t -> field -> value
  val get_value_of_variable: varinfo -> t -> t
  val get_value_of_variable_name: string -> t -> t
  val get_value_of_variable_and_globals: varinfo -> t -> t
  val fold: (field -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val map: (value -> value) -> t -> t
  val meet_local_and_global_state: t -> t -> t
  val remove_all_local_variables: t -> t
  val remove_all_top_variables: t -> t
  val remove_variable: varinfo -> t -> t
  val replace: t -> field -> value -> t
end


module Simple (Val: Lattice.S)
=
struct
  include Printable.Std
  module M = MapDomain.MapTop (Basetype.CilField) (Val)
  let name () = "simple structs"
  type t = M.t
  type field = Cil.fieldinfo
  type value = M.value

  (** Short summary for structs *)
  let short w mapping =
    let usable_length = w - 5 in
    let assoclist = M.fold (fun x y rest -> (x,y)::rest) mapping [] in
    let f (key, st) = "<map><key>" ^ key.fname ^ "</key> <value><data>" ^ (Val.short usable_length st) ^ "</data></value></map>" in
    let whole_str_list = List.rev_map f assoclist in
    Printable.get_short_list_with_separator "" "" usable_length whole_str_list ""

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

module type StructNameMapSignature =
sig
  include Lattice.S
  val add: string -> Cil.compinfo -> t -> t
  val class_name: int -> string
  val classify: t -> int
  val find: string -> t -> Cil.compinfo
  val fold: (string -> Cil.compinfo -> 'b -> 'b) -> t -> 'b -> 'b
  val get_all_fields_of_variable_name: string -> t -> Cil.fieldinfo list
  val get_field_in_compinfo: string -> Cil.compinfo -> Cil.fieldinfo option
  val get_unique_field: Cil.fieldinfo -> string -> t -> Cil.fieldinfo * t
  val mem: string -> t -> bool
  val print: t -> unit
  val remove: string -> t -> t
  val trace_enabled: bool
end
