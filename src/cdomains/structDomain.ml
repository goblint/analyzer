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

module type RelationalStructDomainSignature =
sig
  include Lattice.S
  type value
  type field
  val add_variable_value_list: (Cil.varinfo option * Cil.varinfo * t) list -> t-> t
  val assign: t -> field -> value -> t
  val eval_assert_cil_exp: Cil.exp -> t -> t
  val eval_cil_exp: Cil.exp -> t -> value
  val get: field -> t -> value
  val get_value_of_variable: varinfo -> t -> t
  val get_value_of_variable_and_globals: varinfo -> t -> t
  val fold: (field -> value -> t -> t) -> t -> t -> t
  val map: (value -> value) -> t -> t
  val meet_local_and_global_state: t -> t -> t
  val remove_all_local_variables: t -> t
  val remove_variable: varinfo -> t -> t
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
  type t
  val add: string -> Cil.compinfo -> t -> t
  val empty : t
  val join: t ->  t -> t
  val meet: t ->  t -> t
  val find: string -> t -> Cil.compinfo
  val fold: (string -> Cil.compinfo -> 'b -> 'b) -> t -> 'b -> 'b
  val get_all_fields_of_variable_name: string -> t -> Cil.fieldinfo list
  val get_field_in_compinfo: string -> Cil.compinfo -> Cil.fieldinfo option
  val get_unique_field: Cil.fieldinfo -> string -> t -> Cil.fieldinfo * t
  val mem: string -> t -> bool
  val print: t -> unit
  val remove: string -> t -> t
end


module StructNameMap(Compound: Lattice.S)(Val: Lattice.S with type t = Compound.t * string * bool) : StructNameMapSignature
  with type t = Cil.compinfo Map.Make(String).t =
struct
  module Map =  Map.Make(String)
  type t = Cil.compinfo Map.t
  let fold = Map.fold
  let mem = Map.mem
  let remove = Map.remove
  let find = Map.find
  let iter = Map.iter
  let add = Map.add
  let empty = Map.empty

  let print map =
    iter (fun key value -> Pervasives.print_endline (key ^ ": " ^ value.cname)) map

  let join x y =
    fold (
      fun key value struct_name_mapping -> add key value struct_name_mapping
    ) x y

  let meet x y  =
    let new_struct_name_mapping = empty in
    Map.fold (fun key value new_struct_name_mapping -> if mem key y then add key value new_struct_name_mapping else new_struct_name_mapping) x new_struct_name_mapping

  let copy_comp ci =
    Cil.copyCompInfo ci ("c" ^(Pervasives.string_of_int (Random.int 10000000)))

  let get_field_in_compinfo field_name compInfo =
    List.fold_left (fun result_field field_of_comp -> if field_name = field_of_comp.fname then Some field_of_comp else result_field)  None compInfo.cfields

  let get_unique_field field struct_name struct_name_mapping =
    let struct_map_empty_key = "" in
    let struct_map_key = struct_name in
    let remove_empty_map_key struct_name_mapping =
      if mem struct_map_empty_key struct_name_mapping then
        remove struct_map_empty_key struct_name_mapping
      else struct_name_mapping
    in
    if mem struct_map_key struct_name_mapping then
      let compInfo = find struct_map_key struct_name_mapping in
      match get_field_in_compinfo field.fname compInfo with
      | Some field -> field, remove_empty_map_key struct_name_mapping
      | _ -> field, remove_empty_map_key struct_name_mapping
    else (
      if mem struct_map_empty_key struct_name_mapping then
        let compInfo = find struct_map_empty_key struct_name_mapping in
        let struct_name_mapping = remove_empty_map_key struct_name_mapping in
        match get_field_in_compinfo field.fname compInfo with
        | Some field -> field, (add struct_map_key compInfo struct_name_mapping)
        | _ -> field, (add struct_map_key compInfo struct_name_mapping)
      else
        let compInfo = copy_comp field.fcomp in
        let field = match get_field_in_compinfo field.fname compInfo with Some field -> field | _ -> field in
        field, (add struct_map_key compInfo struct_name_mapping)
    )

  let remove_variable variable_name map =
    if mem variable_name map then
      remove variable_name map
    else map

  let get_all_fields_of_variable_name variable_name map =
    (find variable_name map).cfields
end
