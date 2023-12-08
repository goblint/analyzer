module ID = IntDomain.IntDomTuple
module FD = FloatDomain.FloatDomTupleImpl
module IndexDomain = IntDomain.IntDomWithDefaultIkind (ID) (IntDomain.PtrDiffIkind) (* TODO: add ptrdiff cast into to_int? *)
module Offs = struct
  include Offset.MakeLattice (IndexDomain)

  (** Add the [additional_offset] to the [base_offset]. In case [additional_offset] starts with an index offset, merge this index with the last index occurring in [base_offset]. *)
  let rec add_offset_merge_index_offset ~(base_offset: t) ~(additional_offset: t): t =
    match additional_offset with
    | `NoOffset -> base_offset
    | `Field (f,o) -> add_offset base_offset additional_offset
    | `Index (i,o) ->
      let rec add_offset_index_merged o = match o with
        | `NoOffset -> false, additional_offset
        | `Field (f1,o1) ->
          let merged, offs = add_offset_index_merged o1 in
          merged, `Field (f1, offs)
        | `Index (i1,o1) ->
          let merged, offs = add_offset_index_merged o1 in
          let offs = if merged then
              `Index (i, offs)
            else
              `Index (IndexDomain.add i1 i, o1)
          in
          true, offs
      in
      let _, offs = add_offset_index_merged base_offset in
      offs
end

module Mval = Mval.MakeLattice (Offs)
module AD = AddressDomain.AddressSet (Mval) (ID)
module Addr =
struct
  include AD.Addr
  module Offs = Offs
  module Mval = Mval
end
