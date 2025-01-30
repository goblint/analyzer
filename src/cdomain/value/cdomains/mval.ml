include Mval_intf

open GoblintCil

module M = Messages


module MakePrintable (Offs: Offset.Printable): Printable with type idx = Offs.idx =
struct
  type idx = Offs.idx
  include Printable.StdLeaf
  (* Use Basetype.Variables to print with RichVarinfo. *)
  type t = Basetype.Variables.t * Offs.t [@@deriving eq, ord, hash]

  let name () = Format.sprintf "lval (%s)" (Offs.name ())

  let show ((v, o): t): string = CilType.Varinfo.show v ^ Offs.show o
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )

  let add_offset (v, o) o' = (v, Offs.add_offset o o')

  let type_of (v,o) = try Offs.type_of ~base:v.vtype o with Offset.Type_of_error (t,_) -> t

  let prefix (v1,ofs1) (v2,ofs2) =
    if CilType.Varinfo.equal v1 v2 then
      Offs.prefix ofs1 ofs2
    else
      None

  let to_cil ((v, o): t): lval = (Var v, Offs.to_cil o)
  let to_cil_exp lv = Lval (to_cil lv)

  let is_definite (_, o) = Offs.is_definite o
end

module MakeLattice (Offs: Offset.Lattice): Lattice with type idx = Offs.idx =
struct
  include MakePrintable (Offs)

  let top_indices (x, o) = (x, Offs.top_indices o)

  let semantic_equal (x, xoffs) (y, yoffs) =
    if CilType.Varinfo.equal x y then
      let typ1 = x.vtype in
      let typ2 = y.vtype in
      Offs.semantic_equal ~typ1 xoffs ~typ2 yoffs
    else
      Some false


  let leq (x,o) (y,u) = CilType.Varinfo.equal x y && Offs.leq o u
  let merge op (x,o) (y,u) =
    if CilType.Varinfo.equal x y then
      (x, op o u)
    else
      raise Lattice.Uncomparable

  let join = merge Offs.join
  let meet = merge Offs.meet
  let widen = merge Offs.widen
  let narrow = merge Offs.narrow

  include Lattice.NoBotTop

  let pretty_diff () (x,y) =
    Pretty.dprintf "%s: %a not equal %a" (name ()) pretty x pretty y
end

module Unit = MakePrintable (Offset.Unit)
module Exp = MakePrintable (Offset.Exp)
module Z = MakePrintable (Offset.Z)
