(** Domains for mvalues: simplified lvalues, which start with a {!GoblintCil.varinfo}.
    Mvalues are the result of resolving {{!GoblintCil.Mem} pointer dereferences} in lvalues. *)

open GoblintCil
open Pretty

module M = Messages

module type OffsS =
sig
  type idx
  include Printable.S with type t = idx Offset.t
  val add_offset: t -> t -> t
  val type_offset: typ -> t -> typ
  exception Type_offset of typ * string
  val to_cil: t -> offset
end

module MakePrintable (Offs: OffsS) =
struct
  include Printable.StdLeaf
  (* TODO: version with Basetype.Variables and RichVarinfo for AddressDomain *)
  type t = CilType.Varinfo.t * Offs.t [@@deriving eq, ord, hash]

  let name () = Format.sprintf "lval (%s)" (Offs.name ())

  let show ((v, o): t): string = CilType.Varinfo.show v ^ Offs.show o
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )

  let add_offset (v, o) o' = (v, Offs.add_offset o o')

  let get_type_addr (v,o) = try Offs.type_offset v.vtype o with Offs.Type_offset (t,_) -> t


  let to_cil ((v, o): t): lval = (Var v, Offs.to_cil o)
  let to_cil_exp lv = Lval (to_cil lv)
end

module Unit = MakePrintable (Offset.Unit)
module Exp = MakePrintable (Offset.Exp)
