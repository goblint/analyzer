(** Domains for mvalues: simplified lvalues, which start with a {!GoblintCil.varinfo}.
    Mvalues are the result of resolving {{!GoblintCil.Mem} pointer dereferences} in lvalues. *)

open GoblintCil
open Pretty

module M = Messages

module MakePrintable (Offs: Printable.S) =
struct
  include Printable.StdLeaf
  type t = CilType.Varinfo.t * Offs.t [@@deriving eq, ord, hash]

  let name () = Format.sprintf "lval (%s)" (Offs.name ())

  let show ((v, o): t): string = CilType.Varinfo.show v ^ Offs.show o
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end

module Unit = MakePrintable (Offset.Unit)

module Exp =
struct
  include MakePrintable (Offset.Exp)

  let to_cil ((v, o): t): lval = (Var v, Offset.Exp.to_cil o)
  let to_cil_exp lv = Lval (to_cil lv)
end
