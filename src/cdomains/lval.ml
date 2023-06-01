(** Domains for lvalues. *)

open GoblintCil
open Pretty

module M = Messages

type ('f, 'i) offs = 'i Offset.t [@@deriving eq, ord, hash]

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

module Exp =
struct
  include MakePrintable (Offset.Exp)

  let to_cil ((v, o): t): lval = (Var v, Offset.Exp.to_cil o)
  let to_cil_exp lv = Lval (to_cil lv)
end



module CilLval =
struct
  include Exp

  let class_tag (v,o) =
    match v with
    | _ when v.vglob -> `Global
    | _ when v.vdecl.line = -1 -> `Temp
    | _ when Cilfacade.is_varinfo_formal v -> `Parameter
    | _ -> `Local

  let to_exp = to_cil_exp (* TODO: remove *)
end
