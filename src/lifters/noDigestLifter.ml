open Analyses

module Lifter (S:Spec)
  : Spec with module D = S.D
          and module C = S.C
          and module G = S.G
=
struct
  include S
  module P = struct
    include Printable.Unit
    let of_elt _ = ()
    let printXml _ _ = ()
  end
end