open Pretty
open Cil
module ME = Messages

module Stack (VD: Lattice.S) =
struct
  include MapDomain.MapTop (Basetype.Variables) (VD)
end
