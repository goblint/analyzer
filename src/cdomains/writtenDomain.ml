module VD = ValueDomain.Compound

module AD = struct
  include ValueDomain.AD
  include Printable.Std
end

module Written = MapDomain.MapBot_LiftTop (AD) (VD)

(* module WrittenQueries = Lattice.LiftTop (Written) *)