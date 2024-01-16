module VD = ValueDomain.Compound

module AD = struct
  include Printable.Std
  include ValueDomain.AD
end

module Written = MapDomain.MapBot_LiftTop (AD) (VD)

(* module WrittenQueries = Lattice.LiftTop (Written) *)