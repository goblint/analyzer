
module Var = Basetype.Variables
module ArgSet = SetDomain.ToppedSet (Var) (struct let topname = "all args" end) 

module Dom = 
struct
  include MapDomain.MapBot_LiftTop (Var) (ArgSet) 
end