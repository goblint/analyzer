open Cil
open Pretty

module M = Messages

module EscapedVars = SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All Variables" end)
