module M = Messages
module GU = Goblintutil
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module AD = ValueDomain.AD
(*module BS = Base.Spec*)
module BS = Base.Main
module LF = LibraryFunctions
open Cil
open Pretty
open Analyses

module Owner = SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "Entire heap" end)

module OwnerClass = 
struct
  include Lattice.Prod (Owner) (SetDomain.Make (Basetype.Variables))
end

(* Kalmer: 
    i do not know what this is --- i hoped there would be tests for it but that is not the case 
    *)
module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  exception Top

  module Dom = MusteqDomain.Equ
  module Glob = Global.Make (Lattice.Unit)

  let name = "Equalities"

  let startstate = Dom.top 
  let otherstate = Dom.top 
  
  let exp_equal e1 e2 g s = None

  let return_var = 
    let myvar = makeVarinfo false "RETURN" voidType in
      myvar.vid <- -99;
      myvar

  let assign ctx lval rval = Dom.assign lval rval ctx.local
  let branch ctx exp tv = ctx.local
  let return ctx exp fundec = ctx.local
  let body   ctx f = ctx.local
  let special ctx f arglist = ctx.local

  let enter_func ctx lval f args = []
  let leave_func ctx lval f args st2 = ctx.local
  let special_fn ctx lval f args = []
  let fork       ctx lval f args = []

end

module Analysis = Multithread.Forward(Spec)
