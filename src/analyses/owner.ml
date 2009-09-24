module A = Analyses
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
  exception Top

  module Dom = MusteqDomain.Equ
  module Glob = Global.Make (Lattice.Unit)

  let name = "Equalities"

  let init () = ()
  let finalize () = ()
  let startstate = Dom.top 
  let otherstate = Dom.top 
  let es_to_string f es = f.svar.vname
  
  let exp_equal e1 e2 g s = None
  let query _ _ (x:Dom.t) (q:Queries.t) : Queries.Result.t = Queries.Result.top ()

  let reset_diff x = x
  let get_diff   x = []
  let should_join x y = true

  let return_var = 
    let myvar = makeVarinfo false "RETURN" voidType in
      myvar.vid <- -99;
      myvar

  let assign a lval rval glob st = Dom.assign lval rval st
  let branch a exp tv glob st = st
  let return a exp fundec glob st = st
  let body   a f glob st = st
  let special a f arglist glob st = st

  let enter_func a lval f args glob st = []
  let leave_func a lval f args glob st1 st2 = st1
  let special_fn a lval f args glob st = []
  let fork       a lval f args glob st = []
  
  let eval_funvar a exp glob st = []

end

module Analysis = Multithread.Forward(Spec)
