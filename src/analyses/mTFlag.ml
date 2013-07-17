(** A stand-alone multi-threadedness aanlysis. *)

module GU = Goblintutil
module LF = LibraryFunctions

open Cil
open Pretty
open Analyses


module Spec =
struct
  include Analyses.DefaultSpec
  
  module Flag = ConcDomain.Trivial
  module D = Flag
  module C = Flag
  module G = Lattice.Unit

  let name = "mtflag"

  let startstate v = Flag.bot ()
  let otherstate v = Flag.top ()
  let exitstate  v = Flag.get_main ()
          
  let body ctx f = ctx.local

  let branch ctx exp tv = ctx.local
  
  let return ctx exp fundec  = 
    if fundec.svar.vname = "__goblint_dummy_init" then begin
      if Flag.is_multi ctx.local then ctx.local else Flag.get_main ()
    end else 
      ctx.local

  let assign ctx (lval:lval) (rval:exp) : D.t  = ctx.local

  let enter ctx lval f args = 
    match LF.classify f.vname args with 
      | `ThreadCreate (f,x) -> 
        let new_fl = Flag.join ctx.local (Flag.get_main ()) in
          [ctx.local,new_fl]
      | _ -> [ctx.local,ctx.local]
  
  let combine ctx lval fexp f args st2 = st2    

  let special ctx lval f args = 
  match LF.classify f.vname args with 
    | `ThreadCreate (f,x) -> Flag.join ctx.local (Flag.get_main ())
    | `Unknown _ -> begin 
        match LF.get_invalidate_action f.vname with
          | None -> Flag.join ctx.local (Flag.get_main ())
          | _ -> ctx.local
    end 
    | _ ->  ctx.local
    

  let query ctx x =  `Top

end

let _ = 
  MCP.register_analysis (module Spec : Spec)
