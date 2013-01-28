module GU = Goblintutil
module LF = LibraryFunctions

open Cil
open Pretty
open Analyses


module Spec =
struct
  include Analyses.DefaultSpec
  
  module Flag = ConcDomain.Trivial
  module Dom  = Flag
  module Glob = Glob.Make (Lattice.Unit)

  let name = "multi-threaded"

  let startstate = Flag.bot
  let otherstate = Flag.top
  let exitstate  = Flag.get_main
          
  let body ctx f = ctx.local

  let branch ctx exp tv = ctx.local
  
  let return ctx exp fundec  = 
    if fundec.svar.vname = "__goblint_dummy_init" then begin
      if Flag.is_multi ctx.local then ctx.local else Flag.get_main ()
    end else 
      ctx.local

  let assign ctx (lval:lval) (rval:exp) : Dom.t  = ctx.local

  let enter_func ctx lval f args = 
    match LF.classify f.vname args with 
      | `ThreadCreate (f,x) -> 
        let new_fl = Flag.join ctx.local (Flag.get_main ()) in
          [ctx.local,new_fl]
      | _ -> [ctx.local,ctx.local]
  
  let leave_func ctx lval fexp f args st2 = st2    

  let special_fn ctx lval f args = 
  match LF.classify f.vname args with 
    | `ThreadCreate (f,x) -> 
      let new_fl = Flag.join ctx.local (Flag.get_main ()) in
        [new_fl, Cil.integer 1, true]
    | `Unknown _ -> 
  begin match LF.get_invalidate_action f.vname with
    | None -> 
      let new_fl = Flag.join ctx.local (Flag.get_main ()) in
      [new_fl, Cil.integer 1, true]
    | _ -> [ctx.local, Cil.integer 1, true]
  end 
    | _ ->  [ctx.local, Cil.integer 1, true]
    

  let query ctx x =  `Top

end

module MTFlagMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "mtflag" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `Flag x
                let extract_l x = match x with `Flag x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

module Spec2 = Constraints.Spec2OfSpec (Spec)
let _ = 
  MCP.register_analysis "mtflag" (module Spec2 : Spec2)