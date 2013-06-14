(** Flag state values. *)

open Batteries
open Cil
open Pretty
open Analyses
open GobConfig

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Flag Modes"
  module Dom  = FlagModeDomain.Dom
  module Glob = Glob.Make (Lattice.Unit)
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

  let flag_list = ref []
  
  let init () = flag_list := List.map Json.string @@ get_list "ana.osek.flags"

  let eval_int ask exp = 
    match ask (Queries.EvalInt exp) with
      | `Int l -> Some l
      | _      -> None

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    if ctx.local = Dom.bot() then ctx.local else
      match lval, eval_int ctx.ask rval with
	| (Var f,NoOffset), Some ex when List.mem f.vname !flag_list -> Dom.add f (false,true,ex) ctx.local
	| (Var f,NoOffset), _ -> Dom.remove f ctx.local
	| _ -> ctx.local
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    if ctx.local = Dom.bot() then ctx.local else begin
      match (tv, (constFold false exp)) with
	| false, BinOp(Ne,ex,Lval (Var f, NoOffset),_) (*not neq*)
	| false, BinOp(Ne,Lval (Var f, NoOffset), ex,_) (*not neq*)
	| true, BinOp(Eq,ex,Lval (Var f, NoOffset),_) 
	| true, BinOp(Eq,Lval (Var f, NoOffset), ex,_) when List.mem f.vname !flag_list -> begin
	  let temp = eval_int ctx.ask ex in
	  match temp with
	    | Some value -> begin 
	      try
		match (Dom.find f ctx.local) with
		  | (false,_,old_val) -> if value <> old_val then Dom.bot() else ctx.local
		  | (true,true,old_val) -> if value <> old_val then Dom.bot() else ctx.local
		  | (true,false,old_val) -> if value = old_val then Dom.bot() else Dom.add f (true, true, value) ctx.local
	      with Not_found (*top*) -> Dom.add f (true, true, value) ctx.local
	    end
	    | None -> ctx.local  
	end
	| false, Lval (Var f, NoOffset) when List.mem f.vname !flag_list  -> begin (* f == 0*)
	  let value = 0L in
	  try
	    match (Dom.find f ctx.local) with
	      | (false,_,old_val) -> if value <> old_val then Dom.bot() else ctx.local
	      | (true,true,old_val) -> if value <> old_val then Dom.bot() else ctx.local
	      | (true,false,old_val) -> if value = old_val then Dom.bot() else Dom.add f (true, true, value) ctx.local
	  with Not_found (*top*) -> Dom.add f (true, true, value) ctx.local
	end
	| true, Lval (Var f, NoOffset) when List.mem f.vname !flag_list  -> begin (* f != 0*)
	  let value = 0L in
	  try
	    match (Dom.find f ctx.local) with
	      | (false,_,old_val) -> if value <> old_val then Dom.bot() else ctx.local
	      | (true,true,old_val) -> if value = old_val then Dom.bot() else ctx.local
	      | (true,false,old_val) -> if value <> old_val then Dom.bot() else Dom.add f (true, false, value) ctx.local
	  with Not_found (*top*) -> Dom.add f (true, false, value) ctx.local
	end
	| false, BinOp(Eq,ex,Lval (Var f, NoOffset),_) (*not eq*)
	| false, BinOp(Eq,Lval (Var f, NoOffset), ex,_) (*not eq*)
	| true, BinOp(Ne,ex,Lval (Var f, NoOffset),_) 
	| true, BinOp(Ne,Lval (Var f, NoOffset), ex,_) when List.mem f.vname !flag_list -> begin 
	  let temp = eval_int ctx.ask ex in
	  match temp with
	    | Some value -> begin 
	      try
		match (Dom.find f ctx.local) with
		  | (false,_,old_val) -> if value <> old_val then Dom.bot() else ctx.local
		  | (true,true,old_val) -> if value = old_val then Dom.bot() else ctx.local
		  | (true,false,old_val) -> if value = old_val then Dom.bot() else Dom.add f (true, false, value) ctx.local
	      with Not_found (*top*) -> Dom.add f (true, false, value) ctx.local
	    end
	    | None -> ctx.local  
	  end
	| _ -> ctx.local
    end

  let body ctx (f:fundec) : Dom.t = 
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    ctx.local
  
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    [ctx.local,ctx.local]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    au
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    [ctx.local,Cil.integer 1, true]

  let startstate v = Dom.top ()
  let otherstate v = Dom.top ()
  let exitstate  v = Dom.top ()
end

let _ = 
  let module Spec2 : Spec2 = Constraints.Spec2OfSpec (Spec) in
  MCP.register_analysis "fmode" (module Spec2 : Spec2)
  
module BaseMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "fmode" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `FlagModeDom x
                let extract_l x = match x with `FlagModeDom x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
