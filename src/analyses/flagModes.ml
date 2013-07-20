(** Flag state values. *)

open Batteries
open Cil
open Pretty
open Analyses
open GobConfig

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "fmode"
  module D = FlagModeDomain.Dom
  module C = FlagModeDomain.Dom
  module G = Lattice.Unit
  
  let flag_list = ref []
  
  let init () = flag_list := List.map Json.string @@ get_list "ana.osek.flags"

  let eval_int ask exp = 
    match ask (Queries.EvalInt exp) with
      | `Int l -> Some l
      | _      -> None

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    if ctx.local = D.bot() then ctx.local else
      match lval, eval_int ctx.ask rval with
	| (Var f,NoOffset), Some ex when List.mem f.vname !flag_list -> D.add f (false,true,ex) ctx.local
	| (Var f,NoOffset), _ -> D.remove f ctx.local
	| _ -> ctx.local
   
  let branch ctx (exp:exp) (tv:bool) : D.t = 
    if ctx.local = D.bot() then ctx.local else begin
      match (tv, (constFold false exp)) with
	| false, BinOp(Ne,ex,Lval (Var f, NoOffset),_) (*not neq*)
	| false, BinOp(Ne,Lval (Var f, NoOffset), ex,_) (*not neq*)
	| true, BinOp(Eq,ex,Lval (Var f, NoOffset),_) 
	| true, BinOp(Eq,Lval (Var f, NoOffset), ex,_) when List.mem f.vname !flag_list -> begin
	  let temp = eval_int ctx.ask ex in
	  match temp with
	    | Some value -> begin 
	      try
		match (D.find f ctx.local) with
		  | (false,_,old_val) -> if value <> old_val then D.bot() else ctx.local
		  | (true,true,old_val) -> if value <> old_val then D.bot() else ctx.local
		  | (true,false,old_val) -> if value = old_val then D.bot() else D.add f (true, true, value) ctx.local
	      with Not_found (*top*) -> D.add f (true, true, value) ctx.local
	    end
	    | None -> ctx.local  
	end
	| false, Lval (Var f, NoOffset) when List.mem f.vname !flag_list  -> begin (* f == 0*)
	  let value = 0L in
	  try
	    match (D.find f ctx.local) with
	      | (false,_,old_val) -> if value <> old_val then D.bot() else ctx.local
	      | (true,true,old_val) -> if value <> old_val then D.bot() else ctx.local
	      | (true,false,old_val) -> if value = old_val then D.bot() else D.add f (true, true, value) ctx.local
	  with Not_found (*top*) -> D.add f (true, true, value) ctx.local
	end
	| true, Lval (Var f, NoOffset) when List.mem f.vname !flag_list  -> begin (* f != 0*)
	  let value = 0L in
	  try
	    match (D.find f ctx.local) with
	      | (false,_,old_val) -> if value <> old_val then D.bot() else ctx.local
	      | (true,true,old_val) -> if value = old_val then D.bot() else ctx.local
	      | (true,false,old_val) -> if value <> old_val then D.bot() else D.add f (true, false, value) ctx.local
	  with Not_found (*top*) -> D.add f (true, false, value) ctx.local
	end
	| false, BinOp(Eq,ex,Lval (Var f, NoOffset),_) (*not eq*)
	| false, BinOp(Eq,Lval (Var f, NoOffset), ex,_) (*not eq*)
	| true, BinOp(Ne,ex,Lval (Var f, NoOffset),_) 
	| true, BinOp(Ne,Lval (Var f, NoOffset), ex,_) when List.mem f.vname !flag_list -> begin 
	  let temp = eval_int ctx.ask ex in
	  match temp with
	    | Some value -> begin 
	      try
		match (D.find f ctx.local) with
		  | (false,_,old_val) -> if value <> old_val then D.bot() else ctx.local
		  | (true,true,old_val) -> if value = old_val then D.bot() else ctx.local
		  | (true,false,old_val) -> if value = old_val then D.bot() else D.add f (true, false, value) ctx.local
	      with Not_found (*top*) -> D.add f (true, false, value) ctx.local
	    end
	    | None -> ctx.local  
	  end
	| _ -> ctx.local
    end

  let body ctx (f:fundec) : D.t = 
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t = 
    ctx.local
  
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]
  
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au
  
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.top ()
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ = 
  MCP.register_analysis (module Spec : Spec)
