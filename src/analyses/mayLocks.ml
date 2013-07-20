(** May-lockset analysis. *)

open Cil
open Pretty
open Analyses
open GobConfig
open Batteries

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "maylocks"
  module D = LockDomain.MayLockset
  module C = LockDomain.MayLockset
  module G = Lattice.Unit
  
  (* transfer functions : usual operation just propagates the value *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ctx.local
  let branch ctx (exp:exp) (tv:bool) : D.t = ctx.local
  let body ctx (f:fundec) : D.t = ctx.local
  let return ctx (exp:exp option) (f:fundec) : D.t = ctx.local
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = [ctx.local,ctx.local]
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t = au
    
  (* Helper function to convert query-offsets to valuedomain-offsets *)
  let rec conv_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (Const (CInt64 (i,_,_)),o) -> `Index (ValueDomain.IndexDomain.of_int i, conv_offset o)
      | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
      | `Field (f,o) -> `Field (f, conv_offset o)
  
  (* Query the value (of the locking argument) to a list of locks. *)
  let eval_exp_addr a exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a (Queries.MayPointTo exp) with
      | `LvalSet a when not (Queries.LS.is_top a) -> 
          Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []    
      | `Bot -> []
      | b -> Messages.warn ("Could not evaluate '"^sprint 30 (d_exp () exp)^"' to an points-to set, instead got '"^Queries.Result.short 60 b^"'."); []
  
  (* locking logic -- add all locks we can add *)
  let lock ctx rw may_fail a lv arglist ls : D.ReverseAddrSet.t =
    let add_one ls e = D.add (e,rw) ls in
    let nls = List.fold_left add_one ls (List.concat (List.map (eval_exp_addr a) arglist)) in
    match lv with 
      | None -> nls
      | Some lv -> 
          ctx.split nls (Lval lv) false;
          if may_fail then ctx.split ls (Lval lv) true;
          raise Analyses.Deadcode
  
  (* transfer function to handle library functions --- for us locking & unlocking *)
  let special ctx (lv: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let remove_rw x st = D.remove (x,true) (D.remove (x,false) st) in
    (* unlocking logic *)
    let unlock remove_fn =
      match arglist with
        | x::xs -> begin match  (eval_exp_addr ctx.ask x) with 
                        | [x] -> remove_fn x ctx.local
                        | _ -> ctx.local
                end
        | _ -> ctx.local
    in
    match (LibraryFunctions.classify f.vname arglist, f.vname) with
      | `Lock (failing, rw), _
          -> lock ctx rw failing ctx.ask lv arglist ctx.local
      | `Unlock, _ 
          -> unlock remove_rw
        
      | _ -> ctx.local

  let startstate v = D.empty ()
  let otherstate v = D.empty ()
  let exitstate  v = D.top ()
end

let _ = 
  MCP.register_analysis (module Spec : Spec)
