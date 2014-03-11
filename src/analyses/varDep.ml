open Cil
open Pretty
open Analyses

module LV = Lval.CilLval
module LS = Queries.LS
module LM = MapDomain.MapBot_LiftTop (LV) (LS)

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "vardep"
  module D = LM
  module G = LM
  module C = LM
  
  let is_private ctx v = 
    (not v.vglob) ||
    match ctx.ask (Queries.IsPrivate v) with `Bool tv -> tv | _ -> false
    
  let sync ctx =
    let sync_one (v,os) vs (xs,ys) =
      if is_private ctx v then (xs,ys)
      else (LM.remove (v,os) xs, (v, LM.add (v,os) vs (LM.bot ()))::ys)
    in
    LM.fold sync_one ctx.local (ctx.local, [])  
    
  let rec gen_offs = function
    | `NoOffset 
    | `Index (_, `NoOffset) 
    | `Field (_, `NoOffset) -> `NoOffset
    | `Index (i, os) -> `Index (i, gen_offs os)
    | `Field (f, os) -> `Field (f, gen_offs os)
    
  let rec find (v,o) d = 
    match o with
      | `NoOffset -> LM.find (v,`NoOffset) d
      | o -> 
        if LM.mem (v, o) d 
        then LM.find (v, o) d
        else find (v, gen_offs o) d

  let get_glob ctx (v,os) = 
    let ls = ctx.global v in
    if LM.is_top ls then LS.top () else find (v,os) ls
    
  let get_value ctx (v,os) = 
    if is_private ctx v then find (v,os) ctx.local else get_glob ctx (v, os)
  
  let rec offset = function
    | NoOffset      -> `NoOffset
    | Field (fn,os) -> `Field (fn, offset os)
    | Index _       -> `NoOffset
  
  let rec eval_lval ctx d = function
    | (Mem e,os) -> LS.join (LS.join (eval_offset ctx d os) (ctx_mpt ctx e)) (eval_rval ctx d e)
    | (Var v,os) -> 
      let vr = (v, offset os) in
      let re = get_value ctx vr in
        LS.join (eval_offset ctx d os) (LS.join (LS.singleton vr) re)
    
  and eval_offset ctx d = function
    | NoOffset      -> LS.empty ()
    | Field (_, os) -> eval_offset ctx d os
    | Index (e, os) -> LS.join (eval_rval ctx d e) (eval_offset ctx d os)
    
  and eval_rval ctx d = function
    | Const _              -> LS.empty ()
    | Lval ls              -> eval_lval ctx d ls  
    | SizeOf _             -> LS.empty ()
    | SizeOfE _            -> LS.empty ()
    | SizeOfStr _          -> LS.empty ()
    | AlignOf _            -> LS.empty ()
    | AlignOfE _           -> LS.empty ()
    | UnOp (op,e,_)        -> eval_rval ctx d e
    | BinOp (op,e1,e2,_)   -> LS.join (eval_rval ctx d e1) (eval_rval ctx d e2)
    | CastE (_,e)          -> eval_rval ctx d e
    | AddrOf lv            -> LS.empty ()
    | StartOf lv           -> LS.empty ()
    | AddrOfLabel _        -> LS.empty ()
    | Question (e,e1,e2,_) -> LS.join (eval_rval ctx d e) (LS.join (eval_rval ctx d e1) (eval_rval ctx d e2))
    
  and ctx_mpt ctx e =
    match ctx.ask (Queries.MayPointTo e) with
      | `Bot       -> LS.empty ()
      | `LvalSet e -> e
      | _          -> LS.top ()
               
  let query ctx = function 
    | Queries.VariableDeps (Var v, os) -> `LvalSet (get_value ctx (v, LV.of_ciloffs os))
    | _ -> Queries.Result.top ()
  
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let ls = ctx_mpt ctx (AddrOf lval) in
    let v  = eval_rval ctx ctx.local rval in
    if LS.is_top ls then LM.top () else
      let d = ctx.local in
      let nd = LS.fold (fun l -> LM.add l (LS.join v (LM.find l d))) ls d in
      nd
   
  let branch ctx (exp:exp) (tv:bool) : D.t = 
    ctx.local
  
  let body ctx (f:fundec) : D.t = 
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t = 
    ctx.local
  
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]
  
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au
  
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ = 
  MCP.register_analysis (module Spec : Spec)
