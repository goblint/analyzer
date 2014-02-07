
open Batteries
open Cil
open Pretty
open Analyses
open Apron

open ApronDomain

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "poly"
  
  module D = D
  module G = Lattice.Unit
  module C = Lattice.Unit
  
  let val_of () = D.top ()
  let context _ = ()
  
  let otherstate _ = D.top ()       
  let exitstate  _ = D.top ()      
  let startstate _ = D.top ()       

  let enter      ctx r f args = [ctx.local,ctx.local]  
  let combine    ctx r fe f args d = D.top ()   
  let special    ctx r f args = D.top () 
  let branch     ctx e  b = ctx.local

  open Texpr1 

  let rec exptoexpr env = 
    function 
    | Lval (Var v,NoOffset) ->
        Var (Var.of_string v.vname)
    | Const (CInt64 (i,_,_)) -> 
        Cst (Coeff.s_of_int (Int64.to_int i))
    | UnOp  (Neg ,e,_) -> 
        Unop (Neg,exptoexpr env e,Int,Near)
    | BinOp (PlusA,e1,e2,_) ->
        Binop (Add,exptoexpr env e1,exptoexpr env e2,Int,Near)
    | BinOp (MinusA,e1,e2,_) ->
        Binop (Sub,exptoexpr env e1,exptoexpr env e2,Int,Near)
    | BinOp (Mult,e1,e2,_) ->
        Binop (Mul,exptoexpr env e1,exptoexpr env e2,Int,Near)
    | BinOp (Div,e1,e2,_) ->
        Binop (Div,exptoexpr env e1,exptoexpr env e2,Int,Near)
    | BinOp (Mod,e1,e2,_) ->
        Binop (Mod,exptoexpr env e1,exptoexpr env e2,Int,Near)
    | _ -> Cst (Coeff.s_of_int 666)

  let exptotexpr1 env x =
    Texpr1.of_expr env (exptoexpr env x) 
    

  let return ctx e f = 
    let vars = f.slocals in
    let vars = Array.of_enum (List.enum (List.map (fun v -> Var.of_string v.vname) vars)) in
    A.forget_array Man.mgr ctx.local vars false
  let body ctx f = 
    let vars = f.slocals in
    let vars = Array.of_enum (List.enum (List.map (fun v -> Var.of_string v.vname) vars)) in
    let env = Environment.make vars [||] in
    A.top Man.mgr env
  let assign ctx (lv:lval) e = 
    match lv with
      | Var v, NoOffset -> 
        A.assign_texpr Man.mgr ctx.local (Var.of_string (v.vname)) 
              (exptotexpr1 ctx.local.env (Cil.constFold false e)) None
      | _ -> ctx.local
end

let _ = 
  MCP.register_analysis (module Spec : Spec)
