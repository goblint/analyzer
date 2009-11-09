open Cil
open Pretty

module Equ = MusteqDomain.Equ
module Reg = RegionDomain.Reg
module BS  = Base.Main

module Spec : Analyses.Spec =
struct
  module LD  = Lattice.Prod (Equ) (Reg)
  module Dom = Lattice.Lift (LD) (struct let top_name = "Unknown" let bot_name = "Error" end) 
  module Glob = Global.Make (Lattice.Unit) 

  type glob_fun = Glob.Var.t -> Glob.Val.t

  let regions exp st : Lval.CilLval.t list =
    match st with
      | `Lifted (equ,reg) ->
          let ev = Reg.eval_exp exp in
          let to_exp (v,f) = (v,Lval.Fields.to_offs' f) in
          List.map to_exp (Reg.related_globals ev reg)
      | `Top -> Messages.warn "Region state is broken :("; []
      | `Bot -> []
      
  (* queries *)
  let query ask _ (x:Dom.t) (q:Queries.t) : Queries.Result.t = 
    match q with
      | Queries.Regions e ->
            let ls = List.fold_right Queries.LS.add (regions e x) (Queries.LS.empty ()) in
          if (Queries.LS.is_empty ls)
          then `Bot
          else `LvalSet ls
      | _ -> Queries.Result.top ()
 
  (* transfer functions *)
  let assign a (lval:lval) (rval:exp) (gl:glob_fun) (st:Dom.t) : Dom.t =
    match st with
      | `Lifted (equ,reg) -> 
          let equ = Equ.assign lval rval equ in
          let reg = Reg.assign lval rval reg in
          `Lifted (equ,reg)
      | x -> x
   
  let branch a (exp:exp) (tv:bool) (gl:glob_fun) (st:Dom.t) : Dom.t = 
    st
  
  let body a (f:fundec) (gl:glob_fun) (st:Dom.t) : Dom.t = 
    st

  let return a (exp:exp option) (f:fundec) (gl:glob_fun) (st:Dom.t) : Dom.t = 
    let locals = f.sformals @ f.slocals in
    match st with
      | `Lifted (equ,reg) -> 
          let equ = Equ.kill_vars locals equ in
          let reg = match exp with
             | Some exp -> Reg.assign (BS.return_lval ()) exp reg
             | None -> reg in
          let reg = Reg.kill_vars locals (Reg.remove_vars locals reg) in
          `Lifted (equ,reg)
      | x -> x
    
  
  let eval_funvar a (fv:exp) (gl:glob_fun) (st:Dom.t) : varinfo list = 
    []
    
  let enter_func a (lval: lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (st:Dom.t) : (Dom.t * Dom.t) list =
    match st with
      | `Lifted (equ,reg) ->
           let fundec = Cilfacade.getdec f in
           let f x r eq = Equ.assign (var x) r eq in
           let equ  = List.fold_right2 f fundec.sformals args equ in
           let f x r reg = Reg.assign (var x) r reg in
           let reg = List.fold_right2 f fundec.sformals args reg in 
           [st,`Lifted (equ,reg)]
      | x -> [x,x]
  
  let leave_func a (lval:lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (bu:Dom.t) (au:Dom.t) : Dom.t =
    match au with
      | `Lifted (equ, reg) -> begin
          match lval with
            | None ->
                let reg = Reg.remove_vars [BS.return_varinfo ()] reg in
                `Lifted (equ,reg)
            | Some lval ->
                let reg = Reg.assign lval (AddrOf (BS.return_lval ())) reg in
                let reg = Reg.remove_vars [BS.return_varinfo ()] reg in
                `Lifted (equ,reg)
          end
      | _ -> au
  
  let special_fn ask (lval: lval option) (f:varinfo) (arglist:exp list) (gl:glob_fun) (st:Dom.t) : (Dom.t * Cil.exp * bool) list =
    match f.vname with 
      | "malloc" | "calloc" | "__kmalloc" | "usb_alloc_urb" -> begin
          match st, lval with
            | `Lifted (equ,reg), Some lv -> 
                [`Lifted (equ, Reg.assign_bullet lv reg ), Cil.integer 1, true]
            | _ -> [st,Cil.integer 1, true]
        end
      | _ -> [st,Cil.integer 1, true]
  
  let fork ask lv f args gs ls = 
    match f.vname with 
      | _ -> []

  let startstate () = `Lifted (Equ.top (), Reg.bot ())
  let otherstate () = `Lifted (Equ.top (), Reg.bot ())

  let get_diff _ = []
  let reset_diff x = x
  
  let name = "Region analysis"
  let es_to_string f _ = f.svar.vname

  let should_join _ _ = true
  let finalize () = ()
  let init () = ()
end

module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)
