open Cil
open Pretty

module Equ = MusteqDomain.Equ
module RegMap = RegionDomain.RegMap
module RegPart = RegionDomain.RegPart
module Reg = RegionDomain.Reg
module BS  = Base.Main

module Spec =
struct
  module LD     = RegionDomain.LD
  module Lif    = RegionDomain.Lif
  module Var    = RegionDomain.Var
  module Vars   = RegionDomain.Vars
  module Dom    = RegionDomain.RegionDom
  module Glob = Global.Make (RegPart) 

  type glob_fun = Glob.Var.t -> Glob.Val.t

  let partition_varstore = ref dummyFunDec.svar
  let partition_varinfo () = !partition_varstore 
  
  let get_regpart gf = gf (partition_varinfo ())
  
  let get_diff (_,x) = Vars.elements x
  let reset_diff (e,_) = (e,Vars.empty ())

  let regions exp part (st,_) : Lval.CilLval.t list =
    match st with
      | `Lifted (equ,reg) ->
          let ev = Reg.eval_exp exp in
          let to_exp (v,f) = (v,Lval.Fields.to_offs' f) in
          List.map to_exp (Reg.related_globals ev (part,reg))
      | `Top -> Messages.warn "Region state is broken :("; []
      | `Bot -> []
      
  (* queries *)
  let query ask gf (x:Dom.t) (q:Queries.t) : Queries.Result.t = 
    let regpart = get_regpart gf in
    match q with
      | Queries.Regions e ->
          let ls = List.fold_right Queries.LS.add (regions e regpart x) (Queries.LS.empty ()) in
          if (Queries.LS.is_empty ls)
          then `Bot
          else `LvalSet ls
      | _ -> Queries.Result.top ()
 
  (* transfer functions *)
  let assign a (lval:lval) (rval:exp) (gl:glob_fun) (st:Dom.t) : Dom.t =
    match st with
      | `Lifted (equ,reg), gd -> 
          let old_regpart = get_regpart gl in
          let equ = Equ.assign lval rval equ in
          let regpart, reg = Reg.assign lval rval (old_regpart, reg) in
          if RegPart.leq regpart old_regpart
          then `Lifted (equ,reg), gd
          else `Lifted (equ,reg), Vars.add (partition_varinfo (), regpart) gd
      | x -> x
   
  let branch a (exp:exp) (tv:bool) (gl:glob_fun) (st:Dom.t) : Dom.t = 
    st
  
  let body a (f:fundec) (gl:glob_fun) (st:Dom.t) : Dom.t = 
    st

  let return a (exp:exp option) (f:fundec) (gl:glob_fun) (st:Dom.t) : Dom.t = 
    let locals = f.sformals @ f.slocals in
    match st with
      | `Lifted (equ,reg), gd -> 
          let equ = Equ.kill_vars locals equ in
          let old_regpart = get_regpart gl in
          let part, reg = match exp with
             | Some exp -> Reg.assign (BS.return_lval ()) exp (old_regpart, reg)
             | None -> old_regpart, reg in
          let part, reg = Reg.kill_vars locals (Reg.remove_vars locals (part, reg)) in
          if RegPart.leq part old_regpart
          then `Lifted (equ,reg), gd
          else `Lifted (equ,reg), Vars.add (partition_varinfo (), part) gd 
      | x -> x
    
  
  let eval_funvar a (fv:exp) (gl:glob_fun) (st:Dom.t) : varinfo list = 
    []
    
  let enter_func a (lval: lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (st:Dom.t) : (Dom.t * Dom.t) list =
    match st with
      | `Lifted (equ,reg), gd ->
           let fundec = Cilfacade.getdec f in
           let f x r eq = Equ.assign (var x) r eq in
           let equ  = List.fold_right2 f fundec.sformals args equ in
           let f x r reg = Reg.assign (var x) r reg in
           let old_regpart = get_regpart gl in
           let regpart, reg = List.fold_right2 f fundec.sformals args (old_regpart,reg) in 
           if RegPart.leq regpart old_regpart
           then [st,(`Lifted (equ,reg),gd)]
           else [st,(`Lifted (equ,reg),Vars.add (partition_varinfo (), regpart) gd)]
      | x -> [x,x]
  
  let leave_func a (lval:lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (bu:Dom.t) (au:Dom.t) : Dom.t =
    match au with
      | `Lifted (equ, reg), gd -> begin
          let old_regpart = get_regpart gl in
          match lval with
            | None ->
                let regpart, reg = Reg.remove_vars [BS.return_varinfo ()] (old_regpart, reg) in
                if RegPart.leq regpart old_regpart
                then `Lifted (equ,reg), gd
                else `Lifted (equ,reg), Vars.add (partition_varinfo (), regpart) gd
            | Some lval ->
                let reg = Reg.assign lval (AddrOf (BS.return_lval ())) (old_regpart, reg) in
                let regpart, reg = Reg.remove_vars [BS.return_varinfo ()] reg in
                if RegPart.leq regpart old_regpart
                then `Lifted (equ,reg), gd
                else `Lifted (equ,reg), Vars.add (partition_varinfo (), regpart) gd
          end
      | _ -> au
  
  let special_fn ask (lval: lval option) (f:varinfo) (arglist:exp list) (gl:glob_fun) (st:Dom.t) : (Dom.t * Cil.exp * bool) list =
    match f.vname with 
      | "malloc" | "calloc" | "__kmalloc" | "usb_alloc_urb" -> begin
          match st, lval with
            | (`Lifted (equ,reg), gd), Some lv -> 
                let old_regpart = get_regpart gl in
                let regpart, reg = Reg.assign_bullet lv (old_regpart, reg) in
                let gd = 
                  if RegPart.leq regpart old_regpart
                  then gd
                  else Vars.add (partition_varinfo (), regpart) gd
                in
                [(`Lifted (equ, reg), gd), Cil.integer 1, true]
            | _ -> [st,Cil.integer 1, true]
        end
      | _ -> [st,Cil.integer 1, true]
  
  let fork ask lv f args gs ls = 
    match f.vname with 
      | _ -> []

  let startstate () = 
    `Lifted (Equ.top (), RegMap.bot ()), Vars.empty ()       
    
  let otherstate () = 
    `Lifted (Equ.top (), RegMap.bot ()), Vars.empty ()
  
  let name = "Region analysis"
  let es_to_string f _ = f.svar.vname

  let should_join _ _ = true
  let finalize () = ()
  let init () = 
    partition_varstore := makeVarinfo false "REGION_PARTITIONS" voidType
    
end

module RegionMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "region" 
                type lf = Spec.Dom.t
                let inject_l x = `Region x
                let extract_l x = match x with `Region x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `Region x
                let extract_g x = match x with `Region x -> x | _ -> raise MCP.SpecificationConversionError
         end)

module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)
