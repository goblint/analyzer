(** Assigning static regions to dynamic memory. *)

open Cil
open Pretty
open Analyses

module Equ = MusteqDomain.Equ
module RegMap = RegionDomain.RegMap
module RegPart = RegionDomain.RegPart
module Reg = RegionDomain.Reg
module BS  = Base.Main

module Spec =
struct
  include Analyses.DefaultSpec

  module LD     = RegionDomain.LD
  module Lif    = RegionDomain.Lif
  module Var    = RegionDomain.Var
  module Vars   = RegionDomain.Vars
  module D = 
  struct 
    include RegionDomain.RegionDom
    let toXML_f sf x = 
      match toXML x with
        | Xml.Element (node, [text, _], elems) -> Xml.Element (node, [text, "Region Analysis"], elems)
        | x -> x
        
    let toXML s  = toXML_f short s
  end
  module G = RegPart
  module C = D

  let partition_varstore = ref dummyFunDec.svar
  let partition_varinfo () = !partition_varstore 
  
  let get_regpart gf = gf (partition_varinfo ())
  
  let sync ctx = 
    let (e,x) = ctx.local in
      (e, Vars.empty ()), Vars.elements x

  let regions exp part (st,_) : Lval.CilLval.t list =
    match st with
      | `Lifted (equ,reg) ->
          let ev = Reg.eval_exp exp in
          let to_exp (v,f) = (v,Lval.Fields.to_offs' f) in
          List.map to_exp (Reg.related_globals ev (part,reg))
      | `Top -> Messages.warn "Region state is broken :("; []
      | `Bot -> []
      
  let is_bullet exp part (st,_) : bool =
    match st with
      | `Lifted (equ,reg) ->
          begin match Reg.eval_exp exp with
            | Some (_,v,_) -> (try RegionDomain.RS.is_single_bullet (RegMap.find v reg) with Not_found -> false)
            | _ -> false
          end
      | `Top -> false
      | `Bot -> true   
      
  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t = 
    let regpart = get_regpart ctx.global in
    match q with
      | Queries.Regions e ->
          if is_bullet e regpart ctx.local then `Bot else
          let ls = List.fold_right Queries.LS.add (regions e regpart ctx.local) (Queries.LS.empty ()) in
          `LvalSet ls
      | _ -> Queries.Result.top ()
 
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match ctx.local with
      | `Lifted (equ,reg), gd -> 
          let old_regpart = get_regpart ctx.global in
          let equ = Equ.assign lval rval equ in
          let regpart, reg = Reg.assign lval rval (old_regpart, reg) in
          if RegPart.leq regpart old_regpart
          then `Lifted (equ,reg), gd
          else `Lifted (equ,reg), Vars.add (partition_varinfo (), regpart) gd
      | x -> x
   
  let branch ctx (exp:exp) (tv:bool) : D.t = 
    ctx.local
  
  let body ctx (f:fundec) : D.t = 
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t = 
    let locals = f.sformals @ f.slocals in
    match ctx.local with
      | `Lifted (equ,reg), gd -> 
          let equ = Equ.kill_vars locals equ in
          let old_regpart = get_regpart ctx.global in
          let part, reg = match exp with
             | Some exp -> Reg.assign (BS.return_lval ()) exp (old_regpart, reg)
             | None -> old_regpart, reg in
          let part, reg = Reg.kill_vars locals (Reg.remove_vars locals (part, reg)) in
          if RegPart.leq part old_regpart
          then `Lifted (equ,reg), gd
          else `Lifted (equ,reg), Vars.add (partition_varinfo (), part) gd 
      | x -> x
    
    
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    let rec fold_right2 f xs ys r =
      match xs, ys with
        | x::xs, y::ys -> f x y (fold_right2 f xs ys r)
        | _ -> r
    in
    match ctx.local with
      | `Lifted (equ,reg), gd ->
           let fundec = Cilfacade.getdec f in
           let f x r eq = Equ.assign (var x) r eq in
           let equ  = fold_right2 f fundec.sformals args equ in
           let f x r reg = Reg.assign (var x) r reg in
           let old_regpart = get_regpart ctx.global in
           let regpart, reg = fold_right2 f fundec.sformals args (old_regpart,reg) in 
           if RegPart.leq regpart old_regpart
           then [ctx.local,(`Lifted (equ,reg),gd)]
           else [ctx.local,(`Lifted (equ,reg),Vars.add (partition_varinfo (), regpart) gd)]
      | x -> [x,x]
  
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    match au with
      | `Lifted (equ, reg), gd -> begin
          let old_regpart = get_regpart ctx.global in
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
  
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match f.vname with 
      | "malloc" | "calloc" | "kmalloc" | "__kmalloc" | "usb_alloc_urb" -> begin
          match ctx.local, lval with
            | (`Lifted (equ,reg), gd), Some lv -> 
                let old_regpart = get_regpart ctx.global in
                let regpart, reg = Reg.assign_bullet lv (old_regpart, reg) in
                let gd = 
                  if RegPart.leq regpart old_regpart
                  then gd
                  else Vars.add (partition_varinfo (), regpart) gd
                in
                `Lifted (equ, reg), gd
            | _ -> ctx.local
        end
      | _ -> 
    let t, _, _, _ = splitFunctionTypeVI  f in
    match unrollType t with
      | TPtr (t,_) ->
    begin match Goblintutil.is_blessed t, lval with
      | Some rv, Some lv -> assign ctx lv (AddrOf (Var rv, NoOffset))
      | _ -> ctx.local
    end
      | _ -> ctx.local
  
  let startstate v = 
    `Lifted (Equ.top (), RegMap.bot ()), Vars.empty ()       
    
  let otherstate v = 
    `Lifted (Equ.top (), RegMap.bot ()), Vars.empty ()

  let exitstate = otherstate
  
  let name = "region"

  let init () = 
    partition_varstore := makeVarinfo false "REGION_PARTITIONS" voidType
    
end

let _ = 
  MCP.register_analysis (module Spec : Spec)         
