open Cil
open Analyses
(*
module Path = AccessDomain.Path
module LF   = LibraryFunctions

module Spec =
struct 
  include Analyses.DefaultSpec

  module Glob = Global.Make (AccessDomain.Access.GlobDom)
  module Dom = AccessDomain.Access
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

  let startstate () = Dom.startstate ()
  let otherstate () = Dom.startstate ()
  let exitstate  () = Dom.startstate ()
  
  let sync ctx = 
    match ctx.local with
      | `Bot -> Messages.warn "Access information lost."; (`Bot, [])
      | `Lifted ((m1,d,m2),a,b) -> `Lifted ((m1, Dom.Diff.bot (), m2), a, b), Dom.Diff.elements d
  
  let name = "Access Analysis"

  (* todo:
     return 
     
     Everything that changes must be dropped from PathMaps left hand side 
     and inlined into right hand sides. Assign to vars and globals work, but escaped 
     and indirect changes do not. *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t = 
    List.fold_left (fun x f -> f x) ctx.local
      [ Dom.reset_accs 
      ; Dom.add_access ctx.ask rval true ctx.global
      ; Dom.add_access ctx.ask (Lval lval) false ctx.global
      ; Dom.assign ctx.ask lval rval ctx.global ]    
    
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
      List.fold_left (fun x f -> f x) ctx.local
      [ Dom.reset_accs 
      ; Dom.add_access ctx.ask exp true ctx.global ]    
      
  let body ctx (f:fundec) : Dom.t =  
      List.fold_left (fun x f -> f x) ctx.local
      ( Dom.reset_accs 
      ::List.map Dom.init_local_var f.slocals )
      
  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
      List.fold_left (fun x f -> f x) ctx.local
        ( Dom.reset_accs
        ::List.map  (fun x -> Dom.kill (Dom.Lvals.from_var x)) (f.sformals @ f.slocals))
    
  let eval_funvar ctx (fv:exp) = 
    []
  
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list = 
    let fundec = Cilfacade.getdec f in
    let rec map2 f xs ys =
      match xs, ys with
        | x::xs, y::ys -> f x y :: map2 f xs ys
        | _ -> []
    in
    let nst = 
      List.fold_left (fun x f -> f x) ctx.local
        ( Dom.reset_accs
        ::List.map  (fun x -> Dom.add_access ctx.ask x true ctx.global) args
        @ map2 (fun x y -> Dom.assign ctx.ask (Var x, NoOffset) y ctx.global) fundec.sformals args)
    in
    [ctx.local, nst]
    
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t = 
    let st = 
      match lval with
        | Some v -> Dom.add_access ctx.ask (Lval v) false ctx.global au 
        | None -> au
    in
    List.fold_left (fun x f -> f x) st
      (  Dom.add_access ctx.ask fexp true ctx.global
      :: (List.map (fun e -> Dom.add_access ctx.ask e true ctx.global) args))
(*  
  let heap_hash    = Hashtbl.create 113 

  let get_heap_var loc = 
    try H.find heap_hash loc
    with Not_found ->
      let name = "(alloc@" ^ loc.file ^ ":" ^ string_of_int loc.line ^ ")" in
      let newvar = makeGlobalVar name voidType in
        H.add heap_hash loc newvar;
        newvar

  let heap_var loc = AD.from_var (get_heap_var loc)*)
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    let arg_acc act = 
      match LF.get_invalidate_action f.vname with
        | Some fnc -> (fnc act arglist) 
        | _ -> []
    in
    match lval with 
       | None -> 
          let m = 
            List.fold_left (fun x f -> f x) ctx.local
            (  Dom.reset_accs 
            :: List.map (fun e -> Dom.add_escaped ctx.ask e true  ctx.global)  (arg_acc `Write) 
            @  List.map (fun e -> Dom.add_escaped ctx.ask e false ctx.global) (arg_acc `Read))
          in       
          [m,Cil.integer 1, true]
      | Some (Var v,o) ->
          let m = 
            List.fold_left (fun x f -> f x) ctx.local
            ([ Dom.reset_accs 
            ; Dom.kill (Dom.Lvals.from_var v) 
            ; Dom.add_access ctx.ask (Lval (Var v, o)) true ctx.global ]
            @ List.map (fun e -> Dom.add_escaped ctx.ask e true  ctx.global)  (arg_acc `Write)
            @ List.map (fun e -> Dom.add_escaped ctx.ask e false ctx.global) (arg_acc `Read))
          in 
          begin match f.vname with
            | "malloc" | "kmalloc" | "__kmalloc" | "usb_alloc_urb" | "calloc" ->
                let h_var = BaseDomain.get_heap_var !GU.current_loc in
                [ Dom.assign ctx.ask (Var v,o) (AddrOf (Var h_var, NoOffset)) ctx.global m
                , Cil.integer 1
                , true ]
            | _ -> [m,Cil.integer 1, true]
          end
       | _ -> [Dom.top (),Cil.integer 1, true] (*i think this should not happen*)
end

module AccessMCP =
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "access"
                let depends = []
                type lf = Spec.Dom.t
                let inject_l (x:lf) = (`Access x:MCP.local_state)
                let extract_l x = match x with `Access x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `Access x
                let extract_g x = match x with `Access x -> x | _ -> raise MCP.SpecificationConversionError
         end)
 *)

