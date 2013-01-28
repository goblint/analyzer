open Cil
open Pretty
open Analyses
open OilUtil

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "OSEK trasactionality"
  module Dom  = Lattice.Prod (Osektupel) (Osektupel) (* Summmary x Result *)
  module Glob = Glob.Make (Lattice.Unit)
  module StringSet = Set.Make (String)
  let offpry = Osek.Spec.offensivepriorities
  let funs = Hashtbl.create 16 (* ({vars},tuple) *)
  let _ = Hashtbl.add funs MyCFG.dummy_func.svar.vname ((StringSet.empty  )  ,(-1,-1,-1,-1)) 

  type glob_fun = Glob.Var.t -> Glob.Val.t

  let get_lockset ctx =
    match ctx.sub with
      | [ `OSEK x ; _] -> x
      | [ _ ; `OSEK x] -> x  
      | _ -> failwith "OSEK dependencies not found!"

  let get_stack ctx =
    match ctx.sub with
      | [ `Stack2 x ; _] -> x
      | [ _ ; `Stack2 x] -> x  
      | _ -> failwith "StackTrace dependencies not found!"


  let pry_d dom_elem = 
    if Mutex.Lockset.is_top dom_elem then -1 else 
      List.fold_left max 0 (List.map (fun x -> (pry (Osek.Spec.names x)))  (Mutex.Lockset.ReverseAddrSet.elements dom_elem))
  
  let pry_d' dom_elem r = 
    if Mutex.Lockset.is_top dom_elem then -1 else 
      List.fold_left max 0 (List.map (fun x -> ((fun y -> if y == r.vname then -1 else pry y) (Osek.Spec.names x)))  (Mutex.Lockset.ReverseAddrSet.elements dom_elem))
    
  let min' x y =  
    match (x,y) with
      | (-1,-1) -> -1
      | (-1,_)  -> y
      | (_,-1)  -> x
      | _       -> min x y

  (* composition operator  (b \fcon a) *)
  let fcon (a1,a2,a3,a4 as a) (b1,b2,b3,b4 as b) =  if (Osektupel.is_top b) then a else
    match (a2,b2) with
      | (-1,-1) -> (a1,         a2,         a3,          min' a4 b4 )
      | (-1,_)  -> (b1,         b2,         min' a4 b3  ,min' a4 b4 )
      | (_,-1)  -> (a1,         min' a2 b4 ,a3,          min' a4 b4 )
      | _       -> (min' a2 b3 ,min' a2 b4 ,min' a4 b3  ,min' a4 b4 )

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t = 
    let ((ctxs,ctxr): Dom.t) = ctx.local in
    let p = (pry_d (get_lockset ctx)) in
    let access_one_top = Mutex.Spec.access_one_top in
    let b1 = access_one_top ctx.ask true (Lval lval) in 
    let b2 = access_one_top ctx.ask false rval in
    let stack = get_stack ctx in
    let addvars var fn = let (vars,t) = Hashtbl.find funs fn.vname in
      let _ = Hashtbl.replace funs fn.vname (StringSet.add var vars , t) in
(* let _ = print_endline("Adding " ^ var ^ " to accessset of" ^ fn.vname) in *)
	fn
    in
(* let _ = print_endline(StackDomain.Dom2.short 16 stack) in *)
    let checkvars x = if (StackDomain.Dom2.is_empty stack) then 
(* let _ = print_endline ( "empty Stack") in *)
	() 
      else 
(* let _ = if (StackDomain.Dom2.is_top stack) then print_endline("Top stack") in *)
	begin match x with
	  Mutex.Spec.Concrete (_, vinfo, _, _) -> if vinfo.vglob then 
(* let _ = print_endline ( "checking " ^ vinfo.vname) in *)
	      let _ = StackDomain.Dom2.map (addvars vinfo.vname) stack in () else ()
	  | _ -> 
(* let _ = print_endline ( "no checking") in *)
	      () end
    in
    let _ = List.map checkvars (b1@b2) in
    (ctxs, fcon ctxr (-1,p,p,p))

  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    let (ctxs,ctxr) = ctx.local in
    let p = (pry_d (get_lockset ctx)) in
    (ctxs, fcon  ctxr (-1,-1,-1,p))


  
  let body ctx (f:fundec) : Dom.t = 
(* let _ = print_endline ( "Body " ^f.svar.vname) in  *)
    let _ = if Hashtbl.mem funs f.svar.vname then () else 
(*let _ = print_endline ( "Adding to funs " ^f.svar.vname) in *)
	Hashtbl.add funs f.svar.vname ((StringSet.empty  )  ,(-1,-1,-1,-1)) in Dom.bot()

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    let ((_,ctxr): Dom.t) = ctx.local in
    let (vars,_) = Hashtbl.find funs f.svar.vname in
    let _ = Hashtbl.replace funs f.svar.vname (vars,ctxr) in
      (ctxr, ctxr)
  
  let eval_funvar ctx (fv:exp) : varinfo list = 
    []
    
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    [ctx.local,ctx.local]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    let (ctxs,ctxr) = ctx.local in
    let (aus,aur) = au in
    (ctxs, fcon ctxr aus)
  

  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    let (ctxs,ctxr) = ctx.local in
    let fvname = get_api_names f.vname in
    match fvname with 
      | "ReleaseResource" -> (match arglist with 
          | [Const (CInt64 (c,_,_) ) ] -> let r = makeGlobalVar (find_name (Int64.to_string c)) Cil.voidType in
(* (Hashtbl.find Osek.Spec.constantlocks (Int64.to_string c)) in  *)
                                            let p = (pry_d' (get_lockset ctx) r) in  
                                               [(ctxs, fcon  ctxr (-1,-1,-1,p)) ,Cil.integer 1, true]
          | _ -> let p = (pry_d (get_lockset ctx)) in  [(ctxs ,(fcon  ctxr (-1,-1,-1,p))) ,Cil.integer 1, true])
      | _ -> 
(* let _ = print_endline ( "Specialfn " ^f.vname) in  *)
	      [(ctxs, ctxr),Cil.integer 1, true]

  let startstate () = Dom.bot ()
  let otherstate () = Dom.top ()
  let exitstate  () = Dom.top ()

(** Finalization and other result printing functions: *)

  let transactional = ref true

  let report_trans fname (vars,(pryd,_,_,_)) =
(* let _ = print_endline ( "in report_trans") in *)
    let helper pry var warn = 
(* let _ = print_endline ( (string_of_int !Goblintutil.current_loc.line)  ^ " in " ^ !Goblintutil.current_loc.file) in *)
(* let _ = print_endline ( "Looking for " ^ var) in *)
      if pry = (-1) then begin
(* let _ = print_endline ( (string_of_int !Goblintutil.current_loc.line)  ^ " in " ^ !Goblintutil.current_loc.file) in *)
(* let _ = print_endline ( "Looking for (pry -1) " ^ var) in *)
	  warn
      end else begin
(* let _ = print_endline ( (string_of_int !Goblintutil.current_loc.line)  ^ " in " ^ !Goblintutil.current_loc.file) in *)
(* let _ = print_endline ( "Looking for " ^ var) in *)
        let pryo = try Hashtbl.find offpry var  with 
          | Not_found -> let _ = print_endline ( "Failed to find offensive priority for " ^ var ^ " using -1. Looking at function " ^ fname  ) in -1  
        in
(* let _ = if (pryo > -1) then print_endline ( "Offensive priority for " ^ var ^ ": " ^ (string_of_int pryo)) in *)
        if pry < pryo then let _ = transactional := false in
          ("  variable " ^ var ^ " has offensive priority " ^ (string_of_int pryo))::warn 
        else warn
      end in  
      let rec printlist warn = match warn with
          [] -> ()
        | x::xs -> (printlist xs); print_endline(x);
      in  
      let printwarnings warn = match warn with
          [] -> if !transactional then () else 
		  if ( (fname = "__goblint_dummy_init") || (fname = "goblin_initfun") ) then () else
		    if (pryd == (-1)) then print_endline ("Function " ^ fname ^ " contains (at most) one variable access.") else
		      print_endline ("Function " ^ fname ^ " is transactional with a defensive overall priority of " ^ (string_of_int pryd) ^ " .")
        | _  -> print_endline ("Transactionality violation in function " ^ fname ^ ":");
                (printlist warn);
                print_endline ("versus a defensive overall priority of " ^ (string_of_int pryd) ^ " .");         
      in
      let warnings = StringSet.fold (helper pryd) vars [] in 
      printwarnings warnings

 
  (** postprocess and print races and other output *)
  let finalize () = 
(* let _ = print_endline ( "Finalize trans") in *)
    let _ = Hashtbl.iter report_trans funs in
     if !transactional then 
        print_endline "Goblint did not find any non-transactional behavior in this program!";
    Base.Main.finalize ()

  let init () = ()

end

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "OSEK2" 
                let depends = ["OSEK"; "stack_trace_set"]
                type lf = Spec.Dom.t
                let inject_l x = `OSEK2 x
                let extract_l x = match x with `OSEK2 x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

module Spec2 = Constraints.Spec2OfSpec (Spec)
let _ = 
  MCP.register_analysis "OSEK2" (module Spec2 : Spec2)         