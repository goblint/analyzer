(** Another OSEK analysis. *)

open Cil
open Pretty
open Analyses
open OilUtil

module Spec =
struct
  include Analyses.DefaultSpec

  let violations = ref false (*print negative warnings? *)

  let name = "OSEK2"
  module D = Lattice.Prod (Osektupel) (Osektupel) (* Summmary x Result *)
  module C = D
  module G = Lattice.Unit
  module StringSet = Set.Make (String)
  let offpry = Osek.Spec.offensivepriorities
  let funs = Hashtbl.create 16 (* ({vars},tuple) *)
  let _ = Hashtbl.add funs MyCFG.dummy_func.svar.vname ((StringSet.empty  )  , Osektupel.bot()) 

  let get_lockset ctx = Obj.obj (List.assoc "OSEK" ctx.postsub)
  let get_stack   ctx = Obj.obj (List.assoc "stack_trace_set" ctx.postsub)

  let pry_d dom_elem = 
    if Mutex.Lockset.is_top dom_elem then -1 else 
      List.fold_left max 0 (List.map (fun x -> (pry (Osek.Spec.names x)))  (Mutex.Lockset.ReverseAddrSet.elements dom_elem))
  
  let pry_d' dom_elem lock = 
    if Mutex.Lockset.is_top dom_elem then -1 else 
      let lock_name = match lock with
	| AddrOf (Var varinfo,NoOffset) -> varinfo.vname
	| _ -> failwith "This never happens! osektransactionality.ml:43"
      in
      List.fold_left max 0 (List.map (fun x -> ((fun y -> if y == lock_name then -1 else pry y) (Osek.Spec.names x)))  (Mutex.Lockset.ReverseAddrSet.elements dom_elem))
    
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = 
    let ((ctxs,ctxr): D.t) = ctx.local in
    let p = (pry_d (get_lockset ctx)) in
    let access_one_top = Mutex.Spec.access_one_top in
    let b1 = access_one_top ctx.ask true (Lval lval) in 
    let b2 = access_one_top ctx.ask false rval in
    let stack = get_stack ctx in
    let addvars var fn = let (vars,t) = Hashtbl.find funs fn.vname in
      let _ = Hashtbl.replace funs fn.vname (StringSet.add var vars , Osektupel.join t ctxr) in
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
    (ctxs, Osektupel.fcon ctxr (Osektupel.Bot,Osektupel.Val p, Osektupel.Val p, Osektupel.Val p))

  let branch ctx (exp:exp) (tv:bool) : D.t = 
    let (ctxs,ctxr) = ctx.local in
    let p = (pry_d (get_lockset ctx)) in
    (ctxs, Osektupel.fcon  ctxr (Osektupel.Bot,Osektupel.Bot,Osektupel.Bot,Osektupel.Val p))

  let body ctx (f:fundec) : D.t = 
(* let _ = print_endline ( "Body " ^f.svar.vname) in  *)
    let _ = if Hashtbl.mem funs f.svar.vname then () else 
(*let _ = print_endline ( "Adding to funs " ^f.svar.vname) in *)
	Hashtbl.add funs f.svar.vname ((StringSet.empty  )  ,Osektupel.bot()) in D.bot()

  let return ctx (exp:exp option) (f:fundec) : D.t = 
    let ((_,ctxr): D.t) = ctx.local in
    let (vars,_) = Hashtbl.find funs f.svar.vname in
    let _ = Hashtbl.replace funs f.svar.vname (vars,ctxr) in
      (ctxr, ctxr)
  
  let eval_funvar ctx (fv:exp) : varinfo list = 
    []
    
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]
  
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    let (ctxs,ctxr) = ctx.local in
    let (aus,aur) = au in
    (ctxs, Osektupel.fcon ctxr aus)
  

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let (ctxs,ctxr) = ctx.local in
    let fvname = get_api_names f.vname in
    match fvname with 
      | "ReleaseResource" -> begin
          match arglist with 
	  | [Lval (Var info,_)] -> let r = get_lock info.vname in
(*           | [Const (CInt64 (c,_,_) ) ] -> let r = makeGlobalVar (find_name (Int64.to_string c)) voidType in *)
(* (Hashtbl.find Osek.Spec.constantlocks (Int64.to_string c)) in  *)
                                            let p = (pry_d' (get_lockset ctx) r) in  
                                               (ctxs, Osektupel.fcon  ctxr (Osektupel.Bot,Osektupel.Bot,Osektupel.Bot,Osektupel.Val p))
          | _ -> let p = (pry_d (get_lockset ctx)) in (ctxs ,(Osektupel.fcon  ctxr (Osektupel.Bot,Osektupel.Bot,Osektupel.Bot,Osektupel.Val p)))
        end
      | _ -> 
(* let _ = print_endline ( "Specialfn " ^f.vname) in  *)
	      (ctxs, ctxr)

  let startstate v = D.bot ()
  let otherstate v = D.top ()
  let exitstate  v = D.top ()

(** Finalization and other result printing functions: *)

  let transactional = ref true

  let report_trans fname (vars,(a,_,_,_)) =
    let pryd = match a with Osektupel.Bot ->(-1) | Osektupel.Val a' -> a' in
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
        | _  -> if !violations then begin 
		  print_endline ("Transactionality violation in function " ^ fname ^ ":");
		  (printlist warn);
		  print_endline ("versus a defensive overall priority of " ^ (string_of_int pryd) ^ " .")
		end else ()     
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

let _ = 
  MCP.register_analysis ~dep:["OSEK"; "stack_trace_set"] (module Spec : Spec)
