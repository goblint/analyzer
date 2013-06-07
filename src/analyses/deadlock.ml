
open Cil
open Pretty
open Analyses
open DeadlockDomain
open Printf


let forbiddenList : ( (myowntypeEntry*myowntypeEntry) list ref) = ref []

let equalLock x y = 
  ValueDomain.Addr.equal (x.addr) (y.addr)

let addLock (newLock,lock) lockList =
  if (List.exists (fun (a,b) -> ((equalLock a newLock) && (equalLock b lock)) ) lockList) then ()
  else forbiddenList := !forbiddenList @ [ (newLock,lock) ]

let addLockingInfo newLock lockList =
  (* Check forbidden list *)
  List.iter (fun e -> List.iter (fun (a,b) -> 
      if ((equalLock a e) && (equalLock b newLock)) then (
        let msg = (sprintf "Deadlock warning: Locking order %s, %s in line %i, %i (after %i, %i)\n" (ValueDomain.Addr.short () e.addr) (ValueDomain.Addr.short () newLock.addr) e.loc.line newLock.loc.line b.loc.line a.loc.line) in
        Messages.report msg;
      )
      else () ) !forbiddenList ) lockList;

  (* Add forbidden order *)
  List.iter (
    fun lock -> 
      forbiddenList := !forbiddenList @ [ (newLock,lock) ];
      let transAddList = List.find_all (fun (a,b) -> equalLock a lock) !forbiddenList in
      List.iter (fun (a,b) -> forbiddenList := !forbiddenList @ [ (newLock,b) ] ) transAddList
  ) lockList;
  ()

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Deadlock analysis"

  (* The domain for the analysis *)
  module Dom = DeadlockDomain.Lockset
  module Glob = Glob.Make (DeadlockDomain.Lockset)

  (* Initialization and finalization *)
  let init () = ()

  let finalize () = ()

  (* Some required states *)
  let startstate _ : Dom.t = Dom.empty ()
  let otherstate _ : Dom.t = Dom.empty ()
  let exitstate  _ : Dom.t = Dom.empty ()
  
  (* ======== Transfer functions ======== *)
  (* Called for assignments, branches, ... *)

  (* Assignment lval <- exp *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    ctx.local
   
  (* Branch *)
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    ctx.local
  
  (* Body of a function starts *)
  let body ctx (f:fundec) : Dom.t = 
     ctx.local

  (* Returns from a function *)
  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    ctx.local
  
  (* Calls/Enters a function *)
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    [[],ctx.local]
  
  (* Leaves a function *)
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    au

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
    
  (* Called when calling a special/unknown function *)
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    match (LibraryFunctions.classify f.vname arglist, f.vname) with
      | `Lock (_, _), _
          ->  
             (* Convert first argument to ValueDomain.Addr.t *)
             let lockAddr = List.hd (eval_exp_addr ctx.ask (List.hd arglist)) in
             (*if isDebugging then (printf "LOCK: %s\n" (ValueDomain.Addr.short () lockAddr);()) else ();*)

             (* Add lock *)
             addLockingInfo {addr = lockAddr; loc = !Tracing.current_loc } ctx.local;
             [ctx.local@[{addr = lockAddr; loc = !Tracing.current_loc }], Cil.integer 1, true]
  
      | `Unlock, _ 
          -> 
             (* Convert first argument to ValueDomain.Addr.t *)
             let lockAddr = List.hd (eval_exp_addr ctx.ask (List.hd arglist)) in
             (*if isDebugging then (printf "LOCK: %s\n" (ValueDomain.Addr.short () lockAddr);()) else ();*)

             (* Remove lock *)
             [List.filter (fun e -> ((ValueDomain.Addr.equal lockAddr (e.addr)) == false)) ctx.local, Cil.integer 1, true]
        
      | _ -> [ctx.local, Cil.integer 1, true]

end

module DeadlockMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "deadlock" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `Deadlock x
                let extract_l x = match x with `Deadlock x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> [] | _ -> raise MCP.SpecificationConversionError
         end)

let _ = 
  let module Spec2 = Constraints.Spec2OfSpec (Spec) in
  MCP.register_analysis "deadlock" (module Spec2 : Spec2)         
