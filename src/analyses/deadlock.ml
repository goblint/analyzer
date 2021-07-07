(** Deadlock analysis. *)

open Prelude.Ana
open Analyses
open DeadlockDomain
open Printf

let forbiddenList : ( (myowntypeEntry*myowntypeEntry) list ref) = ref []

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "deadlock"

  (* The domain for the analysis *)
  module D = DeadlockDomain.Lockset (* MayLockset *)
  module C = DeadlockDomain.Lockset
  module G = Lattice.Unit

  let addLockingInfo newLock lockList =
    let add_comb a b =
      if List.exists (fun (x,y) -> MyLock.equal x a && MyLock.equal y b) !forbiddenList then ()
      else forbiddenList := (a,b)::!forbiddenList
    in

    (* Check forbidden list *)
    if !Goblintutil.in_verifying_stage then begin
      D.iter (fun e -> List.iter (fun (a,b) ->
          if ((MyLock.equal a e) && (MyLock.equal b newLock)) then (
            let msg = (sprintf "Deadlock warning: Locking order %s, %s at lines %i, %i violates order at %i, %i." (ValueDomain.Addr.show e.addr) (ValueDomain.Addr.show newLock.addr) e.loc.line newLock.loc.line b.loc.line a.loc.line) in
            Messages.report msg;
            let msg = (sprintf "Deadlock warning: Locking order %s, %s at lines %i, %i violates order at %i, %i." (ValueDomain.Addr.show newLock.addr) (ValueDomain.Addr.show e.addr) b.loc.line a.loc.line e.loc.line newLock.loc.line) in
            Messages.report ~loc:a.loc msg;
          )
          else () ) !forbiddenList ) lockList;

      (* Add forbidden order *)
      D.iter (
        fun lock ->
          add_comb newLock lock;
          let transAddList = List.find_all (fun (a,b) -> MyLock.equal a lock) !forbiddenList in
          List.iter (fun (a,b) -> add_comb newLock b) transAddList
      ) lockList
    end


  (* Initialization and finalization *)
  let init () = ()

  let finalize () = ()

  (* Some required states *)
  let startstate _ : D.t = D.empty ()
  let threadenter ctx lval f args = [D.empty ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  _ : D.t = D.empty ()

  (* ======== Transfer functions ======== *)
  (* Called for assignments, branches, ... *)

  (* Assignment lval <- exp *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  (* Branch *)
  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  (* Body of a function starts *)
  let body ctx (f:fundec) : D.t =
    ctx.local

  (* Returns from a function *)
  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  (* Calls/Enters a function *)
  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [D.bot (),ctx.local]

  (* Leaves a function *)
  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    au

  (* Helper function to convert query-offsets to valuedomain-offsets *)
  let rec conv_offset x =
    match x with
    | `NoOffset    -> `NoOffset
    | `Index (Const (CInt64 (i,ikind,s)),o) -> `Index (IntDomain.of_const (i,ikind,s), conv_offset o)
    | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
    | `Field (f,o) -> `Field (f, conv_offset o)

  (* Query the value (of the locking argument) to a list of locks. *)
  let eval_exp_addr (a: Queries.ask) exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a.f (Queries.MayPointTo exp) with
    | a when not (Queries.LS.is_top a) ->
      Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []
    | b -> Messages.warn @@ Messages.Unknown ("Could not evaluate '"^sprint d_exp exp^"' to an points-to set, instead got '"^Queries.LS.show b^"'."); []

  (* Called when calling a special/unknown function *)
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    if D.is_top ctx.local then ctx.local else
      match LibraryFunctions.classify f.vname arglist with
      | `Lock (_, _, _) ->
        List.fold_left (fun d lockAddr ->
          addLockingInfo {addr = lockAddr; loc = !Tracing.current_loc } ctx.local;
          D.add {addr = lockAddr; loc = !Tracing.current_loc } ctx.local
        ) ctx.local (eval_exp_addr (Analyses.ask_of_ctx ctx) (List.hd arglist))
      | `Unlock ->
        let lockAddrs = eval_exp_addr (Analyses.ask_of_ctx ctx) (List.hd arglist) in
        if List.length lockAddrs = 1 then
          let inLockAddrs e = List.exists (fun r -> ValueDomain.Addr.equal r e.addr) lockAddrs in
          D.filter (neg inLockAddrs) ctx.local
        else ctx.local
      | _ -> ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
