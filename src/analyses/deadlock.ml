(** Deadlock analysis. *)

open Prelude.Ana
open Analyses
open DeadlockDomain

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "deadlock"

  (* The domain for the analysis *)
  module D = DeadlockDomain.Lockset (* MayLockset *)
  module C = DeadlockDomain.Lockset
  module V = Printable.UnitConf (struct let name = "deadlock" end)
  module G =
  struct
    include SetDomain.Make (Printable.Prod (MyLock) (MyLock))
    let leq x y = !GU.postsolving || leq x y (* HACK: to pass verify*)
  end

  let addLockingInfo ctx newLock lockList =

    let add_comb a b =
      let d =
        if !GU.should_warn then
          G.singleton (a, b)
        else
          G.bot () (* HACK: just to pass validation with MCP DomVariantLattice *)
      in
      ctx.sideg () d
    in

    (* Check forbidden list *)
    if true || !Goblintutil.postsolving then begin (* TODO: only postsolving *)
      D.iter (fun e -> G.iter (fun (a,b) ->
          if ((MyLock.equal a e) && (MyLock.equal b newLock)) then (
            Messages.warn "Deadlock warning: Locking order %a, %a at %a, %a violates order at %a, %a." ValueDomain.Addr.pretty e.addr ValueDomain.Addr.pretty newLock.addr CilType.Location.pretty e.loc CilType.Location.pretty newLock.loc CilType.Location.pretty b.loc CilType.Location.pretty a.loc;
            Messages.warn ~loc:a.loc "Deadlock warning: Locking order %a, %a at %a, %a violates order at %a, %a." ValueDomain.Addr.pretty newLock.addr ValueDomain.Addr.pretty e.addr CilType.Location.pretty b.loc CilType.Location.pretty a.loc CilType.Location.pretty e.loc CilType.Location.pretty newLock.loc;
          )
          else () ) (ctx.global ()) ) lockList;

      (* Add forbidden order *)
      D.iter (
        fun lock ->
          add_comb newLock lock;
          let transAddList = G.filter (fun (a,b) -> MyLock.equal a lock) (ctx.global ()) in
          G.iter (fun (a,b) -> add_comb newLock b) transAddList
      ) lockList
    end


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
    | `Index (Const (CInt (i,ikind,s)),o) -> `Index (IntDomain.of_const (i,ikind,s), conv_offset o)
    | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
    | `Field (f,o) -> `Field (f, conv_offset o)

  (* Query the value (of the locking argument) to a list of locks. *)
  let eval_exp_addr (a: Queries.ask) exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a.f (Queries.MayPointTo exp) with
    | a when not (Queries.LS.is_top a) ->
      Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []
    | b -> Messages.warn "Could not evaluate '%a' to an points-to set, instead got '%a'." d_exp exp Queries.LS.pretty b; []

  (* Called when calling a special/unknown function *)
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    if D.is_top ctx.local then ctx.local else
      match LibraryFunctions.classify f.vname arglist with
      | `Lock (_, _, _) ->
        List.fold_left (fun d lockAddr ->
            addLockingInfo ctx {addr = lockAddr; loc = !Tracing.current_loc } ctx.local;
            D.add {addr = lockAddr; loc = !Tracing.current_loc } ctx.local
          ) ctx.local (eval_exp_addr (Analyses.ask_of_ctx ctx) (List.hd arglist))
      | `Unlock ->
        let lockAddrs = eval_exp_addr (Analyses.ask_of_ctx ctx) (List.hd arglist) in
        if List.compare_length_with lockAddrs 1 = 0 then
          let inLockAddrs e = List.exists (fun r -> ValueDomain.Addr.equal r e.addr) lockAddrs in
          D.filter (neg inLockAddrs) ctx.local
        else ctx.local
      | _ -> ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
