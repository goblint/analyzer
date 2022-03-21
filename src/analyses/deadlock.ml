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

    (* Add forbidden order *)
    D.iter (
      fun lock ->
        add_comb newLock lock;
      ) lockList


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

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal _ -> (* just repr of () *)
      let order_set = ctx.global () in
      ignore (Pretty.printf "deadlock: %a\n" G.pretty order_set);
      let module LH = Hashtbl.Make (MyLock) in
      let order = LH.create 12 in
      G.iter (fun (a, b) ->
          LH.modify_def (D.empty ()) a (D.add b) order
        ) order_set;

      (* TODO: find all cycles/SCCs *)
      let global_visited_nodes = LH.create 100 in

      (* DFS *)
      let rec iter_node path_visited_nodes path_visited_nodes' node =
        if D.mem node path_visited_nodes then (
          let pieces =
            List.map (fun lock ->
                let doc = MyLock.pretty () lock in
                (doc, Some lock.loc)
              ) path_visited_nodes'
          in
          M.msg_group Warning "Deadlock order" pieces
        )
        else if not (LH.mem global_visited_nodes node) then begin
          LH.replace global_visited_nodes node ();
          let new_path_visited_nodes = D.add node path_visited_nodes in
          let new_path_visited_nodes' = node :: path_visited_nodes' in
          D.iter (fun to_node ->
              iter_node new_path_visited_nodes new_path_visited_nodes' to_node
            ) (LH.find_default order node (D.empty ()))
        end
      in

      LH.iter (fun a _ ->
          iter_node (D.empty ()) [] a
        ) order
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
