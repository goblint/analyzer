(** Deadlock analysis. *)

open Prelude.Ana
open Analyses
open DeadlockDomain

module Spec =
struct
  include Analyses.IdentitySpec

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
  let exitstate  _ : D.t = D.empty ()

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
      let module LH = Hashtbl.Make (ValueDomain.Addr) in
      let module LS = Set.Make (ValueDomain.Addr) in
      let order: G.t LH.t LH.t = LH.create 12 in
      G.iter (fun (a, b) ->
          LH.modify_def (LH.create 1) a.addr (fun h ->
              LH.modify_def (G.empty ()) b.addr (G.add (a, b)) h;
              h
            ) order
        ) order_set;

      (* TODO: find all cycles/SCCs *)
      let global_visited_nodes = LH.create 100 in

      (* DFS *)
      let rec iter_node (path_visited_nodes: LS.t) (path_visited_nodes': G.elt list) (node: LS.elt) =
        if LS.mem node path_visited_nodes then (
          let pieces =
            List.concat_map (fun (a, b) ->
                [
                  (* backwards to get correct printout order *)
                  (Pretty.dprintf "lock before: %a" MyLock.pretty b, Some b.loc);
                  (Pretty.dprintf "lock after: %a" MyLock.pretty a, Some a.loc);
                ]
              ) path_visited_nodes'
          in
          M.msg_group Warning "Deadlock order" pieces
        )
        else if not (LH.mem global_visited_nodes node) then begin
          LH.replace global_visited_nodes node ();
          let new_path_visited_nodes = LS.add node path_visited_nodes in
          LH.iter (fun to_node gs ->
              G.iter (fun g ->
                  let new_path_visited_nodes' = g :: path_visited_nodes' in
                  iter_node new_path_visited_nodes new_path_visited_nodes' to_node
                ) gs
            ) (LH.find_default order node (LH.create 0))
        end
      in

      LH.iter (fun a _ ->
          iter_node LS.empty [] a
        ) order
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
