(** May-lockset analysis. *)

open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "maylocks"
  module D = LockDomain.MayLockset
  module C = LockDomain.MayLockset

  (* locking logic -- add all locks we can add *)
  (* let lock ctx rw may_fail return_value_on_success a lv arglist ls : D.ReverseAddrSet.t =
    let add_one ls e = D.add (e,rw) ls in
    let nls = List.fold_left add_one ls (List.concat_map (eval_exp_addr a) arglist) in
    match lv with
    | None -> nls
    | Some lv ->
      ctx.split nls [Events.SplitBranch (Lval lv, return_value_on_success)];
      if may_fail then ctx.split ls [Events.SplitBranch (Lval lv, not return_value_on_success)];
      raise Analyses.Deadcode *)

  (* let remove_rw x st = D.remove (x,true) (D.remove (x,false) st)
  (* unlocking logic *)
  let unlock remove_fn =
    match arglist with
    | x::xs -> begin match  (eval_exp_addr (Analyses.ask_of_ctx ctx) x) with
        | [x] -> remove_fn x ctx.local
        | _ -> ctx.local
      end
    | _ -> ctx.local *)

  let startstate v = D.empty ()
  let threadenter ctx lval f args = [D.empty ()]
  let exitstate  v = D.top ()

  let event ctx e octx =
    match e with
    | Events.Lock l ->
      D.add l ctx.local
    | Events.Unlock l ->
      D.remove (l, true) (D.remove (l, false) ctx.local)
    | _ ->
      ctx.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
