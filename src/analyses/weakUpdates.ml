(** TODO *)

open GoblintCil
open Analyses

module M = Messages
module AD = Queries.AD
module Q = Queries
module WeakUpdates = BaseDomain.WeakUpdates


module Spec =
struct
  (* include Analyses.DefaultSpec *)
  include Analyses.IdentitySpec

  module D = WeakUpdates
  include Analyses.ValueContexts (D)
  module P = IdentityP (D)

  let name () = "weakUpdates"

  let startstate v = D.bot ()
  let exitstate  v = D.bot ()

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Q.IsMultiple v -> WeakUpdates.mem v man.local ||
                        (hasAttribute "thread" v.vattr && v.vaddrof) (* thread-local variables if they have their address taken, as one could then compare several such variables *)
    | _ -> Queries.Result.top q

  let reachable (ask: Queries.ask) e: D.t =
    match ask.f (Queries.ReachableFrom e) with
    | ad when not (Queries.AD.is_top ad) ->
      let to_extra addr set =
        match addr with
        | Queries.AD.Addr.Addr (v,_) -> D.add v set
        | _ -> set
      in
      Queries.AD.fold to_extra ad (D.empty ())
    (* Ignore soundness warnings, as invalidation proper will raise them. *)
    | ad ->
      if M.tracing then M.tracel "escape" "reachable %a: %a" d_exp e Queries.AD.pretty ad;
      D.empty ()

  let make_entry ?(thread=false) (man:(D.t, G.t, C.t, V.t) Analyses.man) fundec args: D.t =
    let ask = Analyses.ask_of_man man in
    let st = man.local in
    (* Evaluate the arguments. *)
    (* let vals = List.map (eval_rv ~man st) args in *)
    (* generate the entry states *)
    (* If we need the globals, add them *)
    (* TODO: make this is_private PrivParam dependent? PerMutexOplusPriv should keep *)

    (* List of reachable variables *)
    (* let reachable = AD.to_var_may (reachable_vars ~man st (get_ptrs vals)) in
    let reachable = List.filter (fun v -> CPA.mem v st.cpa) reachable in *)
    let reachable =
      List.map (reachable ask) args
      |> List.concat_map D.elements
    in

    (* Identify locals of this fundec for which an outer copy (from a call down the callstack) is reachable *)
    let reachable_other_copies = List.filter (fun v -> GobOption.exists (CilType.Fundec.equal fundec) @@ Cilfacade.find_scope_fundec v) reachable in
    (* Add to the set of weakly updated variables *)
    let new_weak = WeakUpdates.join st (WeakUpdates.of_list reachable_other_copies) in
    new_weak

  let enter man lval fn args : (D.t * D.t) list =
    [man.local, make_entry man fn args]

  let threadenter man ~multiple (lval: lval option) (f: varinfo) (args: exp list): D.t list =
    let st = man.local in
    match Cilfacade.find_varinfo_fundec f with
    | fd ->
      [make_entry ~thread:true man fd args]
    | exception Not_found ->
      (* Unknown functions *)
      [st]

  let combine_env man (lval:lval option) fexp (f:fundec) (args:exp list) fc au (f_ask: Queries.ask) =
    man.local  (* keep weak from caller *)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
