(** An analysis for the detection of use-after-free vulnerabilities. *)

open GoblintCil
open Analyses

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "useafterfree"
  (* module D = Lattice.Unit *)
  module D = SetDomain.Make(ValueDomain.Blobs)
  module C = Lattice.Unit

  (** TODO: What about context-sensitivity? *)
  let context _ _ = ()


  (* HELPER FUNCTIONS *)

  let check_exp (exp:exp) ctx =
    let state = ctx.local in
    match ctx.ask (Queries.EvalValue exp) with
    | a when not (Queries.VD.is_top a) ->
      begin match a with
        | `Blob (v, s, t) -> if D.mem (v, s, t) state then true else false
        | _ -> false
      end
    | _ -> false

  let check_lval (lval:lval) ctx =
    let state = ctx.local in
    match lval with
    | (Mem e, _) ->
      begin match ctx.ask (Queries.EvalValue e) with
        | a when not (Queries.VD.is_top a) ->
          begin match a with
            | `Blob (v, s, t) -> if D.mem (v, s, t) state then true else false
            | _ -> false
          end
        | _ -> false
      end
    | _ -> false


  (* TRANSFER FUNCTIONS *)

  let assign ctx (lval:lval) (rval:exp) : D.t =
    let state = ctx.local in
    (* Intuition:
      * Check if lval and/or lval has an expression that points to a "maybe freed" blob
      * If yes -> send out a WARNING; otherwise -> don't WARN
      * In either case above -> don't change the CFG node's state
    *)
    match check_lval lval ctx, check_exp rval ctx with
    | true, true -> M.warn "WARN: lval and rval contain maybe freed blob"; state
    | false, true -> M.warn "WARN: rval contains maybe freed blob"; state
    | true, false -> M.warn "WARN: lval contains maybe freed blob"; state
    | false, false -> state

  let branch ctx (exp:exp) (tv:bool) : D.t =
    let state = ctx.local in
    if check_exp exp ctx then
      M.warn "WARN: branch exp contains maybe freed blob";
    state

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let state = ctx.local in
    match exp with
    | Some e ->
      if check_exp e ctx then
        M.warn "WARN: return expression contains maybe freed blob";
      state
    | None -> state

  let enter ctx (lval:lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let caller_state = ctx.local in
    let filtered_args = List.filter (fun x -> check_exp x ctx) args in
    let args_to_add_to_callee_state = List.map (fun x ->
        match ctx.ask (Queries.EvalValue x) with
        | a when not (Queries.VD.is_top a) ->
          begin match a with
            | `Blob (v, s, t) -> (v, s, t)
            | _ -> ValueDomain.Blobs.top () (* TODO: Is this correct? *)
          end
        | _ -> ValueDomain.Blobs.top () (* TODO: Is this correct? *)
      ) filtered_args in
    let callee_state = D.of_list args_to_add_to_callee_state in
    match lval with
    | Some lval ->
      if check_lval lval ctx then
        M.warn "WARN: lval in enter contains maybe freed blob";
      [caller_state, callee_state]
    | None -> [caller_state, callee_state]

  (* TODO: Unsure about this transfer function. Should we leave it as an identity function? *)
  let combine_env ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask:Queries.ask) : D.t =
    callee_local

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask: Queries.ask): D.t =
    let state = ctx.local in
    let () = List.iter (fun x ->
        if check_exp x ctx then
          M.warn "argument in combine_assign contains maybe freed blob"
      ) args in
    match lval with
    | Some lval ->
      if check_lval lval ctx then
        M.warn "lval in combine_assign contains maybe freed blob";
      state
    | None -> state

  let special ctx (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let state = ctx.local in
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Free ptr ->
      if not (check_exp ptr ctx) then
        begin match ctx.ask (Queries.EvalValue ptr) with
          | a when not (Queries.VD.is_top a) ->
            begin match a with
              | `Blob (v, s, t) -> D.add (v, s, t) state
              | _ -> state
            end
          | _ -> state
        end
      else
        (* TODO: Is this way of printing in the else block okayish? *)
        let () = M.warn "WARN: trying to free an already freed ptr" in
        state
    (*
     * Check if we're allocating a maybe freed ptr -> in this case remove
     * it from the maybe freed set
     * TODO: Does this make sense?
     *)
    | Malloc _
    | Calloc _
    | Realloc _ ->
      begin match lval with
        | Some lval ->
          begin match lval with
            | (Mem e, _) ->
              begin match ctx.ask (Queries.EvalValue e) with
                | a when not (Queries.VD.is_top a) ->
                  begin match a with
                    | `Blob (v, s, t) ->
                      (*
                       * If we're trying to allocate a blob that is maybe freed,
                       * then we can remove it from the maybe freed set
                       * TODO: Makes sense?
                       *)
                      if D.mem (v, s, t) state then
                        D.remove (v, s, t) state
                      else state
                    | _ -> state
                  end
                | _ -> state
              end
            | _ -> state
          end
        | None -> state
      end
    (*
     * If we're not dealing with free or *alloc,
     * then check if the lval we're assigning to is a ptr to a maybe freed blob.
     * If yes, then WARN
     *)
    | _ ->
      match lval with
      | Some lval ->
        if check_lval lval ctx then
          M.warn "WARN: lval in special contains maybe freed blob";
        state
      | None -> state

  let threadenter ctx lval f args = [ctx.local]
  let threadspawn ctx lval f args fctx = ctx.local

  let startstate v = D.bot ()
  let exitstate v = D.top ()

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)