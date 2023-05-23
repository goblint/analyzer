(** A wrapper for analysis that deactivates/activates them in modular/non-modular mode *)

open GoblintCil
open Analyses

module Spec (S : MCPSpec) : MCPPostSpec =
struct

  include S

  let name () = S.name ()
  module V = S.V
  module D = struct
    include Lattice.Option (S.D)
    let to_modular x =
      (* let stack = Printexc.get_callstack 10 in *)
      (* let stack = Printexc.raw_backtrace_to_string stack in *)
      (* M.tracel "to_modular" "%s\n" stack; *)
      let text, r = match S.modular_support () with
        | Modular
        | Both -> "unchanged", x
        | NonModular -> "none", None
      in
      M.tracel "to_modular" "Calling to_modular for analysis %s. Results is %s!\n" (S.name ()) text;
      r

    let to_non_modular x =
      match S.modular_support () with
      | NonModular -> x
      | Both
      | Modular -> None
  end

  (* Extend G to the Lattice.T signature *)
  module G = struct
    include S.G
    let to_modular x = x
    let to_non_modular x = x
  end

  module C = Printable.Option (S.C) (struct let name = S.C.name () end)
  module A = struct
    include Printable.Option (S.A) (struct let name = S.A.name () end)
    let may_race x y = match x, y with
      | Some x, Some y -> A.may_race x y
      | _, _ -> false

    let should_print x =
      BatOption.map_default A.should_print false x
  end

  let context (f: fundec) (v: D.t) = Option.map (S.context f) v

  let to_analysis_ctx (ctx: (D.t, S.G.t, C.t, S.V.t) ctx) : (S.D.t, S.G.t, S.C.t, S.V.t) ctx option =
    match ctx.local with
    | Some local ->
      Some { ask = ctx.ask
           ; emit = ctx.emit
           ; node = ctx.node
           ; prev_node = ctx.prev_node
           ; control_context = ctx.control_context
           ; context = (fun () -> Option.get (ctx.context ()))
           ; edge = ctx.edge
           ; local
           ; global = ctx.global
           ; spawn = ctx.spawn
           ; split = (fun d events -> ctx.split (Some d) events)
           ; sideg = ctx.sideg
           }
    | _ ->
      None

  let map_ctx f ctx =
    match to_analysis_ctx ctx with
    | Some ctx -> Some (f ctx)
    | None -> None

  let map_ctx_fs_fc f ~ctx ~fs ~fc =
    match to_analysis_ctx ctx, fs, fc with
    | _, _, Some None -> None (* Means no-context is not active, but there is still not context; do not continue *)
    | Some ctx, Some fs, Some ((Some _) as fc) (* matches regular "no-context" case *)
    | Some ctx, Some fs, (None as fc)
      ->
      Some (f ctx fs fc)
    | _ ->
      None

  let map_ctx_fctx f ~ctx ~fctx =
    match to_analysis_ctx ctx, to_analysis_ctx fctx with
    | Some ctx, Some fctx -> Some (f ctx fctx)
    | _ -> None


  let map_ctx_list f ctx =
    match to_analysis_ctx ctx with
    | Some ctx -> List.map Option.some (f ctx)
    | None -> [None]

  let map_ctx_tuple_list f ctx =
    match to_analysis_ctx ctx with
    | Some ctx -> List.map (Batteries.Tuple2.mapn Option.some) (f ctx)
    | None -> [None, None]


  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    map_ctx (fun ctx -> S.assign ctx lval rval) ctx

  let branch ctx (exp:exp) (tv:bool) : D.t =
    map_ctx (fun ctx -> S.branch ctx exp tv) ctx

  let body ctx (f:fundec) : D.t =
    map_ctx (fun ctx -> S.body ctx f) ctx

  let return ctx (exp:exp option) (f:fundec) : D.t =
    map_ctx (fun ctx -> S.return ctx exp f) ctx

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    map_ctx_tuple_list (fun ctx -> S.enter ctx lval f args) ctx

  let combine_env ctx lval fexp f args fc fs f_ask =
    map_ctx_fs_fc (fun ctx fs fc -> S.combine_env ctx lval fexp f args fc fs f_ask) ~ctx ~fs ~fc

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (fs:D.t) (f_ask: Queries.ask) : D.t =
    map_ctx_fs_fc (fun ctx fs fc -> S.combine_assign ctx lval fexp f args fc fs f_ask) ~ctx ~fs ~fc

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    map_ctx (fun ctx -> S.special ctx lval f arglist) ctx

  let startstate v =
    Some (S.startstate v)

  let threadenter ctx lval f args =
    map_ctx_list (fun ctx -> S.threadenter ctx lval f args) ctx

  let threadspawn ctx lval f args (fctx: (D.t, G.t, C.t, V.t) ctx) : D.t =
    map_ctx_fctx (fun ctx fctx -> S.threadspawn ctx lval f args fctx) ~ctx ~fctx

  let exitstate v =
    Some (S.exitstate v)

  let access (ctx: (D.t, G.t, C.t, V.t) ctx) (a: Queries.access) : A.t =
    match to_analysis_ctx ctx with
    | Some ctx -> Some (S.access ctx a)
    | None -> None

  let morphstate v d =
    match d with
    | Some d -> Some (S.morphstate v d)
    | None -> None

  let should_join d1 d2 =
    match d1, d2 with
    | Some d1, Some d2 ->
      S.should_join d1 d2
    | Some _, None
    | None, Some _ -> true
    | None, None -> true

  let sync ctx reason =
    map_ctx (fun ctx -> S.sync ctx reason) ctx

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match to_analysis_ctx ctx with
    | Some ctx -> (fun ctx -> S.query ctx q) ctx
    | None -> Queries.Result.top q

  let vdecl ctx varinfo =
    map_ctx (fun ctx -> S.vdecl ctx varinfo) ctx

  let asm ctx =
    map_ctx S.asm ctx

  let skip ctx =
    map_ctx S.skip ctx

  let paths_as_set ctx =
    map_ctx_list S.paths_as_set ctx

  let event ctx events ctx2 =
    map_ctx_fctx (fun ctx ctx2 -> S.event ctx events ctx2) ~ctx ~fctx:ctx2

  let modular_call ctx lval fd exp f_ask =
      map_ctx (fun ctx -> S.modular_call ctx lval fd exp f_ask) ctx
end

(* let _ =
   MCP.register_analysis (module Spec : MCPSpec) *)
