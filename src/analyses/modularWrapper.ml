(** A wrapper for analysis that deactivates/activates them in modular/non-modular mode *)

open GoblintCil
open Analyses

module Spec (S : MCPSpec) : MCPPostSpec =
struct

  include S

  let name () = S.name ()
  module V = S.V

  module Either = struct
    include Printable.Either (S.D) (S.D)
    module Base1 = S.D
    module Base2 = S.D

    type domain = Left | Right
    let chosen_domain = Right

    let unop_to_t unop_left unop_right u =
      match chosen_domain with
      | Left -> `Left (unop_left u)
      | Right -> `Right (unop_right u)

    let binop_error = "Binary opration on Either received values where not the same component is set"

    let unop_of_t unop_left unop_right u =
      match u with
      | `Left x -> (unop_left x)
      | `Right x -> (unop_right x)

    let unop_on_t unop_left unop_right u =
      match u with
      | `Left x -> `Left (unop_left x)
      | `Right x -> `Right (unop_right x)

    let binop_to_t binop_left binop_right x y  =
      match x, y with
      | `Left x, `Left y -> `Left (binop_left x y)
      | `Right x, `Right y -> `Right (binop_right x y)
      | _ -> failwith binop_error

    let binop binop_left binop_right x y  =
      match x, y with
      | `Left x, `Left y -> binop_left x y
      | `Right x, `Right y -> binop_right x y
      | _ -> failwith binop_error

    let bot () =
      unop_to_t Base1.bot Base2.bot ()

    let top () =
      unop_to_t Base1.top Base2.top ()

    let leq =
      binop Base1.leq Base2.leq

    let join =
      binop_to_t Base1.join Base2.join

    let meet =
      binop_to_t Base1.meet Base2.meet

    let widen =
      binop_to_t Base1.widen Base2.widen

    let narrow =
      binop_to_t Base1.narrow Base2.narrow

    let is_top =
      unop_of_t Base1.is_top Base2.is_top

    let is_bot =
      unop_of_t Base1.is_bot Base2.is_bot

    let pretty_diff () (x, y) =
      match x, y with
      | `Left x, `Left y -> Base1.pretty_diff () (x, y)
      | `Right x, `Right y -> Base2.pretty_diff () (x, y)
      | `Left x, `Right y -> Pretty.dprintf "%s: %a not leq %a" (name ()) Base1.pretty x Base2.pretty y
      | `Right x, `Left y -> Pretty.dprintf "%s: %a not leq %a" (name ()) Base2.pretty x Base1.pretty y

  end
  module D = struct
    include Lattice.Option (Either)

    let right_to_left = function
      | Some (`Right x) -> Some (`Left x)
      | x -> x

    let remove_non_modular x =
      match S.modular_support () with
      | Modular
      | Both -> x
      | NonModular -> None

    let to_modular x =
      match S.modular_support () with
      | Modular
      | Both -> x
      | NonModular -> right_to_left x

    let to_non_modular x =
      match S.modular_support () with
      | NonModular -> x
      | Both
      | Modular -> right_to_left x
  end

  (* Extend G to the Lattice.T signature *)
  module G = struct
    include S.G
    let remove_non_modular x = x
    let to_modular x = x
    let to_non_modular x = x
  end

  module C = Printable.Option (S.C) (struct let name = S.C.name () end)
  module A = struct
    include Printable.Option (S.A) (struct let name = S.A.name () end)
    let may_race x y = match x, y with
      | Some x, Some y -> A.may_race x y
      | _, _ -> true

    let should_print x =
      BatOption.map_default A.should_print false x
  end

  module P = struct
    module E = Printable.Either (S.P) (S.P)
    include Printable.Option (E) (struct let name = S.P.name () end)
    let of_elt (x: D.t) : t =
      (* TODO: Fix of_elt *)
      match x with
      | Some (`Right x) -> Some (`Right (S.P.of_elt x))
      | Some (`Left _)
      | None -> None
  end

  let wrap_default x = Some (`Right x)

  let context (f: fundec) (v: D.t) = match v with
    | Some (`Right d) -> Some (S.context f d)
    | _ -> None

  let some_ctx_to_analysis_ctx ctx local =
    { ask = ctx.ask
    ; emit = ctx.emit
    ; node = ctx.node
    ; prev_node = ctx.prev_node
    ; control_context = ctx.control_context
    ; context = (fun () -> Option.get (ctx.context ()))
    ; edge = ctx.edge
    ; local
    ; global = ctx.global
    ; spawn = ctx.spawn
    ; split = (fun d events -> ctx.split (wrap_default d) events)
    ; sideg = ctx.sideg
    }

  let to_analysis_ctx (ctx: (D.t, S.G.t, C.t, S.V.t) ctx) : (S.D.t, S.G.t, S.C.t, S.V.t) ctx option =
    match ctx.local with
    | Some (`Right local) ->
      Some (some_ctx_to_analysis_ctx ctx local)
    | _ ->
      None

  let to_query_ctx (ctx: (D.t, S.G.t, C.t, S.V.t) ctx) : (S.D.t, S.G.t, S.C.t, S.V.t) ctx option =
    match ctx.local with
    | Some (`Right local)
    | Some (`Left local) ->
      Some (some_ctx_to_analysis_ctx ctx local)
    | _ ->
      None
  let map_ctx f ctx =
    match to_analysis_ctx ctx with
    | Some ctx -> wrap_default (f ctx)
    | None -> None

  let map_ctx_fs_fc f ~ctx ~fs ~fc =
    match to_analysis_ctx ctx, fs, fc with
    | _, _, Some None -> None (* Means no-context is not active, but there is still not context; do not continue *)
    | Some ctx, Some (`Right fs), Some ((Some _) as fc) (* matches regular "no-context" case *)
    | Some ctx, Some (`Right fs), (None as fc)
      ->
      wrap_default (f ctx fs fc)
    | _ ->
      None

  let map_ctx_fctx f ~ctx ~fctx =
    match to_analysis_ctx ctx, to_analysis_ctx fctx with
    | Some ctx, Some fctx -> wrap_default (f ctx fctx)
    | _ -> None


  let map_ctx_list f ctx =
    match to_analysis_ctx ctx with
    | Some ctx -> List.map (fun x -> Option.some (`Right x)) (f ctx)
    | None -> [None]

  let map_ctx_tuple_list f ctx =
    match to_analysis_ctx ctx with
    | Some ctx -> List.map (Batteries.Tuple2.mapn wrap_default) (f ctx)
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
    wrap_default (S.startstate v)

  let threadenter ctx lval f args =
    map_ctx_list (fun ctx -> S.threadenter ctx lval f args) ctx

  let threadspawn ctx lval f args (fctx: (D.t, G.t, C.t, V.t) ctx) : D.t =
    map_ctx_fctx (fun ctx fctx -> S.threadspawn ctx lval f args fctx) ~ctx ~fctx

  let exitstate v =
    wrap_default (S.exitstate v)

  let access (ctx: (D.t, G.t, C.t, V.t) ctx) (a: Queries.access) : A.t =
    match to_analysis_ctx ctx with
    | Some ctx -> Some (S.access ctx a)
    | None -> None

  let morphstate v (d : D.t) =
    match d with
    | Some (`Right d) -> wrap_default (S.morphstate v d)
    | Some (`Left _)
    | None -> None

  let sync ctx reason =
    map_ctx (fun ctx -> S.sync ctx reason) ctx

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match to_query_ctx ctx with
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

  let modular_combine_env ctx lval fd exp f_ask =
    map_ctx (fun ctx -> S.modular_combine_env ctx lval fd exp f_ask) ctx
  let modular_combine_assign ctx lval fd exp f_ask =
    map_ctx (fun ctx -> S.modular_combine_assign ctx lval fd exp f_ask) ctx

end

(* let _ =
   MCP.register_analysis (module Spec : MCPSpec) *)
