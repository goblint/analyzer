
open Prelude.Ana
open Analyses

module Q = Queries
module VD = ValueDomain.Compound
module Spec (B: Analyses.MCPSpec) : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "writtenLvals"
  module D = B.D
  module LS = Q.LS
  module Map = MapDomain.MapBot (Lval.CilLval) (VD)
  module G = Lattice.Prod (B.G) (Map)
  module C = B.C

  let project_first (f: ('a -> ('b * 'c))) = function x -> fst (f x)

  let project_side sideg = function x -> function v -> sideg x (v, Map.bot ())

  let project_ctx (ctx: (D.t, G.t, C.t) ctx) : (D.t, B.G.t, C.t) ctx =
    {ctx with global = project_first ctx.global; sideg = project_side ctx.sideg}

  let init = B.init
  let finalize = B.finalize
  let call_descr = B.call_descr
  let vdecl ctx = B.vdecl (project_ctx ctx)
  let event ctx e fctx = B.event (project_ctx ctx) e (project_ctx fctx)
  let sync ctx = B.sync (project_ctx ctx)
  let context = B.context
  let val_of = B.val_of

  let side_to_f (ctx: ('a, G.t, 'b) ctx) (side: Map.t) =
    let get_current_fun () = Option.map Node.find_fundec !MyCFG.current_node in
    let f = get_current_fun () in
    match f with
    | Some f -> ctx.sideg f.svar (B.G.bot (), side)
    | None -> ()

  let is_allocated_var ctx (v: varinfo) = ctx.ask (Q.IsAllocatedVar v)
  (* Returns the set of argument and global vars within the set of lvalues *)
  let filter_outer_vars ctx (s: Q.LS.t): Q.LS.t = Q.LS.filter (fun (v,offset) -> v.vglob && not (is_allocated_var ctx v)) s

  let add_written_lval (ctx: ('a, G.t, 'b) ctx) (lval:lval) (v: VD.t): unit =
    (* If we write to a top address, we warn here *)
    let query_may_point_to_handle_top e = match ctx.ask (Q.MayPointTo e) with
      | `Top -> M.warn @@ "Write to top address occurs in expression " ^ (Pretty.sprint ~width:100 (Cil.d_exp () e)) ^ "\n"; LS.bot ()
      | s -> s
    in
    let written = match lval with
      | Mem e, NoOffset
      | Mem e, Index _ ->
          let may_point_to = query_may_point_to_handle_top e in
          filter_outer_vars ctx may_point_to
      | Mem e, Field (finfo, offs) ->
        begin
          let may_point_to = query_may_point_to_handle_top e in
          filter_outer_vars ctx may_point_to |> Q.LS.map (fun (v, offset) -> (v, `Field (finfo, offset)))
        end
      | Var v, offs ->
        if v.vglob && not (is_allocated_var ctx v) then
          begin
            let offs = Lval.CilLval.of_ciloffs offs in
            LS.singleton (v, offs)
          end
        else
          begin
            M.tracel "writtenLvals" "Ignoring writte to lval %a\n" Cil.d_lval lval;
            LS.bot ()
          end
    in
    let side = LS.fold (fun lv acc -> Map.add lv v acc) written (Map.empty ()) in
    side_to_f ctx side

  let add_written_option_lval ctx (lval: lval option): unit =
    let v = VD.top () in
    match lval with
    | Some lval -> add_written_lval ctx lval v
    | None -> ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let v = VD.top () in
    add_written_lval ctx lval v;
    B.assign (project_ctx ctx) lval rval

  let branch ctx (exp:exp) (tv:bool) : D.t =
    B.branch (project_ctx ctx) exp tv

  let body ctx (f:fundec) : D.t =
    B.body (project_ctx ctx) f

  let return ctx (exp:exp option) (f:fundec) : D.t =
    B.return (project_ctx ctx) exp f

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    B.enter (project_ctx ctx) lval f args

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    (* We have a call: [lval =] f(e1,...,ek) *)
    (* Set the lval to written, if any. *)
    add_written_option_lval ctx lval;

    (* Find the heapvars that are reachable from the passed arguments *)
    let reachable_heap_vars =
      let reachable_exp exp = ctx.ask (Q.ReachableFrom exp) in
      let reachable_from_exp = List.fold (fun acc exp -> Q.LS.join (reachable_exp exp) acc) (Q.LS.bot ()) args in
      match reachable_from_exp with
      | `Top -> M.warn "Top address is reachable from the expression (unhandled!)."; Q.LS.bot ()
      | `Lifted s -> filter_outer_vars ctx (`Lifted s)
    in
    let reachable_heap_var_typesigs = match reachable_heap_vars with
      | `Lifted s -> (Q.LS.elements reachable_heap_vars) |> List.map (fun (v,o) -> Cil.typeSig v.vtype) |> Set.of_list
      | `Top -> failwith "This should not happen!"
    in
    (* Filter the written lvals by f to passed heapvars *)
    let (b, written_by_f) = ctx.global f.svar in
    let written_heap_vars_by_f = Map.filter (fun (v, o) _ -> Set.mem (Cil.typeSig v.vtype) reachable_heap_var_typesigs) written_by_f in
    side_to_f ctx written_heap_vars_by_f;
    B.combine (project_ctx ctx) lval fexp f args fc au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let r = B.special (project_ctx ctx) lval f arglist in
    add_written_option_lval ctx lval;
    r

  let startstate = B.startstate
  let threadenter ctx = B.threadenter (project_ctx ctx)
  let threadspawn ctx lv v exps fctx = B.threadspawn (project_ctx ctx) lv v exps (project_ctx fctx)
  let exitstate = B.exitstate

  let query ctx (type a) (q: a Q.t): a Q.result = match q with
    | Q.WrittenLvals f ->
      let lvals = List.fold (fun acc (lv,_) -> LS.add lv acc) (LS.empty ()) (Map.bindings (snd (ctx.global f))) in
      lvals
    | _ -> B.query (project_ctx ctx) q

end
