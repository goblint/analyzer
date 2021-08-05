
open Prelude.Ana
open Analyses

module Q = Queries
module VD = ValueDomain.Compound
module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "writtenLvals"
  module D = Lattice.Unit
  module LS = Q.LS
  module G = MapDomain.MapBot (Lval.CilLval) (VD)
  module C = Lattice.Unit
  let val_of _ = ()

  let side_to_f ctx (side: G.t) =
    let get_current_fun () = Option.map MyCFG.getFun !MyCFG.current_node in
    let f = get_current_fun () in
    match f with
    | Some f -> ctx.sideg f.svar side
    | None -> ()

  let is_allocated_var ctx (v: varinfo) = ctx.ask (Q.IsAllocatedVar v)
  (* Returns the set of argument and global vars within the set of lvalues *)
  let filter_outer_vars ctx (s: Q.LS.t): Q.LS.t = Q.LS.filter (fun (v,offset) -> v.vglob && not (is_allocated_var ctx v)) s

  let add_written_lval ctx (lval:lval) (v: VD.t): unit =
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
    let side = LS.fold (fun lv acc -> G.add lv v acc) written (G.empty ()) in
    side_to_f ctx side

  let add_written_option_lval ctx (lval: lval option): unit =
    let v = VD.top () in
    match lval with
    | Some lval -> add_written_lval ctx lval v
    | None -> ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let v = VD.top () in
    add_written_lval ctx lval v

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, D.bot ()]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
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
    let written_by_f = ctx.global f in
    let written_heap_vars_by_f = G.filter (fun (v, o) _ -> Set.mem (Cil.typeSig v.vtype) reachable_heap_var_typesigs) written_by_f in
    side_to_f ctx written_heap_vars_by_f

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    add_written_option_lval ctx lval

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = D.bot ()
  let exitstate v = D.top ()

  let query ctx (type a) (q: a Q.t): a Q.result = match q with
    | Q.WrittenLvals f ->
      let lvals = List.fold (fun acc (lv,_) -> LS.add lv acc) (LS.empty ()) (G.bindings (ctx.global f)) in
      lvals
    | _ -> Q.Result.top q

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
