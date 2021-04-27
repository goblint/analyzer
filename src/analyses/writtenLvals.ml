
open Prelude.Ana
open Analyses

module Q = Queries

module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "writtenLvals"
  module D = Lattice.Unit
  module G = Q.LS
  module C = Lattice.Unit

  let val_of _ = ()

  let side_to_f ctx side =
    let get_current_fun () = Option.map MyCFG.getFun !MyCFG.current_node in
    let f = get_current_fun () in
    match f with
      | Some f -> ctx.sideg f.svar side
      | None -> ()

  let is_heap_var ctx (v: varinfo) =
    match ctx.ask (Q.IsHeapVar v) with
      | `MustBool true -> true
      | _ -> false

  let filter_heap_vars ctx (s: Q.LS.t) = Q.LS.filter (fun (v,offset) -> is_heap_var ctx v) s

  (* transfer functions *)
  let add_written_lval ctx (lval:lval): unit =
    let query e = ctx.ask (Q.MayPointTo e) in
    let side = match lval with
      | Mem e, NoOffset
      | Mem e, Index _ ->
        (match query e with
          | `LvalSet s -> filter_heap_vars ctx s
          | _ -> G.bot ()
        )
      | Mem e, Field (finfo, offs) ->
        (match query e with
          | `LvalSet s -> filter_heap_vars ctx s |> Q.LS.map (fun (v, offset) -> (v, `Field (finfo, offset)))
          | _ -> G.bot ()
        )
      | _, _ -> G.bot ()
    in
    side_to_f ctx side

  let add_written_option_lval ctx (lval: lval option): unit =
    match lval with
    | Some lval -> add_written_lval ctx lval
    | None -> ()

  let assign ctx (lval:lval) (rval:exp) : D.t =
    add_written_lval ctx lval

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
      let reachable_exp exp = match ctx.ask (Q.ReachableFrom exp) with
        | `LvalSet s -> s
        | `Top -> All
        | `Bot | _ -> Q.LS.bot ()
      in
      List.fold (fun acc exp -> Q.LS.join (reachable_exp exp) acc) (Q.LS.bot ()) args
        |> filter_heap_vars ctx
    in
    let reachable_heap_var_typesigs = match reachable_heap_vars with
      | Set s -> (Q.LS.S.to_list s) |> List.map (fun (v,o) -> Cil.typeSig v.vtype) |> Set.of_list
      | All -> failwith "This should not happen!"
    in
    (* Filter the written lvals by f to passed heapvars *)
    let written_by_f = ctx.global f in
    let written_heap_vars_by_f = Q.LS.filter (fun (v, o) -> Set.mem (Cil.typeSig v.vtype) reachable_heap_var_typesigs) written_by_f in
    side_to_f ctx written_heap_vars_by_f

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    add_written_option_lval ctx lval

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = D.bot ()
  let exitstate  v = D.top ()

  let query ctx (q:Q.t) = match q with
    | Q.WrittenLvals f ->
      `LvalSet (ctx.global f)
    | _ -> `Top

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
