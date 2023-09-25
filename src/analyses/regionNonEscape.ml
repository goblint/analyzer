(** Analysis of disjoint heap regions for dynamically allocated memory ([region]).

    @see <https://doi.org/10.1007/978-3-642-03237-0_13> Seidl, H., Vojdani, V. Region Analysis for Race Detection. *)

open Batteries
open GoblintCil
open Analyses

module RegMap = RegionNonEscapeDomain.RegMap
module Reg = RegionNonEscapeDomain.Reg
module RS = RegionNonEscapeDomain.RS

module Spec =
struct
  include Analyses.DefaultSpec

  module D = RegionNonEscapeDomain.RegionDom
  module G = Lattice.Unit
  module C = D
  module V =
  struct
    include Printable.UnitConf (struct let name = "partitions" end)
    include StdV
  end

  let regions exp reg = Reg.related_globals (Reg.eval_exp exp) reg

  let is_bullet exp reg =
    match Reg.eval_exp exp with
    | Some (_, v) -> (try RegionNonEscapeDomain.RS.is_single_bullet (RegMap.find v reg) with Not_found -> false)
    | _ -> false

  let get_region ctx e =	
    if is_bullet e ctx.local then	
      None	
    else	
      Some (regions e ctx.local)

  (* queries *)
  let query ctx (type a) (q: a Queries.t): a Queries.result = Queries.Result.top q

  module A =
  struct
    include BoolDomain.Bool
    let name () = "regionNonEscape"
    let may_race f1 f2 = not (f1 || f2)
    let should_print f = f
  end
  let access ctx (a: Queries.access) =
    match a with
    | Memory {exp = e; _} -> Option.is_none (get_region ctx e)
    | Point -> false

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = Reg.assign lval rval ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t = ctx.local

  let body ctx (f:fundec) : D.t = ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let locals = f.sformals @ f.slocals in
    let reg = ctx.local in
    let reg = match exp with
      | Some exp ->
        let module BS = (val Base.get_main ()) in
        Reg.assign (BS.return_lval ()) exp reg
      | None -> reg
    in
    Reg.remove_vars locals reg


  let enter ctx (lval: lval option) (fundec:fundec) (args:exp list) : (D.t * D.t) list =
    let rec fold_right2 f xs ys r =
      match xs, ys with
      | x::xs, y::ys -> f x y (fold_right2 f xs ys r)
      | _ -> r
    in
    let f x r reg = Reg.assign (var x) r reg in
    let reg = fold_right2 f fundec.sformals args ctx.local in
    [ctx.local, reg]

  let combine_env ctx lval fexp f args fc au f_ask =
    ctx.local

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    let module BS = (val Base.get_main ()) in
    let reg = match lval with
      | None -> au
      | Some lval -> Reg.assign lval (AddrOf (BS.return_lval ())) au
    in
    Reg.remove_vars [BS.return_varinfo ()] reg

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist, lval with
    (* TODO: should realloc use arg region if failed/in-place? *)
    | (Malloc _ | Calloc _ | Realloc _), Some lv -> Reg.assign_bullet lv ctx.local   
    | _, _ -> ctx.local

  let startstate v = RegMap.bot ()
  let threadenter ctx lval f args = [RegMap.bot ()]
  let threadspawn ctx lval f args fctx =
    let reg = ctx.local in
    match args with
    | [ptc_arg] ->
      begin match Reg.eval_exp ptc_arg with
        | Some (deref_y, y) when not (Reg.is_global y) ->
          (* Variable escapes if used as an argument when spawning a new thread *)
          Reg.add_set (RS.join RS.single_vf (RegMap.find y reg)) [y] reg
        | _ -> reg
      end
    | _ -> reg
  let exitstate v = RegMap.bot ()
  let name () = "regionNonEscape"
end

let _ = MCP.register_analysis (module Spec : MCPSpec)
