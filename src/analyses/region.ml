(** Analysis of disjoint heap regions for dynamically allocated memory ([region]).

    @see <https://doi.org/10.1007/978-3-642-03237-0_13> Seidl, H., Vojdani, V. Region Analysis for Race Detection. *)

open Batteries
open GoblintCil
open Analyses

module RegMap = RegionDomain.RegMap
module Reg = RegionDomain.Reg

module Spec =
struct
  include Analyses.DefaultSpec

  module D = RegionDomain.RegionDom
  module G = Lattice.Unit
  module C = D
  module V =
  struct
    include Printable.UnitConf (struct let name = "partitions" end)
    include StdV
  end

  let is_bullet exp st : bool =
    match st with
    | `Lifted reg ->
      begin match Reg.eval_exp exp with
        | Some (_,v,_) -> (try RegionDomain.RS.is_single_bullet (RegMap.find v reg) with Not_found -> false)
        | _ -> false
      end
    | `Top -> false
    | `Bot -> true

  (* queries *)
  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | _ -> Queries.Result.top q

  module Lvals = SetDomain.Make (Mval.Exp)
  module A =
  struct
    include Printable.Option (Lvals) (struct let name = "no region" end)
    let name () = "region"
    let may_race r1 r2 = match r1, r2 with
      | None, _
      | _, None -> false
      (* TODO: Should it happen in the first place that RegMap has empty value? Happens in 09-regions/34-escape_rc *)
      | Some r1, _ when Lvals.is_empty r1 -> true
      | _, Some r2 when Lvals.is_empty r2 -> true
      | Some r1, Some r2 when Lvals.disjoint r1 r2 -> false
      | _, _ -> true
    let should_print r = match r with
      | Some r when Lvals.is_empty r -> false
      | _ -> true
  end
  let access ctx (a: Queries.access) =
    match a with
    | Point ->
      Some (Lvals.empty ())
    | Memory {exp = e; _} ->
      (* TODO: remove regions that cannot be reached from the var*)
      (* forget specific indices *)
      (* TODO: If indices are topped, could they not be collected in the first place? *)
      Some (Lvals.empty ())

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
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate v = RegMap.bot ()
  let name () = "region"
end

let _ = MCP.register_analysis (module Spec : MCPSpec)
