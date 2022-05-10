(** Assigning static regions to dynamic memory. *)

open Prelude.Ana
open Analyses

module RegMap = RegionDomain.RegMap
module RegPart = RegionDomain.RegPart
module Reg = RegionDomain.Reg

module Spec =
struct
  include Analyses.DefaultSpec

  module D = RegionDomain.RegionDom
  module G = RegPart
  module C = D
  module V = Printable.UnitConf (struct let name = "partitions" end)

  let regions exp part st : Lval.CilLval.t list =
    match st with
    | `Lifted reg ->
      let ev = Reg.eval_exp exp in
      let to_exp (v,f) = (v,Lval.Fields.to_offs' f) in
      List.map to_exp (Reg.related_globals ev (part,reg))
    | `Top -> Messages.warn "Region state is broken :("; []
    | `Bot -> []

  let is_bullet exp part st : bool =
    match st with
    | `Lifted reg ->
      begin match Reg.eval_exp exp with
        | Some (_,v,_) -> (try RegionDomain.RS.is_single_bullet (RegMap.find v reg) with Not_found -> false)
        | _ -> false
      end
    | `Top -> false
    | `Bot -> true

  let get_region ctx e =
    let regpart = ctx.global () in
    if is_bullet e regpart ctx.local then
      None
    else
      Some (regions e regpart ctx.local)

  (* queries *)
  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.Regions e ->
      let regpart = ctx.global () in
      if is_bullet e regpart ctx.local then Queries.Result.bot q (* TODO: remove bot *) else
        let ls = List.fold_right Queries.LS.add (regions e regpart ctx.local) (Queries.LS.empty ()) in
        ls
    | _ -> Queries.Result.top q

  module Lvals = SetDomain.Make (Lval.CilLval)
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
      | Some r1, Some r2 when Lvals.is_empty (Lvals.inter r1 r2) -> false
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
      let rec unknown_index = function
        | `NoOffset -> `NoOffset
        | `Field (f, os) -> `Field (f, unknown_index os)
        | `Index (i, os) -> `Index (MyCFG.unknown_exp, unknown_index os) (* forget specific indices *)
      in
      Option.map (Lvals.of_list % List.map (Tuple2.map2 unknown_index)) (get_region ctx e)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match ctx.local with
    | `Lifted reg ->
      let old_regpart = ctx.global () in
      let regpart, reg = Reg.assign lval rval (old_regpart, reg) in
      if not (RegPart.leq regpart old_regpart) then
        ctx.sideg () regpart;
      `Lifted reg
    | x -> x

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let locals = f.sformals @ f.slocals in
    match ctx.local with
    | `Lifted reg ->
      let old_regpart = ctx.global () in
      let regpart, reg = match exp with
        | Some exp ->
          let module BS = (val Base.get_main ()) in
          Reg.assign (BS.return_lval ()) exp (old_regpart, reg)
        | None -> (old_regpart, reg)
      in
      let regpart, reg = Reg.kill_vars locals (Reg.remove_vars locals (regpart, reg)) in
      if not (RegPart.leq regpart old_regpart) then
        ctx.sideg () regpart;
      `Lifted reg
    | x -> x


  let enter ctx (lval: lval option) (fundec:fundec) (args:exp list) : (D.t * D.t) list =
    let rec fold_right2 f xs ys r =
      match xs, ys with
      | x::xs, y::ys -> f x y (fold_right2 f xs ys r)
      | _ -> r
    in
    match ctx.local with
    | `Lifted reg ->
      let f x r reg = Reg.assign (var x) r reg in
      let old_regpart = ctx.global () in
      let regpart, reg = fold_right2 f fundec.sformals args (old_regpart,reg) in
      if not (RegPart.leq regpart old_regpart) then
        ctx.sideg () regpart;
      [ctx.local, `Lifted reg]
    | x -> [x,x]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    match au with
    | `Lifted reg -> begin
      let old_regpart = ctx.global () in
      let module BS = (val Base.get_main ()) in
      let regpart, reg = match lval with
        | None -> (old_regpart, reg)
        | Some lval -> Reg.assign lval (AddrOf (BS.return_lval ())) (old_regpart, reg)
      in
      let regpart, reg = Reg.remove_vars [BS.return_varinfo ()] (regpart, reg) in
      if not (RegPart.leq regpart old_regpart) then
        ctx.sideg () regpart;
      `Lifted reg
      end
    | _ -> au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match (LibraryFunctions.find f.vname).special arglist with
    | Malloc _ | Calloc _ -> begin
        match ctx.local, lval with
        | `Lifted reg, Some lv ->
          let old_regpart = ctx.global () in
          let regpart, reg = Reg.assign_bullet lv (old_regpart, reg) in
          if not (RegPart.leq regpart old_regpart) then
            ctx.sideg () regpart;
          `Lifted reg
        | _ -> ctx.local
      end
    | _ ->
      let t, _, _, _ = splitFunctionTypeVI  f in
      match unrollType t with
      | TPtr (t,_) ->
        begin match Goblintutil.is_blessed t, lval with
          | Some rv, Some lv -> assign ctx lv (AddrOf (Var rv, NoOffset))
          | _ -> ctx.local
        end
      | _ -> ctx.local

  let startstate v =
    `Lifted (RegMap.bot ())

  let threadenter ctx lval f args =
    [`Lifted (RegMap.bot ())]
  let threadspawn ctx lval f args fctx = ctx.local

  let exitstate v = `Lifted (RegMap.bot ())

  let name () = "region"
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
