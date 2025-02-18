(** Analysis of disjoint heap regions for dynamically allocated memory ([region]).

    @see <https://doi.org/10.1007/978-3-642-03237-0_13> Seidl, H., Vojdani, V. Region Analysis for Race Detection. *)

open Batteries
open GoblintCil
open Analyses

module RegMap = RegionDomain.RegMap
module RegPart = RegionDomain.RegPart
module Reg = RegionDomain.Reg

module Spec =
struct
  include Analyses.DefaultSpec

  module D = RegionDomain.RegionDom
  module G = RegPart
  include Analyses.ValueContexts(D)

  module V =
  struct
    include Printable.UnitConf (struct let name = "partitions" end)
    include StdV
  end

  let regions exp part st : Mval.Exp.t list =
    match st with
    | `Lifted reg ->
      let ev = Reg.eval_exp exp in
      Reg.related_globals ev (part,reg)
    | `Top -> Messages.info ~category:Unsound "Region state is broken :("; []
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

  let get_region man e =
    let regpart = man.global () in
    if is_bullet e regpart man.local then
      None
    else
      Some (regions e regpart man.local)

  (* queries *)
  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.Regions e ->
      let regpart = man.global () in
      if is_bullet e regpart man.local then Queries.Result.bot q (* TODO: remove bot *) else
        let ls = List.fold_left (Fun.flip @@ Queries.LS.add) (Queries.LS.empty ()) (regions e regpart man.local) in
        ls
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
  let access man (a: Queries.access) =
    match a with
    | Point ->
      Some (Lvals.empty ())
    | Memory {exp = e; _} ->
      (* TODO: remove regions that cannot be reached from the var*)
      (* forget specific indices *)
      (* TODO: If indices are topped, could they not be collected in the first place? *)
      Option.map (Lvals.of_list % List.map (Tuple2.map2 Offset.Exp.top_indices)) (get_region man e)

  (* transfer functions *)
  let assign man (lval:lval) (rval:exp) : D.t =
    match man.local with
    | `Lifted reg ->
      let old_regpart = man.global () in
      let regpart, reg = Reg.assign lval rval (old_regpart, reg) in
      if not (RegPart.leq regpart old_regpart) then
        man.sideg () regpart;
      `Lifted reg
    | x -> x

  let branch man (exp:exp) (tv:bool) : D.t =
    man.local

  let body man (f:fundec) : D.t =
    man.local

  let return man (exp:exp option) (f:fundec) : D.t =
    let locals = f.sformals @ f.slocals in
    match man.local with
    | `Lifted reg ->
      let old_regpart = man.global () in
      let regpart, reg = match exp with
        | Some exp ->
          Reg.assign (ReturnUtil.return_lval ()) exp (old_regpart, reg)
        | None -> (old_regpart, reg)
      in
      let regpart, reg = Reg.kill_vars locals (Reg.remove_vars locals (regpart, reg)) in
      if not (RegPart.leq regpart old_regpart) then
        man.sideg () regpart;
      `Lifted reg
    | x -> x


  let enter man (lval: lval option) (fundec:fundec) (args:exp list) : (D.t * D.t) list =
    let rec fold_right2 f xs ys r =
      match xs, ys with
      | x::xs, y::ys -> f x y (fold_right2 f xs ys r)
      | _ -> r
    in
    match man.local with
    | `Lifted reg ->
      let f x r reg = Reg.assign (var x) r reg in
      let old_regpart = man.global () in
      let regpart, reg = fold_right2 f fundec.sformals args (old_regpart,reg) in
      if not (RegPart.leq regpart old_regpart) then
        man.sideg () regpart;
      [man.local, `Lifted reg]
    | x -> [x,x]

  let combine_env man lval fexp f args fc au f_ask =
    man.local

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    match au with
    | `Lifted reg -> begin
        let old_regpart = man.global () in
        let regpart, reg = match lval with
          | None -> (old_regpart, reg)
          | Some lval -> Reg.assign lval (AddrOf (ReturnUtil.return_lval ())) (old_regpart, reg)
        in
        let regpart, reg = Reg.remove_vars [ReturnUtil.return_varinfo ()] (regpart, reg) in
        if not (RegPart.leq regpart old_regpart) then
          man.sideg () regpart;
        `Lifted reg
      end
    | _ -> au

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Malloc _ | Calloc _ | Realloc _ | Alloca _ -> begin
        match man.local, lval with
        | `Lifted reg, Some lv ->
          let old_regpart = man.global () in
          (* TODO: should realloc use arg region if failed/in-place? *)
          let regpart, reg = Reg.assign_bullet lv (old_regpart, reg) in
          if not (RegPart.leq regpart old_regpart) then
            man.sideg () regpart;
          `Lifted reg
        | _ -> man.local
      end
    | _ ->
      man.local

  let startstate v =
    `Lifted (RegMap.bot ())

  let threadenter man ~multiple lval f args =
    [`Lifted (RegMap.bot ())]
  let threadspawn man ~multiple lval f args fman =
    match man.local with
    | `Lifted reg ->
      let old_regpart = man.global () in
      let regpart, reg = List.fold_right Reg.assign_escape args (old_regpart, reg) in
      if not (RegPart.leq regpart old_regpart) then
        man.sideg () regpart;
      `Lifted reg
    | x -> x

  let exitstate v = `Lifted (RegMap.bot ())

  let name () = "region"
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
