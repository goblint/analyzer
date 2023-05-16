(** Data race analysis. *)

open GoblintCil
open Analyses


(** Data race analyzer without base --- this is the new standard *)
module Spec =
struct
  include UnitAnalysis.Spec

  let name () = "race"

  (* Two global invariants:
     1. (lval, type) -> accesses  --  used for warnings
     2. varinfo -> set of (lval, type)  --  used for IterSysVars Global *)

  module V0 = Printable.Prod (Access.LVOpt) (Access.T)
  module V =
  struct
    include Printable.Either (V0) (CilType.Varinfo)
    let name () = "race"
    let access x = `Left x
    let vars x = `Right x
    let is_write_only _ = true
  end

  module V0Set = SetDomain.Make (V0)
  module G =
  struct
    include Lattice.Lift2 (Access.AS) (V0Set) (Printable.DefaultNames)

    let access = function
      | `Bot -> Access.AS.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "Race.access"
    let vars = function
      | `Bot -> V0Set.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "Race.vars"
    let create_access access = `Lifted1 access
    let create_vars vars = `Lifted2 vars
  end

  let safe       = ref 0
  let vulnerable = ref 0
  let unsafe     = ref 0

  let init marshal =
    safe := 0;
    vulnerable := 0;
    unsafe := 0

  let side_vars ctx lv_opt ty =
    match lv_opt with
    | Some (v, _) ->
      if !GU.should_warn then
        ctx.sideg (V.vars v) (G.create_vars (V0Set.singleton (lv_opt, ty)))
    | None ->
      ()

  let side_access ctx ty lv_opt (conf, w, loc, e, a) =
    let ty =
      if Option.is_some lv_opt then
        `Type Cil.voidType (* avoid unsound type split for alloc variables *)
      else
        ty
    in
    if !GU.should_warn then
      ctx.sideg (V.access (lv_opt, ty)) (G.create_access (Access.AS.singleton (conf, w, loc, e, a)));
    side_vars ctx lv_opt ty

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g' -> (* accesses *)
          (* ignore (Pretty.printf "WarnGlobal %a\n" CilType.Varinfo.pretty g); *)
          let accs = G.access (ctx.global g) in
          let (lv, ty) = g' in
          let mem_loc_str = GobPretty.sprint Access.d_memo (ty, lv) in
          Timing.wrap ~args:[("memory location", `String mem_loc_str)] "race" (Access.warn_global safe vulnerable unsafe g') accs
        | `Right _ -> (* vars *)
          ()
      end
    | IterSysVars (Global g, vf) ->
      V0Set.iter (fun v ->
          vf (Obj.repr (V.access v))
        ) (G.vars (ctx.global (V.vars g)))
    | _ -> Queries.Result.top q

  let event ctx e octx =
    match e with
    | Events.Access {exp=e; lvals; kind; reach} when ThreadFlag.is_currently_multi (Analyses.ask_of_ctx ctx) -> (* threadflag query in post-threadspawn ctx *)
      (* must use original (pre-assign, etc) ctx queries *)
      let conf = 110 in
      let module LS = Queries.LS in
      let part_access (vo:varinfo option) (oo: offset option): MCPAccess.A.t =
        (*partitions & locks*)
        Obj.obj (octx.ask (PartAccess (Memory {exp=e; var_opt=vo; kind})))
      in
      let add_access conf vo oo =
        let a = part_access vo oo in
        Access.add (side_access octx) e kind conf vo oo a;
      in
      let add_access_struct conf ci =
        let a = part_access None None in
        Access.add_struct (side_access octx) e kind conf (`Struct (ci,`NoOffset)) None a
      in
      let has_escaped g = octx.ask (Queries.MayEscape g) in
      (* The following function adds accesses to the lval-set ls
         -- this is the common case if we have a sound points-to set. *)
      let on_lvals ls includes_uk =
        let ls = LS.filter (fun (g,_) -> g.vglob || has_escaped g) ls in
        let conf = if reach then conf - 20 else conf in
        let conf = if includes_uk then conf - 10 else conf in
        let f (var, offs) =
          let coffs = Lval.CilLval.to_ciloffs offs in
          if CilType.Varinfo.equal var dummyFunDec.svar then
            add_access conf None (Some coffs)
          else
            add_access conf (Some var) (Some coffs)
        in
        LS.iter f ls
      in
      begin match lvals with
        | ls when not (LS.is_top ls) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
          (* the case where the points-to set is non top and does not contain unknown values *)
          on_lvals ls false
        | ls when not (LS.is_top ls) ->
          (* the case where the points-to set is non top and contains unknown values *)
          let includes_uk = ref false in
          (* now we need to access all fields that might be pointed to: is this correct? *)
          begin match octx.ask (ReachableUkTypes e) with
            | ts when Queries.TS.is_top ts ->
              includes_uk := true
            | ts ->
              if Queries.TS.is_empty ts = false then
                includes_uk := true;
              let f = function
                | TComp (ci, _) ->
                  add_access_struct (conf - 50) ci
                | _ -> ()
              in
              Queries.TS.iter f ts
          end;
          on_lvals ls !includes_uk
        | _ ->
          add_access (conf - 60) None None
      end;
      ctx.local
    | _ ->
      ctx.local

  let finalize () =
    let total = !safe + !unsafe + !vulnerable in
    if total > 0 then (
      M.msg_group Info ~category:Race "Memory locations race summary" [
        (Pretty.dprintf "safe: %d" !safe, None);
        (Pretty.dprintf "vulnerable: %d" !vulnerable, None);
        (Pretty.dprintf "unsafe: %d" !unsafe, None);
        (Pretty.dprintf "total memory locations: %d" total, None);
      ];
    )
end

let _ =
  MCP.register_analysis ~dep:["access"] (module Spec : MCPSpec)
