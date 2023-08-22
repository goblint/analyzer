(** Data race analysis ([race]). *)

open GoblintCil
open Analyses


(** Data race analyzer without base --- this is the new standard *)
module Spec =
struct
  include UnitAnalysis.Spec

  let name () = "race"

  (* Two global invariants:
     1. memoroot -> (offset -> accesses)  --  used for warnings
     2. varinfo -> set of memo  --  used for IterSysVars Global *)

  module V =
  struct
    include Printable.Either (Access.MemoRoot) (CilType.Varinfo)
    let name () = "race"
    let access x = `Left x
    let vars x = `Right x
    let is_write_only _ = true
  end

  module MemoSet = SetDomain.Make (Access.Memo)

  module OneOffset =
  struct
    include Printable.StdLeaf
    type t =
      | Field of CilType.Fieldinfo.t
      | Index
    [@@deriving eq, ord, hash, to_yojson]

    let name () = "oneoffset"

    let show = function
      | Field f -> CilType.Fieldinfo.show f
      | Index -> "?"

    include Printable.SimpleShow (struct
        type nonrec t = t
        let show = show
      end)

    let to_offset : t -> Offset.Unit.t = function
      | Field f -> `Field (f, `NoOffset)
      | Index -> `Index ((), `NoOffset)
  end

  module OffsetTrie =
  struct
    include TrieDomain.Make (OneOffset) (Access.AS)

    let rec singleton (offset : Offset.Unit.t) (value : value) : t =
      match offset with
      | `NoOffset -> (value, ChildMap.empty ())
      | `Field (f, offset') -> (Access.AS.empty (), ChildMap.singleton (Field f) (singleton offset' value))
      | `Index ((), offset') -> (Access.AS.empty (), ChildMap.singleton Index (singleton offset' value))
  end

  module G =
  struct
    include Lattice.Lift2 (OffsetTrie) (MemoSet) (Printable.DefaultNames)

    let access = function
      | `Bot -> OffsetTrie.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "Race.access"
    let vars = function
      | `Bot -> MemoSet.bot ()
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

  let side_vars ctx memo =
    match memo with
    | (`Var v, _) ->
      if !AnalysisState.should_warn then
        ctx.sideg (V.vars v) (G.create_vars (MemoSet.singleton memo))
    | _ ->
      ()

  let side_access ctx (conf, w, loc, e, a) ((memoroot, offset) as memo) =
    if !AnalysisState.should_warn then
      ctx.sideg (V.access memoroot) (G.create_access (OffsetTrie.singleton offset (Access.AS.singleton (conf, w, loc, e, a))));
    side_vars ctx memo

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g' -> (* accesses *)
          (* ignore (Pretty.printf "WarnGlobal %a\n" Access.MemoRoot.pretty g'); *)
          let trie = G.access (ctx.global g) in
          (** Distribute access to contained fields. *)
          let rec distribute_inner offset (accs, children) ancestor_accs =
            let ancestor_accs' = Access.AS.union ancestor_accs accs in
            OffsetTrie.ChildMap.iter (fun child_key child_trie ->
                distribute_inner (Offset.Unit.add_offset offset (OneOffset.to_offset child_key)) child_trie ancestor_accs'
              ) children;
            if not (Access.AS.is_empty accs) then (
              let memo = (g', offset) in
              let mem_loc_str = GobPretty.sprint Access.Memo.pretty memo in
              Timing.wrap ~args:[("memory location", `String mem_loc_str)] "race" (Access.warn_global ~safe ~vulnerable ~unsafe ~ancestor_accs memo) accs
            )
          in
          distribute_inner `NoOffset trie (Access.AS.empty ())
        | `Right _ -> (* vars *)
          ()
      end
    | IterSysVars (Global g, vf) ->
      MemoSet.iter (fun v ->
          vf (Obj.repr (V.access v))
        ) (G.vars (ctx.global (V.vars g)))
    | _ -> Queries.Result.top q

  let event ctx e octx =
    match e with
    | Events.Access {exp=e; lvals; kind; reach} when ThreadFlag.is_currently_multi (Analyses.ask_of_ctx ctx) -> (* threadflag query in post-threadspawn ctx *)
      (* must use original (pre-assign, etc) ctx queries *)
      let conf = 110 in
      let module AD = Queries.AD in
      let part_access (vo:varinfo option): MCPAccess.A.t =
        (*partitions & locks*)
        Obj.obj (octx.ask (PartAccess (Memory {exp=e; var_opt=vo; kind})))
      in
      let loc = Option.get !Node.current_node in
      let add_access conf voffs =
        let a = part_access (Option.map fst voffs) in
        Access.add (side_access octx (conf, kind, loc, e, a)) e voffs;
      in
      let add_access_struct conf ci =
        let a = part_access None in
        Access.add_one (side_access octx (conf, kind, loc, e, a)) (`Type (TComp (ci, [])), `NoOffset)
      in
      let has_escaped g = octx.ask (Queries.MayEscape g) in
      (* The following function adds accesses to the lval-set ls
         -- this is the common case if we have a sound points-to set. *)
      let on_lvals ad includes_uk =
        let conf = if reach then conf - 20 else conf in
        let conf = if includes_uk then conf - 10 else conf in
        let f addr =
          match addr with
          | AD.Addr.Addr (g,o) when g.vglob || has_escaped g ->
            let coffs = Offset.Exp.to_cil (ValueDomain.Offs.to_exp o) in
            add_access conf (Some (g, coffs))
          | UnknownPtr -> add_access conf None
          | _ -> () 
        in
        AD.iter f ad
      in
      begin match lvals with
        | ad when not (AD.is_top ad) ->
          (* the case where the points-to set is non top and does not contain unknown values *)
          on_lvals ad false
        | ad ->
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
          on_lvals ad !includes_uk
          (* | _ ->
             add_access (conf - 60) None *)
      end;
      ctx.local
    | _ ->
      ctx.local

  let special ctx (lvalOpt: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* perform shallow and deep invalidate according to Library descriptors *)
    let desc = LibraryFunctions.find f in
    if List.mem LibraryDesc.ThreadUnsafe desc.attrs then (
      let e = Lval (Var f, NoOffset) in
      let conf = 110 in
      let loc = Option.get !Node.current_node in
      let vo = Some f in
      let a = Obj.obj (ctx.ask (PartAccess (Memory {exp=e; var_opt=vo; kind=Call}))) in
      side_access ctx (conf, Call, loc, e, a) ((`Var f), `NoOffset) ;
    );
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
