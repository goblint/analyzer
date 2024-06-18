open Analyses

module Cil = GoblintCil.Cil

module NH = CfgTools.NH
module FH = Hashtbl.Make (CilType.Fundec)
module EH = Hashtbl.Make (CilType.Exp)

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "violationSequence"

  module ChainParams =
  struct
    (* let n = List.length Arg.path *)
    let n () = -1
    let names x = "segment " ^ string_of_int x
  end
  module CC =
  struct
    include Printable.Chain (ChainParams)
    let join _ _ = assert false
  end
  module D = Lattice.Flat (CC)
  include Analyses.ValueContexts (D)
  module P = IdentityP (D) (* fully path-sensitive *)

  let startstate _ = `Lifted 0
  let morphstate _ _ = `Lifted 0
  let exitstate _ = `Lifted 0

  module Locator = WitnessUtil.Locator (Node)

  let location_locator: Locator.t = Locator.create () (* empty default, so don't have to use option everywhere *)

  let segments: YamlWitnessType.ViolationSequence.Segment.t list ref = ref []

  let init _ =
    Locator.clear location_locator;
    let module FileCfg =
    struct
      let file = !Cilfacade.current_file
      module Cfg = (val !MyCFG.current_cfg)
    end  in
    let module WitnessInvariant = WitnessUtil.YamlInvariant (FileCfg) in

    (* DFS, copied from CfgTools.find_backwards_reachable *)
    let reachable = NH.create 100 in
    let rec iter_node node =
      if not (NH.mem reachable node) then begin
        NH.replace reachable node ();
        Option.iter (fun loc ->
            Locator.add location_locator loc node
          ) (WitnessInvariant.location_location node);
        List.iter (fun (_, prev_node) ->
            iter_node prev_node
          ) (FileCfg.Cfg.prev node)
      end
    in

    Cil.iterGlobals !Cilfacade.current_file (function
        | GFun (fd, _) ->
          let return_node = Node.Function fd in
          iter_node return_node
        | _ -> ()
      );

    let yaml = match Yaml_unix.of_file (Fpath.v (GobConfig.get_string "witness.yaml.observe")) with
      | Ok yaml -> yaml
      | Error (`Msg m) -> failwith ("Yaml_unix.of_file: " ^ m)
    in
    let yaml_entries = yaml |> GobYaml.list |> BatResult.get_ok in

    (* let module InvariantParser = WitnessUtil.InvariantParser in
    let inv_parser = InvariantParser.create !Cilfacade.current_file in *)

    segments := [];

    let observe_entry (entry: YamlWitnessType.Entry.t) =
      let target_type = YamlWitnessType.EntryType.entry_type entry.entry_type in

      match YamlWitness.entry_type_enabled target_type, entry.entry_type with
      | true, ViolationSequence x ->
        segments := x.content
        (* TODO: error on 0 or 2+ violation sequences *)
      | false, (ViolationSequence _) ->
        M.info_noloc ~category:Witness "disabled entry of type %s" target_type
      | _ ->
        M.info_noloc ~category:Witness "cannot observe entry of type %s" target_type
    in

    List.iter (fun yaml_entry ->
        match YamlWitnessType.Entry.of_yaml yaml_entry with
        | Ok entry -> observe_entry entry
        | Error (`Msg e) -> M.info_noloc ~category:Witness "couldn't parse entry: %s" e
      ) yaml_entries

  let step_ctx ctx =
    match ctx.local with
    | `Lifted i when not !AnalysisState.global_initialization ->
      let segment = List.nth !segments i in
      Logs.debug "%d" (List.length !segments);
      let waypoints = segment.segment in
      (* ctx.split (`Lifted i) []; *)
      if i + 1 < List.length !segments then (
        ctx.split (`Lifted (i + 1)) [];
        Logs.debug "splitting %d" (i + 1)
      );
      (* raise Analyses.Deadcode *)
      ctx.local
    | _ ->
      ctx.local

  let assign ctx lv e =
    step_ctx ctx

  let branch ctx e tv =
    step_ctx ctx

  let body ctx fd =
    ctx.local (* TODO *)

  let asm ctx =
    step_ctx ctx

  let skip ctx =
    step_ctx ctx

  let special ctx lv f args =
    step_ctx ctx

  let enter ctx lv f args =
    [(ctx.local, ctx.local)] (* TODO: Step *)

  let combine_env ctx lval fexp f args fc au f_ask =
    ctx.local (* not here because isn't final transfer function on edge *)

  let combine_assign ctx lv fe f args fc fd f_ask =
    (* step_ctx ctx *)
    ctx.local (* TODO: *)

  let return ctx exp f =
    step_ctx ctx

  let threadenter ctx ~multiple lval f args = failwith "TODO"
  let threadspawn ctx ~multiple lval f args fctx = failwith "TODO"

  (* not in sync, query, entry because they aren't final transfer function on edge *)
  (* not in vdecl because unnecessary targets for invariants? *)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
