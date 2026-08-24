(** Unassume analysis ([unassume]).

    Emits unassume events for other analyses based on YAML witness invariants. *)

open Analyses

module Cil = GoblintCil.Cil

module NH = CfgTools.NH

module Spec =
struct
  include UnitAnalysis.Spec
  let name () = "unassume"

  module Locator = WitnessUtil.Locator (Node)

  let location_locator: Locator.t = Locator.create () (* empty default, so don't have to use option everywhere *)
  let loop_locator: Locator.t = Locator.create () (* empty default, so don't have to use option everywhere *)

  type inv = {
    exp: Cil.exp;
    token: WideningToken.t;
  }

  let invs: inv NH.t = NH.create 100

  let init _ =
    Locator.clear location_locator;
    Locator.clear loop_locator;
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
        Option.iter (fun loc ->
            Locator.add loop_locator loc node
          ) (WitnessInvariant.loop_location node);
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

    let yaml = match GobResult.Syntax.(Fpath.of_string (GobConfig.get_string "witness.yaml.unassume") >>= Yaml_unix.of_file) with
      | Ok yaml -> yaml
      | Error (`Msg m) ->
        Logs.error "Yaml_unix.of_file: %s" m;
        Svcomp.errorwith "witness missing"
    in
    let yaml_entries = yaml |> GobYaml.list |> BatResult.get_ok in

    let module InvariantParser = WitnessUtil.InvariantParser in
    let inv_parser = InvariantParser.create !Cilfacade.current_file in

    NH.clear invs;

    let unassume_entry (entry: YamlWitnessType.Entry.t) =
      let uuid = entry.metadata.uuid in
      let target_type = YamlWitnessType.EntryType.entry_type entry.entry_type in

      let unassume_nodes_invariant ~loc ~nodes ?i inv =
        let msgLoc: M.Location.t = CilLocation loc in
        match InvariantParser.parse_cabs inv with
        | Ok inv_cabs ->

          Locator.ES.iter (fun n ->
              let fundec = Node.find_fundec n in

              match InvariantParser.parse_cil inv_parser ~check:false ~fundec ~loc inv_cabs with
              | Ok inv_exp ->
                M.debug ~category:Witness ~loc:msgLoc "located invariant to %a: %a" Node.pretty n Cil.d_exp inv_exp;
                NH.add invs n {exp = inv_exp; token = (uuid, i)}
              | Error e ->
                M.error ~category:Witness ~loc:msgLoc "CIL couldn't parse invariant: %s" inv;
                M.info ~category:Witness ~loc:msgLoc "invariant has undefined variables or side effects: %s" inv
            ) nodes;

        | Error e ->
          M.error ~category:Witness ~loc:msgLoc "Frontc couldn't parse invariant: %s" inv;
          M.info ~category:Witness ~loc:msgLoc "invariant has invalid syntax: %s" inv
      in

      let unassume_invariant_set (invariant_set: YamlWitnessType.InvariantSet.t) =

        let unassume_location_invariant ~i (location_invariant: YamlWitnessType.InvariantSet.LocationInvariant.t) =
          let loc = YamlWitness.loc_of_location location_invariant.location in
          let inv = location_invariant.value in
          let msgLoc: M.Location.t = CilLocation loc in

          match Locator.find_opt location_locator loc with
          | Some nodes ->
            unassume_nodes_invariant ~loc ~nodes ~i inv
          | None ->
            M.warn ~category:Witness ~loc:msgLoc "couldn't locate invariant: %s" inv
        in

        let unassume_loop_invariant ~i (loop_invariant: YamlWitnessType.InvariantSet.LoopInvariant.t) =
          let loc = YamlWitness.loc_of_location loop_invariant.location in
          let inv = loop_invariant.value in
          let msgLoc: M.Location.t = CilLocation loc in

          match Locator.find_opt loop_locator loc with
          | Some nodes ->
            unassume_nodes_invariant ~loc ~nodes ~i inv
          | None ->
            M.warn ~category:Witness ~loc:msgLoc "couldn't locate invariant: %s" inv
        in

        let validate_invariant i (invariant: YamlWitnessType.InvariantSet.Invariant.t) =
          let target_type = YamlWitnessType.InvariantSet.InvariantType.invariant_type invariant.invariant_type in
          match YamlWitness.invariant_type_enabled target_type, invariant.invariant_type with
          | true, LocationInvariant ({labels = (None | Some []); _} as x) ->
            unassume_location_invariant ~i x
          | true, LoopInvariant ({labels = (None | Some []); _} as x) ->
            unassume_loop_invariant ~i x
          | false, (LocationInvariant _ | LoopInvariant _) ->
            M.info_noloc ~category:Witness "disabled invariant of type %s" target_type
          | _ ->
            M.warn_noloc ~category:Witness "cannot unassume invariant of type %s" target_type
        in

        let validate_invariant_kind i (invariant_kind: YamlWitnessType.InvariantSet.InvariantKind.t) =
          let target_type = YamlWitnessType.InvariantSet.InvariantKind.invariant_kind invariant_kind in
          match invariant_kind with
          | Invariant x ->
            validate_invariant i x
          | _ ->
            M.warn_noloc ~category:Witness "cannot validate invariant of kind %s" target_type
        in

        List.iteri validate_invariant_kind invariant_set.content
      in

      match YamlWitness.entry_type_enabled target_type, entry.entry_type with
      | true, InvariantSet x ->
        unassume_invariant_set x
      | false, InvariantSet _ ->
        M.info_noloc ~category:Witness "disabled entry of type %s" target_type
      | _ ->
        M.warn_noloc ~category:Witness "cannot unassume entry of type %s" target_type
    in

    List.iter (fun yaml_entry ->
        match YamlWitnessType.Entry.of_yaml yaml_entry with
        | Ok entry -> unassume_entry entry
        | Error (`Msg e) -> M.error_noloc ~category:Witness "couldn't parse entry: %s" e
      ) yaml_entries

  let emit_unassume man =
    let es = NH.find_all invs man.node in
    match es with
    | x :: xs ->
      let e = List.fold_left (fun a {exp = b; _} -> Cil.(BinOp (LAnd, a, b, intType))) x.exp xs in
      M.info ~category:Witness "unassume invariant: %a" CilType.Exp.pretty e;
      if not !AnalysisState.postsolving then (
        if not (GobConfig.get_bool "ana.unassume.precheck" && Queries.eval_bool (Analyses.ask_of_man man) e = `Lifted false) then (
          let tokens = x.token :: List.map (fun {token; _} -> token) xs in
          man.emit (Unassume {exp = e; tokens});
          List.iter WideningTokenLifter.add tokens
        )
      )
    | [] ->
      ()

  let assign man lv e =
    emit_unassume man

  let branch man e tv =
    emit_unassume man

  let body man fd =
    emit_unassume man

  let asm man =
    emit_unassume man

  let skip man =
    emit_unassume man

  let special man lv f args =
    emit_unassume man

  let combine_env man lval fexp f args fc au f_ask =
    man.local (* not here because isn't final transfer function on edge *)

  let combine_assign man lv fe f args fc fd f_ask =
    emit_unassume man

  (* not in sync, query, entry, threadenter because they aren't final transfer function on edge *)
  (* not in vdecl, return, threadspawn because unnecessary targets for invariants? *)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
