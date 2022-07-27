(** Unassume analysis. *)
open Analyses

module NH = CfgTools.NH

module Spec =
struct
  include UnitAnalysis.Spec
  let name () = "unassume"

  module Locator = WitnessUtil.Locator (Node)

  let locator: Locator.t ref = ref (Locator.create ()) (* empty default, so don't have to use option everywhere *)

  let invs: Cil.exp NH.t = NH.create 100

  let init _ =
    locator := Locator.create (); (* TODO: add Locator.clear *)
    let module Cfg = (val !MyCFG.current_cfg) in

    (* DFS, copied from CfgTools.find_backwards_reachable *)
    let reachable = NH.create 100 in
    let rec iter_node node =
      if not (NH.mem reachable node) then begin
        NH.replace reachable node ();
        Locator.add !locator (Node.location node) node;
        List.iter (fun (_, prev_node) ->
            iter_node prev_node
          ) (Cfg.prev node)
      end
    in

    Cil.iterGlobals !Cilfacade.current_file (function
        | GFun (fd, _) ->
          let return_node = Node.Function fd in
          iter_node return_node
        | _ -> ()
      );

    let loc_of_location (location: YamlWitnessType.Location.t): Cil.location = {
        file = location.file_name;
        line = location.line;
        column = location.column + 1;
        byte = -1;
        endLine = -1;
        endColumn = -1;
        endByte = -1;
        synthetic = false;
      }
    in

    let yaml = Yaml_unix.of_file_exn (Fpath.v (GobConfig.get_string "witness.yaml.unassume")) in
    let yaml_entries = yaml |> GobYaml.list |> BatResult.get_ok in

    let module InvariantParser = WitnessUtil.InvariantParser in
    let inv_parser = InvariantParser.create !Cilfacade.current_file in

    NH.clear invs;

    let unassume_entry (entry: YamlWitnessType.Entry.t) =

      let unassume_nodes_invariant ~loc ~nodes inv =
        match InvariantParser.parse_cabs inv with
        | Ok inv_cabs ->

          Locator.ES.iter (fun n ->
              let fundec = Node.find_fundec n in

              match InvariantParser.parse_cil inv_parser ~fundec ~loc inv_cabs with
              | Ok inv_exp ->
                M.debug ~category:Witness ~loc "located invariant to %a: %a" Node.pretty n Cil.d_exp inv_exp;
                NH.add invs n inv_exp
              | Error e ->
                M.error ~category:Witness ~loc "CIL couldn't parse invariant: %s" inv;
                M.info ~category:Witness ~loc "invariant has undefined variables or side effects: %s" inv
            ) nodes;

        | Error e ->
          M.error ~category:Witness ~loc "Frontc couldn't parse invariant: %s" inv;
          M.info ~category:Witness ~loc "invariant has invalid syntax: %s" inv
      in

      let unassume_loop_invariant (loop_invariant: YamlWitnessType.LoopInvariant.t) =
        let loc = loc_of_location loop_invariant.location in
        let inv = loop_invariant.loop_invariant.string in

        match Locator.find_opt !locator loc with
        | Some nodes ->
          unassume_nodes_invariant ~loc ~nodes inv
        | None ->
          M.warn ~category:Witness ~loc "couldn't locate invariant: %s" inv
      in

      match entry.entry_type with
      | LoopInvariant x ->
        unassume_loop_invariant x
      | entry_type ->
        M.info_noloc ~category:Witness "cannot unassume entry of type %s" (YamlWitnessType.EntryType.entry_type entry_type)
    in

    List.iter (fun yaml_entry ->
        match YamlWitnessType.Entry.of_yaml yaml_entry with
        | Ok entry -> unassume_entry entry
        | Error (`Msg e) -> M.info_noloc ~category:Witness "couldn't parse entry: %s" e
      ) yaml_entries

  let emit_unassume ctx =
    match NH.find_all invs ctx.node with
    | x :: xs ->
      let e = List.fold_left (fun a b -> Cil.(BinOp (LAnd, a, b, intType))) x xs in
      ctx.emit (Unassume e)
    | [] ->
      ()

  let assign ctx lv e =
    emit_unassume ctx

  let branch ctx e tv =
    emit_unassume ctx

  let body ctx fd =
    emit_unassume ctx

  let asm ctx =
    emit_unassume ctx

  let skip ctx =
    emit_unassume ctx

  let special ctx lv f args =
    emit_unassume ctx

  let combine ctx lv fe f args fc fd =
    emit_unassume ctx

  (* not in sync, query, entry, threadenter because they aren't final transfer function on edge *)
  (* not in vdecl, return, threadspawn because unnecessary targets for invariants? *)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
