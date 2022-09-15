(** Unassume analysis. *)
open Analyses

module Cil = GoblintCil.Cil

module NH = CfgTools.NH
module FH = Hashtbl.Make (CilType.Fundec)
module EH = Hashtbl.Make (CilType.Exp)

module Spec =
struct
  include Analyses.IdentitySpec
  let name () = "unassume"

  module C = Printable.Unit
  module D = SetDomain.Make (CilType.Exp)

  let startstate _ = D.empty ()
  let morphstate _ _ = D.empty ()
  let exitstate _ = D.empty ()

  let context _ _ = ()
  let should_join _ _ = false

  module Locator = WitnessUtil.Locator (Node)

  let locator: Locator.t ref = ref (Locator.create ()) (* empty default, so don't have to use option everywhere *)

  type inv = {
    exp: Cil.exp;
    uuid: string;
  }

  let invs: inv NH.t = NH.create 100

  let fun_pres: Cil.exp FH.t = FH.create 100
  let pre_invs: inv EH.t NH.t = NH.create 100

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
    FH.clear fun_pres;
    NH.clear pre_invs;

    let unassume_entry (entry: YamlWitnessType.Entry.t) =
      let uuid = entry.metadata.uuid in

      let unassume_nodes_invariant ~loc ~nodes inv =
        let msgLoc: M.Location.t = CilLocation loc in
        match InvariantParser.parse_cabs inv with
        | Ok inv_cabs ->

          Locator.ES.iter (fun n ->
              let fundec = Node.find_fundec n in

              match InvariantParser.parse_cil inv_parser ~fundec ~loc inv_cabs with
              | Ok inv_exp ->
                M.debug ~category:Witness ~loc:msgLoc "located invariant to %a: %a" Node.pretty n Cil.d_exp inv_exp;
                NH.add invs n {exp = inv_exp; uuid}
              | Error e ->
                M.error ~category:Witness ~loc:msgLoc "CIL couldn't parse invariant: %s" inv;
                M.info ~category:Witness ~loc:msgLoc "invariant has undefined variables or side effects: %s" inv
            ) nodes;

        | Error e ->
          M.error ~category:Witness ~loc:msgLoc "Frontc couldn't parse invariant: %s" inv;
          M.info ~category:Witness ~loc:msgLoc "invariant has invalid syntax: %s" inv
      in

      let unassume_loop_invariant (loop_invariant: YamlWitnessType.LoopInvariant.t) =
        let loc = loc_of_location loop_invariant.location in
        let inv = loop_invariant.loop_invariant.string in
        let msgLoc: M.Location.t = CilLocation loc in

        match Locator.find_opt !locator loc with
        | Some nodes ->
          unassume_nodes_invariant ~loc ~nodes inv
        | None ->
          M.warn ~category:Witness ~loc:msgLoc "couldn't locate invariant: %s" inv
      in

      let unassume_precondition_nodes_invariant ~loc ~nodes pre inv =
        let msgLoc: M.Location.t = CilLocation loc in
        match InvariantParser.parse_cabs pre, InvariantParser.parse_cabs inv with
        | Ok pre_cabs, Ok inv_cabs ->

          Locator.ES.iter (fun n ->
              let fundec = Node.find_fundec n in

              match InvariantParser.parse_cil inv_parser ~fundec ~loc pre_cabs with
              | Ok pre_exp ->
                M.debug ~category:Witness ~loc:msgLoc "located precondition to %a: %a" CilType.Fundec.pretty fundec Cil.d_exp pre_exp;
                FH.add fun_pres fundec pre_exp;

                begin match InvariantParser.parse_cil inv_parser ~fundec ~loc inv_cabs with
                  | Ok inv_exp ->
                    M.debug ~category:Witness ~loc:msgLoc "located invariant to %a: %a" Node.pretty n Cil.d_exp inv_exp;
                    if not (NH.mem pre_invs n) then
                      NH.replace pre_invs n (EH.create 10);
                    EH.add (NH.find pre_invs n) pre_exp {exp = inv_exp; uuid}
                  | Error e ->
                    M.error ~category:Witness ~loc:msgLoc "CIL couldn't parse invariant: %s" inv;
                    M.info ~category:Witness ~loc:msgLoc "invariant has undefined variables or side effects: %s" inv
                end

              | Error e ->
                M.error ~category:Witness ~loc:msgLoc "CIL couldn't parse precondition: %s" pre;
                M.info ~category:Witness ~loc:msgLoc "precondition has undefined variables or side effects: %s" pre
            ) nodes;

        | Error e, _ ->
          M.error ~category:Witness ~loc:msgLoc "Frontc couldn't parse precondition: %s" pre;
          M.info ~category:Witness ~loc:msgLoc "precondition has invalid syntax: %s" pre

        | _, Error e ->
          M.error ~category:Witness ~loc:msgLoc "Frontc couldn't parse invariant: %s" inv;
          M.info ~category:Witness ~loc:msgLoc "invariant has invalid syntax: %s" inv
      in

      let unassume_precondition_loop_invariant (precondition_loop_invariant: YamlWitnessType.PreconditionLoopInvariant.t) =
        let loc = loc_of_location precondition_loop_invariant.location in
        let pre = precondition_loop_invariant.precondition.string in
        let inv = precondition_loop_invariant.loop_invariant.string in
        let msgLoc: M.Location.t = CilLocation loc in

        match Locator.find_opt !locator loc with
        | Some nodes ->
          unassume_precondition_nodes_invariant ~loc ~nodes pre inv
        | None ->
          M.warn ~category:Witness ~loc:msgLoc "couldn't locate invariant: %s" inv
      in

      match entry.entry_type with
      | LoopInvariant x ->
        unassume_loop_invariant x
      | PreconditionLoopInvariant x ->
        unassume_precondition_loop_invariant x
      | entry_type ->
        M.info_noloc ~category:Witness "cannot unassume entry of type %s" (YamlWitnessType.EntryType.entry_type entry_type)
    in

    List.iter (fun yaml_entry ->
        match YamlWitnessType.Entry.of_yaml yaml_entry with
        | Ok entry -> unassume_entry entry
        | Error (`Msg e) -> M.info_noloc ~category:Witness "couldn't parse entry: %s" e
      ) yaml_entries

  let emit_unassume ctx =
    let es = NH.find_all invs ctx.node in
    let es = D.fold (fun pre acc ->
        match NH.find_option pre_invs ctx.node with
        | Some eh -> EH.find_all eh pre @ acc
        | None -> acc
      ) ctx.local es
    in
    begin match es with
      | x :: xs ->
        let e = List.fold_left (fun a {exp = b; _} -> Cil.(BinOp (LAnd, a, b, intType))) x.exp xs in
        let uuids = x.uuid :: List.map (fun {uuid; _} -> uuid) xs in
        ctx.emit (Unassume {exp = e; uuids});
        List.iter WideningTokens.perform uuids
      | [] ->
        ()
    end;
    ctx.local

  let assign ctx lv e =
    emit_unassume ctx

  let branch ctx e tv =
    emit_unassume ctx

  let body ctx fd =
    let pres = FH.find_all fun_pres fd in
    let st = List.fold_left (fun acc pre ->
        let v = ctx.ask (EvalInt pre) in
        (* M.debug ~category:Witness "%a precondition %a evaluated to %a" CilType.Fundec.pretty fd CilType.Exp.pretty pre Queries.ID.pretty v; *)
        if Queries.ID.to_bool v = Some true then
          D.add pre acc
        else
          acc
      ) (D.empty ()) pres
    in

    emit_unassume {ctx with local = st} (* doesn't query, so no need to redefine ask *)

  let asm ctx =
    emit_unassume ctx

  let skip ctx =
    emit_unassume ctx

  let special ctx lv f args =
    emit_unassume ctx

  let enter ctx lv f args =
    [(ctx.local, D.empty ())]

  let combine ctx lv fe f args fc fd =
    emit_unassume ctx

  (* not in sync, query, entry, threadenter because they aren't final transfer function on edge *)
  (* not in vdecl, return, threadspawn because unnecessary targets for invariants? *)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
