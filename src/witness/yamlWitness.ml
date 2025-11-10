(** YAML witness generation and validation. *)

open Analyses
open GoblintCil

let uuid_random_state = Random.State.make_self_init ()

let sha256_file f = try Sha256.(to_hex (file f)) with Sys_error _ -> ""
let sha256_file_cache = BatCache.make_ht ~gen:sha256_file ~init_size:5
let sha256_file = sha256_file_cache.get

module Entry =
struct
  open YamlWitnessType

  (* yaml_conf is too verbose *)
  (* let yaml_conf: Yaml.value = Json_repr.convert (module Json_repr.Yojson) (module Json_repr.Ezjsonm) (!GobConfig.json_conf) in *)
  let producer: Producer.t = {
    name = "Goblint";
    version = Goblint_build_info.version;
    command_line = Some GobSys.command_line;
  }

  let metadata ~format_version ?task (): Metadata.t =
    let uuid = Uuidm.v4_gen uuid_random_state () in
    let conf_format_version = YamlWitnessVersion.of_option () in
    if YamlWitnessVersion.compare format_version conf_format_version > 0 then
      M.warn_noloc ~category:Witness "witness entry version (%a) exceeds configured witness.yaml.format-version (%a)" YamlWitnessVersion.pretty format_version YamlWitnessVersion.pretty conf_format_version;
    let creation_time = TimeUtil.iso8601_now () in
    {
      format_version = YamlWitnessVersion.show format_version;
      uuid = Uuidm.to_string uuid;
      creation_time;
      producer;
      task
    }

  let with_metadata ~task entry_type: Entry.t = {
    entry_type;
    metadata = metadata ~format_version:(EntryType.min_version entry_type) ~task ();
  }

  let task ~input_files ~data_model ~(specification): Task.t =
    {
      input_files;
      input_file_hashes = List.map (fun file ->
          (file, sha256_file file)
        ) input_files;
      data_model;
      language = "C";
      specification
    }

  let location ~location:(loc: Cil.location) ~(location_function): Location.t =
    {
      file_name = loc.file;
      file_hash = None;
      line = loc.line;
      column = Some loc.column;
      function_ = Some location_function;
    }

  let location_invariant ~location ~(invariant): InvariantSet.InvariantKind.t =
    Invariant {
      invariant_type = LocationInvariant {
          location;
          value = invariant;
          format = "c_expression";
          labels = None;
        };
    }

  let loop_invariant ~location ~(invariant): InvariantSet.InvariantKind.t =
    Invariant {
      invariant_type = LoopInvariant {
          location;
          value = invariant;
          format = "c_expression";
          labels = None;
        };
    }

  (* non-standard extension *)
  let flow_insensitive_invariant ~(invariant): InvariantSet.InvariantKind.t =
    Invariant {
      invariant_type = FlowInsensitiveInvariant {
          value = invariant;
          format = "c_expression";
        };
    }

  let invariant_set ~task ~(invariants): Entry.t =
    with_metadata ~task @@ InvariantSet {
      content = invariants;
    }

  let ghost_variable' ~variable ~type_ ~(initial): GhostInstrumentation.Variable.t = {
    name = variable;
    scope = "global";
    type_;
    initial = {
      value = initial;
      format = "c_expression";
    };
  }

  let ghost_update' ~variable ~(expression): GhostInstrumentation.Update.t = {
    variable;
    value = expression;
    format = "c_expression";
  }

  let ghost_location_update' ~location ~(updates): GhostInstrumentation.LocationUpdate.t = {
    location;
    updates;
  }

  let ghost_instrumentation ~task ~variables ~(location_updates): Entry.t =
    with_metadata ~task @@ GhostInstrumentation {
      ghost_variables = variables;
      ghost_updates = location_updates;
    }
end

let yaml_entries_to_file yaml_entries file =
  let yaml = `A yaml_entries in
  (* Yaml_unix.to_file_exn file yaml *)
  (* to_file/to_string uses a fixed-size buffer... *)
  let text = match GobYaml.to_string' yaml with
    | Ok text -> text
    | Error (`Msg m) -> failwith ("Yaml.to_string: " ^ m)
  in
  Batteries.output_file ~filename:(Fpath.to_string file) ~text

let entry_type_enabled entry_type =
  List.mem entry_type (GobConfig.get_string_list "witness.yaml.entry-types")

let invariant_type_enabled invariant_type =
  List.mem invariant_type (GobConfig.get_string_list "witness.yaml.invariant-types")

module Make (R: ResultQuery.SpecSysSol2) =
struct
  open R
  open SpecSys

  module NH = BatHashtbl.Make (Node)
  module WitnessInvariant = WitnessUtil.YamlInvariant (FileCfg)
  module FMap = BatHashtbl.Make (CilType.Fundec)
  module FCMap = BatHashtbl.Make (Printable.Prod (CilType.Fundec) (Spec.C))
  type con_inv = {node: Node.t; context: Spec.C.t; invariant: Invariant.t; state: Spec.D.t}

  (* TODO: fix location hack *)
  module LH = BatHashtbl.Make (CilType.Location)
  let location_nodes: Node.t list LH.t Lazy.t = lazy (
    let lh = LH.create 113 in
    NH.iter (fun n _ ->
        Option.iter (fun loc ->
            LH.modify_def [] loc (List.cons n) lh
          ) (WitnessInvariant.location_location n)
      ) (Lazy.force nh);
    lh
  )
  let loop_nodes: Node.t list LH.t Lazy.t = lazy (
    let lh = LH.create 113 in
    NH.iter (fun n _ ->
        Option.iter (fun loc ->
            LH.modify_def [] loc (List.cons n) lh
          ) (WitnessInvariant.loop_location n)
      ) (Lazy.force nh);
    lh
  )

  let write () =
    let input_files = GobConfig.get_string_list "files" in
    let data_model = match GobConfig.get_string "exp.architecture" with
      | "64bit" -> "LP64"
      | "32bit" -> "ILP32"
      | _ -> failwith "invalid architecture"
    in
    let specification = Option.map (fun (module Task: Svcomp.Task) ->
        Svcomp.Specification.to_string Task.specification
      ) !Svcomp.task
    in
    let task = Entry.task ~input_files ~data_model ~specification in

    let local_lvals n local =
      if GobConfig.get_bool "witness.invariant.accessed" then (
        match R.ask_local_node n ~local MayAccessed with
        | `Top ->
          Lval.Set.top ()
        | (`Lifted _) as es ->
          let lvals = AccessDomain.EventSet.fold (fun e lvals ->
              match e with
              | {var_opt = Some var; offs_opt = Some offs; kind = Write} ->
                Lval.Set.add (Var var, offs) lvals
              | _ ->
                lvals
            ) es (Lval.Set.empty ())
          in
          let lvals =
            FileCfg.Cfg.next n
            |> List.to_seq
            |> Seq.filter_map (fun (_, next_n) ->
                match R.ask_local_node next_n MayAccessed with
                | `Top -> None
                | `Lifted _ as es -> Some es)
            |> Seq.fold_left AccessDomain.EventSet.union (AccessDomain.EventSet.empty ())
            |> fun es -> AccessDomain.EventSet.fold (fun e lvals ->
                match e with
                | {var_opt = Some var; offs_opt = Some offs; kind = Read} ->
                  Lval.Set.add (Var var, offs) lvals
                | _ ->
                  lvals
              ) es lvals
          in
          lvals
      )
      else
        Lval.Set.top ()
    in

    let entries = [] in

    let cnt_loop_invariant = ref 0 in
    let cnt_location_invariant = ref 0 in
    let cnt_flow_insensitive_invariant = ref 0 in

    let invariant_global_nodes = lazy (R.ask_global InvariantGlobalNodes) in

    let fold_flow_insensitive_as_location ~inv f acc =
      (* Currently same invariants (from InvariantGlobal query) for all nodes (from InvariantGlobalNodes query).
         The alternative would be querying InvariantGlobal per local unknown when looping over them to generate location invariants.
         See: https://github.com/goblint/analyzer/pull/1394#discussion_r1698149514. *)
      let invs = Invariant.Exp.process inv in
      Queries.NS.fold (fun n acc ->
          let fundec = Node.find_fundec n in
          match WitnessInvariant.location_location n with (* Not just using Node.location because it returns expression location which may be invalid for location invariant (e.g. inside conditional). *)
          | Some loc ->
            let location_function = fundec.svar.vname in
            let location = Entry.location ~location:loc ~location_function in
            List.fold_left (fun acc inv ->
                f ~location ~inv acc
              ) acc invs
          | None -> acc
        ) (Lazy.force invariant_global_nodes) acc
    in

    (* Generate flow-insensitive entries (ghost instrumentation) *)
    let entries =
      if entry_type_enabled YamlWitnessType.GhostInstrumentation.entry_type then (
        (* TODO: only at most one ghost_instrumentation entry can ever be produced, so this fold and deduplication is overkill *)
        let module EntrySet = Queries.YS in
        fst @@ GHT.fold (fun g v accs ->
            match g with
            | `Left g -> (* global unknown from analysis Spec *)
              begin match R.ask_global (YamlEntryGlobal (Obj.repr g, task)) with
                | `Lifted _ as inv ->
                  Queries.YS.fold (fun entry (acc, acc') ->
                      if EntrySet.mem entry acc' then (* deduplicate only with other global entries because local ones have different locations anyway *)
                        accs
                      else
                        (entry :: acc, EntrySet.add entry acc')
                    ) inv accs
                | `Top ->
                  accs
              end
            | `Right _ -> (* global unknown for FromSpec contexts *)
              accs
          ) gh (entries, EntrySet.empty ())
      )
      else
        entries
    in

    (* Generate invariant set *)
    let entries =
      if entry_type_enabled YamlWitnessType.InvariantSet.entry_type then (
        let invariants = [] in

        (* Generate location invariants *)
        let invariants =
          if invariant_type_enabled YamlWitnessType.InvariantSet.LocationInvariant.invariant_type then (
            LH.fold (fun loc ns acc ->
                let inv = List.fold_left (fun acc n ->
                    let local = try NH.find (Lazy.force nh) n with Not_found -> Spec.D.bot () in
                    let lvals = local_lvals n local in
                    Invariant.(acc || R.ask_local_node n ~local (Invariant {Invariant.default_context with lvals})) [@coverage off] (* bisect_ppx cannot handle redefined (||) *)
                  ) (Invariant.bot ()) ns
                in
                match inv with
                | `Lifted inv ->
                  let fundec = Node.find_fundec (List.hd ns) in (* TODO: fix location hack *)
                  let location_function = fundec.svar.vname in
                  let location = Entry.location ~location:loc ~location_function in
                  let invs = Invariant.Exp.process inv in
                  List.fold_left (fun acc inv ->
                      let invariant = Invariant.Exp.show inv in
                      let invariant = Entry.location_invariant ~location ~invariant in
                      incr cnt_location_invariant;
                      invariant :: acc
                    ) acc invs
                | `Bot | `Top -> (* TODO: 0 for bot (dead code)? *)
                  acc
              ) (Lazy.force location_nodes) invariants
          )
          else
            invariants
        in

        (* Generate loop invariants *)
        let invariants =
          if invariant_type_enabled YamlWitnessType.InvariantSet.LoopInvariant.invariant_type then (
            LH.fold (fun loc ns acc ->
                if WitnessInvariant.emit_loop_head then ( (* TODO: remove double condition? *)
                  let inv = List.fold_left (fun acc n ->
                      let local = try NH.find (Lazy.force nh) n with Not_found -> Spec.D.bot () in
                      Invariant.(acc || R.ask_local_node n ~local (Invariant Invariant.default_context)) [@coverage off] (* bisect_ppx cannot handle redefined (||) *)
                    ) (Invariant.bot ()) ns
                  in
                  match inv with
                  | `Lifted inv ->
                    let fundec = Node.find_fundec (List.hd ns) in (* TODO: fix location hack *)
                    let location_function = fundec.svar.vname in
                    let location = Entry.location ~location:loc ~location_function in
                    let invs = Invariant.Exp.process inv in
                    List.fold_left (fun acc inv ->
                        let invariant = Invariant.Exp.show inv in
                        let invariant = Entry.loop_invariant ~location ~invariant in
                        incr cnt_loop_invariant;
                        invariant :: acc
                      ) acc invs
                  | `Bot | `Top -> (* TODO: 0 for bot (dead code)? *)
                    acc
                )
                else
                  acc
              ) (Lazy.force loop_nodes) invariants
          )
          else
            invariants
        in

        (* Generate flow-insensitive invariants *)
        let invariants =
          if invariant_type_enabled YamlWitnessType.InvariantSet.FlowInsensitiveInvariant.invariant_type then (
            GHT.fold (fun g v acc ->
                match g with
                | `Left g -> (* global unknown from analysis Spec *)
                  begin match R.ask_global (InvariantGlobal (Obj.repr g)), GobConfig.get_string "witness.invariant.flow_insensitive-as" with
                    | `Lifted inv, "invariant_set-flow_insensitive_invariant" ->
                      let invs = Invariant.Exp.process inv in
                      List.fold_left (fun acc inv ->
                          let invariant = Invariant.Exp.show inv in
                          let invariant = Entry.flow_insensitive_invariant ~invariant in
                          incr cnt_flow_insensitive_invariant;
                          invariant :: acc
                        ) acc invs
                    | `Lifted inv, "invariant_set-location_invariant" ->
                      (* TODO: fold_flow_insensitive_as_location is now only used here, inline/move? *)
                      fold_flow_insensitive_as_location ~inv (fun ~location ~inv acc ->
                          let invariant = Invariant.Exp.show inv in
                          let invariant = Entry.location_invariant ~location ~invariant in
                          incr cnt_location_invariant;
                          invariant :: acc
                        ) acc
                    | `Lifted _, _
                    | `Bot, _ | `Top, _ -> (* global bot might only be possible for alloc variables, if at all, so emit nothing *)
                      acc
                  end
                | `Right _ -> (* global unknown for FromSpec contexts *)
                  acc
              ) gh invariants
          )
          else
            invariants
        in

        let invariants = List.rev invariants in
        let entry = Entry.invariant_set ~task ~invariants in
        entry :: entries
      )
      else
        entries
    in

    let yaml_entries = List.rev_map YamlWitnessType.Entry.to_yaml entries in (* reverse to make entries in file in the same order as generation messages *)

    M.msg_group Info ~category:Witness "witness generation summary" [
      (Pretty.dprintf "location invariants: %d" !cnt_location_invariant, None);
      (Pretty.dprintf "loop invariants: %d" !cnt_loop_invariant, None);
      (Pretty.dprintf "flow-insensitive invariants: %d" !cnt_flow_insensitive_invariant, None);
      (Pretty.dprintf "total generation entries: %d" (List.length yaml_entries), None);
    ];

    yaml_entries_to_file yaml_entries (Fpath.v (GobConfig.get_string "witness.yaml.path"))

  let write () =
    Timing.wrap "yaml witness" write ()

  let write ~svcomp_result =
    if GobConfig.get_bool "witness.yaml.sv-comp-true-only" then (
      match svcomp_result with
      | Some (Ok Svcomp.Result.True) -> write ()
      | _ -> ()
    )
    else
      write ()
end

let init () =
  match GobConfig.get_string "witness.yaml.validate" with
  | "" -> ()
  | path ->
    (* Check witness existence before doing the analysis. *)
    if not (Sys.file_exists path) then (
      Logs.error "witness.yaml.validate: %s not found" path;
      Svcomp.errorwith "witness missing"
    )

let loc_of_location (location: YamlWitnessType.Location.t): Cil.location = {
  file = location.file_name;
  line = location.line;
  column = Option.value location.column ~default:1;
  byte = -1;
  endLine = -1;
  endColumn = -1;
  endByte = -1;
  synthetic = false;
}

module ValidationResult =
struct
  (* constructor order is important for the chain lattice *)
  type result =
    | Confirmed
    | Unconfirmed
    | Refuted
    | ParseError
  [@@deriving enum, show]

  module ChainParams =
  struct
    let n () = max_result - 1
    let names i = show_result (Option.get (result_of_enum i))
  end
  include Lattice.Chain (ChainParams)
end

(* TODO: record *)
let cnt_confirmed = ref 0
let cnt_unconfirmed = ref 0
let cnt_refuted = ref 0
let cnt_unchecked = ref 0
let cnt_unsupported = ref 0
let cnt_error = ref 0
let cnt_disabled = ref 0

module Validator (R: ResultQuery.SpecSysSol2) =
struct
  open R
  open SpecSys

  module Locator = WitnessUtil.Locator (EQSys.LVar)
  module LvarS = Locator.ES
  module WitnessInvariant = WitnessUtil.YamlInvariant (FileCfg)
  module InvariantParser = WitnessUtil.InvariantParser
  module VR = ValidationResult

  let validate () =
    let location_locator = Locator.create () in
    let loop_locator = Locator.create () in
    (* TODO: add all CFG nodes, not just live ones from lh, like UnassumeAnalysis *)
    LHT.iter (fun ((n, _) as lvar) _ ->
        Option.iter (fun loc ->
            Locator.add location_locator loc lvar
          ) (WitnessInvariant.location_location n);
        Option.iter (fun loc ->
            Locator.add loop_locator loc lvar
          ) (WitnessInvariant.loop_location n)
      ) lh;

    let inv_parser = InvariantParser.create FileCfg.file in

    let yaml = match GobResult.Syntax.(Fpath.of_string (GobConfig.get_string "witness.yaml.validate") >>= Yaml_unix.of_file) with
      | Ok yaml -> yaml
      | Error (`Msg m) ->
        Logs.error "Yaml_unix.of_file: %s" m;
        Svcomp.errorwith "witness missing"
    in
    let yaml_entries = yaml |> GobYaml.list |> BatResult.get_ok in

    cnt_confirmed := 0;
    cnt_unconfirmed := 0;
    cnt_refuted := 0;
    cnt_unchecked := 0;
    cnt_unsupported := 0;
    cnt_error := 0;
    cnt_disabled := 0;

    let validate_entry (entry: YamlWitnessType.Entry.t): unit =
      let target_type = YamlWitnessType.EntryType.entry_type entry.entry_type in

      let validate_lvars_invariant ~loc ~lvars inv =
        let msgLoc: M.Location.t = CilLocation loc in
        match InvariantParser.parse_cabs inv with
        | Ok inv_cabs ->

          let result = LvarS.fold (fun ((n, _) as lvar) (acc: VR.t) ->
              let fundec = Node.find_fundec n in

              let result: VR.result = match InvariantParser.parse_cil inv_parser ~fundec ~loc inv_cabs with
                | Ok inv_exp ->
                  begin match Queries.eval_bool {f = (fun (type a) (q: a Queries.t) -> ask_local lvar q)} inv_exp with
                    | `Bot -> Option.get (VR.result_of_enum (VR.bot ())) (* dead code *)
                    | `Lifted true -> Confirmed
                    | `Lifted false -> Refuted
                    | `Top -> Unconfirmed
                  end
                | Error e ->
                  ParseError
              in
              VR.join acc (VR.result_to_enum result)
            ) lvars (VR.bot ())
          in

          begin match Option.get (VR.result_of_enum result) with
            | Confirmed ->
              incr cnt_confirmed;
              M.success ~category:Witness ~loc:msgLoc "invariant confirmed: %s" inv
            | Unconfirmed ->
              incr cnt_unconfirmed;
              M.warn ~category:Witness ~loc:msgLoc "invariant unconfirmed: %s" inv
            | Refuted ->
              incr cnt_refuted;
              M.error ~category:Witness ~loc:msgLoc "invariant refuted: %s" inv
            | ParseError ->
              incr cnt_error;
              M.error ~category:Witness ~loc:msgLoc "CIL couldn't parse invariant: %s" inv;
              M.info ~category:Witness ~loc:msgLoc "invariant has undefined variables or side effects: %s" inv
          end
        | Error e ->
          incr cnt_error;
          M.error ~category:Witness ~loc:msgLoc "Frontc couldn't parse invariant: %s" inv;
          M.info ~category:Witness ~loc:msgLoc "invariant has invalid syntax: %s" inv
      in

      let validate_invariant_set (invariant_set: YamlWitnessType.InvariantSet.t) =

        let validate_location_invariant (location_invariant: YamlWitnessType.InvariantSet.LocationInvariant.t) =
          let loc = loc_of_location location_invariant.location in
          let inv = location_invariant.value in

          match Locator.find_opt location_locator loc with
          | Some lvars ->
            validate_lvars_invariant ~loc ~lvars inv
          | None ->
            incr cnt_error;
            M.warn ~category:Witness ~loc:(CilLocation loc) "couldn't locate invariant: %s" inv;
        in

        let validate_loop_invariant (loop_invariant: YamlWitnessType.InvariantSet.LoopInvariant.t) =
          let loc = loc_of_location loop_invariant.location in
          let inv = loop_invariant.value in

          match Locator.find_opt loop_locator loc with
          | Some lvars ->
            validate_lvars_invariant ~loc ~lvars inv
          | None ->
            incr cnt_error;
            M.warn ~category:Witness ~loc:(CilLocation loc) "couldn't locate invariant: %s" inv;
        in

        let validate_invariant (invariant: YamlWitnessType.InvariantSet.Invariant.t) =
          let target_type = YamlWitnessType.InvariantSet.InvariantType.invariant_type invariant.invariant_type in
          match invariant_type_enabled target_type, invariant.invariant_type with
          | true, LocationInvariant ({labels = (None | Some []); _} as x) ->
            validate_location_invariant x
          | true, LoopInvariant ({labels = (None | Some []); _} as x) ->
            validate_loop_invariant x
          | false, (LocationInvariant _ | LoopInvariant _) ->
            incr cnt_disabled;
            M.info_noloc ~category:Witness "disabled invariant of type %s" target_type
          | _ ->
            incr cnt_unsupported;
            M.warn_noloc ~category:Witness "cannot validate invariant of type %s" target_type
        in

        let validate_invariant_kind (invariant_kind: YamlWitnessType.InvariantSet.InvariantKind.t) =
          let target_type = YamlWitnessType.InvariantSet.InvariantKind.invariant_kind invariant_kind in
          match invariant_kind with
          | Invariant x ->
            validate_invariant x
          | _ ->
            incr cnt_unsupported;
            M.warn_noloc ~category:Witness "cannot validate invariant of kind %s" target_type
        in

        List.iter validate_invariant_kind invariant_set.content
      in

      let validate_violation_sequence (violation_sequence: YamlWitnessType.ViolationSequence.t) =
        (* TODO: update cnt-s appropriately (needs access to SV-COMP result pre-witness validation) *)
        (* Nothing needs to be checked here!
           If program is correct and we can prove it, we output true, which counts as refutation of violation witness.
           If program is correct and we cannot prove it, we output unknown.
           If program is incorrect, we output unknown. *)
        ()
      in

      match entry_type_enabled target_type, entry.entry_type with
      | true, InvariantSet x ->
        validate_invariant_set x
      | true, ViolationSequence x ->
        validate_violation_sequence x
      | false, (InvariantSet _ | ViolationSequence _) ->
        incr cnt_disabled;
        M.info_noloc ~category:Witness "disabled entry of type %s" target_type
      | _ ->
        incr cnt_unsupported;
        M.warn_noloc ~category:Witness "cannot validate entry of type %s" target_type
    in

    List.iter (fun yaml_entry ->
        match YamlWitnessType.Entry.of_yaml yaml_entry with
        | Ok entry ->
          validate_entry entry
        | Error (`Msg e) ->
          incr cnt_error;
          M.error_noloc ~category:Witness "couldn't parse entry: %s" e
      ) yaml_entries;

    M.msg_group Info ~category:Witness "witness validation summary" [
      (Pretty.dprintf "confirmed: %d" !cnt_confirmed, None);
      (Pretty.dprintf "unconfirmed: %d" !cnt_unconfirmed, None);
      (Pretty.dprintf "refuted: %d" !cnt_refuted, None);
      (Pretty.dprintf "error: %d" !cnt_error, None);
      (Pretty.dprintf "unchecked: %d" !cnt_unchecked, None);
      (Pretty.dprintf "unsupported: %d" !cnt_unsupported, None);
      (Pretty.dprintf "disabled: %d" !cnt_disabled, None);
      (Pretty.dprintf "total validation entries: %d" (!cnt_confirmed + !cnt_unconfirmed + !cnt_refuted + !cnt_unchecked + !cnt_unsupported + !cnt_error + !cnt_disabled), None);
    ];

    match GobConfig.get_bool "witness.yaml.strict" with
    | true when !cnt_error > 0 ->
      Error "witness error"
    | true when !cnt_unsupported > 0 ->
      Error "witness unsupported"
    | true when !cnt_disabled > 0 ->
      Error "witness disabled"
    | _ when !cnt_refuted > 0 ->
      (* Refuted only when assuming the invariant is reachable. *)
      (* Ok (Svcomp.Result.False None) *) (* Wasn't a problem because valid*->correctness->false gave 0 points under old validator track scoring schema: https://doi.org/10.1007/978-3-031-22308-2_8. *)
      Ok Svcomp.Result.Unknown (* Now valid*->correctness->false gives 1p (negative) points under new validator track scoring schema: https://doi.org/10.1007/978-3-031-57256-2_15. *)
    | _ when !cnt_unconfirmed > 0 ->
      Ok Unknown
    | _ ->
      Ok True
end
