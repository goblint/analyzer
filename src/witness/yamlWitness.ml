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

  let metadata ?task (): Metadata.t =
    let uuid = Uuidm.v4_gen uuid_random_state () in
    let creation_time = TimeUtil.iso8601_now () in
    {
      format_version = GobConfig.get_string "witness.yaml.format-version";
      uuid = Uuidm.to_string uuid;
      creation_time;
      producer;
      task
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
    let file_hash =
      match GobConfig.get_string "witness.yaml.format-version" with
      | "0.1" -> Some (sha256_file loc.file)
      | "2.0" -> None
      | _ -> assert false
    in
    {
      file_name = loc.file;
      file_hash;
      line = loc.line;
      column = Some loc.column;
      function_ = Some location_function;
    }

  let invariant invariant: Invariant.t = {
    string = invariant;
    type_ = "assertion";
    format = "C";
  }

  let location_invariant ~task ~location ~(invariant): Entry.t = {
    entry_type = LocationInvariant {
        location;
        location_invariant = invariant;
      };
    metadata = metadata ~task ();
  }

  let loop_invariant ~task ~location ~(invariant): Entry.t = {
    entry_type = LoopInvariant {
        location;
        loop_invariant = invariant;
      };
    metadata = metadata ~task ();
  }

  (* non-standard extension *)
  let flow_insensitive_invariant ~task ~(invariant): Entry.t = {
    entry_type = FlowInsensitiveInvariant {
        flow_insensitive_invariant = invariant;
      };
    metadata = metadata ~task ();
  }

  (* non-standard extension *)
  let precondition_loop_invariant ~task ~location ~precondition ~(invariant): Entry.t = {
    entry_type = PreconditionLoopInvariant {
        location;
        loop_invariant = invariant;
        precondition;
      };
    metadata = metadata ~task ();
  }

  let location_invariant' ~location ~(invariant): InvariantSet.Invariant.t = {
    invariant_type = LocationInvariant {
        location;
        value = invariant;
        format = "c_expression";
      };
  }

  let loop_invariant' ~location ~(invariant): InvariantSet.Invariant.t = {
    invariant_type = LoopInvariant {
        location;
        value = invariant;
        format = "c_expression";
      };
  }

  let invariant_set ~task ~(invariants): Entry.t = {
    entry_type = InvariantSet {
        content = invariants;
      };
    metadata = metadata ~task ();
  }

  let target ~uuid ~type_ ~(file_name): Target.t = {
    uuid;
    type_;
    file_hash = sha256_file file_name;
  }

  let certification verdict: Certification.t = {
    string = if verdict then "confirmed" else "rejected";
    type_ = "verdict";
    format = "confirmed | rejected";
  }

  let loop_invariant_certificate ~target ~(certification): Entry.t = {
    entry_type = LoopInvariantCertificate {
        target;
        certification;
      };
    metadata = metadata ();
  }

  let precondition_loop_invariant_certificate ~target ~(certification): Entry.t = {
    entry_type = PreconditionLoopInvariantCertificate {
        target;
        certification;
      };
    metadata = metadata ();
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

  let ghost_instrumentation ~task ~variables ~(location_updates): Entry.t = {
    entry_type = GhostInstrumentation {
        ghost_variables = variables;
        ghost_updates = location_updates;
      };
    metadata = metadata ~task ();
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
            |> BatList.enum
            |> BatEnum.filter_map (fun (_, next_n) ->
                match R.ask_local_node next_n MayAccessed with
                | `Top -> None
                | `Lifted _ as es -> Some es)
            |> BatEnum.fold AccessDomain.EventSet.union (AccessDomain.EventSet.empty ())
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
    (* TODO: precondition invariants? *)

    (* Generate location invariants (without precondition) *)
    let entries =
      if entry_type_enabled YamlWitnessType.LocationInvariant.entry_type then (
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
              let invs = WitnessUtil.InvariantExp.process_exp inv in
              List.fold_left (fun acc inv ->
                  let invariant = Entry.invariant (CilType.Exp.show inv) in
                  let entry = Entry.location_invariant ~task ~location ~invariant in
                  incr cnt_location_invariant;
                  entry :: acc
                ) acc invs
            | `Bot | `Top -> (* TODO: 0 for bot (dead code)? *)
              acc
          ) (Lazy.force location_nodes) entries
      )
      else
        entries
    in

    (* Generate loop invariants (without precondition) *)
    let entries =
      if entry_type_enabled YamlWitnessType.LoopInvariant.entry_type then (
        LH.fold (fun loc ns acc ->
            if WitnessInvariant.emit_loop_head then ( (* TODO: remove double condition? needs both loop_invariant entry enabled and witness.invariant.loop-head option enabled *)
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
                let invs = WitnessUtil.InvariantExp.process_exp inv in
                List.fold_left (fun acc inv ->
                    let invariant = Entry.invariant (CilType.Exp.show inv) in
                    let entry = Entry.loop_invariant ~task ~location ~invariant in
                    incr cnt_loop_invariant;
                    entry :: acc
                  ) acc invs
              | `Bot | `Top -> (* TODO: 0 for bot (dead code)? *)
                acc
            )
            else
              acc
          ) (Lazy.force loop_nodes) entries
      )
      else
        entries
    in

    let invariant_global_nodes = lazy (R.ask_global InvariantGlobalNodes) in

    let fold_flow_insensitive_as_location ~inv f acc =
      (* Currently same invariants (from InvariantGlobal query) for all nodes (from InvariantGlobalNodes query).
         The alternative would be querying InvariantGlobal per local unknown when looping over them to generate location invariants.
         See: https://github.com/goblint/analyzer/pull/1394#discussion_r1698149514. *)
      let invs = WitnessUtil.InvariantExp.process_exp inv in
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

    (* Generate flow-insensitive invariants *)
    let entries =
      if entry_type_enabled YamlWitnessType.FlowInsensitiveInvariant.entry_type then (
        GHT.fold (fun g v acc ->
            match g with
            | `Left g -> (* global unknown from analysis Spec *)
              begin match R.ask_global (InvariantGlobal (Obj.repr g)), GobConfig.get_string "witness.invariant.flow_insensitive-as" with
                | `Lifted inv, "flow_insensitive_invariant" ->
                  let invs = WitnessUtil.InvariantExp.process_exp inv in
                  List.fold_left (fun acc inv ->
                      let invariant = Entry.invariant (CilType.Exp.show inv) in
                      let entry = Entry.flow_insensitive_invariant ~task ~invariant in
                      incr cnt_flow_insensitive_invariant;
                      entry :: acc
                    ) acc invs
                | `Lifted inv, "location_invariant" ->
                  fold_flow_insensitive_as_location ~inv (fun ~location ~inv acc ->
                      let invariant = Entry.invariant (CilType.Exp.show inv) in
                      let entry = Entry.location_invariant ~task ~location ~invariant in
                      incr cnt_location_invariant;
                      entry :: acc
                    ) acc
                | `Lifted _, _
                | `Bot, _ | `Top, _ -> (* global bot might only be possible for alloc variables, if at all, so emit nothing *)
                  acc
              end
            | `Right _ -> (* global unknown for FromSpec contexts *)
              acc
          ) gh entries
      )
      else
        entries
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

    (* Generate precondition loop invariants.
       We do this in three steps:
       1. Collect contexts for each function
       2. For each function context, find "matching"/"weaker" contexts that may satisfy its invariant
       3. Generate precondition loop invariants. The postcondition is a disjunction over the invariants for matching states. *)
    let entries =
      if entry_type_enabled YamlWitnessType.PreconditionLoopInvariant.entry_type then (
        (* 1. Collect contexts for each function *)
        (* TODO: Use [IterSysVars] for this when #391 is merged. *)
        let fun_contexts : con_inv list FMap.t = FMap.create 103 in
        LHT.iter (fun ((n, c) as lvar) local ->
            begin match n with
              | FunctionEntry f ->
                let invariant = R.ask_local lvar ~local (Invariant Invariant.default_context) in
                FMap.modify_def [] f (fun acc -> {context = c; invariant; node = n; state = local}::acc) fun_contexts
              | _ -> ()
            end
          ) lh;

        (* 2. For all contexts and their invariants, find all contexts such that their start state may satisfy the invariant. *)
        let fc_map : con_inv list FCMap.t = FCMap.create 103 in
        FMap.iter (fun f con_invs ->
            List.iter (fun current_c ->
                begin match current_c.invariant with
                  | `Lifted c_inv ->
                    (* Collect all start states that may satisfy the invariant of current_c *)
                    List.iter (fun c ->
                        let x = R.ask_local (c.node, c.context) ~local:c.state (Queries.EvalInt c_inv) in
                        if Queries.ID.is_bot x || Queries.ID.is_bot_ikind x then (* dead code *)
                          failwith "Bottom not expected when querying context state" (* Maybe this is reachable, failwith for now so we see when this happens *)
                        else if Queries.ID.to_bool x = Some false then () (* Nothing to do, the c does definitely not satisfy the predicate of current_c *)
                        else begin
                          (* Insert c into the list of weaker contexts of f *)
                          FCMap.modify_def [] (f, current_c.context) (fun cs -> c::cs) fc_map;
                        end
                      ) con_invs;
                  | `Bot | `Top ->
                    (* If the context invariant is None, we will not generate a precondition invariant. Nothing to do here. *)
                    ()
                end
              ) con_invs;
          ) fun_contexts;

        (** Given [(n,c)] retrieves all [(n,c')], with [c'] such that [(f, c')] may satisfy the precondition generated for [c].*)
        let find_matching_states ((n, c) : LHT.key) =
          let f = Node.find_fundec n in
          let contexts =  FCMap.find fc_map (f, c) in
          List.filter_map (fun c -> LHT.find_option lh (n, c.context)) contexts
        in

        (* 3. Generate precondition invariants *)
        LHT.fold (fun ((n, c) as lvar) local acc ->
            match WitnessInvariant.loop_location n with
            | Some loc ->
              let fundec = Node.find_fundec n in
              let pre_lvar = (Node.FunctionEntry fundec, c) in
              let query = Queries.Invariant Invariant.default_context in
              begin match R.ask_local pre_lvar query with
                | `Lifted c_inv ->
                  (* Find unknowns for which the preceding start state satisfies the precondtion *)
                  let xs = find_matching_states lvar in

                  (* Generate invariants. Give up in case one invariant could not be generated. *)
                  let invs = GobList.fold_while_some (fun acc local ->
                      let lvals = local_lvals n local in
                      match R.ask_local_node n ~local (Invariant {Invariant.default_context with lvals}) with
                      | `Lifted c -> Some ((`Lifted c)::acc)
                      | `Bot | `Top -> None
                    ) [] xs
                  in
                  begin match invs with
                    | None
                    | Some [] -> acc
                    | Some (x::xs) ->
                      begin match List.fold_left (fun acc inv -> Invariant.(acc || inv) [@coverage off]) x xs with (* bisect_ppx cannot handle redefined (||) *)
                        | `Lifted inv ->
                          let invs = WitnessUtil.InvariantExp.process_exp inv in
                          let c_inv = InvariantCil.exp_replace_original_name c_inv in (* cannot be split *)
                          List.fold_left (fun acc inv ->
                              let location_function = (Node.find_fundec n).svar.vname in
                              let location = Entry.location ~location:loc ~location_function in
                              let precondition = Entry.invariant (CilType.Exp.show c_inv) in
                              let invariant = Entry.invariant (CilType.Exp.show inv) in
                              let entry = Entry.precondition_loop_invariant ~task ~location ~precondition ~invariant in
                              entry :: acc
                            ) acc invs
                        | `Bot | `Top -> acc
                      end
                  end
                | _ -> (* Do not construct precondition invariants if we cannot express precondition *)
                  acc
              end
            | None ->
              acc
          ) lh entries
      )
      else
        entries
    in

    (* Generate invariant set *)
    let entries =
      if entry_type_enabled YamlWitnessType.InvariantSet.entry_type || entry_type_enabled YamlWitnessType.FlowInsensitiveInvariant.entry_type && GobConfig.get_string "witness.invariant.flow_insensitive-as" = "invariant_set-location_invariant" then (
        let invariants = [] in

        (* Generate location invariants *)
        let invariants =
          if entry_type_enabled YamlWitnessType.InvariantSet.entry_type && invariant_type_enabled YamlWitnessType.InvariantSet.LocationInvariant.invariant_type then (
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
                  let invs = WitnessUtil.InvariantExp.process_exp inv in
                  List.fold_left (fun acc inv ->
                      let invariant = CilType.Exp.show inv in
                      let invariant = Entry.location_invariant' ~location ~invariant in
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
          if entry_type_enabled YamlWitnessType.InvariantSet.entry_type && invariant_type_enabled YamlWitnessType.InvariantSet.LoopInvariant.invariant_type then (
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
                    let invs = WitnessUtil.InvariantExp.process_exp inv in
                    List.fold_left (fun acc inv ->
                        let invariant = CilType.Exp.show inv in
                        let invariant = Entry.loop_invariant' ~location ~invariant in
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

        (* Generate flow-insensitive invariants as location invariants *)
        let invariants =
          if entry_type_enabled YamlWitnessType.FlowInsensitiveInvariant.entry_type && GobConfig.get_string "witness.invariant.flow_insensitive-as" = "invariant_set-location_invariant" then (
            GHT.fold (fun g v acc ->
                match g with
                | `Left g -> (* global unknown from analysis Spec *)
                  begin match R.ask_global (InvariantGlobal (Obj.repr g)) with
                    | `Lifted inv ->
                      fold_flow_insensitive_as_location ~inv (fun ~location ~inv acc ->
                          let invariant = CilType.Exp.show inv in
                          let invariant = Entry.location_invariant' ~location ~invariant in
                          incr cnt_location_invariant;
                          invariant :: acc
                        ) acc
                    | `Bot | `Top -> (* global bot might only be possible for alloc variables, if at all, so emit nothing *)
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

    let validate_entry (entry: YamlWitnessType.Entry.t): YamlWitnessType.Entry.t option =
      let uuid = entry.metadata.uuid in
      let target_type = YamlWitnessType.EntryType.entry_type entry.entry_type in

      let validate_lvars_invariant ~entry_certificate ~loc ~lvars inv =
        let msgLoc: M.Location.t = CilLocation loc in
        match InvariantParser.parse_cabs inv with
        | Ok inv_cabs ->

          let result = LvarS.fold (fun ((n, _) as lvar) (acc: VR.t) ->
              let fundec = Node.find_fundec n in

              let result: VR.result = match InvariantParser.parse_cil inv_parser ~fundec ~loc inv_cabs with
                | Ok inv_exp ->
                  let x = ask_local lvar (Queries.EvalInt inv_exp) in
                  if Queries.ID.is_bot x || Queries.ID.is_bot_ikind x then (* dead code *)
                    Option.get (VR.result_of_enum (VR.bot ()))
                  else (
                    match Queries.ID.to_bool x with
                    | Some true -> Confirmed
                    | Some false -> Refuted
                    | None -> Unconfirmed
                  )
                | Error e ->
                  ParseError
              in
              VR.join acc (VR.result_to_enum result)
            ) lvars (VR.bot ())
          in

          begin match Option.get (VR.result_of_enum result) with
            | Confirmed ->
              incr cnt_confirmed;
              M.success ~category:Witness ~loc:msgLoc "invariant confirmed: %s" inv;
              Option.map (fun entry_certificate ->
                  let target = Entry.target ~uuid ~type_:target_type ~file_name:loc.file in
                  let certification = Entry.certification true in
                  let certificate_entry = entry_certificate ~target ~certification in
                  certificate_entry
                ) entry_certificate
            | Unconfirmed ->
              incr cnt_unconfirmed;
              M.warn ~category:Witness ~loc:msgLoc "invariant unconfirmed: %s" inv;None
            | Refuted ->
              incr cnt_refuted;
              M.error ~category:Witness ~loc:msgLoc "invariant refuted: %s" inv;
              Option.map (fun entry_certificate ->
                  let target = Entry.target ~uuid ~type_:target_type ~file_name:loc.file in
                  let certification = Entry.certification false in
                  let certificate_entry = entry_certificate ~target ~certification in
                  certificate_entry
                ) entry_certificate
            | ParseError ->
              incr cnt_error;
              M.error ~category:Witness ~loc:msgLoc "CIL couldn't parse invariant: %s" inv;
              M.info ~category:Witness ~loc:msgLoc "invariant has undefined variables or side effects: %s" inv;
              None
          end
        | Error e ->
          incr cnt_error;
          M.error ~category:Witness ~loc:msgLoc "Frontc couldn't parse invariant: %s" inv;
          M.info ~category:Witness ~loc:msgLoc "invariant has invalid syntax: %s" inv;
          None
      in

      let validate_location_invariant (location_invariant: YamlWitnessType.LocationInvariant.t) =
        let loc = loc_of_location location_invariant.location in
        let inv = location_invariant.location_invariant.string in
        let entry_certificate = (* TODO: Wrong, because there's no location_invariant_certificate, but this is the closest thing for now. *)
          if entry_type_enabled YamlWitnessType.LoopInvariantCertificate.entry_type then
            Some Entry.loop_invariant_certificate
          else
            None
        in

        match Locator.find_opt location_locator loc with
        | Some lvars ->
          validate_lvars_invariant ~entry_certificate ~loc ~lvars inv
        | None ->
          incr cnt_error;
          M.warn ~category:Witness ~loc:(CilLocation loc) "couldn't locate invariant: %s" inv;
          None
      in

      let validate_loop_invariant (loop_invariant: YamlWitnessType.LoopInvariant.t) =
        let loc = loc_of_location loop_invariant.location in
        let inv = loop_invariant.loop_invariant.string in
        let entry_certificate =
          if entry_type_enabled YamlWitnessType.LoopInvariantCertificate.entry_type then
            Some Entry.loop_invariant_certificate
          else
            None
        in

        match Locator.find_opt loop_locator loc with
        | Some lvars ->
          validate_lvars_invariant ~entry_certificate ~loc ~lvars inv
        | None ->
          incr cnt_error;
          M.warn ~category:Witness ~loc:(CilLocation loc) "couldn't locate invariant: %s" inv;
          None
      in

      let validate_precondition_loop_invariant (precondition_loop_invariant: YamlWitnessType.PreconditionLoopInvariant.t) =
        let loc = loc_of_location precondition_loop_invariant.location in
        let pre = precondition_loop_invariant.precondition.string in
        let inv = precondition_loop_invariant.loop_invariant.string in
        let entry_certificate =
          if entry_type_enabled YamlWitnessType.PreconditionLoopInvariantCertificate.entry_type then
            Some Entry.precondition_loop_invariant_certificate
          else
            None
        in
        let msgLoc: M.Location.t = CilLocation loc in

        match Locator.find_opt loop_locator loc with
        | Some lvars ->
          begin match InvariantParser.parse_cabs pre with
            | Ok pre_cabs ->

              let precondition_holds (n, c) =
                let fundec = Node.find_fundec n in
                let pre_lvar = (Node.FunctionEntry fundec, c) in

                match InvariantParser.parse_cil inv_parser ~fundec ~loc pre_cabs with
                | Ok pre_exp ->
                  let x = ask_local pre_lvar (Queries.EvalInt pre_exp) in
                  if Queries.ID.is_bot x || Queries.ID.is_bot_ikind x then (* dead code *)
                    true
                  else (
                    match Queries.ID.to_bool x with
                    | Some b -> b
                    | None -> false
                  )
                | Error e ->
                  M.error ~category:Witness ~loc:msgLoc "CIL couldn't parse precondition: %s" pre;
                  M.info ~category:Witness ~loc:msgLoc "precondition has undefined variables or side effects: %s" pre;
                  false
              in

              let lvars = LvarS.filter precondition_holds lvars in
              if LvarS.is_empty lvars then (
                incr cnt_unchecked;
                M.warn ~category:Witness ~loc:msgLoc "precondition never definitely holds: %s" pre;
                None
              )
              else
                validate_lvars_invariant ~entry_certificate ~loc ~lvars inv
            | Error e ->
              incr cnt_error;
              M.error ~category:Witness ~loc:msgLoc "Frontc couldn't parse precondition: %s" pre;
              M.info ~category:Witness ~loc:msgLoc "precondition has invalid syntax: %s" pre;
              None
          end
        | None ->
          incr cnt_error;
          M.warn ~category:Witness ~loc:msgLoc "couldn't locate invariant: %s" inv;
          None
      in

      let validate_invariant_set (invariant_set: YamlWitnessType.InvariantSet.t) =

        let validate_location_invariant (location_invariant: YamlWitnessType.InvariantSet.LocationInvariant.t) =
          let loc = loc_of_location location_invariant.location in
          let inv = location_invariant.value in

          match Locator.find_opt location_locator loc with
          | Some lvars ->
            ignore (validate_lvars_invariant ~entry_certificate:None ~loc ~lvars inv)
          | None ->
            incr cnt_error;
            M.warn ~category:Witness ~loc:(CilLocation loc) "couldn't locate invariant: %s" inv;
        in

        let validate_loop_invariant (loop_invariant: YamlWitnessType.InvariantSet.LoopInvariant.t) =
          let loc = loc_of_location loop_invariant.location in
          let inv = loop_invariant.value in

          match Locator.find_opt loop_locator loc with
          | Some lvars ->
            ignore (validate_lvars_invariant ~entry_certificate:None ~loc ~lvars inv)
          | None ->
            incr cnt_error;
            M.warn ~category:Witness ~loc:(CilLocation loc) "couldn't locate invariant: %s" inv;
        in

        let validate_invariant (invariant: YamlWitnessType.InvariantSet.Invariant.t) =
          let target_type = YamlWitnessType.InvariantSet.InvariantType.invariant_type invariant.invariant_type in
          match invariant_type_enabled target_type, invariant.invariant_type with
          | true, LocationInvariant x ->
            validate_location_invariant x
          | true, LoopInvariant x ->
            validate_loop_invariant x
          | false, (LocationInvariant _ | LoopInvariant _) ->
            incr cnt_disabled;
            M.info_noloc ~category:Witness "disabled invariant of type %s" target_type;
        in

        List.iter validate_invariant invariant_set.content;
        None
      in

      let validate_violation_sequence (violation_sequence: YamlWitnessType.ViolationSequence.t) =
        (* TODO: update cnt-s appropriately (needs access to SV-COMP result pre-witness validation) *)
        (* Nothing needs to be checked here!
           If program is correct and we can prove it, we output true, which counts as refutation of violation witness.
           If program is correct and we cannot prove it, we output unknown.
           If program is incorrect, we output unknown. *)
        None
      in

      match entry_type_enabled target_type, entry.entry_type with
      | true, LocationInvariant x ->
        validate_location_invariant x
      | true, LoopInvariant x ->
        validate_loop_invariant x
      | true, PreconditionLoopInvariant x ->
        validate_precondition_loop_invariant x
      | true, InvariantSet x ->
        validate_invariant_set x
      | true, ViolationSequence x ->
        validate_violation_sequence x
      | false, (LocationInvariant _ | LoopInvariant _ | PreconditionLoopInvariant _ | InvariantSet _ | ViolationSequence _) ->
        incr cnt_disabled;
        M.info_noloc ~category:Witness "disabled entry of type %s" target_type;
        None
      | _ ->
        incr cnt_unsupported;
        M.warn_noloc ~category:Witness "cannot validate entry of type %s" target_type;
        None
    in

    let yaml_entries' = List.fold_left (fun yaml_entries' yaml_entry ->
        match YamlWitnessType.Entry.of_yaml yaml_entry with
        | Ok entry ->
          let certificate_entry = validate_entry entry in
          let yaml_certificate_entry = Option.map YamlWitnessType.Entry.to_yaml certificate_entry in
          Option.to_list yaml_certificate_entry @ yaml_entry :: yaml_entries'
        | Error (`Msg e) ->
          incr cnt_error;
          M.error_noloc ~category:Witness "couldn't parse entry: %s" e;
          yaml_entry :: yaml_entries'
      ) [] yaml_entries
    in

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

    let certificate_path = GobConfig.get_string "witness.yaml.certificate" in
    if certificate_path <> "" then
      yaml_entries_to_file (List.rev yaml_entries') (Fpath.v certificate_path);

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
