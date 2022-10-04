open Analyses
open GoblintCil

let uuid_random_state = Random.State.make_self_init ()

let sha256_file f = Sha256.(to_hex (file f))
let sha256_file_cache = BatCache.make_ht ~gen:sha256_file ~init_size:5
let sha256_file = sha256_file_cache.get

module Entry =
struct
  open YamlWitnessType

  (* yaml_conf is too verbose *)
  (* let yaml_conf: Yaml.value = Json_repr.convert (module Json_repr.Yojson) (module Json_repr.Ezjsonm) (!GobConfig.json_conf) in *)
  let producer: Producer.t = {
    name = "Goblint";
    version = Version.goblint;
    command_line = Goblintutil.command_line;
  }

  let metadata ?task (): Metadata.t =
    let uuid = Uuidm.v4_gen uuid_random_state () in
    let creation_time = TimeUtil.iso8601_now () in
    {
      format_version = "0.1";
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

  let location ~location:(loc: Cil.location) ~(location_function): Location.t = {
    file_name = loc.file;
    file_hash = sha256_file loc.file;
    line = loc.line;
    column = loc.column - 1;
    function_ = location_function;
  }

  let invariant invariant: Invariant.t = {
    string = invariant;
    type_ = "assertion";
    format = "C";
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
end

let yaml_entries_to_file yaml_entries file =
  let yaml = `A yaml_entries in
  (* Yaml_unix.to_file_exn file yaml *)
  (* to_file/to_string uses a fixed-size buffer... *)
  (* estimate how big it should be + extra in case empty *)
  let text = Yaml.to_string_exn ~len:(List.length yaml_entries * 2048 + 2048) yaml in
  Batteries.output_file ~filename:(Fpath.to_string file) ~text

module Query
    (Spec : Spec)
    (EQSys : GlobConstrSys with module LVar = VarF (Spec.C)
                            and module GVar = GVarF (Spec.V)
                            and module D = Spec.D
                            and module G = GVarG (Spec.G) (Spec.C))
    (GHT : BatHashtbl.S with type key = EQSys.GVar.t) =
struct
  let ask_local (gh: EQSys.G.t GHT.t) (lvar:EQSys.LVar.t) local =
    (* build a ctx for using the query system *)
    let rec ctx =
      { ask    = (fun (type a) (q: a Queries.t) -> Spec.query ctx q)
      ; emit   = (fun _ -> failwith "Cannot \"emit\" in witness context.")
      ; node   = fst lvar
      ; prev_node = MyCFG.dummy_node
      ; control_context = (fun () -> Obj.magic (snd lvar)) (* magic is fine because Spec is top-level Control Spec *)
      ; context = (fun () -> snd lvar)
      ; edge    = MyCFG.Skip
      ; local  = local
      ; global = (fun g -> try EQSys.G.spec (GHT.find gh (EQSys.GVar.spec g)) with Not_found -> Spec.G.bot ()) (* TODO: how can be missing? *)
      ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in witness context.")
      ; split  = (fun d es   -> failwith "Cannot \"split\" in witness context.")
      ; sideg  = (fun v g    -> failwith "Cannot \"sideg\" in witness context.")
      }
    in
    Spec.query ctx

  let ask_local_node (gh: EQSys.G.t GHT.t) (n: Node.t) local =
    (* build a ctx for using the query system *)
    let rec ctx =
      { ask    = (fun (type a) (q: a Queries.t) -> Spec.query ctx q)
      ; emit   = (fun _ -> failwith "Cannot \"emit\" in witness context.")
      ; node   = n
      ; prev_node = MyCFG.dummy_node
      ; control_context = (fun () -> ctx_failwith "No context in witness context.")
      ; context = (fun () -> ctx_failwith "No context in witness context.")
      ; edge    = MyCFG.Skip
      ; local  = local
      ; global = (fun g -> try EQSys.G.spec (GHT.find gh (EQSys.GVar.spec g)) with Not_found -> Spec.G.bot ()) (* TODO: how can be missing? *)
      ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in witness context.")
      ; split  = (fun d es   -> failwith "Cannot \"split\" in witness context.")
      ; sideg  = (fun v g    -> failwith "Cannot \"sideg\" in witness context.")
      }
    in
    Spec.query ctx

  let ask_global (gh: EQSys.G.t GHT.t) =
    (* copied from Control for WarnGlobal *)
    (* build a ctx for using the query system *)
    let rec ctx =
      { ask    = (fun (type a) (q: a Queries.t) -> Spec.query ctx q)
      ; emit   = (fun _ -> failwith "Cannot \"emit\" in query context.")
      ; node   = MyCFG.dummy_node (* TODO maybe ask should take a node (which could be used here) instead of a location *)
      ; prev_node = MyCFG.dummy_node
      ; control_context = (fun () -> ctx_failwith "No context in query context.")
      ; context = (fun () -> ctx_failwith "No context in query context.")
      ; edge    = MyCFG.Skip
      ; local  = Spec.startstate dummyFunDec.svar (* bot and top both silently raise and catch Deadcode in DeadcodeLifter *) (* TODO: is this startstate bad? *)
      ; global = (fun v -> EQSys.G.spec (try GHT.find gh (EQSys.GVar.spec v) with Not_found -> EQSys.G.bot ())) (* TODO: how can be missing? *)
      ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in query context.")
      ; split  = (fun d es   -> failwith "Cannot \"split\" in query context.")
      ; sideg  = (fun v g    -> failwith "Cannot \"split\" in query context.")
      }
    in
    Spec.query ctx
end

module Make
    (File: WitnessUtil.File)
    (Cfg: MyCFG.CfgBidir)
    (Spec : Spec)
    (EQSys : GlobConstrSys with module LVar = VarF (Spec.C)
                            and module GVar = GVarF (Spec.V)
                            and module D = Spec.D
                            and module G = GVarG (Spec.G) (Spec.C))
    (LHT : BatHashtbl.S with type key = EQSys.LVar.t)
    (GHT : BatHashtbl.S with type key = EQSys.GVar.t) =
struct

  module NH = BatHashtbl.Make (Node)
  module WitnessInvariant = WitnessUtil.Invariant (File) (Cfg)
  module FMap = BatHashtbl.Make (CilType.Fundec)
  module FCMap = BatHashtbl.Make (Printable.Prod (CilType.Fundec) (Spec.C))
  module Query = Query (Spec) (EQSys) (GHT)

  type con_inv = {node: Node.t; context: Spec.C.t; invariant: Invariant.t; state: Spec.D.t}

  (* copied from Constraints.CompareNode *)
  let join_contexts (lh: Spec.D.t LHT.t): Spec.D.t NH.t =
    let nh = NH.create 113 in
    LHT.iter (fun (n, _) d ->
        let d' = try Spec.D.join (NH.find nh n) d with Not_found -> d in
        NH.replace nh n d'
      ) lh;
    nh

  let write lh gh =
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

    let nh = join_contexts lh in

    let is_invariant_node (n : Node.t) =
      let loc = Node.location n in
      match n with
      | Statement _ when not loc.synthetic && WitnessInvariant.is_invariant_node n -> true
      | _ ->
        (* avoid FunctionEntry/Function, because their locations are not inside the function where asserts could be inserted *)
        false
    in

    let local_lvals n local =
      if GobConfig.get_bool "witness.invariant.accessed" then (
        match Query.ask_local_node gh n local MayAccessed with
        | `Top ->
          CilLval.Set.top ()
        | (`Lifted _) as es ->
          let lvals = AccessDomain.EventSet.fold (fun e lvals ->
              match e with
              | {var_opt = Some var; offs_opt = Some offs; kind = Write} ->
                CilLval.Set.add (Var var, offs) lvals
              | _ ->
                lvals
            ) es (CilLval.Set.empty ())
          in
          let lvals =
            Cfg.next n
            |> BatList.enum
            |> BatEnum.filter_map (fun (_, next_n) ->
                let next_local = NH.find nh next_n in
                match Query.ask_local_node gh next_n next_local MayAccessed with
                | `Top -> None
                | `Lifted _ as es -> Some es)
            |> BatEnum.fold AccessDomain.EventSet.union (AccessDomain.EventSet.empty ())
            |> fun es -> AccessDomain.EventSet.fold (fun e lvals ->
                match e with
                | {var_opt = Some var; offs_opt = Some offs; kind = Read} ->
                  CilLval.Set.add (Var var, offs) lvals
                | _ ->
                  lvals
              ) es lvals
          in
          lvals
      )
      else
        CilLval.Set.top ()
    in

    (* Generate location invariants (wihtout precondition) *)
    let entries = NH.fold (fun n local acc ->
        let loc = Node.location n in
        if is_invariant_node n then (
          let lvals = local_lvals n local in
          match Query.ask_local_node gh n local (Invariant {Invariant.default_context with lvals}) with
          | `Lifted inv ->
            let invs = WitnessUtil.InvariantExp.process_exp inv in
            List.fold_left (fun acc inv ->
                let location_function = (Node.find_fundec n).svar.vname in
                let location = Entry.location ~location:loc ~location_function in
                let invariant = Entry.invariant (CilType.Exp.show inv) in
                let entry = Entry.loop_invariant ~task ~location ~invariant in
                entry :: acc
              ) acc invs
          | `Bot | `Top -> (* TODO: 0 for bot (dead code)? *)
            acc
        )
        else
          acc
      ) nh []
    in

    (* Generate flow-insensitive invariants *)
    let entries = GHT.fold (fun g v acc ->
        match g with
        | `Left g -> (* Spec global *)
          begin match Query.ask_global gh (InvariantGlobal (Obj.repr g)) with
            | `Lifted inv ->
              let invs = WitnessUtil.InvariantExp.process_exp inv in
              List.fold_left (fun acc inv ->
                  let invariant = Entry.invariant (CilType.Exp.show inv) in
                  let entry = Entry.flow_insensitive_invariant ~task ~invariant in
                  entry :: acc
                ) acc invs
            | `Bot | `Top -> (* global bot might only be possible for alloc variables, if at all, so emit nothing *)
              acc
          end
        | `Right _ -> (* contexts global *)
          acc
      ) gh entries
    in

    (* Generate precondition invariants.
       We do this in three steps:
       1. Collect contexts for each function
       2. For each function context, find "matching"/"weaker" contexts that may satisfy its invariant
       3. Generate precondition invariants. The postcondition is a disjunction over the invariants for matching states. *)

    (* 1. Collect contexts for each function *)
    (* TODO: Use [IterSysVars] for this when #391 is merged. *)
    let fun_contexts : con_inv list FMap.t = FMap.create 103 in
    LHT.iter (fun ((n, c) as lvar) local ->
        begin match n with
          | FunctionEntry f ->
            let invariant = Query.ask_local gh lvar local (Invariant Invariant.default_context) in
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
                    let x = Query.ask_local gh (c.node, c.context) c.state (Queries.EvalInt c_inv) in
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
    let entries = LHT.fold (fun ((n, c) as lvar) local acc ->
        if is_invariant_node n then (
          let fundec = Node.find_fundec n in
          let pre_lvar = (Node.FunctionEntry fundec, c) in
          let pre_local = LHT.find lh pre_lvar in
          let query = Queries.Invariant Invariant.default_context in
          match Query.ask_local gh pre_lvar pre_local query with
          | `Lifted c_inv ->
            let loc = Node.location n in
            (* Find unknowns for which the preceding start state satisfies the precondtion *)
            let xs = find_matching_states lvar in

            (* Generate invariants. Give up in case one invariant could not be generated. *)
            let invs = GobList.fold_while_some (fun acc local ->
                let lvals = local_lvals n local in
                match Query.ask_local_node gh n local (Invariant {Invariant.default_context with lvals}) with
                | `Lifted c -> Some ((`Lifted c)::acc)
                | `Bot | `Top -> None
              ) [] xs
            in
            begin match invs with
              | None
              | Some [] -> acc
              | Some (x::xs) ->
                begin match List.fold_left (fun acc inv -> Invariant.(acc || inv)) x xs with
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
        )
        else
          acc
      ) lh entries
    in

    let yaml_entries = List.rev_map YamlWitnessType.Entry.to_yaml entries in (* reverse to make entries in file in the same order as generation messages *)

    M.msg_group Info ~category:Witness "witness generation summary" [
      (Pretty.dprintf "total: %d" (List.length yaml_entries), None);
    ];

    yaml_entries_to_file yaml_entries (Fpath.v (GobConfig.get_string "witness.yaml.path"))
end


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

module Validator
    (Spec : Spec)
    (EQSys : GlobConstrSys with module LVar = VarF (Spec.C)
                            and module GVar = GVarF (Spec.V)
                            and module D = Spec.D
                            and module G = GVarG (Spec.G) (Spec.C))
    (LHT : BatHashtbl.S with type key = EQSys.LVar.t)
    (GHT : BatHashtbl.S with type key = EQSys.GVar.t) =
struct
  module Locator = WitnessUtil.Locator (EQSys.LVar)
  module LvarS = Locator.ES
  module InvariantParser = WitnessUtil.InvariantParser
  module VR = ValidationResult
  module Query = Query (Spec) (EQSys) (GHT)

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

  let validate lh gh (file: Cil.file) =
    let locator = Locator.create () in
    LHT.iter (fun ((n, _) as lvar) _ ->
        let loc = Node.location n in
        if not loc.synthetic then
          Locator.add locator loc lvar
      ) lh;

    let inv_parser = InvariantParser.create file in

    let ask_local = Query.ask_local gh in

    let yaml = Yaml_unix.of_file_exn (Fpath.v (GobConfig.get_string "witness.yaml.validate")) in
    let yaml_entries = yaml |> GobYaml.list |> BatResult.get_ok in

    let cnt_confirmed = ref 0 in
    let cnt_unconfirmed = ref 0 in
    let cnt_refuted = ref 0 in
    let cnt_unchecked = ref 0 in
    let cnt_unsupported = ref 0 in
    let cnt_error = ref 0 in

    let validate_entry (entry: YamlWitnessType.Entry.t): YamlWitnessType.Entry.t option =
      let uuid = entry.metadata.uuid in
      let target_type = YamlWitnessType.EntryType.entry_type entry.entry_type in

      let validate_lvars_invariant ~entry_certificate ~loc ~lvars inv =
        let msgLoc: M.Location.t = CilLocation loc in
        match InvariantParser.parse_cabs inv with
        | Ok inv_cabs ->

          let result = LvarS.fold (fun ((n, _) as lvar) (acc: VR.t) ->
              let d = LHT.find lh lvar in
              let fundec = Node.find_fundec n in

              let result: VR.result = match InvariantParser.parse_cil inv_parser ~fundec ~loc inv_cabs with
                | Ok inv_exp ->
                  let x = ask_local lvar d (Queries.EvalInt inv_exp) in
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
              let target = Entry.target ~uuid ~type_:target_type ~file_name:loc.file in
              let certification = Entry.certification true in
              let certificate_entry = entry_certificate ~target ~certification in
              Some certificate_entry
            | Unconfirmed ->
              incr cnt_unconfirmed;
              M.warn ~category:Witness ~loc:msgLoc "invariant unconfirmed: %s" inv;None
            | Refuted ->
              incr cnt_refuted;
              M.error ~category:Witness ~loc:msgLoc "invariant refuted: %s" inv;
              let target = Entry.target ~uuid ~type_:target_type ~file_name:loc.file in
              let certification = Entry.certification false in
              let certificate_entry = entry_certificate ~target ~certification in
              Some certificate_entry
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

      let validate_loop_invariant (loop_invariant: YamlWitnessType.LoopInvariant.t) =
        let loc = loc_of_location loop_invariant.location in
        let inv = loop_invariant.loop_invariant.string in
        let entry_certificate = Entry.loop_invariant_certificate in
        let msgLoc: M.Location.t = CilLocation loc in

        match Locator.find_opt locator loc with
        | Some lvars ->
          validate_lvars_invariant ~entry_certificate ~loc ~lvars inv
        | None ->
          incr cnt_error;
          M.warn ~category:Witness ~loc:msgLoc "couldn't locate invariant: %s" inv;
          None
      in

      let validate_precondition_loop_invariant (precondition_loop_invariant: YamlWitnessType.PreconditionLoopInvariant.t) =
        let loc = loc_of_location precondition_loop_invariant.location in
        let pre = precondition_loop_invariant.precondition.string in
        let inv = precondition_loop_invariant.loop_invariant.string in
        let entry_certificate = Entry.precondition_loop_invariant_certificate in
        let msgLoc: M.Location.t = CilLocation loc in

        match Locator.find_opt locator loc with
        | Some lvars ->
          begin match InvariantParser.parse_cabs pre with
            | Ok pre_cabs ->

              let precondition_holds ((n, c) as lvar) =
                let fundec = Node.find_fundec n in
                let pre_d = LHT.find lh (FunctionEntry fundec, c) in

                match InvariantParser.parse_cil inv_parser ~fundec ~loc pre_cabs with
                | Ok pre_exp ->
                  let x = ask_local lvar pre_d (Queries.EvalInt pre_exp) in
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

      match entry.entry_type with
      | LoopInvariant x ->
        validate_loop_invariant x
      | PreconditionLoopInvariant x ->
        validate_precondition_loop_invariant x
      | _ ->
        incr cnt_unsupported;
        M.info_noloc ~category:Witness "cannot validate entry of type %s" target_type;
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
          M.info_noloc ~category:Witness "couldn't parse entry: %s" e;
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
      (Pretty.dprintf "total: %d" (!cnt_confirmed + !cnt_unconfirmed + !cnt_refuted + !cnt_unchecked + !cnt_unsupported + !cnt_error), None);
    ];

    yaml_entries_to_file (List.rev yaml_entries') (Fpath.v (GobConfig.get_string "witness.yaml.certificate"))
end
