open Analyses

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


module Make
    (File: WitnessUtil.File)
    (Cfg: MyCFG.CfgBidir)
    (Spec : Spec)
    (EQSys : GlobConstrSys with module LVar = VarF (Spec.C)
                            and module GVar = GVarF (Spec.V)
                            and module D = Spec.D
                            and module G = Spec.G)
    (LHT : BatHashtbl.S with type key = EQSys.LVar.t)
    (GHT : BatHashtbl.S with type key = EQSys.GVar.t) =
struct

  module NH = BatHashtbl.Make (Node)
  module WitnessInvariant = WitnessUtil.Invariant (File) (Cfg)

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

    let entries = NH.fold (fun n local acc ->
        match n with
        | Statement _ when WitnessInvariant.is_invariant_node n ->
          let context: Invariant.context = {
            scope=Node.find_fundec n;
            i = -1;
            lval=None;
            offset=Cil.NoOffset;
            deref_invariant=(fun _ _ _ -> Invariant.none) (* TODO: should throw instead? *)
          }
          in
          begin match Spec.D.invariant context local with
            | Some inv ->
              let loc = Node.location n in
              let invs = WitnessUtil.InvariantExp.process_exp inv in
              List.fold_left (fun acc inv ->
                  let location_function = (Node.find_fundec n).svar.vname in
                  let location = Entry.location ~location:loc ~location_function in
                  let invariant = Entry.invariant (CilType.Exp.show inv) in
                  let entry = Entry.loop_invariant ~task ~location ~invariant in
                  entry :: acc
                ) acc invs
            | None ->
              acc
          end
        | _ -> (* avoid FunctionEntry/Function because their locations are not inside the function where assert could be inserted *)
          acc
      ) nh []
    in

    (* TODO: deduplicate *)
    let entries = LHT.fold (fun (n, c) local acc ->
        match n with
        | Statement _ when WitnessInvariant.is_invariant_node n ->
          let context: Invariant.context = {
            scope=Node.find_fundec n;
            i = -1;
            lval=None;
            offset=Cil.NoOffset;
            deref_invariant=(fun _ _ _ -> Invariant.none) (* TODO: should throw instead? *)
          }
          in
          begin match Spec.C.invariant context c, Spec.D.invariant context local with
            | Some c_inv, Some inv ->
              let loc = Node.location n in
              (* TODO: group by precondition *)
              let c_inv = InvariantCil.exp_replace_original_name c_inv in (* cannot be split *)
              let invs = WitnessUtil.InvariantExp.process_exp inv in
              List.fold_left (fun acc inv ->
                  let location_function = (Node.find_fundec n).svar.vname in
                  let location = Entry.location ~location:loc ~location_function in
                  let precondition = Entry.invariant (CilType.Exp.show c_inv) in
                  let invariant = Entry.invariant (CilType.Exp.show inv) in
                  let entry = Entry.precondition_loop_invariant ~task ~location ~precondition ~invariant in
                  entry :: acc
                ) acc invs
            | _, _ -> (* TODO: handle some other combination? *)
              acc
          end
        | _ -> (* avoid FunctionEntry/Function because their locations are not inside the function where assert could be inserted *)
          acc
      ) lh entries
    in

    let yaml = `A (List.map YamlWitnessType.Entry.to_yaml entries) in
    Yaml_unix.to_file_exn (Fpath.v (GobConfig.get_string "witness.yaml.path")) yaml
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
    let n = max_result - 1
    let names i = show_result (Option.get (result_of_enum i))
  end
  include Lattice.Chain (ChainParams)
end

module Validator
    (Spec : Spec)
    (EQSys : GlobConstrSys with module LVar = VarF (Spec.C)
                            and module GVar = GVarF (Spec.V)
                            and module D = Spec.D
                            and module G = Spec.G)
    (LHT : BatHashtbl.S with type key = EQSys.LVar.t)
    (GHT : BatHashtbl.S with type key = EQSys.GVar.t) =
struct
  module Locator = WitnessUtil.Locator (EQSys.LVar)
  module LvarS = Locator.ES
  module InvariantParser = WitnessUtil.InvariantParser

  let loc_of_location (location: YamlWitnessType.Location.t): Cil.location = {
    file = location.file_name;
    line = location.line;
    column = location.column + 1;
    byte = -1;
    endLine = -1;
    endColumn = -1;
    endByte = -1;
  }

  let validate lh gh (file: Cil.file) =
    let locator = Locator.create () in
    LHT.iter (fun ((n, _) as lvar) _ ->
        let loc = Node.location n in
        Locator.add locator loc lvar
      ) lh;

    let inv_parser = InvariantParser.create file in

    let ask_local (lvar:EQSys.LVar.t) local =
      (* build a ctx for using the query system *)
      let rec ctx =
        { ask    = (fun (type a) (q: a Queries.t) -> Spec.query ctx q)
        ; emit   = (fun _ -> failwith "Cannot \"emit\" in witness context.")
        ; node   = fst lvar
        ; prev_node = MyCFG.dummy_node
        ; control_context = Obj.repr (fun () -> snd lvar)
        ; context = (fun () -> snd lvar)
        ; edge    = MyCFG.Skip
        ; local  = local
        ; global = GHT.find gh
        ; presub = (fun _ -> raise Not_found)
        ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in witness context.")
        ; split  = (fun d es   -> failwith "Cannot \"split\" in witness context.")
        ; sideg  = (fun v g    -> failwith "Cannot \"sideg\" in witness context.")
        }
      in
      Spec.query ctx
    in

    let yaml = Yaml_unix.of_file_exn (Fpath.v (GobConfig.get_string "witness.yaml.validate")) in
    let yaml_entries = match yaml with
      | `A yaml_entries -> yaml_entries
      | _ -> failwith "invalid YAML"
    in

    let validate_entry (entry: YamlWitnessType.Entry.t): YamlWitnessType.Entry.t option =
      let uuid = entry.metadata.uuid in
      let target_type = YamlWitnessType.EntryType.entry_type entry.entry_type in

      let validate_lvars_invariant ~entry_certificate ~loc ~lvars inv =
        match InvariantParser.parse_cabs inv with
        | Ok inv_cabs ->
          let module VR = ValidationResult in

          let result = LvarS.fold (fun ((n, _) as lvar) (acc: VR.t) ->
              let d = LHT.find lh lvar in
              let fundec = Node.find_fundec n in

              let result: VR.result = match InvariantParser.parse_cil inv_parser ~fundec ~loc inv_cabs with
                | Ok inv_exp ->
                  begin match ask_local lvar d (Queries.EvalInt inv_exp) with
                    | x when Queries.ID.is_bool x ->
                      let verdict = Option.get (Queries.ID.to_bool x) in
                      if verdict then
                        Confirmed
                      else
                        Refuted
                    | _ ->
                      Unconfirmed
                  end
                | Error e ->
                  ParseError
              in
              VR.join acc (VR.result_to_enum result)
            ) lvars (VR.bot ())
          in

          begin match Option.get (VR.result_of_enum result) with
            | Confirmed ->
              M.success ~category:Witness ~loc "invariant confirmed: %s" inv;
              let target = Entry.target ~uuid ~type_:target_type ~file_name:loc.file in
              let certification = Entry.certification true in
              let certificate_entry = entry_certificate ~target ~certification in
              Some certificate_entry
            | Unconfirmed ->
              M.warn ~category:Witness ~loc "invariant unconfirmed: %s" inv;None
            | Refuted ->
              M.error ~category:Witness ~loc "invariant refuted: %s" inv;
              let target = Entry.target ~uuid ~type_:target_type ~file_name:loc.file in
              let certification = Entry.certification false in
              let certificate_entry = entry_certificate ~target ~certification in
              Some certificate_entry
            | ParseError ->
              M.error ~category:Witness ~loc "CIL couldn't parse invariant: %s" inv;
              M.info ~category:Witness ~loc "invariant has undefined variables or side effects: %s" inv;
              None
          end
        | Error e ->
          M.error ~category:Witness ~loc "Frontc couldn't parse invariant: %s" inv;
          M.info ~category:Witness ~loc "invariant has invalid syntax: %s" inv;
          None
      in

      let validate_loop_invariant (loop_invariant: YamlWitnessType.LoopInvariant.t) =
        let loc = loc_of_location loop_invariant.location in
        let inv = loop_invariant.loop_invariant.string in
        let entry_certificate = Entry.loop_invariant_certificate in

        match Locator.find_opt locator loc with
        | Some lvars ->
          validate_lvars_invariant ~entry_certificate ~loc ~lvars inv
        | None ->
          M.warn ~category:Witness ~loc "couldn't locate invariant: %s" inv;
          None
      in

      let validate_precondition_loop_invariant (precondition_loop_invariant: YamlWitnessType.PreconditionLoopInvariant.t) =
        let loc = loc_of_location precondition_loop_invariant.location in
        let pre = precondition_loop_invariant.precondition.string in
        let inv = precondition_loop_invariant.loop_invariant.string in
        let entry_certificate = Entry.precondition_loop_invariant_certificate in

        match Locator.find_opt locator loc with
        | Some lvars ->
          begin match InvariantParser.parse_cabs pre with
            | Ok pre_cabs ->

              let precondition_holds ((n, c) as lvar) =
                let fundec = Node.find_fundec n in
                let pre_d = LHT.find lh (FunctionEntry fundec, c) in

                match InvariantParser.parse_cil inv_parser ~fundec ~loc pre_cabs with
                | Ok pre_exp ->
                  begin match ask_local lvar pre_d (Queries.EvalInt pre_exp) with
                    | x when Queries.ID.is_bool x ->
                      Option.get (Queries.ID.to_bool x)
                    | _ ->
                      false (* unknown precondition is excluded from checking *)
                  end
                | Error e ->
                  M.error ~category:Witness ~loc "CIL couldn't parse precondition: %s" inv;
                  M.info ~category:Witness ~loc "precondition has undefined variables or side effects: %s" inv;
                  false
              in

              let lvars = LvarS.filter precondition_holds lvars in
              if LvarS.is_empty lvars then (
                M.warn ~category:Witness ~loc "precondition never definitely holds: %s" pre;
                None
              )
              else
                validate_lvars_invariant ~entry_certificate ~loc ~lvars inv
            | Error e ->
              M.error ~category:Witness ~loc "Frontc couldn't parse precondition: %s" pre;
              M.info ~category:Witness ~loc "precondition has invalid syntax: %s" pre;
              None
          end
        | None ->
          M.warn ~category:Witness ~loc "couldn't locate invariant: %s" inv;
          None
      in

      match entry.entry_type with
      | LoopInvariant x ->
        validate_loop_invariant x
      | PreconditionLoopInvariant x ->
        validate_precondition_loop_invariant x
      | _ ->
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
          M.info_noloc ~category:Witness "couldn't parse entry: %s" e;
          yaml_entry :: yaml_entries'
      ) [] yaml_entries
    in

    let yaml' = `A (List.rev yaml_entries') in
    Yaml_unix.to_file_exn (Fpath.v (GobConfig.get_string "witness.yaml.certificate")) yaml'
end
