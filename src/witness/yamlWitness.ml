open Analyses

let uuid_random_state = Random.State.make_self_init ()

let sha256_file f = Sha256.(to_hex (file f))
let sha256_file_cache = BatCache.make_ht ~gen:sha256_file ~init_size:5
let sha256_file = sha256_file_cache.get

module Entry =
struct
  (* yaml_conf is too verbose *)
  (* let yaml_conf: Yaml.value = Json_repr.convert (module Json_repr.Yojson) (module Json_repr.Ezjsonm) (!GobConfig.json_conf) in *)
  let yaml_producer = `O [
      ("name", `String "Goblint");
      ("version", `String Version.goblint);
      (* TODO: configuration *)
      (* ("configuration", yaml_conf); *) (* yaml_conf is too verbose *)
      ("command_line", `String Goblintutil.command_line);
      (* TODO: description *)
    ]

  let yaml_metadata ?(extra=[]) () =
    let uuid = Uuidm.v4_gen uuid_random_state () in
    let creation_time = TimeUtil.iso8601_now () in
    `O ([
        ("format_version", `String "0.1");
        ("uuid", `String (Uuidm.to_string uuid));
        ("creation_time", `String creation_time);
        ("producer", yaml_producer);
      ] @ extra)

  let yaml_task ~input_files ~data_model ~specification =
    `O ([
        ("input_files", `A (List.map Yaml.Util.string input_files));
        ("input_file_hashes", `O (List.map (fun file ->
             (file, `String (sha256_file file))
           ) input_files));
        ("data_model", `String data_model);
        ("language", `String "C");
      ] @ match specification with
      | Some specification -> [
          ("specification", `String specification)
        ]
      | None ->
        []
      )

  let yaml_loop_invariant ~yaml_task ~location:(loc:Cil.location) ~location_function ~invariant =
    `O [
      ("entry_type", `String "loop_invariant");
      ("metadata", yaml_metadata ~extra:[
          ("task", yaml_task);
        ] ());
      ("location", `O [
          ("file_name", `String loc.file);
          ("file_hash", `String (sha256_file loc.file));
          ("line", `Float (float_of_int loc.line));
          ("column", `Float (float_of_int (loc.column - 1)));
          ("function", `String location_function);
        ]);
      ("loop_invariant", `O [
          ("string", `String invariant);
          ("type", `String "assertion");
          ("format", `String "C");
        ]);
    ]

  let yaml_loop_invariant_certificate ~target_uuid ~target_file_name ~verdict =
    `O [
      ("entry_type", `String "loop_invariant_certificate");
      ("metadata", yaml_metadata ());
      ("target", `O [
          ("uuid", `String target_uuid);
          ("type", `String "loop_invariant"); (* TODO: check *)
          ("file_hash", `String (sha256_file target_file_name));
        ]);
      ("certification", `O [
          ("string", `String (if verdict then "confirmed" else "rejected"));
          ("type", `String "verdict");
          ("format", `String "confirmed | rejected");
        ]);
    ]
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
    let yaml_task = Entry.yaml_task ~input_files ~data_model ~specification in

    let nh = join_contexts lh in

    let yaml_entries = NH.fold (fun n local acc ->
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
                  let invariant = CilType.Exp.show inv in
                  let entry = Entry.yaml_loop_invariant ~yaml_task ~location:loc ~location_function ~invariant in
                  entry :: acc
                ) acc invs
            | None ->
              acc
          end
        | _ -> (* avoid FunctionEntry/Function because their locations are not inside the function where assert could be inserted *)
          acc
      ) nh []
    in

    let yaml = `A yaml_entries in
    Yaml_unix.to_file_exn (Fpath.v (GobConfig.get_string "witness.yaml.path")) yaml
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

  module FileH = BatHashtbl.Make (Basetype.RawStrings)
  module LocM = BatMap.Make (CilType.Location)
  module LvarS = BatSet.Make (EQSys.LVar)

  let validate lh gh (file: Cil.file) =
    (* for each file, locations (of lvar nodes) have total order, so LocM essentially does binary search *)
    let file_loc_lvars: LvarS.t LocM.t FileH.t = FileH.create 100 in
    LHT.iter (fun ((n, _) as lvar) _ ->
        let loc = Node.location n in
        FileH.modify_def LocM.empty loc.file (
          LocM.modify_def LvarS.empty loc (LvarS.add lvar)
        ) file_loc_lvars
      ) lh;

    let global_vars = List.filter_map (function
        | Cil.GVar (v, _, _) -> Some v
        | _ -> None
      ) file.globals
    in

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
        ; postsub= (fun _ -> raise Not_found)
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

    let yaml_entries' = List.fold_left (fun yaml_entries' yaml_entry ->
        let yaml_metadata = Yaml.Util.(yaml_entry |> find_exn "metadata" |> Option.get) in
        let uuid = Yaml.Util.(yaml_metadata |> find_exn "uuid" |> Option.get |> to_string_exn) in
        let yaml_location = Yaml.Util.(yaml_entry |> find_exn "location" |> Option.get) in
        let file = Yaml.Util.(yaml_location |> find_exn "file_name" |> Option.get |> to_string_exn) in
        let line = Yaml.Util.(yaml_location |> find_exn "line" |> Option.get |> to_float_exn |> int_of_float) in
        let column = Yaml.Util.(yaml_location |> find_exn "column" |> Option.get |> to_float_exn |> int_of_float) + 1 in
        let inv = Yaml.Util.(yaml_entry |> find_exn "loop_invariant" |> Option.get |> find_exn "string" |> Option.get |> to_string_exn) in
        let loc: Cil.location = {
          file;
          line;
          column;
          byte = -1;
          endLine = -1;
          endColumn = -1;
          endByte = -1;
        }
        in

        match Frontc.parse_standalone_exp inv with
        | inv_cabs ->

          let lvars_opt: LvarS.t option =
            let (let*) = Option.bind in (* TODO: move to general library *)
            let* loc_lvars = FileH.find_option file_loc_lvars loc.file in
            (* for each file, locations (of lvar nodes) have total order, so LocM essentially does binary search *)
            let* (_, lvars) = LocM.find_first_opt (fun loc' ->
                CilType.Location.compare loc loc' <= 0 (* allow inexact match *)
              ) loc_lvars
            in
            if LvarS.is_empty lvars then
              None
            else
              Some lvars
          in

          begin match lvars_opt with
            | Some lvars ->
              (* TODO: only one certificate per entry *)
              LvarS.fold (fun ((n, _) as lvar) yaml_entries' ->
                  let d = LHT.find lh lvar in
                  let fd = Node.find_fundec n in
                  let vars = fd.sformals @ fd.slocals @ global_vars in

                  let genv = Cabs2cil.genvironment in
                  let env = Hashtbl.copy genv in
                  List.iter (fun (v: Cil.varinfo) ->
                      Hashtbl.replace env v.vname (Cabs2cil.EnvVar v, v.vdecl)
                    ) (fd.sformals @ fd.slocals);

                  let inv_exp_opt =
                    Cil.currentLoc := loc;
                    Cil.currentExpLoc := loc;
                    Cabs2cil.currentFunctionFDEC := fd;
                    let old_locals = fd.slocals in
                    let old_useLogicalOperators = !Cil.useLogicalOperators in
                    Fun.protect ~finally:(fun () ->
                        fd.slocals <- old_locals; (* restore locals, Cabs2cil may mangle them by inserting temporary variables *)
                        Cil.useLogicalOperators := old_useLogicalOperators
                      ) (fun () ->
                        Cil.useLogicalOperators := true;
                        Cabs2cil.convStandaloneExp ~genv ~env inv_cabs
                      )
                  in

                  match inv_exp_opt with
                  | Some inv_exp ->
                    if Check.checkStandaloneExp ~vars inv_exp then (
                      match ask_local lvar d (Queries.EvalInt inv_exp) with
                      | x when Queries.ID.is_bool x ->
                        let verdict = Option.get (Queries.ID.to_bool x) in
                        if verdict then
                          M.success ~category:Witness ~loc "invariant confirmed: %s" inv
                        else
                          M.error ~category:Witness ~loc "invariant refuted: %s" inv;
                        let certificate_entry = Entry.yaml_loop_invariant_certificate ~target_uuid:uuid ~target_file_name:loc.file ~verdict in
                        certificate_entry :: yaml_entry :: yaml_entries'
                      | _ ->
                        M.warn ~category:Witness ~loc "invariant unconfirmed: %s" inv;
                        yaml_entry :: yaml_entries'
                    )
                    else (
                      M.error ~category:Witness ~loc "broken CIL expression invariant: %s (%a)" inv Cil.d_plainexp inv_exp;
                      yaml_entry :: yaml_entries'
                    )
                  | None ->
                    M.error ~category:Witness ~loc "CIL couldn't parse invariant: %s" inv;
                    yaml_entry :: yaml_entries'
                ) lvars yaml_entries'
            | None ->
              M.warn ~category:Witness ~loc "couldn't locate invariant: %s" inv;
              yaml_entry :: yaml_entries'
          end
        | exception Frontc.ParseError _ ->
          Errormsg.log "\n"; (* CIL prints garbage without \n before *)
          M.error ~category:Witness ~loc "Frontc couldn't parse invariant: %s" inv;
          yaml_entry :: yaml_entries'
      ) [] yaml_entries
    in

    let yaml' = `A (List.rev yaml_entries') in
    Yaml_unix.to_file_exn (Fpath.v (GobConfig.get_string "witness.yaml.certificate")) yaml'
end
