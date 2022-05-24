open Analyses

let uuid_random_state = Random.State.make_self_init ()

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
    (* yaml_conf is too verbose *)
    (* let yaml_conf: Yaml.value = Json_repr.convert (module Json_repr.Yojson) (module Json_repr.Ezjsonm) (!GobConfig.json_conf) in *)
    let yaml_creation_time = `String (TimeUtil.iso8601_now ()) in
    let yaml_producer = `O [
        ("name", `String "Goblint");
        ("version", `String Version.goblint);
        (* TODO: configuration *)
        (* ("configuration", yaml_conf); *) (* yaml_conf is too verbose *)
        ("command_line", `String Goblintutil.command_line);
        (* TODO: description *)
      ]
    in
    let files = GobConfig.get_string_list "files" in
    let sha256_file f = Sha256.(to_hex (file f)) in
    let sha256_file_cache = BatCache.make_ht ~gen:sha256_file ~init_size:5 in
    let sha256_file = sha256_file_cache.get in
    let yaml_task = `O ([
        ("input_files", `A (List.map Yaml.Util.string files));
        ("input_file_hashes", `O (List.map (fun file ->
             (file, `String (sha256_file file))
           ) files));
        ("data_model", `String (match GobConfig.get_string "exp.architecture" with
             | "64bit" -> "LP64"
             | "32bit" -> "ILP32"
             | _ -> failwith "invalid architecture"));
        ("language", `String "C");
      ] @ match !Svcomp.task with
      | Some (module Task) -> [
          ("specification", `String (Svcomp.Specification.to_string Task.specification))
        ]
      | None ->
        []
      )
    in

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
                  let uuid = Uuidm.v4_gen uuid_random_state () in
                  let entry = `O [
                      ("entry_type", `String "loop_invariant");
                      ("metadata", `O [
                          ("format_version", `String "0.1");
                          ("uuid", `String (Uuidm.to_string uuid));
                          ("creation_time", yaml_creation_time);
                          ("producer", yaml_producer);
                          ("task", yaml_task);
                        ]);
                      ("location", `O [
                          ("file_name", `String loc.file);
                          ("file_hash", `String (sha256_file loc.file));
                          ("line", `Float (float_of_int loc.line));
                          ("column", `Float (float_of_int (loc.column - 1)));
                          ("function", `String (Node.find_fundec n).svar.vname);
                        ]);
                      ("loop_invariant", `O [
                          ("string", `String (CilType.Exp.show inv));
                          ("type", `String "assertion");
                          ("format", `String "C");
                        ]);
                    ]
                  in
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

  module NH = BatHashtbl.Make (Node)

  (* copied from Constraints.CompareNode *)
  let join_contexts (lh: Spec.D.t LHT.t): Spec.D.t NH.t =
    let nh = NH.create 113 in
    LHT.iter (fun (n, _) d ->
        let d' = try Spec.D.join (NH.find nh n) d with Not_found -> d in
        NH.replace nh n d'
      ) lh;
    nh

  let validate lh gh (file: Cil.file) =

    let global_variables =
      file.globals
      |> List.filter_map (function Cil.GVar (v, _, _) -> Some (v.vname, Cil.Fv v) | _ -> None)
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
    List.iter (fun yaml_entry ->
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

        (* TODO: better node finding *)
        let found = LHT.fold (fun ((n, _) as lvar) d found ->
            let nloc = Node.location n in
            if loc.file = nloc.file && loc.line = nloc.line && loc.column = nloc.column then (
              let fd = Node.find_fundec n in
              let local_variables = (fd.slocals @ fd.sformals) |> List.map (fun (v : Cil.varinfo) -> v.vname, Cil.Fv v) in

              begin match Formatcil.cExp inv (local_variables @ global_variables) with
                | inv_exp ->
                  begin match ask_local lvar d (Queries.EvalInt inv_exp) with
                    | x when Queries.ID.is_bool x ->
                      if Option.get (Queries.ID.to_bool x) then
                        M.success ~category:Witness ~loc "invariant confirmed: %s" inv
                      else
                        M.error ~category:Witness ~loc "invariant refuted: %s" inv
                    | _ ->
                      M.warn ~category:Witness ~loc "invariant unconfirmed: %s" inv
                  end
                | exception _ ->
                  M.error ~category:Witness ~loc "couldn't parse invariant: %s" inv
              end;
              true
            )
            else
              found
          ) lh false
        in
        if not found then
          M.warn ~category:Witness ~loc "couldn't locate invariant: %s" inv
      ) yaml_entries
end
