module TransformationBase = Transform
module Transform : TransformationBase.S =
  struct

    module LocationMap = Map.Make
      (struct
        type t = Cil.location
        let compare (location_1 : t) (location_2 : t) =
          compare location_1.byte location_2.byte
      end)

    let get_statements functions =
      let rec resolve_statements (block : Cil.block) =
        block.bstmts
          |> List.map expand
          |> List.flatten
      and expand statement =
        match statement.skind with
        | (Cil.Instr _ | Cil.Return _ | Cil.Goto _ | Cil.Break _ | Cil.Continue _) ->
            [statement]
        | (Cil.Switch (_, block, _, _) | Cil.Loop (block, _, _, _) | Cil.Block block) ->
            (resolve_statements block)
        | Cil.If (_, block_1, block_2, _) ->
            (resolve_statements block_1) @ (resolve_statements block_2)
        | _ ->
            []
      in
      functions
        (* Take all statements *)
        |> List.map (fun (f : Cil.fundec) -> resolve_statements f.sbody |> List.map (fun s -> f, s))
        |> List.flatten
        (* Add locations *)
        |> List.map (fun (f, (s : Cil.stmt)) -> (Cil.get_stmtLoc s.skind, f, s))
        (* Filter artificial ones by impossible location *)
        |> List.filter (fun ((l : Cil.location), _, _) -> l.line >= 0)
        (* Transform to mapping *)
        |> List.fold_left (fun statements (l, f, s) -> LocationMap.add l (f, s) statements) LocationMap.empty

    let transform ask (file : Cil.file) =
      let functions = List.filter_map (function Cil.GFun (f, _) -> Some f | _ -> None) file.globals in

      let query_file_name = "test.json" in (* TODO *)

      let query = SyntacticalAnalyzer.JsonParser.parse_json_file query_file_name in
      let query_result = SyntacticalAnalyzer.QueryMapping.map_query query file in

      print_endline (SyntacticalAnalyzer.ResultPrinter.print_result query_result query);

      let variable_name = match query.tar with SyntacticalAnalyzer.JsonParser.Name_t name -> name | _ -> raise Exit in
      let variables =
        functions
          |> List.map (fun (f : Cil.fundec) -> f.slocals)
          |> List.flatten
          |> List.fold_left (fun variables (v : Cil.varinfo) -> Hashtbl.add variables v.vid v; variables) (Hashtbl.create 0)
      in

      let global_translation =
        file.globals
          |> List.filter_map (function Cil.GVar (variable, _, _) -> Some (variable.vname, Cil.Fv variable) | _ -> None)
      in

      let statements = get_statements functions in

      print_string "Expression: ";
      flush stdout;
      let expression_string = read_line () in (* TODO *)

      let evaluate_single (_, location, _, identifier) =
        let delimiter = "-----" in
        print_endline delimiter;
        match LocationMap.find_opt location statements with
        | Some (function_declaration, statement) ->
            let translation =
              match Hashtbl.find_opt variables identifier with
              | Some variable -> (variable_name, Cil.Fv variable)::global_translation
              | None -> global_translation
            in
            let expression = Formatcil.cExp expression_string translation in
            (match ask location (Queries.EvalInt expression) with
            | `Bot -> "Bot"
            | `Int value -> IntDomain.FlatPureIntegers.to_yojson value |> Yojson.Safe.to_string
            | `Top -> "Top"
            | _ -> raise Exit)
              |> print_endline
        | None ->
            print_endline "No statement"
      in
      List.iter evaluate_single query_result
  end

let _ =
  TransformationBase.register "expeval" (module Transform)
