module LocationMap = Map.Make
  (struct
    type t = Cil.location
    let compare (location_1 : t) (location_2 : t) =
      compare location_1.byte location_2.byte
  end)

type query_mode =
  | Must [@name "must"]
  | May [@name "may"]
  [@@deriving yojson]
type query =
  {
    kind : SyntacticalAnalyzer.JsonParser.kind; [@key "kind"]
    target : SyntacticalAnalyzer.JsonParser.target; [@key "target"]
    find : SyntacticalAnalyzer.JsonParser.find; [@key "find"]
    mode : query_mode; [@key "mode"]
  }
  [@@deriving yojson]

class expression_evaluator ask (file : Cil.file) =

  (* Get all functions *)
  let functions =
    file.globals
      |> List.filter_map (function Cil.GFun (f, _) -> Some f | _ -> None)
  in

  (* Get all (global and local) variables *)
  let variables =
    (file.globals
      |> List.filter_map (function Cil.GVar (v, _, _) -> Some (v.vname, Cil.Fv v) | _ -> None))
    @
    (functions
      |> List.map (fun (f : Cil.fundec) -> f.slocals)
      |> List.flatten
      |> List.map (fun (v : Cil.varinfo) -> v.vname, Cil.Fv v))
  in

  (* Get all statements *)
  let statements =
    let rec resolve_statements (block : Cil.block) =
      block.bstmts
        |> List.map expand
        |> List.flatten
    and expand statement =
      match statement.skind with
      | (Instr _ | Return _ | Goto _ | Break _ | Continue _) ->
          [statement]
      | (Switch (_, block, _, _) | Loop (block, _, _, _) | Block block) ->
          (resolve_statements block)
      | If (_, block_1, block_2, _) ->
          (resolve_statements block_1) @ (resolve_statements block_2)
      | _ ->
          []
    in
    functions
      (* Find statements in function bodies *)
      |> List.map (fun (f : Cil.fundec) -> resolve_statements f.sbody)
      |> List.flatten
      (* Add their locations *)
      |> List.map (fun (s : Cil.stmt) -> Cil.get_stmtLoc s.skind, s)
      (* Filter artificial ones by impossible location *)
      |> List.filter (fun ((l : Cil.location), _) -> l.line >= 0)
      (* Transform list to map *)
      |> List.fold_left (fun statements (l, s) -> LocationMap.add l s statements) LocationMap.empty
  in

  object (self)

    method evaluate query expression =
      let syntax_query : SyntacticalAnalyzer.JsonParser.query =
        {
          sel = [];
          k = query.kind;
          tar = query.target;
          f = query.find;
          str = None_s;
          lim = None_c;
        }
      in
      SyntacticalAnalyzer.QueryMapping.map_query syntax_query file
        (* Use only location results *)
        |> List.map (fun (_, location, _, _) -> location)
        (* Sort by .line, since .byte does not work here (?) *)
        |> List.sort (fun (location_1 : Cil.location) (location_2 : Cil.location) -> compare location_1.line location_2.line)
        (* Following *)
        |> List.filter_map
          begin
            fun location ->
              match LocationMap.split location statements with
              | (_, Some _, following_statements) ->
                  let following_location, _ = LocationMap.find_first (fun _ -> true) following_statements in
                  begin
                    match ask following_location (Queries.EvalInt (Formatcil.cExp expression variables)) with
                    | `Bot -> None (* Not reachable *)
                    | `Int value ->
                        if value = Int64.zero then
                          None (* False/zero *)
                        else
                          Some (Some value)
                    | `Top ->
                        begin
                          match query.mode with
                          | Must -> None (* Unknown *)
                          | May -> Some None (* TODO use "Query function answered"? *)
                        end
                    | _ -> raise Exit
                  end
              | (_, None, _) -> None (* No statement *)
          end

  end

let _ =

  let module ExpressionEvaluationTransform : Transform.S =
    struct

      let transform (ask : Cil.location -> Queries.t -> Queries.Result.t) (file : Cil.file) =

        let query_file_name = GobConfig.get_string "trans.expeval.query_file_name" in
        let expression = GobConfig.get_string "trans.expeval.expression" in

        print_endline ("Using query file: \"" ^ query_file_name ^ "\"");
        print_endline ("Evaluating expression: \"" ^ expression ^ "\"");

        let query =
          match Yojson.Safe.from_file query_file_name |> query_of_yojson with
          | Ok parsed_query -> parsed_query
          | Error _ -> raise Exit
        in

        let evaluator = new expression_evaluator ask file in
        evaluator#evaluate query expression
          (* Show results *)
          |> List.iter
            begin
              fun value ->
                print_endline
                  begin
                    match value with
                    | Some definite_value -> Int64.to_string definite_value
                    | None -> "Unknown"
                  end
            end

    end
  in
  Transform.register "expeval" (module ExpressionEvaluationTransform)
