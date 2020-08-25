module LocationMap = Map.Make
  (struct
    type t = Cil.location
    let compare (location_1 : t) (location_2 : t) =
      compare location_1.byte location_2.byte
  end)

type expression_query_mode =
  | Must [@name "must"]
  | May [@name "may"]
  [@@deriving yojson]
type expression_query =
  {
    kind : SyntacticalAnalyzer.JsonParser.kind; [@key "kind"]
    target : SyntacticalAnalyzer.JsonParser.target; [@key "target"]
    find : SyntacticalAnalyzer.JsonParser.find; [@key "find"]
    expression : string; [@key "expression"]
    mode : expression_query_mode; [@key "mode"]
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

    val mutable debug : (Cil.location * (Cil.stmt * (Cil.location * Cil.stmt) option) option) list = []

    method get_debug () =
      debug

    method evaluate query =
      debug <- [];
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
      let evaluations =
        SyntacticalAnalyzer.QueryMapping.map_query syntax_query file
          (* Use only location results *)
          |> List.map (fun (_, location, _, _) -> location)
          (* Sort locations *)
          |> List.sort (fun (location_1 : Cil.location) (location_2 : Cil.location) -> compare location_1.byte location_2.byte)

          (***)

          (* Get updated location using statements *)
          |> List.map
            begin
              fun preceding_location ->
                match LocationMap.find_opt preceding_location statements with
                | Some preceding_statement ->
                    if List.length preceding_statement.succs = 1 then
                      begin
                        let succeeding_statement = (List.hd preceding_statement.succs) in
                        let succeeding_location = Cil.get_stmtLoc succeeding_statement.skind in
                        debug <- (preceding_location, Some (preceding_statement, Some (succeeding_location, succeeding_statement)))::debug;
                        Some succeeding_location
                      end
                    else
                      begin
                        debug <- (preceding_location, Some (preceding_statement, None))::debug;
                        Some preceding_location
                      end
                | None ->
                    debug <- (preceding_location, None)::debug;
                    None
            end

          (***)

          (* Evaluate at statement locations *)
          |> List.map
            begin
              function
              | Some location ->
                  Some
                    begin
                      match ask location (Queries.EvalInt (Formatcil.cExp query.expression variables)) with
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
              | None -> None
            end
      in
      debug <- List.rev debug;
      evaluations

  end

let _ =

  let module ExpressionEvaluationTransform : Transform.S =
    struct

      let padding = 5

      let string_of_int_padded value =
        let value_string = string_of_int value in
        (String.make (padding - String.length value_string) ' ') ^ value_string
      let string_of_statement statement =
        statement
          |> Cil.d_stmt ()
          |> Pretty.sprint ~width:0
          |> String.split_on_char '\n'
          |> List.filter (fun line -> line.[0] <> '#')
          |> List.map String.trim
          |> List.fold_left (^) ""

      let transform (ask : Cil.location -> Queries.t -> Queries.Result.t) (file : Cil.file) =
        let query_file_name = GobConfig.get_string "trans.expeval.query_file_name" in
        print_endline ("Using query file: \"" ^ query_file_name ^ "\"");
        let query =
          match Yojson.Safe.from_file query_file_name |> expression_query_of_yojson with
          | Ok parsed_query -> parsed_query
          | Error _ ->
              print_endline "Parsing error";
              raise Exit
        in
        let evaluator = new expression_evaluator ask file in
        let evaluations = evaluator#evaluate query |> Array.of_list in
        let evaluation_debug = evaluator#get_debug () |> Array.of_list in
        (* Show results *)
        for index = 0 to Array.length evaluations - 1 do
          print_endline (String.make padding '-');
          let evaluation = evaluations.(index) in
          begin
            match evaluation_debug.(index) with
            | (p_location, Some (p_statement, Some (s_location, s_statement))) ->
                print_endline ((p_location.line |> string_of_int_padded) ^ ": " ^ (p_statement |> string_of_statement));
                if s_location.line < 0 then
                  print_string (String.make padding ' ')
                else
                  print_string (s_location.line |> string_of_int_padded);
                print_endline (": " ^ (s_statement |> string_of_statement))
            | (p_location, Some (p_statement, None)) ->
                print_endline ((p_location.line |> string_of_int_padded) ^ ": " ^ (p_statement |> string_of_statement));
                print_string ((String.make padding ' ') ^ "  No successor")
            | (p_location, None) ->
                print_endline ((p_location.line |> string_of_int_padded) ^ ": No statement")
          end;
          print_string ((String.make padding ' ') ^ "  -> ");
          print_endline
            begin
              match evaluation with
              | Some Some Some value -> Int64.to_string value
              | Some Some None -> "Unknown"
              | Some None -> "Not reachable or false/zero"
              | None -> "Not evaluable"
            end;
        done

    end
  in
  Transform.register "expeval" (module ExpressionEvaluationTransform)
