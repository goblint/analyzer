let transformation_name = "expeval"

module Transformation : Transform.S =
  struct

    type evaluation_mode =
      | Must [@name "must"]
      | May [@name "may"]
      [@@deriving yojson]
    type query =
      {
        kind : SyntacticalAnalyzer.JsonParser.kind; [@key "kind"]
        target : SyntacticalAnalyzer.JsonParser.target; [@key "target"]
        find : SyntacticalAnalyzer.JsonParser.find; [@key "find"]
        expression : string; [@key "expression"]
        mode : evaluation_mode; [@key "mode"]
      }
      [@@deriving yojson]

    class evaluator (file : Cil.file) (ask : Cil.location -> Queries.t -> Queries.Result.t) =

      let global_functions =
        file.globals
          |> List.filter_map (function Cil.GFun (f, _) -> Some f | _ -> None)
      in
      let global_variables =
        file.globals
          |> List.filter_map (function Cil.GVar (v, _, _) -> Some (v.vname, Cil.Fv v) | _ -> None)
      in

      let function_table, statement_table =
        let function_statements =
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
          global_functions
            (* Take all statements *)
            |> List.map (fun (f : Cil.fundec) -> resolve_statements f.sbody |> List.map (fun s -> f, s))
            |> List.flatten
            (* Add locations *)
            |> List.map (fun (f, (s : Cil.stmt)) -> (Cil.get_stmtLoc s.skind, f, s))
            (* Filter artificial ones by impossible location *)
            |> List.filter (fun ((l : Cil.location), _, _) -> l.line >= 0)
        in
        function_statements
          |> List.fold_left (fun statements (l, f, _) -> Hashtbl.add statements l f; statements) (Hashtbl.create 0),
        function_statements
          |> List.fold_left (fun statements (l, _, s) -> Hashtbl.add statements l s; statements) (Hashtbl.create 0)
      in

      object (self)

        val mutable debug_info_1 : bool = false
        val mutable debug_info_2 : (Cil.stmt * (Cil.location * Cil.stmt) option) option = None
        val mutable debug_info_3 : bool option = None

        method get_debug_info_1 () = debug_info_1
        method get_debug_info_2 () = debug_info_2
        method get_debug_info_3 () = debug_info_3

        method evaluate location expression_string =
          debug_info_1 <- false;
          debug_info_2 <- None;
          debug_info_3 <- None;
          match self#get_query location expression_string with
          | Some query ->
              begin
                match self#get_evaluable_location location with
                | Some evaluable_location ->
                    let ask_result =
                      try Some (ask evaluable_location query)
                      with Not_found -> None
                    in
                    begin
                      match self#get_value ask_result with
                      | Some value -> value
                      | None -> Some false
                    end
                | None -> Some false
              end
          | None -> Some false

        method private get_query location expression_string =
          let local_variables =
            match Hashtbl.find_opt function_table location with
            | Some function_declaration ->
                function_declaration.slocals
                  |> List.map (fun (v : Cil.varinfo) -> v.vname, Cil.Fv v)
            | None -> []
          in
          match
            begin
              try Some (Formatcil.cExp expression_string (local_variables @ global_variables))
              with _ -> None
            end
          with
          | Some expression ->
              debug_info_1 <- true;
              Some (Queries.EvalInt expression)
          | None ->
              debug_info_1 <- false;
              None
        method private get_evaluable_location location =
          match Hashtbl.find_opt statement_table location with
          | Some statement ->
              if List.length statement.succs = 1 then
                begin
                  let succeeding_statement = (List.hd statement.succs) in
                  let succeeding_location = Cil.get_stmtLoc succeeding_statement.skind in
                  debug_info_2 <- Some (statement, Some (succeeding_location, succeeding_statement));
                  Some succeeding_location
                end
              else
                begin
                  debug_info_2 <- Some (statement, None);
                  Some location
                end
          | None ->
              debug_info_2 <- None;
              None
        method private get_value =
          function
          | Some `Bot ->
              debug_info_3 <- None;
              None
          | Some `Int value ->
              debug_info_3 <- Some true;
              Some (Some (value <> Int64.zero))
          | Some `Top ->
              debug_info_3 <- Some false;
              Some None
          | Some _ -> raise Exit
          | None ->
              debug_info_3 <- None;
              None

      end

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

    let parse_query () =
      let query_file_name = GobConfig.get_string ("trans." ^ transformation_name ^ ".query_file_name") in
      match Yojson.Safe.from_file query_file_name |> query_of_yojson with
      | Ok parsed_query -> parsed_query
      | Error message ->
          print_endline ("Parsing error: " ^ message);
          raise Exit

    let transform (ask : Cil.location -> Queries.t -> Queries.Result.t) (file : Cil.file) =
      let evaluator = new evaluator file ask in
      let query = parse_query () in
      let query_syntactic : SyntacticalAnalyzer.JsonParser.query =
        {
          sel = [];
          k = query.kind;
          tar = query.target;
          f = query.find;
          str = None_s;
          lim = None_c;
        }
      in
      SyntacticalAnalyzer.QueryMapping.map_query query_syntactic file
        (* Use only location results *)
        |> List.map (fun (_, l, _, _) -> l)
        (* Sort locations *)
        |> List.sort (fun (l_1 : Cil.location) (l_2 : Cil.location) -> compare l_1.byte l_2.byte)
        (* Evaluate *)
        |> List.iter
          begin
            fun location ->
              begin
                match evaluator#evaluate location query.expression with
                | Some value ->
                    if value then
                      print_endline (location.line |> string_of_int_padded)
                | None ->
                    begin
                      match query.mode with
                      | Must -> ()
                      | May -> print_endline ((location.line |> string_of_int_padded) ^ " (Possibly)")
                    end
              end;
              (* TODO: Debug output *)
          end

  end

let _ =
  Transform.register transformation_name (module Transformation)
