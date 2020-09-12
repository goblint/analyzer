let transformation_name = "expeval"

module Transformation : Transform.S =
  struct

    type query =
      {
        kind : SyntacticalAnalyzer.JsonParser.kind;
        target : SyntacticalAnalyzer.JsonParser.target;
        find : SyntacticalAnalyzer.JsonParser.find;
        structure : (SyntacticalAnalyzer.JsonParser.structure [@default None_s]);
        limitation : (SyntacticalAnalyzer.JsonParser.constr [@default None_c]);
        expression : string;
        mode :
          [
          | `Must
          | `May
          ];
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
          global_functions
            (* Take all statements *)
            |> List.map (fun (f : Cil.fundec) -> f.sallstmts |> List.map (fun s -> f, s))
            |> List.flatten
            (* Add locations *)
            |> List.map (fun (f, (s : Cil.stmt)) -> (Cil.get_stmtLoc s.skind, f, s))
            (* Filter artificial ones by impossible location *)
            |> List.filter (fun ((l : Cil.location), _, _) -> l.line >= 0)
        in
        function_statements
          |> List.fold_left (fun functions (l, f, _) -> Hashtbl.add functions l f; functions) (Hashtbl.create 0),
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
                match self#get_value location query with
                | Some value ->
                    begin
                      match self#get_succeeding_location location with
                      | Some succeeding_location ->
                          if succeeding_location = location then
                            value
                          else
                            begin
                              match self#get_value succeeding_location query with
                              | Some succeeding_value -> succeeding_value
                              | None -> value
                            end
                      | None -> Some false (* Not necessary *)
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
          let parse_result =
            try Some (Formatcil.cExp expression_string (local_variables @ global_variables))
            with _ -> None
          in
          match parse_result with
          | Some expression ->
              debug_info_1 <- true;
              Some (Queries.EvalInt expression)
          | None ->
              debug_info_1 <- false;
              None
        method private get_succeeding_location location =
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
        method private get_value location query =
          let ask_result =
            try Some (ask location query)
            with Not_found -> None
          in
          match ask_result with
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
    let string_of_location (location : Cil.location) =
      location.file ^ ":" ^ (location.line |> string_of_int) ^ " [" ^ (location.byte |> string_of_int) ^ "]"
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
          str = query.structure;
          lim = query.limitation;
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
                      print_endline (location |> string_of_location)
                | None ->
                    begin
                      match query.mode with
                      | `Must -> ()
                      | `May -> print_endline ((location |> string_of_location) ^ " (Possibly)")
                    end
              end;
              print_endline ">>>";
              print_endline ("\"" ^ location.file ^ "\":");
              print_string ((location.line |> string_of_int_padded) ^ " | ");
              begin
                match evaluator#get_debug_info_2 () with
                | Some (statement, successor) ->
                    print_endline (statement |> string_of_statement);
                    begin
                      match successor with
                      | Some (succeeding_location, succeeding_statement) ->
                          if succeeding_location.line < 1 then
                            print_string (String.make padding ' ')
                          else
                            print_string (succeeding_location.line |> string_of_int_padded);
                          print_endline (" | " ^ (succeeding_statement |> string_of_statement))
                      | None ->
                          print_endline ((String.make padding ' ') ^ " | (No succeeding statement)")
                    end
                | None ->
                    print_endline "(No statement)"
              end;
              if evaluator#get_debug_info_1 () then
                print_endline "Successfully parsed expression"
              else
                print_endline "Failed to parse expression";
              print_endline
                begin
                  match evaluator#get_debug_info_3 () with
                  | Some value ->
                      if value then
                        "Definite"
                      else
                        "Unknown"
                  | None ->
                      "Not reachable"
                end;
              print_endline "<<<";
              print_newline ()
          end

  end

let _ =
  Transform.register transformation_name (module Transformation)
