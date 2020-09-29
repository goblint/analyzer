open Batteries

let transformation_identifier = "expeval"
let transformation_query_file_name_identifier = "trans." ^ transformation_identifier ^ ".query_file_name"

module ExpEval : Transform.S =
  struct

    let (~?) exception_function =
      try Some (exception_function ()) with
      | _ -> None

    class evaluator (file : Cil.file) (ask : Cil.location -> Queries.t -> Queries.Result.t) =
      object (self)

        val global_variables =
          file.globals
            |> List.filter_map (function Cil.GVar (v, _, _) -> Some (v.vname, Cil.Fv v) | _ -> None)
        val statements =
          file.globals
            |> List.filter_map (function Cil.GFun (f, _) -> Some f | _ -> None)
            (* Take all statements *)
            |> List.map (fun (f : Cil.fundec) -> f.sallstmts |> List.map (fun s -> f, s))
            |> List.flatten
            (* Add locations *)
            |> List.map (fun (f, (s : Cil.stmt)) -> (Cil.get_stmtLoc s.skind, f, s))
            (* Filter artificial ones by impossible location *)
            |> List.filter (fun ((l : Cil.location), _, _) -> l.line >= 0)
            (* Create hash table *)
            |> List.fold_left (fun ss (l, f, s) -> Hashtbl.add ss l (f, s); ss) (Hashtbl.create 0)

        method evaluate location expression_string =
          (* Compute the available local variables *)
          let local_variables =
            match Hashtbl.find_option statements location with
            | Some (function_definition, _) -> function_definition.slocals |> List.map (fun (v : Cil.varinfo) -> v.vname, Cil.Fv v)
            | None -> []
          in
          (* Compute evaluation result *)
          match ~? (fun () -> Formatcil.cExp expression_string (local_variables @ global_variables)) with
          | None ->
              Some false
          | Some expression ->
              match self#try_ask location expression with
              | None ->
                  Some false
              | Some value_before ->
                  (* Use the last (TODO -> find_all) matching statement *)
                  let _, statement = Hashtbl.find statements location in
                  (* Use the first (TODO) reachable successor *)
                  let successor_evaluation =
                    statement.succs
                      |> List.find_map_opt (fun (s : Cil.stmt) -> self#try_ask (Cil.get_stmtLoc s.skind) expression)
                  in
                  match successor_evaluation with
                  | Some value_after ->
                      value_after
                  | None ->
                      value_before

        method private try_ask location expression =
          match ~? (fun () -> ask location (Queries.EvalInt expression)) with
            (* Evaluable: Definite *)
          | Some `Int value -> Some (Some (value <> Int64.zero))
            (* Evaluable: Inconclusive *)
          | Some `Top -> Some None
            (* Inapplicable: Unreachable *)
          | Some `Bot -> None
            (* Inapplicable: Unlisted *)
          | None -> None
            (* Unexpected result *)
          | Some _ -> raise Exit

      end

    type query =
      {

        kind : SyntacticalAnalyzer.JsonParser.kind;
        target : SyntacticalAnalyzer.JsonParser.target;
        find : SyntacticalAnalyzer.JsonParser.find;
        structure : (SyntacticalAnalyzer.JsonParser.structure [@default None_s]);
        limitation : (SyntacticalAnalyzer.JsonParser.constr [@default None_c]);

        expression : string;
        mode : [ `Must | `May ];

      } [@@deriving yojson]

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

    let location_file_compare (location_1 : Cil.location) (location_2 : Cil.location) = compare location_1.file location_2.file
    let location_byte_compare (location_1 : Cil.location) (location_2 : Cil.location) = compare location_1.byte location_2.byte

    let transform (ask : Cil.location -> Queries.t -> Queries.Result.t) (file : Cil.file) =
      (* Create an evaluator *)
      let evaluator = new evaluator file ask in
      (* Syntactic query *)
      let query_file_name = GobConfig.get_string transformation_query_file_name_identifier in
      let query =
        match Yojson.Safe.from_file query_file_name |> query_of_yojson with
        | Ok parsed_query -> parsed_query
        | Error message ->
            prerr_endline ("Parsing error: " ^ message);
            raise Exit
      in
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
      let locations =
        SyntacticalAnalyzer.QueryMapping.map_query query_syntactic file
          (* Use only locations *)
          |> List.map (fun (_, l, _, _) -> l)
          (* Group by source files *)
          |> List.group location_file_compare
          (* Sort and remove duplicates *)
          |> List.map (fun ls -> List.sort_uniq location_byte_compare ls)
          (* Ungroup *)
          |> List.flatten
      in
      (* Semantic queries *)
      let debug = GobConfig.get_bool "dbg.verbose" in
      let evaluate location =
        match evaluator#evaluate location query.expression with
        | Some value ->
            if value then
              print_endline (location |> string_of_location)
            else if debug then
              print_endline ((location |> string_of_location) ^ " x")
        | None ->
            if query.mode = `May || debug then
              print_endline ((location |> string_of_location) ^ " ?")
      in
      List.iter evaluate locations

  end

let _ =
  Transform.register transformation_identifier (module ExpEval)
