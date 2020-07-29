module TransformationBase = Transform (* Avoid naming collision *)
module Transform : TransformationBase.S =
  struct

    module LocationMap = Map.Make
      (struct
        type t = Cil.location
        let compare (location_1 : t) (location_2 : t) =
          compare location_1.byte location_2.byte
      end)

    let read_line_with_prompt text =
      print_string text;
      flush stdout;
      read_line ()

    let get_statements functions =
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
        (* Take all statements *)
        |> List.map (fun (f : Cil.fundec) -> resolve_statements f.sbody |> List.map (fun s -> f, s))
        |> List.flatten
        (* Add locations *)
        |> List.map (fun (f, (s : Cil.stmt)) -> (Cil.get_stmtLoc s.skind, f, s))
        (* Filter artificial ones by impossible location *)
        |> List.filter (fun ((l : Cil.location), _, _) -> l.line >= 0)
        (* Transform to mapping *)
        |> List.fold_left (fun statements (l, f, s) -> LocationMap.add l (f, s) statements) LocationMap.empty
    let get_variables functions =
      functions
        |> List.map (fun (f : Cil.fundec) -> f.slocals)
        |> List.flatten
        |> List.fold_left (fun variables (v : Cil.varinfo) -> Hashtbl.add variables v.vid v; variables) (Hashtbl.create 0)

    let transform ask (file : Cil.file) =

      let functions = List.filter_map (function Cil.GFun (f, _) -> Some f | _ -> None) file.globals in
      let statements = get_statements functions in
      let variables = get_variables functions in

      let global_translation = List.filter_map (function Cil.GVar (v, _, _) -> Some (v.vname, Cil.Fv v) | _ -> None) file.globals in

      let exit = ref false in
      while not !exit do

        let variable_name = read_line_with_prompt "Variable: " (* TODO *) in
        let expression_string = read_line_with_prompt "Expression: " (* TODO *) in

        let query : SyntacticalAnalyzer.JsonParser.query =
          {
            sel = [];
            k = Var_k;
            tar = Name_t variable_name;
            f = Uses_f;
            str = NonCond_s;
            lim = None_c;
          }
        in
        let query_result = SyntacticalAnalyzer.QueryMapping.map_query query file in

        let evaluate_single (_, (location : Cil.location), _, identifier) =
          print_endline ("------ Line " ^ (string_of_int location.line) ^ ":");
          print_endline
            begin
              match LocationMap.find_opt location statements with
              | Some (function_declaration, statement) ->
                  let translation =
                    match Hashtbl.find_opt variables identifier with
                    | Some variable -> (variable_name, Cil.Fv variable)::global_translation
                    | None -> global_translation
                  in
                  let expression = Formatcil.cExp expression_string translation in
                  begin
                    match ask location (Queries.EvalInt expression) with
                    | `Bot -> "Bot"
                    | `Int value -> IntDomain.FlatPureIntegers.to_yojson value |> Yojson.Safe.to_string
                    | `Top -> "Top"
                    | _ -> raise Exit
                  end
              | None -> "No statement"
            end
        in

        List.iter evaluate_single query_result;

        if read_line_with_prompt "Exit? " = "y" then
          exit := true

      done

  end

let _ =
  TransformationBase.register "expeval" (module Transform)
