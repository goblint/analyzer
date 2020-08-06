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

    let transform ask (file : Cil.file) =

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

      let exit = ref false in
      while not !exit do

        let query_file_name =
          read_line_with_prompt "Query file name: " (* TODO *)
        in
        let query =
          match Yojson.Safe.from_file query_file_name |> query_of_yojson with
          | Ok parsed_query -> parsed_query
          | Error _ -> raise Exit
        in
        let query_must =
          match query.mode with
          | Must -> true
          | May -> false
        in
        let query_result =
          SyntacticalAnalyzer.QueryMapping.map_query
          {
            sel = [];
            k = query.kind;
            tar = query.target;
            f = query.find;
            str = None_s;
            lim = None_c;
          }
          file
        in

        let expression_string =
          read_line_with_prompt "Expression: " (* TODO *)
        in

        let evaluate (location : Cil.location) =
          match LocationMap.find_opt location statements with
          | Some statement ->
              let expression = Formatcil.cExp expression_string variables in
              begin
                match ask location (Queries.EvalInt expression) with
                | `Bot -> None
                | `Int value ->
                    if value = Int64.zero then
                      None
                    else
                      Some (Some value)
                | `Top ->
                    if query_must then
                      None
                    else
                      Some None (* TODO use "Query function answered"? *)
                | _ -> raise Exit
              end
          | None -> None
        in
        let print evaluation =
          match evaluation with
          | ((location : Cil.location), Some value) ->
              print_string ("Line " ^ (string_of_int location.line) ^ ": ");
              print_endline
              begin
                match value with
                | Some definite_value -> Int64.to_string definite_value
                | None -> "Unknown"
              end
          | (_, None) -> ()
        in

        query_result
          |> List.map (fun (_, location, _, _) -> location, evaluate location)
          |> List.iter print;

        if read_line_with_prompt "Exit? " = "y" then
          exit := true

      done

  end

let _ =
  TransformationBase.register "expeval" (module Transform)
