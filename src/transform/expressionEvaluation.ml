(** Transformation for evaluating expressions on the analysis results ([expeval]).
    {e Hack for Gobview}. *)

open Batteries
open GoblintCil
open Syntacticsearch

let transformation_identifier = "expeval"
let transformation_query_file_name_identifier = "trans." ^ transformation_identifier ^ ".query_file_name"

type query =
  {

    kind : CodeQuery.kind;
    target : CodeQuery.target;
    find : CodeQuery.find;
    structure : (CodeQuery.structure [@default None_s]);
    limitation : (CodeQuery.constr [@default None_c]);

    expression : (string option [@default None]);
    mode : [ `Must | `May ];

  } [@@deriving yojson]

(* These are meant to be used by GobView *)
let gv_query = ref None
let gv_results = ref []

module ExpEval : Transform.S =
struct

  let (~?) exception_function =
    try Some (exception_function ()) with
    | exn when GobExn.catch_all_filter exn -> None (* TODO: don't catch all *)
  let (~!) value_option =
    match value_option with
    | Some value -> value
    | None -> raise Stdlib.Exit

  let string_of_evaluation_result evaluation_result =
    match evaluation_result with
    | Some value -> if value then "TRUE" else "FALSE"
    | None -> "UNKNOWN"
  let string_of_statement statement =
    statement
    (* Pretty print *)
    |> CilType.Stmt.show
    (* Split into lines *)
    |> String.split_on_char '\n'
    (* Remove indentation *)
    |> List.map String.trim
    (* Concatenate lines into one *)
    |> List.fold_left (^) ""

  class evaluator (file : Cil.file) (ask : ?node:Node.t -> Cil.location -> Queries.ask) =
    object (self)

      val global_variables =
        file.globals
        |> List.filter_map (function
            | Cil.GVar (v, _, _) -> Some (v.vname, Cil.Fv v)
            | Cil.GFun (f, l) -> Some (f.svar.vname, Cil.Fv f.svar)
            | Cil.GVarDecl (v, l) -> Some (v.vname, Cil.Fv v)
            | _ -> None)
      val statements =
        file.globals
        |> List.filter_map (function Cil.GFun (f, _) -> Some f | _ -> None)
        (* Take all statements *)
        |> List.concat_map (fun (f : Cil.fundec) -> f.sallstmts |> List.map (fun s -> f, s))
        (* Add locations *)
        |> List.map (fun (f, (s : Cil.stmt)) -> (Cil.get_stmtLoc s.skind, f, s)) (* nosemgrep: cilfacade *) (* Must use CIL's because syntactic search is in CIL. *)
        (* Filter artificial ones by impossible location *)
        |> List.filter (fun ((l : Cil.location), _, _) -> l.line >= 0)
        (* Create hash table *)
        |> List.fold_left (fun ss (l, f, s) -> Hashtbl.add ss l (f, s); ss) (Hashtbl.create 0)

      method evaluate location expression_string =
        (* Compute the available local variables *)
        let local_variables =
          match Hashtbl.find_option statements location with
          | Some (fd, _) -> fd.slocals @ fd.sformals |> List.map (fun (v : Cil.varinfo) -> v.vname, Cil.Fv v)
          | None -> []
        in
        (* Parse expression *)
        match ~? (fun () -> Formatcil.cExp expression_string (local_variables @ global_variables)) with
        (* Expression unparseable at this location *)
        | None ->
          Logs.debug "| (Unparseable)";
          Some false
        (* Successfully parsed expression *)
        | Some expression ->
          (* Evaluate at (directly before) the location *)
          match self#try_ask location expression with
          (* Dead code or not listed as part of the control flow *)
          | None ->
            Logs.debug "| (Unreachable)";
            Some false
          (* Valid location *)
          | Some value_before ->
            (* Use the last listed matching statement; TODO: use Hashtbl.find_all to consider other matching statements *)
            match Hashtbl.find_option statements location with
            | None -> None (* location is not location of statement *)
            | Some (_, statement) ->
              (* Use the first evaluable successor; TODO: consider other successors *)
              let succeeding_statement = ref None in
              let successor_evaluation =
                try BatList.find_map
                      begin
                        fun (s : Cil.stmt) ->
                          succeeding_statement := Some s;
                          (* Evaluate at (directly before) a succeeding location *)
                          Some(self#try_ask (Cil.get_stmtLoc s.skind) expression) (* nosemgrep: cilfacade *) (* Must use CIL's because syntactic search is in CIL. *)
                      end
                      statement.succs
                with Not_found -> None
              in
              (* Prefer successor evaluation *)
              match successor_evaluation with
              | None ->
                Logs.debug "%s" ("| /*" ^ (value_before |> string_of_evaluation_result) ^ "*/" ^ (statement |> string_of_statement));
                value_before
              | Some value_after ->
                Logs.debug "%s" ("| " ^ (statement |> string_of_statement) ^ "/*" ^ (value_after |> string_of_evaluation_result) ^ "*/");
                Logs.debug "%s" ("| " ^ (~! !succeeding_statement |> string_of_statement));
                value_after

      method private try_ask location expression =
        match ~? (fun () -> Queries.eval_bool (ask location) expression) with
        | Some `Bot -> None (* Inapplicable: Unreachable *)
        | Some (`Lifted b) -> Some (Some b) (* Evaluable: Definite *)
        | Some `Top -> Some None (* Evaluable: Inconclusive *)
        (* Inapplicable: Unlisted *)
        | None
        | exception Not_found -> None

    end

  let query_from_file name =
    match ~? (fun () -> Yojson.Safe.from_file name) with
    | None ->
      Error ("ExpEval: Invalid JSON query file: \"" ^ name ^ "\". Specify via " ^ transformation_query_file_name_identifier ^ ".")
    | Some query_yojson ->
      match query_yojson |> query_of_yojson with
      | Error message ->
        Error ("ExpEval: Unable to parse JSON query file: \"" ^ name ^ "\" (" ^ message ^ ")")
      | Ok query ->
        Logs.debug "Successfully parsed JSON query file: \"%s\"" name;
        Ok query

  let string_of_location (location : Cil.location) =
    CilType.Location.show location ^ " [" ^ (location.byte |> string_of_int) ^ "]"

  let file_compare (_, l, _, _) (_, l', _, _) = let open Cil in compare l.file l'.file
  let byte_compare (_, l, _, _) (_, l', _, _) = let open Cil in compare l.byte l'.byte

  let transform (q : Transform.queries) (file : Cil.file) =
    let query = match !gv_query with
      | Some q -> Ok q
      | _ -> query_from_file (GobConfig.get_string transformation_query_file_name_identifier)
    in
    match query with
    | Ok query ->
      (* Create an evaluator *)
      let evaluator = new evaluator file q.ask in
      (* Syntactic query *)
      let query_syntactic : CodeQuery.query =
        {
          sel = [];
          k = query.kind;
          tar = query.target;
          f = query.find;
          str = query.structure;
          lim = query.limitation;
        }
      in
      let results =
        QueryMapping.map_query query_syntactic file
        (* Group by source files *)
        |> List.group file_compare
        (* Sort, remove duplicates, ungroup *)
        |> List.concat_map (fun ls -> List.sort_uniq byte_compare ls)
        (* Semantic queries if query.expression is some *)
        |> List.map (fun (n, l, s, i) -> ((n, l, s, i), Option.map_default (evaluator#evaluate l) (Some true) query.expression))
      in
      let print ((_, loc, _, _), res) =
        match res with
        | Some value ->
          if value then
            Logs.info "%s" (loc |> string_of_location)
          else
            Logs.debug "%s x" (loc |> string_of_location)
        | None ->
          if query.mode = `May then
            Logs.info "%s ?" (loc |> string_of_location)
          else
            Logs.debug "%s ?" (loc |> string_of_location)
      in
      gv_results := results;
      List.iter print results
    | Error e -> Logs.error "%s" e

  let name = transformation_identifier

  let requires_file_output = false

end

let _ =
  Transform.register (module ExpEval)
