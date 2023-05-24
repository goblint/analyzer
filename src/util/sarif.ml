(** The Sarif format is a standardised output format for static analysis tools. https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html *)
open Batteries

open SarifType
open SarifRules


(*A reportingDescriptor offers a lot of information about a Goblint rule *)
let createReportingDescriptor categoryInformation: ReportingDescriptor.t = {
  ruleId = categoryInformation.ruleId;
  ruleName = categoryInformation.name;
  helpUri = categoryInformation.helpUri;
  help = { text = categoryInformation.helpText };
  shortDescription = { text = categoryInformation.shortDescription };
  fullDescription = { text = categoryInformation.longDescription };
}

let transformToReportingDescriptor (id:String.t)=
  createReportingDescriptor  (getRuleInformation id)

let goblintTool: Tool.t = {
  driver = {
    name = "Goblint";
    fullName = "Goblint static analyser";
    informationUri = "https://goblint.in.tum.de/home";
    organization = "TUM - i2 and UTartu - SWS";
    version = Version.goblint;
    rules = List.map transformToReportingDescriptor (List.map (fun rule -> rule.name) rules)
  };
}


(*returns the Rule corresponding to a message entry *)
let getCategoryInformationID (tags:Messages.Tags.t) =
  let getCWE (tag:Messages.Tag.t) = match tag with
    | CWE cwe-> Some cwe;
    | Category cat -> None;
  in
  (* if a CWE is present only the CWE is used, since using multiple ones for the same result doesn' make sense.
     If only Categorys are present, all of them are displayed.*)
  match List.filter_map getCWE tags with
  | cwe :: _ ->  string_of_int cwe;
  | [] -> match tags with
    | [] -> ""
    | x::xs -> match x with
      |Category cat-> MessageCategory.categoryName cat
      | CWE c-> "" (*this case should not be reachable *)



let location_of_cil_location ({file; line; column; endLine; endColumn; _}: GoblintCil.location): Location.t = {
  physicalLocation = {
    artifactLocation = { uri = file };
    region = {
      startLine = line;
      startColumn = if column >= 0 then Some column else None;
      endLine;
      endColumn = if endColumn >= 0 then Some endColumn else None;
    };
  }
}

let result_of_message (message: Messages.Message.t): Result.t list =
  let ruleId = (getRuleInformation (getCategoryInformationID message.tags)).ruleId in
  let (kind, level) = match message.severity with
    | Error -> ("fail", "error")
    | Warning -> ("fail", "warning")
    | Info -> ("informational", "none")
    | Debug -> ("informational", "none")
    | Success -> ("pass", "none")
  in
  let piece_location (piece: Messages.Piece.t) = match piece.loc with
    | Some loc -> [location_of_cil_location (Messages.Location.to_cil loc)]
    | None -> []
  in
  let prefix = Format.asprintf "%a " Messages.Tags.pp message.tags in
  match message.multipiece with
  | Single piece ->
    let result: Result.t = {
      ruleId;
      kind;
      level;
      message = { text = prefix ^ piece.text };
      locations = piece_location piece;
      relatedLocations = [];
    }
    in
    [result]
  | Group {group_text; pieces} ->
    (* each grouped piece becomes a separate result with the other locations as related *)
    let piece_locations = List.map piece_location pieces in
    List.map2i (fun i piece locations ->
        let text = prefix ^ group_text ^ "\n" ^ piece.Messages.Piece.text in
        let relatedLocations = List.unique ~eq:Location.equal (List.flatten (List.remove_at i piece_locations)) in
        let result: Result.t = {
          ruleId;
          kind;
          level;
          message = { text };
          locations;
          relatedLocations;
        }
        in
        result
      ) pieces piece_locations

let files_of_message (message: Messages.Message.t): string list =
  let piece_file (piece: Messages.Piece.t) = match piece.loc with
    | Some loc -> Some (Messages.Location.to_cil loc).file
    | None -> None
  in
  match message.multipiece with
  | Single piece ->
    Option.map_default List.singleton [] (piece_file piece)
  | Group {pieces; _} ->
    List.filter_map piece_file pieces

(* TODO: just get all files from AST? *)
let artifacts_of_messages (messages: Messages.Message.t list): Artifact.t list =
  messages
  |> List.enum
  |> Enum.map files_of_message
  |> Enum.map List.enum
  |> Enum.flatten
  |> Enum.uniq (* polymorphic equality fine on string *)
  |> Enum.map (fun file -> {
        Artifact.location = { uri = file };
      }
    )
  |> List.of_enum

let to_yojson messages =
  SarifLog.to_yojson {
    version = "2.1.0";
    schema = "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0-rtm.5.json";
    runs = [{
        invocations = [{
            commandLine = GobSys.command_line;
            executionSuccessful = true;
          }];
        artifacts = artifacts_of_messages messages;
        tool = goblintTool;
        defaultSourceLanguage = "C";
        results = List.concat_map result_of_message messages;
      }]
  }
