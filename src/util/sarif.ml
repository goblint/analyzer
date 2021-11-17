(** The Sarif format is a standardised output format for static analysis tools. https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html *)
open Prelude

open SarifType
open SarifRules

module Region = SarifType.Region (* TODO: why is this needed if SarifType is opened? *)


(*matches the Goblint severity to the Sarif property level.*)
let severityToLevel (severity:Messages.Severity.t)= match severity with
  | Error -> "error"
  | Warning -> "warning"
  | Info -> "note"
  | Debug -> "none"
  | Success -> "none"

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


let createArtifact (uri:string) =
  {
    Artifact.location={
      ArtifactLocation.uri=uri;
    }
  }
let createArtifactObject (uri:string) =
  {
    ArtifactLocation.uri=uri;
  }
let hasLocation (piece:Messages.Piece.t) = match  piece.loc with
  |Some loc -> true
  |None -> false
(*should only be called after hasLocation*)
let deOptionalizeLocation (piece:Messages.Piece.t)= match piece.loc with
  | Some loc ->loc
  | None -> assert false

let createPhysicalLocationObject (piece:Messages.Piece.t) =
  let createRegionObject {Cil.line; column; endLine; endColumn; _} =
    {
      Region.startLine=line;
      Region.startColumn=column;
      Region.endLine;
      Region.endColumn;
    }
  in
  {
    Location.physicalLocation={
      PhysicalLocation.artifactLocation=  createArtifactObject  (deOptionalizeLocation piece).file;
      PhysicalLocation.region=createRegionObject (deOptionalizeLocation piece);
    }
  }


let createLocationsObject (multiPiece:Messages.MultiPiece.t) = match multiPiece with
  | Single piece ->List.map createPhysicalLocationObject (List.filter hasLocation  [piece]);
  | Group {group_text = n; pieces = e} ->List.map createPhysicalLocationObject  (List.take 10 (List.filter hasLocation e))



let createResult (message:Messages.Message.t) =
  let getMessage (multiPiece:Messages.MultiPiece.t)=  match multiPiece with
    | Single piece ->piece.text;
    | Group {group_text = n; pieces = e} ->n
  in
  {
    Result.ruleId=(getRuleInformation (getCategoryInformationID message.tags)).ruleId;
    Result.level=severityToLevel message.severity;
    Result.message={ text = getMessage message.multipiece };
    Result.locations=createLocationsObject message.multipiece;
  }

let getFileLocation (multipiece:Messages.MultiPiece.t)=
  let getFile (loc:Cil.location) =
    loc.file
  in
  let toLocation = match multipiece with

    | Single piece ->[deOptionalizeLocation piece];
    | Group {group_text = n; pieces = e} ->
      List.map deOptionalizeLocation  (List.filter hasLocation e);
  in
  List.map getFile toLocation

(* TODO: just get all files from AST? *)
let collectAllFileLocations (msgList:Messages.Message.t list)=
  let getUris=
    List.flatten (List.map (fun (msg:Messages.Message.t)-> getFileLocation msg.multipiece) msgList)
  in
  (* TODO: don't reimplement unique *)
  let uniques x xs = if List.mem x xs then xs else x::xs;
  in
  List.fold_right uniques getUris []


let to_yojson msgList =
  SarifLog.to_yojson {
    version = "2.1.0";
    schema = "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0-rtm.5.json";
    runs = [{
        invocations = [{
            commandLine = String.concat ", " (Array.to_list Sys.argv); (* TODO: remove commas, quote *)
            executionSuccessful = true;
          }];
        artifacts = List.map createArtifact (collectAllFileLocations msgList);
        tool = goblintTool;
        defaultSourceLanguage = "C";
        results = List.map createResult (List.take 5000 msgList); (* TODO: why limit? *)
      }]
  }
