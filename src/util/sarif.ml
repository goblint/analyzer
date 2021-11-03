(** The Sarif format is a standardised output format for static analysis tools. https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html *)
open Prelude
open GobConfig
module Category = MessageCategory
module GU = Goblintutil

open SarifRules


(*matches the Goblint severity to the Sarif property level.*)
let severityToLevel (severity:Messages.Severity.t)= match severity with
  | Error -> "error"
  | Warning -> "warning"
  | Info -> "note"
  | Debug -> "none"
  | Success -> "none"

module InvocationObject =
struct
  type t = {
    commandLine:string;
    executionSuccessful:bool;

  } [@@deriving  to_yojson]


end
(*endColumn and endLine are not produced by Goblint yet, however the Github action uses these properties. *)
module Region =
struct
  type t = {
    startLine:int;
    startColumn:int;
    endColumn:int;
    endLine:int;
  } [@@deriving  to_yojson]


end
module ArtifactLocation =
struct
  type t = {
    uri:string;

  } [@@deriving  to_yojson]


end
module PhysicalLocation =
struct
  type t = {
    artifactLocation:ArtifactLocation.t;
    region:Region.t;

  } [@@deriving  to_yojson]
end

(*This type represents the Sarif locationProperty.
  It is needed twice, since Sarif requires differen keys names, depending on where it is used.*)
module LocationObject =
struct  type t = {
    location:ArtifactLocation.t;

  } [@@deriving  to_yojson]
end


module Locations =
struct
  type t = {

    physicalLocation:PhysicalLocation.t;

  } [@@deriving  to_yojson]

end
module SairfMessageObject =
struct
  type t = {
    text:string;

  } [@@deriving  to_yojson]


end
module ReportingDescriptor =
struct
  type t = {
    ruleId:string;   [@key "id"]
    ruleName:string;  [@key "name"]
    helpUri:string;
    help:SairfMessageObject.t;
    shortDescription:SairfMessageObject.t;
    fullDescription:SairfMessageObject.t;

  } [@@deriving  to_yojson]


end

module Driver =
struct
  type t = {
    name:string;
    fullName:string;
    informationUri:string;
    organization:string;
    version:string;
    rules:ReportingDescriptor.t list
  } [@@deriving  to_yojson]


end
module Tool =
struct
  type t = {
    driver:Driver.t;
  } [@@deriving  to_yojson]


end


module ResultObject =
struct
  type t = {
    ruleId:string;
    level:string;
    message:SairfMessageObject.t;
    locations:Locations.t list;
  } [@@deriving  to_yojson]


end

module Run =
struct
  type t = {
    tool:Tool.t;
    defaultSourceLanguage:string;
    invocations:InvocationObject.t list;
    artifacts:LocationObject.t list;
    results:ResultObject.t list
  }[@@deriving  to_yojson]


end



let createMessageObject (text:String.t) =
  {
    SairfMessageObject.text=text;
  }
(*A reportingDescriptor offers a lot of information about a Goblint rule *)
let createReportingDescriptor categoryInformation =
  {
    ReportingDescriptor.ruleId=categoryInformation.ruleId;
    ReportingDescriptor.ruleName=categoryInformation.name;
    ReportingDescriptor.helpUri=categoryInformation.helpUri;
    ReportingDescriptor.help=(createMessageObject categoryInformation.helpText);
    ReportingDescriptor.shortDescription=(createMessageObject categoryInformation.shortDescription);
    ReportingDescriptor.fullDescription=(createMessageObject categoryInformation.longDescription);
  }

let transformToReportingDescriptor (id:String.t)=
  createReportingDescriptor  (getRuleInformation id)

let (driverObject:Driver.t) =
  {
    Driver.name="Goblint";
    Driver.fullName= "Goblint static analyser";
    Driver.informationUri="https://goblint.in.tum.de/home";
    Driver.organization="TUM - i2 and UTartu - SWS";
    Driver.version=Version.goblint;
    Driver.rules=List.map transformToReportingDescriptor (List.map (fun rule -> rule.name) rules)
  }
let (toolObject:Tool.t) =
  {
    Tool.driver=driverObject;
  }


(*returns the Rule corresponding to a message entry *)
let getCategoryInformationID (tags:Messages.Tags.t) =
  let getCWE (tag:Messages.Tag.t) = match tag with
    | CWE cwe-> Some cwe;
    | Category cat -> None;
  in
  (* if a CWE is present only the CWE is used, since using multiple ones for the same result doesn' make sense.
     If only Categorys are present, all of them are displayed.*)
  match List.find_map_opt getCWE tags with
  | Some cwe ->  string_of_int cwe;
  | None -> match tags with
    | [] -> ""
    | x::xs -> match x with
      |Category cat-> MessageCategory.categoryName cat
      | CWE c-> "" (*this case should not be reachable *)

(* for the github action. Removes leading directory.*)
let trimFile (path:string) =
  let lengthRemove = (String.length (get_string "removePath"))
  in
  if get_string "removePath" == "" then path else
  "./"^(String.sub path lengthRemove  ((String.length path)-lengthRemove) )

let createArtifactLocationObject (uri:string) =
  {
    LocationObject.location={
      ArtifactLocation.uri=trimFile uri;
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
  let createRegionObject (line,column)=
    {
      Region.startLine=line;
      Region.startColumn=column;
      Region.endLine=line+4;
      Region.endColumn=column+4;
    }
  in
  {
    Locations.physicalLocation={
      PhysicalLocation.artifactLocation=  createArtifactObject (trimFile (deOptionalizeLocation piece).file);
      PhysicalLocation.region=createRegionObject ((deOptionalizeLocation piece).line,(deOptionalizeLocation piece).column);
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
    ResultObject.ruleId=(getRuleInformation (getCategoryInformationID message.tags)).ruleId;
    ResultObject.level=severityToLevel message.severity;
    ResultObject.message=createMessageObject (getMessage message.multipiece);
    ResultObject.locations=createLocationsObject message.multipiece;
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

let collectAllFileLocations (msgList:Messages.Message.t list)=
  let getUris=
    List.flatten (List.map (fun (msg:Messages.Message.t)-> getFileLocation msg.multipiece) msgList)
  in
  let uniques x xs = if List.mem x xs then xs else x::xs;
  in
  List.fold_right uniques getUris []
let runObject msgList=
  {
    Run.invocations=[{
        InvocationObject.commandLine=String.concat  ", " (BatArray.to_list BatSys.argv)  ;
        InvocationObject.executionSuccessful=true;
      }];
    Run.artifacts= List.map createArtifactLocationObject (collectAllFileLocations   msgList);
    Run.tool=toolObject;
    Run.defaultSourceLanguage="C";
    Run.results=List.map createResult (List.take 5000 msgList);
  }
module SarifLog =
struct
  type t = {
    (* Sarif prefers numerical Versions, this could be a future addition to Goblint. *)
    version:string;
    schema :string [@key "$schema"];
    runs:Run.t list  ;
  } [@@deriving  to_yojson]

end


module Sarif =
struct

  type t =
    | SarifLog   (*[@name "sarifLog"]*)
  [@@deriving yojson]
  let sarifObject msgList={SarifLog.version="2.1.0";
                           SarifLog.schema="https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0-rtm.5.json";
                           SarifLog.runs=[runObject msgList] };

end


let to_yojson  msgList=   [%to_yojson: SarifLog.t]  (Sarif.sarifObject msgList)





