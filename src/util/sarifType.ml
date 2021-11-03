
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

module SarifLog =
struct
  type t = {
    (* Sarif prefers numerical Versions, this could be a future addition to Goblint. *)
    version:string;
    schema :string [@key "$schema"];
    runs:Run.t list  ;
  } [@@deriving  to_yojson]

end
