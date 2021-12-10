
module Invocation =
struct
  type t = {
    commandLine: string;
    executionSuccessful: bool;
  } [@@deriving to_yojson]
end

module Region =
struct
  type t = {
    startLine: int;
    startColumn: int option [@default None];
    endLine: int;
    endColumn: int option [@default None];
  } [@@deriving to_yojson, eq]
end

module ArtifactLocation =
struct
  type t = {
    uri: string;
  } [@@deriving to_yojson, eq]
end

module PhysicalLocation =
struct
  type t = {
    artifactLocation: ArtifactLocation.t;
    region: Region.t;
  } [@@deriving to_yojson, eq]
end

module Artifact =
struct
  type t = {
    location: ArtifactLocation.t;
  } [@@deriving to_yojson]
end

module Location =
struct
  type t = {
    physicalLocation: PhysicalLocation.t;
  } [@@deriving to_yojson, eq]
end

module Message =
struct
  type t = {
    text: string;
  } [@@deriving to_yojson]
end

module ReportingDescriptor =
struct
  type t = {
    ruleId: string; [@key "id"]
    ruleName: string; [@key "name"]
    helpUri: string;
    help: Message.t;
    shortDescription: Message.t;
    fullDescription: Message.t;
  } [@@deriving to_yojson]
end

module ToolComponent =
struct
  type t = {
    name: string;
    fullName: string;
    informationUri: string;
    organization: string;
    version: string;
    rules: ReportingDescriptor.t list;
  } [@@deriving to_yojson]
end

module Tool =
struct
  type t = {
    driver: ToolComponent.t;
  } [@@deriving to_yojson]
end

module Result =
struct
  type t = {
    ruleId: string;
    kind: string;
    level: string;
    message: Message.t;
    locations: Location.t list [@default []];
    relatedLocations: Location.t list [@default []]; (* VSCode SARIF plugin doesn't show these *)
  } [@@deriving to_yojson]
end

(* ppx_deriving_yojson quoter fixed upstream (https://github.com/ocaml-ppx/ppx_deriving_yojson/pull/140)
   but unreleased (https://github.com/ocaml-ppx/ppx_deriving_yojson/issues/141),
   so use workaround for goblint release *)
(* TODO: remove workaround when new ppx_deriving_yojson released *)
let result_to_yojson = Result.to_yojson (* workaround for ppx_deriving problem on Result *)

module Run =
struct
  type t = {
    tool: Tool.t;
    defaultSourceLanguage: string;
    invocations: Invocation.t list;
    artifacts: Artifact.t list;
    results: (Result.t [@to_yojson result_to_yojson]) list; (* workaround for ppx_deriving problem on Result *)
  } [@@deriving to_yojson]
end

(* sarif prefix explicit in standard *)
module SarifLog =
struct
  type t = {
    version: string;
    schema: string [@key "$schema"];
    runs: Run.t list;
  } [@@deriving to_yojson]
end
