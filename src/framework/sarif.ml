(** The Sarif format is a standardised output format for static analysis tools. https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html *)


open GobConfig
module GU = Goblintutil
module Category = MessageCategory



type categoryInformation = {
  name:string;
  ruleId:string;
  helpText:string;
  shortDescription:string;
  helpUri:string;
  longDescription:string;
} 
let unknownRule = {
  name="Unknown";
  ruleId="GO000";
  helpText="";
  shortDescription="";
  helpUri="";
  longDescription="";
}

let rules = [
  {
  name="Assert";
  ruleId="GO0001";
  helpText="Assert Error";
  shortDescription="This is an internal Goblint Category";
  helpUri="https://goblint.in.tum.de/home";
  longDescription="";
  };
  {
  name="Analyzer";
  ruleId="GO0002";
  helpText="Analyzer Error";
  shortDescription="This is an internal Goblint Category";
  helpUri="https://goblint.in.tum.de/home";
  longDescription="";
  };
  {
  name="Cast";
  ruleId="GO0003";
  helpText="Cast Error";
  shortDescription="This is an internal Goblint Cast Error, most likely not indicating a problem in the analyzed program.";
  helpUri="https://goblint.in.tum.de/home";
  longDescription="";
  };
  {
  name="Deadcode";
  ruleId="GO0004";
  helpText="Deadcode";
  shortDescription="This code is never used in the program.";
  helpUri="https://goblint.in.tum.de/home";
  longDescription="";
  };
  {
  name="Race";
  ruleId="GO0005";
  helpText="A race condition";
  shortDescription="The program contains a code sequence that can run concurrently with other code, and the code sequence requires temporary, exclusive access to a shared resource, but a timing window exists in which the shared resource can be modified by another code sequence that is operating concurrently. ";
  helpUri="https://goblint.in.tum.de/home";
  longDescription="";
  };
  {
  name="362";
  ruleId="GO005";
  helpText="Concurrent Execution using Shared Resource with Improper Synchronization ('Race Condition')";
  shortDescription="The program contains a code sequence that can run concurrently with other code, and the code sequence requires temporary, exclusive access to a shared resource, but a timing window exists in which the shared resource can be modified by another code sequence that is operating concurrently. ";
  helpUri="https://cwe.mitre.org/data/definitions/362.html";
  longDescription="";
  };
  {
  name="416";
  ruleId="GO007";
  helpText="Use After Free";
  shortDescription="Referencing memory after it has been freed can cause a program to crash, use unexpected values, or execute code. ";
  helpUri="https://cwe.mitre.org/data/definitions/416.html";
  longDescription="";
  };
  {
  name="570";
  ruleId="GO0010";
  helpText="Expression is Always False";
  shortDescription="The software contains an expression that will always evaluate to false.";
  helpUri="https://cwe.mitre.org/data/definitions/570.html";
  longDescription="";
  };
  {
  name="571";
  ruleId="GO0011";
  helpText="Expression is Always True";
  shortDescription="The software contains an expression that will always evaluate to true.";
  helpUri="https://cwe.mitre.org/data/definitions/571.html";
  longDescription="";
  }
]
(*  
   | "476" -> ("GO008","CWE 476:NULL Pointer Dereference",
              "A NULL pointer dereference occurs when the application dereferences a pointer that it expects to be valid, but is NULL, typically causing a crash or exit. ",
              "https://cwe.mitre.org/data/definitions/476.html",
              "NULL pointer dereference issues can occur through a number of flaws, including race conditions, and simple programming omissions. ");
  | "561" |"DeadCode"-> ("GO009","CWE 561:Dead Code",
                         "The software contains dead code, which can never be executed.  ",
                         "https://cwe.mitre.org/data/definitions/561.html",
                         "Dead code is source code that can never be executed in a running program. The surrounding code makes it impossible for a section of code to ever be executed. ");
  
  | "787" -> ("GO012",
              "CWE 787: Out-of-bounds Write",
              "The software writes data past the end, or before the beginning, of the intended buffer. ",
              "https://cwe.mitre.org/data/definitions/787.html",
              "Typically, this can result in corruption of data, a crash, or code execution. The software may modify an index or perform pointer arithmetic that references a memory location that is outside of the boundaries of the buffer. "
              ^" A subsequent write operation then produces undefined or unexpected results. ");        
  | "788" -> ("GO013",
              "CWE 788:Access of Memory Location After End of Buffer",
              "The software reads or writes to a buffer using an index or pointer that references a memory location after the end of the buffer. ",
              "https://cwe.mitre.org/data/definitions/788.html",
              "This typically occurs when a pointer or its index is decremented to a position before the buffer;"
              ^"when pointer arithmetic results in a position before the buffer; or when a negative index is used, which generates a position before the buffer.  ");
*)

let getCategoryInformation (searchName:string) = 
    match List.find_opt (fun rule -> rule.name=searchName) rules with 
      | None ->unknownRule ;
      | Some rule -> rule

let getCWEDescription (cwe:int) = match cwe with 
  | 570 -> ("GO0010","CWE 570:Expression is Always False",
              "The software contains an expression that will always evaluate to false. ",
              "https://cwe.mitre.org/data/definitions/570.html",
              "");
  | 571  -> ("GO0011","CWE 571:Expression is Always True",
               "The software contains an expression that will always evaluate to true.  ",
               "https://cwe.mitre.org/data/definitions/571.html",
               "");
  | _ -> ("GO"^(string_of_int cwe),"CWE"^(string_of_int cwe),
               "",
               "https://cwe.mitre.org/data/definitions/"^(string_of_int cwe)^".html",
               "")

(* Given a Goblint Category
   returns (Ruleid,helpText,shortDescription,helpUri,longDescription) *)
let getDescription (id:string) = match id with 
  |"Analyzer" -> ("GO001","Goblint Category Analyzer","","https://goblint.in.tum.de/home","");
  |"Assert" -> ("GO002","Goblint Category Assert","","https://goblint.in.tum.de/home","");
 
  | _ -> ("GO000","Unknown Category","Unknown Category","Unknown Category","Unknown Category")

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
module Region =
struct
  type t = {
    startLine:int;
    startColumn:int;
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
    createReportingDescriptor  (getCategoryInformation id)
 
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
  match List.find_map getCWE tags with 
    | Some cwe ->  string_of_int cwe; 
    | None -> match tags with
      | [] -> ""
      | x::xs -> match x with 
        |Category cat-> MessageCategory.show cat
        | CWE c-> "" (*this case should not be reachable *)

let createArtifactLocationObject (uri:string) =
{
    LocationObject.location={
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
    let createRegionObject (line,column)=
    {
      Region.startLine=line;
      Region.startColumn=column;
    }
    in
    {
       Locations.physicalLocation={
       PhysicalLocation.artifactLocation=  createArtifactObject (deOptionalizeLocation piece).file;
        PhysicalLocation.region=createRegionObject ((deOptionalizeLocation piece).line,(deOptionalizeLocation piece).column);
    }
    }
    

let createLocationsObject (multiPiece:Messages.MultiPiece.t) = match multiPiece with 
  | Single piece ->List.map createPhysicalLocationObject (List.filter hasLocation  [piece]);
  | Group {group_text = n; pieces = e} ->List.map createPhysicalLocationObject (List.filter hasLocation e)



let createResult (message:Messages.Message.t) = 
  let rec getMessage (multiPiece:Messages.MultiPiece.t)=  match multiPiece with 
    | Single piece ->piece.text;
    | Group {group_text = n; pieces = e} ->n
  in
    {
      ResultObject.ruleId=(getCategoryInformation (getCategoryInformationID message.tags)).ruleId;
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
    Run.results=List.map createResult msgList;
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





