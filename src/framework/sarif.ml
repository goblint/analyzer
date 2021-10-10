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
  | "362"-> ("GO005",
                     "CWE 362: Concurrent Execution using Shared Resource with Improper Synchronization ('Race Condition')",
                     "The program contains a code sequence that can run concurrently with other code, and the code sequence requires temporary, exclusive access to a shared resource, but a timing window exists in which the shared resource can be modified by another code sequence that is operating concurrently. ",
                     "https://cwe.mitre.org/data/definitions/362.html",
                     "")  
  | "416" -> ("GO007",
              "CWE 416:Use After Free",
              "Referencing memory after it has been freed can cause a program to crash, use unexpected values, or execute code. ",
              "https://cwe.mitre.org/data/definitions/416.html",
              "The use of previously-freed memory can have any number of adverse consequences, ranging from the corruption of valid data to the execution of arbitrary code, depending on the instantiation and timing of the flaw. "
              ^"The simplest way data corruption may occur involves the system's reuse of the freed memory. Use-after-free errors have two common and sometimes overlapping causes:"
              ^"  Error conditions and other exceptional circumstances."
              ^"  Confusion over which part of the program is responsible for freeing the memory." 
              ^"In this scenario, the memory in question is allocated to another pointer validly at some point after it has been freed. The original pointer to the freed memory is used again and points to somewhere within the new allocation. As the data is changed, it corrupts the validly used memory; this induces undefined behavior in the process."
              ^"If the newly allocated data chances to hold a class, in C++ for example, various function pointers may be scattered within the heap data. If one of these function pointers is overwritten with an address to valid shellcode, execution of arbitrary code can be achieved. "); 
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
  | _ -> ("GO000","Unknown Category","Unknown Category","Unknown Category","Unknown Category")



let getRuleIDOld (id:string) = match (getDescription id ) with
  | (ruleId,_,_,_,_) -> ruleId
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
    results:ResultObject.t list
  }[@@deriving  to_yojson] 

  
end 


let createMessageObject (text:String.t) = 
  {
    SairfMessageObject.text=text;
  }
(*A reportingDescriptor offers a lot of information about a Goblint rule *)
let createReportingDescriptor name = 
  match getDescription name with
    |  (id,helpText,shortDescription,helpUri,longDescription) -> 
   {
    ReportingDescriptor.ruleId=id;   
    ReportingDescriptor.ruleName=name;  
    ReportingDescriptor.helpUri=helpUri;
    ReportingDescriptor.help=(createMessageObject helpText);
    ReportingDescriptor.shortDescription=(createMessageObject shortDescription);
    ReportingDescriptor.fullDescription=(createMessageObject longDescription);
  }  
let transformToReportingDescriptor (id:String.t)=  
    createReportingDescriptor  id

let (driverObject:Driver.t) =       
    {
    Driver.name="Goblint";
    Driver.fullName= "Goblint static analyser";
    Driver.informationUri="https://goblint.in.tum.de/home";
    Driver.organization="TUM - i2 and UTartu - SWS";
    Driver.version=Version.goblint;
    Driver.rules=List.map transformToReportingDescriptor ["Analyzer";"119"]
    }
let (toolObject:Tool.t) = 
  {
      Tool.driver=driverObject;     
  }


(*returns the Rule corresponding to a message entry *)
let getRuleID (tags:Messages.Tags.t) = 
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



let createPhysicalLocationObject (piece:Messages.Piece.t) = 
    let createRegionObject (line,column)=
    {
      Region.startLine=line;
      Region.startColumn=column;
    }
    in
    match piece.loc with 
    (*This case is filtered out in hasLocation, but the compiler complains if it is not matched here. *)
    | None -> {
      Locations.physicalLocation={
          PhysicalLocation.artifactLocation= {
              ArtifactLocation.uri="no file was provided";
           };
        PhysicalLocation.region=createRegionObject (0,0);
      }
    };
    | Some loc ->{
       Locations.physicalLocation={
       PhysicalLocation.artifactLocation= {
          ArtifactLocation.uri=loc.file;
        };
        PhysicalLocation.region=createRegionObject (loc.line,loc.column);
    }
    }
    
let hasLocation (piece:Messages.Piece.t) = match  piece.loc with
  |Some loc -> true
  |None -> false

let createLocationsObject (multiPiece:Messages.MultiPiece.t) = match multiPiece with 
  | Single piece ->List.map createPhysicalLocationObject (List.filter hasLocation  [piece]);
  | Group {group_text = n; pieces = e} ->List.map createPhysicalLocationObject (List.filter hasLocation e)



let createResult (message:Messages.Message.t) = 
  let rec getMessage (multiPiece:Messages.MultiPiece.t)=  match multiPiece with 
    | Single piece ->piece.text;
    | Group {group_text = n; pieces = e} ->n
  in
    {
      ResultObject.ruleId=getRuleID message.tags;
      ResultObject.level=severityToLevel message.severity;
      ResultObject.message=createMessageObject (getMessage message.multipiece);
      ResultObject.locations=createLocationsObject message.multipiece;
    }

let runObject msgList= 
{
    Run.invocations=[{
        InvocationObject.commandLine=String.concat  ", " (BatArray.to_list BatSys.argv)  ;
        InvocationObject.executionSuccessful=true;
    }];
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





