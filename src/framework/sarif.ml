(** The Sarif format is a standardised output format for static analysis tools. https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html *)


open Cil
open Pretty
open GobConfig

module GU = Goblintutil



module Information =
struct
  type t = {
    shortDescription: String.t;
    helpText:String.t;
    helpUri:String.t;
    fullDescription: String.t;
  }

end
module Sarif =
struct 
 type t = {
    id: String.t;
    information:Information.t;    
  } 

  (* returns (id,helpText,shortDescription,helpUri,longDescription) *)
  let getDescription (category:string) = match category with 
         |"Analyzer" -> ("Analyzer","The category analyser describes ....","","https://goblint.in.tum.de/home","");
          |"119" -> ("GO"^category,
          "Improper Restriction of Operations within the Bounds of a Memory Buffer"
          ,"The software performs operations on a memory buffer, but it can read from or write to a memory location that is outside of the intended boundary of the buffer. ",
          "https://cwe.mitre.org/data/definitions/119.html",
          "Certain languages allow direct addressing of memory locations and do not automatically ensure that these locations are valid for the memory buffer that is being referenced"
          ^"This can cause read or write operations to be performed on memory locations that may be associated with other variables, data structures, or internal program data."
          ^"As a result, an attacker may be able to execute arbitrary code, alter the intended control flow, read sensitive information, or cause the system to crash. ");
         | "190" -> ("GO"^category ,
         "The software performs a calculation that can produce an integer overflow or wraparound, when the logic assumes that the resulting value will always be larger than the original value." 
         ^"This can introduce other weaknesses when the calculation is used for resource management or execution control. "
         ,"Integer Overflow or Wraparound","https://cwe.mitre.org/data/definitions/190.html",
        "An integer overflow or wraparound occurs when an integer value is incremented to a value that is too large to store in the associated representation. When this occurs, the value may wrap to become a very small or negative number. While this may be intended behavior in circumstances that rely on wrapping, it can have security consequences if the wrap is unexpected. This is especially the case if the integer overflow can be triggered using user-supplied inputs. This becomes security-critical when the result is used to control looping, make a security decision, or determine the offset or size in behaviors such as memory allocation, copying, concatenation, etc.  ");
        | "241" -> ("GO"^category,"Improper Handling of Unexpected Data Type",
          "The software does not handle or incorrectly handles when a particular element is not the expected type, e.g. it expects a digit (0-9) but is provided with a letter (A-Z). ","https://cwe.mitre.org/data/definitions/241.html",
           "")
        | "362" -> ("GO"^category," Concurrent Execution using Shared Resource with Improper Synchronization ('Race Condition')",
        "The program contains a code sequence that can run concurrently with other code, and the code sequence requires temporary, exclusive access to a shared resource, but a timing window exists in which the shared resource can be modified by another code sequence that is operating concurrently. ",
        "https://cwe.mitre.org/data/definitions/362.html",
        "")
         | "369" -> ("GO"^category," Divide By Zero",
         "The product divides a value by zero. ",
         "https://cwe.mitre.org/data/definitions/369.html",
        "This weakness typically occurs when an unexpected value is provided to the product, or if an error occurs that is not properly detected. It frequently occurs in calculations involving physical dimensions such as size, length, width, and height.   ");
        | "416" -> ("GO"^category,
        "Use After Free",
        "Referencing memory after it has been freed can cause a program to crash, use unexpected values, or execute code. ",
        "https://cwe.mitre.org/data/definitions/416.html",
        "The use of previously-freed memory can have any number of adverse consequences, ranging from the corruption of valid data to the execution of arbitrary code, depending on the instantiation and timing of the flaw. "
        ^"The simplest way data corruption may occur involves the system's reuse of the freed memory. Use-after-free errors have two common and sometimes overlapping causes:"
          ^"  Error conditions and other exceptional circumstances."
           ^"  Confusion over which part of the program is responsible for freeing the memory." 
        ^"In this scenario, the memory in question is allocated to another pointer validly at some point after it has been freed. The original pointer to the freed memory is used again and points to somewhere within the new allocation. As the data is changed, it corrupts the validly used memory; this induces undefined behavior in the process."
        ^"If the newly allocated data chances to hold a class, in C++ for example, various function pointers may be scattered within the heap data. If one of these function pointers is overwritten with an address to valid shellcode, execution of arbitrary code can be achieved. "); 
        | "476" -> ("GO"^category,"NULL Pointer Dereference",
        "A NULL pointer dereference occurs when the application dereferences a pointer that it expects to be valid, but is NULL, typically causing a crash or exit. ",
        "https://cwe.mitre.org/data/definitions/476.html",
        "NULL pointer dereference issues can occur through a number of flaws, including race conditions, and simple programming omissions. ");
         | "787" -> ("GO"^category,
         "Out-of-bounds Write",
         "The software writes data past the end, or before the beginning, of the intended buffer. ",
         "https://cwe.mitre.org/data/definitions/787.html",
        "Typically, this can result in corruption of data, a crash, or code execution. The software may modify an index or perform pointer arithmetic that references a memory location that is outside of the boundaries of the buffer. "
       ^" A subsequent write operation then produces undefined or unexpected results. ");        
        | "788" -> ("GO"^category,
        "Access of Memory Location After End of Buffer",
        "The software reads or writes to a buffer using an index or pointer that references a memory location after the end of the buffer. ",
        "https://cwe.mitre.org/data/definitions/788.html",
        "This typically occurs when a pointer or its index is decremented to a position before the buffer;"
         ^"when pointer arithmetic results in a position before the buffer; or when a negative index is used, which generates a position before the buffer.  ");
        | _ -> ("invalid","invalid","invalid","invalid","invalid")

   let rec printCategorieRules f (categories:string list) = 
      let printSingleCategory f cat = match getDescription cat with 
        | ("invalid","invalid","invalid","invalid","invalid") -> BatPrintf.fprintf f "";
        | (id,shortDescription,helpText,helpUri,longDescription) -> 
        BatPrintf.fprintf f "      {\n";
        BatPrintf.fprintf f "           \"id\": \"%s\",\n" id;
        BatPrintf.fprintf f "           \"helpUri\": \"%s\",\n" helpUri;
        BatPrintf.fprintf f "           \"help\": {\n";
        BatPrintf.fprintf f "               \"text\": \"%s\"\n" helpText;
        BatPrintf.fprintf f "           },\n";
        BatPrintf.fprintf f "          \"shortDescription\": {\n";
        BatPrintf.fprintf f "               \"text\": \"%s\"\n" shortDescription;
        BatPrintf.fprintf f "           },\n";
        BatPrintf.fprintf f "           \"fullDescription\": {\n";
        BatPrintf.fprintf f "               \"text\": \"%s\"\n" longDescription;
        BatPrintf.fprintf f "           }\n  ";
        BatPrintf.fprintf f "     }"
      in
      match categories with 
        | [] ->  BatPrintf.fprintf f "";
        | x::[] -> printSingleCategory f x;
        | x::xs -> printSingleCategory f x;
        (*BatPrintf.fprintf f ",";*)
        BatPrintf.fprintf f "\n";                
          printCategorieRules f xs
     
  let getBehaviorCategory (behavior:MessageCategory.behavior) = match behavior with
        | Implementation-> "Implementation";
        | Machine-> "Machine";
        | Undefined u-> match u with 
          | NullPointerDereference -> "476";
          | UseAfterFree -> "416"
          | ArrayOutOfBounds arrayOutOfBounds -> match arrayOutOfBounds with
              | PastEnd -> "788";
              | BeforeStart -> "786:";
              | Unknown -> "119"
        
  let returnCategory (cat:MessageCategory.category)= match cat with
    | MessageCategory.Assert -> "Assert";
    | MessageCategory.Deadcode -> "Deadcode";
    | MessageCategory.Race -> "Race";
    | MessageCategory.Unknown -> "Category Unknown";
    | MessageCategory.Analyzer -> "Analyzer";
    | MessageCategory.Behavior b -> getBehaviorCategory b;
    | MessageCategory.Cast c -> "241";
    | MessageCategory.Integer i -> match i with 
          | Overflow -> "190";
          | DivByZero -> "369"
 
end

let print_physicalLocationPiece f Messages.Piece.{loc; text = m; context=con;} =
        let printContext f (context:Obj.t option) =  match context with 
              | Some c -> BatPrintf.fprintf f "has context\n";
                      BatPrintf.fprintf f "%d\n" (Obj.reachable_words c);
              | None -> ();
        in
     (* for the github action removes leading ./analysistarget/*)
        let trimFile (path:string) = 
          Str.string_after  path 17;  
          in
        match loc with
        | None ->
          BatPrintf.fprintf f "";
        | Some l ->
            BatPrintf.fprintf f "       \"physicalLocation\": " ;             
            BatPrintf.fprintf f "{\n              \"artifactLocation\": {\n                \"uri\":\"%s\"\n              },\n" ( l.file) ;
            BatPrintf.fprintf f "              \"region\": {\n";
            BatPrintf.fprintf f "                \"startLine\":%d,\n" l.line ; 
            BatPrintf.fprintf f "                \"startColumn\":%d,\n" l.column ; 
            BatPrintf.fprintf f "                \"endColumn\":%d,\n" l.column ; 
            BatPrintf.fprintf f "                \"endLine\":%d\n" l.line ;    
            (*printContext f con;*)
            BatPrintf.fprintf f "             }\n"
           
           
       
         
  let printMultipiece f (mp:Messages.MultiPiece.t)= 

      let printMessageText f Messages.Piece.{loc; text = m; _} =
          BatPrintf.fprintf f "\n        \"message\": {\n            \"text\": \"%s\"\n         }," m ;       
      in 
      let printMessages f (pieces:Messages.Piece.t list) = 
          let toMessage Messages.Piece.{loc; text = m; _} =m in
          match pieces with
          | [] -> BatPrintf.fprintf f "";
          | x::xs ->  BatPrintf.fprintf f "\n        \"message\": {\n";  
                      BatPrintf.fprintf f "           \"text\": \"%s\"\n    " (String.concat ";   " (List.map toMessage pieces)) ;   
                      BatPrintf.fprintf f "      }\n";
      in
      let rec printPieces f (pieces:Messages.Piece.t list)= match pieces with 
            | [] ->      BatPrintf.fprintf f "";
            | x::[] ->  print_physicalLocationPiece f  x; 
            | x::xs ->  print_physicalLocationPiece f  x; 
                        BatPrintf.fprintf f "     },\n";
                        printPieces f xs;
                       
      in     
         match mp with
           | Single (piece: Messages.Piece.t) -> 
               printMessageText f piece;
               BatPrintf.fprintf f "\n        \"locations\": [\n        {\n    ";
               print_physicalLocationPiece f  piece;  
               BatPrintf.fprintf f "       }\n       ]";            
           | Group {group_text = n; pieces = e} ->
                BatPrintf.fprintf f "\n        \"locations\": [\n        {\n    ";
                printPieces f e;                
                BatPrintf.fprintf f "           }\n" ;   
                BatPrintf.fprintf f "       }\n     ],";           
                printMessages f e

 let severityToLevel (severity:Messages.Severity.t)= match severity with
      | Error -> "error"
      | Warning -> "warning"
      | Info -> "note"
      | Debug -> "none"
      | Success -> "none"

let printSarifResults f =         
          let rec printTags f (tags:Messages.Tags.t)= match tags with 
           | [] ->BatPrintf.fprintf f "  Unexpected Error,  empty tags in Messages.Tags";
           | x::xs -> match x with 
            | CWE cwe->  BatPrintf.fprintf f "    {\n        \"ruleId\": \"%s\"," (string_of_int cwe);            
            | Category cat ->  BatPrintf.fprintf f "    {\n        \"ruleId\": \"%s\"," (Sarif.returnCategory cat );
          in       
         let printOneResult (message:Messages.Message.t )=             
             printTags f   message.tags;    
             BatPrintf.fprintf f "\n        \"level\": \"%s\"," (severityToLevel message.severity) ;            
             printMultipiece f message.multipiece;             
             BatPrintf.fprintf f "\n    }";   
         in
         let rec printResults (message_table:Messages.Message.t list)= 
            match message_table with 
               [] -> BatPrintf.fprintf f "\n";
               |x::[] -> printOneResult x;
               BatPrintf.fprintf f "\n";
               | x::xs ->printOneResult x;
                    BatPrintf.fprintf f ",\n";
                    printResults xs;
          in  
          (*BatPrintf.fprintf f "MessageTable length %d"(List.length !Messages.Table.messages_list) ; *)
          printResults (List.rev !Messages.Table.messages_list)



let createSarifOutput f =
        BatPrintf.fprintf f "{\n \"$schema\": \"%s\",\n  " "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0-rtm.5.json";
        BatPrintf.fprintf f "\"version\": \"%s\",\n  " "2.1.0";
        BatPrintf.fprintf f "\"runs\": [\n  ";
        BatPrintf.fprintf f "{\n  ";
        BatPrintf.fprintf f "\"tool\": {\n    ";
        BatPrintf.fprintf f "\ \"driver\": {\n       ";
        BatPrintf.fprintf f "\"name\": \"%s\",\n       " "Goblint";
        BatPrintf.fprintf f "\"fullName\": \"%s\",\n       " "Goblint static analyser";   
        BatPrintf.fprintf f "\"informationUri\": \"%s\",\n       " "https://goblint.in.tum.de/home";
        BatPrintf.fprintf f "\"organization\": \"%s\",\n       " "TUM ";
        BatPrintf.fprintf f "\"version\": \"%s\",\n       " Version.goblint; 
        BatPrintf.fprintf f "\"downloadUri\": \"%s\",\n    " "https://github.com/goblint/analyzer";
        BatPrintf.fprintf f "    \"rules\": [\n  ";
        Sarif.printCategorieRules f ["124";"190";"281"];
        BatPrintf.fprintf f "     ]\n  ";
        BatPrintf.fprintf f "   }\n  ";  
        BatPrintf.fprintf f "},\n";
        BatPrintf.fprintf f "\   \"invocations\": [\n       ";
        BatPrintf.fprintf f "{\n";        
        BatPrintf.fprintf f "        \"commandLine\": \"%a\",\n" (BatArray.print ~first:"" ~last:"" ~sep:" " BatString.print) BatSys.argv;
        BatPrintf.fprintf f "        \"executionSuccessful\": %B\n    " true;        
        BatPrintf.fprintf f "   }\n";  
        BatPrintf.fprintf f "   ],\n" ;
        BatPrintf.fprintf f "   \"defaultSourceLanguage\": \"%s\",\n" "C";
        BatPrintf.fprintf f "   \"results\": [\n";
         printSarifResults f;
        BatPrintf.fprintf f "   ]\n" ;
        BatPrintf.fprintf f "   }\n  " ;
        BatPrintf.fprintf f "]\n" ;       
        BatPrintf.fprintf f "}\n";
