(** SARIF rule definitions for Goblint. *)

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
  helpUri="https://goblint.in.tum.de/home";
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
    helpText="Concurrent Execution using Shared Resource with Improper Synchronization";
    shortDescription="Race Condition";
    helpUri="https://goblint.in.tum.de/home";
    longDescription="The program contains a code sequence that can run concurrently with other code, and the code sequence requires temporary, exclusive access to a shared resource, but a timing window exists in which the shared resource can be modified by another code sequence that is operating concurrently.";
  };
  {
    name="Overflow";
    ruleId="GO0006";
    helpText="Integer Overflow";
    shortDescription="Result of arithmetic operation might be outside of the valid range. ";
    helpUri="https://en.wikipedia.org/wiki/Integer_overflow";
    longDescription="";
  };
  {
    name="DivByZero";
    ruleId="GO0007";
    helpText="Division by zero";
    shortDescription="Division by zero can result in undefined behaviour.";
    helpUri="https://goblint.in.tum.de/home";
    longDescription="";
  };
  {
    name="Implementation";
    ruleId="GO0008";
    (*TODO is this correct? *)
    helpText="Implementation of function might not be present on all versions of the target platform.";
    shortDescription=" Implementation of function might not be present on all versions of the target platform.";
    helpUri="https://goblint.in.tum.de/home";
    longDescription="";
  };
  {
    name="Machine";
    ruleId="GO0009";
    helpText="TODO";
    shortDescription="TODO ";
    helpUri="https://goblint.in.tum.de/home";
    longDescription="";
  };
  {
    name="NullPointerDereference";
    ruleId="GO0010";
    helpText="Nullpointer dereference";
    shortDescription="A NULL pointer dereference occurs when the application dereferences a pointer that it expects to be valid, but is NULL, typically causing a crash or exit. ";
    helpUri="https://cwe.mitre.org/data/definitions/476.html";
    longDescription="";
  };
  {
    name="UseAfterFree";
    ruleId="GO0011";
    helpText="TODO";
    shortDescription="TODO ";
    helpUri="https://goblint.in.tum.de/home";
    longDescription="";
  };
  {
    name="PastEnd";
    ruleId="GO0012";
    helpText="TODO";
    shortDescription="TODO ";
    helpUri="https://goblint.in.tum.de/home";
    longDescription="";
  };
  {
    name="BeforeStart";
    ruleId="GO0013";
    helpText="TODO";
    shortDescription="TODO ";
    helpUri="https://goblint.in.tum.de/home";
    longDescription="";
  };
  {
    name="Unknown Aob";
    ruleId="GO0014";
    helpText="Array out of Bounds Error";
    shortDescription="Array out of Bounds Error ";
    helpUri="https://goblint.in.tum.de/home";
    longDescription="";
  };
  {
    name="362";
    ruleId="GO015";
    helpText="Concurrent Execution using Shared Resource with Improper Synchronization ('Race Condition')";
    shortDescription="The program contains a code sequence that can run concurrently with other code, and the code sequence requires temporary, exclusive access to a shared resource, but a timing window exists in which the shared resource can be modified by another code sequence that is operating concurrently. ";
    helpUri="https://cwe.mitre.org/data/definitions/362.html";
    longDescription="";
  };
  {
    name="416";
    ruleId="GO0016";
    helpText="Use After Free";
    shortDescription="Referencing memory after it has been freed can cause a program to crash, use unexpected values, or execute code. ";
    helpUri="https://cwe.mitre.org/data/definitions/416.html";
    longDescription="";
  };
  {
    name="476";
    ruleId="GO0017";
    helpText="NULL Pointer Dereference";
    shortDescription="A NULL pointer dereference occurs when the application dereferences a pointer that it expects to be valid, but is NULL, typically causing a crash or exit. ";
    helpUri="https://cwe.mitre.org/data/definitions/476.html";
    longDescription="";
  };
  {
    name="570";
    ruleId="GO0018";
    helpText="Expression is Always False";
    shortDescription="The software contains an expression that will always evaluate to false.";
    helpUri="https://cwe.mitre.org/data/definitions/570.html";
    longDescription="";
  };
  {
    name="571";
    ruleId="GO0019";
    helpText="Expression is Always True";
    shortDescription="The software contains an expression that will always evaluate to true.";
    helpUri="https://cwe.mitre.org/data/definitions/571.html";
    longDescription="";
  };
  {
    name="787";
    ruleId="GO0020";
    helpText="Out-of-bounds Write";
    shortDescription="The software writes data past the end, or before the beginning, of the intended buffer. ";
    helpUri="https://cwe.mitre.org/data/definitions/787.html";
    longDescription="";
  };
  {
    name="788";
    ruleId="GO0021";
    helpText="Access of Memory Location After End of Buffer";
    shortDescription="The software reads or writes to a buffer using an index or pointer that references a memory location after the end of the buffer. ";
    helpUri="https://cwe.mitre.org/data/definitions/788.html";
    longDescription="";
  }
]

(*Returns the information about a Rule. If the rule is not found, a default rule will be returned. *)
let getRuleInformation (searchName:string) =
  match List.find_opt (fun rule -> rule.name=searchName) rules with
  | None ->unknownRule ;
  | Some rule -> rule
