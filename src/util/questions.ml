open Printf
open Scanf

let rec split text zeichen =
  try
    let index = String.index text zeichen in
    (String.sub text 0 index, String.sub text (index+1) ((String.length text)-index-1) )
  with Not_found -> (text,"")

type questionvaluetype = QVNone | QVInt of int | QVBool of bool | QVString of string | QVIntList of int list | QVBoolList of bool list | QVStringList of string list
type questionvalue = {qv_line_start: int; qv_line_end: int; qv_value: questionvaluetype}
type questionentry = {qf_analysis: string; qf_question: string; qf_type: int; qf_values : questionvalue list ref; qf_default : questionvalue ref}

let questionentrylist = ref []

let type_to_num s =
  match s with
    "int" -> 1
  | "bool" -> 2
  | "string" -> 3
  | "intlist" -> 4
  | "boollist" -> 5
  | "stringlist" -> 6
  | _ -> 0


let num_to_type n =
  match n with
    1 -> "int"
  | 2 -> "bool"
  | 3 -> "string"
  | 4 -> "intlist"
  | 5 -> "boollist"
  | 6 -> "stringlist"
  | _ -> ""


let value_to_str value =
  match value with
    QVInt i -> string_of_int i
  | QVBool b -> if (b == true) then "true" else "false"
  | QVString s -> s
  | QVIntList l ->
    begin
      if ((List.length l) == 0) then "[]"
      else let s = List.fold_left (fun s i -> s^","^(string_of_int i)) (string_of_int (List.hd l)) (List.tl l)
        in "["^s^"]"
    end
  | QVBoolList l ->
    begin
      if ((List.length l) == 0) then "[]"
      else let s = List.fold_left (fun s i -> s^","^(string_of_bool i)) (string_of_bool (List.hd l)) (List.tl l)
        in "["^s^"]"
    end
  | QVStringList l ->
    begin
      if ((List.length l) == 0) then "[]"
      else let s = List.fold_left (fun s i -> s^","^i) (List.hd l) (List.tl l)
        in "["^s^"]"
    end
  | _ -> ""



(* Load the question database from a file *)
let question_load_db filename =
  printf "Load: %s\n" filename;
  let chan = Pervasives.open_in filename in
  try
    let mode = ref 0 in
    let entryid = ref 0 in

    while true; do
      let line = String.trim (input_line chan) in
      if ((String.length line) >= 1) then begin
        (* No active entry (mode == 0) : begin entry and check for { *)
        if (!mode == 0) then begin
          let (aname,strbuf) = split line ':' in
          let (qnameraw,typename) = split strbuf ':' in
          let qname = String.sub qnameraw 1 ((String.length qnameraw)-2) in
          if ((String.compare (String.trim (input_line chan)) "{") != 0) then printf "Question Database Error: \"{\" expected!\n"
          else ();
          mode := 1;
          entryid := List.length !questionentrylist;
          questionentrylist := !questionentrylist @ [{qf_analysis = aname; qf_question = qname; qf_type = (type_to_num typename); qf_values = ref []; qf_default = ref {qv_line_start = 0; qv_line_end = 0; qv_value = QVNone} }];
        end
        (* Add values to current entry *)
        else if (!mode == 1) then begin
          if ((String.compare line "}") == 0) then begin
            mode := 0
          end
          else begin
            (* Seperate line and value *)
            let (valuename,valuestring) = (split line '=') in
            (* Parse line start and line end *)
            let (linestart,lineend) =
              if ((String.compare valuename "default") == 0) then (0,0)
              else begin
                let (linestartstr,lineendstr) = split valuename '-' in
                if ((String.length lineendstr) == 0) then (int_of_string linestartstr,int_of_string linestartstr)
                else (int_of_string linestartstr,int_of_string lineendstr)
              end
            in
            (* Parse value *)
            let str_to_bool s = if ((String.compare s "true") == 0) then true else false in
            let qentry = (List.nth !questionentrylist !entryid) in
            let value =
              if (qentry.qf_type == 1) then QVInt (int_of_string valuestring)
              else if (qentry.qf_type == 2) then QVBool (str_to_bool valuestring)
              else if (qentry.qf_type == 3) then QVString valuestring
              else if (qentry.qf_type == 4) then begin
                let rec parsestr s =
                  let (a,b) = split s ',' in
                  if ((String.length a) == 0) then []
                  else if ((String.length b) == 0) then [int_of_string a]
                  else [int_of_string a]@(parsestr b)
                in
                QVIntList (parsestr (String.sub valuestring 1 ((String.length valuestring)-2)))
              end
              else if (qentry.qf_type == 5) then begin
                let rec parsestr s =
                  let (a,b) = split s ',' in
                  if ((String.length a) == 0) then []
                  else if ((String.length b) == 0) then [str_to_bool a]
                  else [str_to_bool a]@(parsestr b)
                in
                QVBoolList (parsestr (String.sub valuestring 1 ((String.length valuestring)-2)))
              end
              else if (qentry.qf_type == 6) then begin
                let rec parsestr s =
                  let (a,b) = split s ',' in
                  if ((String.length a) == 0) then []
                  else if ((String.length b) == 0) then [a]
                  else [a]@(parsestr b)
                in
                QVStringList (parsestr (String.sub valuestring 1 ((String.length valuestring)-2)))
              end
              else QVNone
            in
            (* Add value to the entry's list *)
            if (linestart == 0) then qentry.qf_default := {qv_line_start = 0; qv_line_end = 0; qv_value = value}
            else qentry.qf_values := !(qentry.qf_values) @ [{qv_line_start = linestart; qv_line_end = lineend; qv_value = value}]
          end
        end
        else ()
      end
      else ()
    done;
  with End_of_file ->
    Pervasives.close_in chan

(* Save the question database to a file *)
let question_save_db filename =
  printf "Save: %s\n" filename;
  let chan = Pervasives.open_out filename in
  let writeentry e =
    fprintf chan "%s:\"%s\":%s\n{\n" e.qf_analysis e.qf_question (num_to_type e.qf_type);
    fprintf chan "  default=%s\n" (value_to_str !(e.qf_default).qv_value);
    let writevalue v =
      if (v.qv_line_end > v.qv_line_start) then fprintf chan "  %i-%i=%s\n" v.qv_line_start v.qv_line_end (value_to_str v.qv_value)
      else fprintf chan "  %i=%s\n" v.qv_line_start (value_to_str v.qv_value)
    in
    List.iter writevalue !(e.qf_values);
    fprintf chan "}\n\n"
  in
  List.iter writeentry !questionentrylist;
  Pervasives.close_out chan


(* Register a question *)
let question_register analysisname question defaultvalue =
  (* Find question name for an analysis *)
  let rec findindex l current =
    if ((List.length l) == 0) then -1
    else
      let e = (List.hd l) in
      let issame = (((String.compare e.qf_analysis analysisname) == 0) && ((String.compare e.qf_question question) == 0)) in
      if (issame == true) then current
      else findindex (List.tl l) (current+1)
  in
  let index = findindex (!questionentrylist) 0 in
  if (index >= 0) then index
  else begin
    let typenum =
      match defaultvalue with
        QVInt _ -> 1
      | QVBool _ -> 2
      | QVString _ -> 3
      | QVIntList _ -> 4
      | QVBoolList _ -> 5
      | QVStringList _ -> 6
      | _ -> 0
    in
    questionentrylist := !questionentrylist @ [{qf_analysis = analysisname; qf_question = question; qf_type = typenum; qf_values = ref []; qf_default = ref {qv_line_start = 0; qv_line_end = 0; qv_value = defaultvalue} }];
    (List.length !questionentrylist)-1
  end

(* Print question database for debugging *)
let question_db_print qlist =
  printf "DB entries : %i\n" (List.length qlist);
  let printentry e =
    printf "Question: %s in analysis \"%s\"\n" e.qf_question e.qf_analysis;
    let printvalue v =
      printf "  > %i to %i\n" v.qv_line_start v.qv_line_end
    in
    List.iter printvalue !(e.qf_values)
  in
  List.iter printentry qlist

let question_getvalue questionid line =
  let entry = (List.nth !questionentrylist questionid) in
  let value = ref !(entry.qf_default).qv_value in
  List.iter (fun e -> if ((line >= e.qv_line_start) && (line <= e.qv_line_end)) then value := e.qv_value else ()) !(entry.qf_values);
  !value

let question_getvalue_int questionid line =
  match (question_getvalue questionid line) with
    QVInt i -> i
  | _ -> 0

let question_getvalue_bool questionid line =
  match (question_getvalue questionid line) with
    QVBool i -> i
  | _ -> false

let question_getvalue_string questionid line =
  match (question_getvalue questionid line) with
    QVString s -> s
  | _ -> ""

let question_getvalue_intlist questionid line =
  match (question_getvalue questionid line) with
    QVIntList i -> i
  | _ -> []

let question_getvalue_boollist questionid line =
  match (question_getvalue questionid line) with
    QVBoolList i -> i
  | _ -> []

let question_getvalue_stringlist questionid line =
  match (question_getvalue questionid line) with
    QVStringList s -> s
  | _ -> []
