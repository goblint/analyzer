
open Printf;;
open Xml;;

(* File utils for reading and writing *)
let readFileLines filename = 
	let lines = ref [] in
	let chan = open_in filename in
	try
	  while true; do
	    lines := input_line chan :: !lines
	  done; []
	with End_of_file -> close_in chan;
	List.rev !lines;;

let readFileText filename = 
	let lines = readFileLines filename in
        String.concat "\n" lines;;

let saveFileText filename text = 
	let chan = open_out filename in
        output_string chan text;
	close_out chan;;


let getShLine lineStr =
	lineStr;;


(* Create html code lines from a source file *)
let createCodeLines filename xmlNode lineCount = 
	let htmlTemp_Codeline = (readFileText "html_template/codeline.html") in
	let lines = readFileLines filename in
	let isLineAnalyzed lineid = Xml.fold (fun b xmlChild -> (b or ((String.compare (Xml.attrib xmlChild "line") (string_of_int lineid)) = 0)) ) false xmlNode in
	let prepareCodeLine lineStr = 
		let a = Str.global_replace (Str.regexp_string "<") "&lt;" (getShLine lineStr) in
		let b = Str.global_replace (Str.regexp_string ">") "&gt;" a in
		b
	in
	let createCodeLine lineStr lineid = 
		let getReplaceText lineid = if ((isLineAnalyzed lineid) = true) then "<a href=\"javascript:showLine(%lineid%);\">" else "" in 
		let x = Str.global_replace (Str.regexp_string "%linkstart%") (getReplaceText lineid) htmlTemp_Codeline in
		let y = Str.global_replace (Str.regexp_string "%linkend%") "</a>" x in
 		let a = Str.global_replace (Str.regexp_string "%lineid%") (string_of_int lineid) y in
		let b = Str.global_replace (Str.regexp_string "%linetype%") (string_of_int (lineid mod 2)) a in
		Str.global_replace (Str.regexp_string "%code%") (prepareCodeLine lineStr) b
        in 
	let a,_ = (List.fold_left (fun (s,id) line -> (s^(createCodeLine line id),id+1)) ("",1) lines) in
	lineCount := List.length lines;
	a;;

(* Create analysis data tree view *)
let createAnalysisData filename xmlNode = 
	let htmlTemp_DivListEntry = (readFileText "html_template/divlistentry.html") in
	let htmlTemp_ListElement = (readFileText "html_template/listelement.html") in
	let createLineAnalysisContent lineid = 
		let rec addListEntry x =
			if (List.length (Xml.children x) = 0) then (Xml.attrib x "text")^"<br>"
			else 
				let m = Xml.fold (fun s child -> s^(addListEntry child)) "" x in
				if ((String.compare (Xml.attrib x "text") "Analyses") == 0) or ((String.compare (Xml.attrib x "text") "mapping") == 0) then m
				else 
					let o = Str.global_replace (Str.regexp_string "%listtitle%") (Xml.attrib x "text") htmlTemp_ListElement
					in Str.global_replace (Str.regexp_string "%listcontent%") m o
		in
		let getParentContent xmlChild = Xml.fold (fun s x -> s^(addListEntry x)) "" xmlChild in
		Xml.fold (fun s xmlChild -> if (String.compare (Xml.attrib xmlChild "line") lineid) = 0 then s^(getParentContent xmlChild) else s) "" xmlNode;
	in
	let createLineAnalysis lineid = 
        	let a = Str.global_replace (Str.regexp_string "%lineid%") lineid htmlTemp_DivListEntry in
		Str.global_replace (Str.regexp_string "%divcontent%") (createLineAnalysisContent lineid) a
        in
	Xml.fold (fun s xmlChild -> (s^(createLineAnalysis (Xml.attrib xmlChild "line"))) ) "" xmlNode;;


(* Create html output file for one .c source file *)
let createHtmlFile filename xmlNode =
	(* printf "Create html file : %s\n" filename;*)
        let htmlTemp_Base = (readFileText "html_template/base.html") in
	let shortFilename = Filename.basename filename in
	printf "Create html file : %s\n" shortFilename;
	let htmlStr = ref "" in
	let lineCount = ref 0 in 

	(* HTML title *)
	htmlStr := Str.global_replace (Str.regexp_string "%filename%") filename htmlTemp_Base;

	(* Code lines *)
	htmlStr := Str.global_replace (Str.regexp_string "%codelines%") (createCodeLines filename xmlNode lineCount) !htmlStr;

	(* Analysis data *)
	htmlStr := Str.global_replace (Str.regexp_string "%analysis_divs%") (createAnalysisData filename xmlNode) !htmlStr;

	(* Line count *)
	htmlStr := Str.global_replace (Str.regexp_string "%linecount%") (string_of_int !lineCount) !htmlStr;

	(* Save html file *)
	saveFileText (shortFilename^".html") !htmlStr;;


(* === print_fmt : html output === *)
let print_html chan xmlNode = 
	(*printf "Create html output ...\n";*)

	(* Create filename list *)
	let filenameList = Xml.fold (fun l xmlChild -> if (List.exists (fun e -> (String.compare e (Xml.attrib xmlChild "file")) = 0) l) then l else l@[Xml.attrib xmlChild "file"]) [] xmlNode in

	(* Crate html output for each filename *)
	List.iter (fun fileName -> createHtmlFile fileName xmlNode) filenameList;;


