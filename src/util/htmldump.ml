open Pretty
open Printf;;
open Xml;;
open Cil;;
open Unix;;
open Lazy;;
open Html_template;;

let htmlGlobalWarningList : (string*int*string) list ref = ref []

(* Some lists for syntax highlighting *)
let cppPreprocessorList = ["#endif";"#ifdef";"#undef";"#else";"include";"#define"];;
let cppKeywordList = ["int";"return";"void";"char";"short";"long";"unsigned";"if";"else";"switch";"default";"while";"do";"for";"typedef";"class";"struct";"sizeof";"case"];;

type entryType = {filename : string ; analysis_out : out_channel list ref ; lines : string list ref ; lineInfo : int array ref ; deadcodeInfo : int array ref} ;;
type functionInfo = {funname : string ; funid : int};;

let functionInfoList : functionInfo list ref = ref [];;
let functionInfoString = "";;

let createFunctionInfoList cilFile =
	let iterGlobals global =
		match global with
		  GFun (fundec,loc) -> functionInfoList := !functionInfoList @ [ { funname = fundec.svar.vname ; funid = fundec.svar.vid } ]
		| _ -> ()
	in
	List.iter (iterGlobals) cilFile.globals;
	();;

let createHtmlFunctionInfo outchan cilFile =
	let iterGlobals global =
		match global with
		  GFun (fundec,loc) -> 
		  	fprintf outchan "<div id=\"function_info%i\" style=\"display: none;\">Name: %s ()<br/>Line %i<br/>File: %s</div>" fundec.svar.vid fundec.svar.vname loc.line loc.file
		| _ -> ()
	in
	List.iter (iterGlobals) cilFile.globals;
	();;

let createHtmlWarningsInfo outchan fileEntry = 
	let htmlPrintWarningBox line = 
		fprintf outchan "<div id=\"warning_info%i\" style=\"display: none;\">" line;
		List.iter (fun (filename,lineTwo,msg) -> if (((String.compare filename fileEntry.filename) = 0) && (line = lineTwo)) then fprintf outchan "%s <br/>\n" msg else ()) !htmlGlobalWarningList;
		fprintf outchan "</div>"
	in
	Array.iteri (fun i x -> if (x >= 2) then htmlPrintWarningBox i) !(fileEntry.lineInfo);
	();;

let createHtmlWarningsListBox outchan fileEntry = 
	let ltid = ref 0 in
	let printWarningLine (filename,line,msg) =
		if ((String.compare filename fileEntry.filename) = 0) then fprintf outchan "<div class=\"lt%i\"><pre><a href=\"javascript:switchToLine('%s',%i);\">Line %i</a>: %s</pre></div>" (!ltid mod 2) (Filename.basename fileEntry.filename) line line msg else ();
		ltid := (!ltid)+1;
	in
	List.iter printWarningLine !htmlGlobalWarningList;
	if (!ltid) = 0 then fprintf outchan "No warnings found!";;

let createDeadcodeListBox outchan fileEntry = 
	let firstDeadLine = ref 0 in
	let ltid = ref 0 in

	let processArrayEntry i entry =  
		if ((entry == 2) && (!firstDeadLine == 0)) then firstDeadLine := i
		else if ((entry < 0) && (!firstDeadLine > 0)) then begin
			fprintf outchan "<div class=\"lt%i\"><pre><a href=\"javascript:switchToLine('%s',%i);\">Line %i</a> : %i line(s)</pre></div>" (!ltid mod 2) (Filename.basename fileEntry.filename) (!firstDeadLine) (!firstDeadLine) (i-(!firstDeadLine));
			firstDeadLine := 0;
			ltid := (!ltid)+1;
		end
	in
	Array.iteri processArrayEntry !(fileEntry.deadcodeInfo);
	if (!ltid) = 0 then fprintf outchan "No dead code found!";;

let createCodeLines outchan shortFilename lines lineInfo deadcodeInfo = 
	let currentLine = ref 1 in

	let isLineAnalyzed lineNo = (Array.get !lineInfo lineNo) > 0 in
	let isLineDeadcode lineNo = (Array.get !deadcodeInfo lineNo) > 1 in

	(* Replaces < and > with special html codes *)
	let replaceHtmlTags lineStr =
		let a = Str.global_replace (Str.regexp_string "<") "&lt;" lineStr in
		Str.global_replace (Str.regexp_string ">") "&gt;" a
	in

	let keywordHighlighter keyword =
		(* Keyword matching *)
		let isKeyword = List.exists (fun kw -> (String.compare kw keyword) = 0) cppKeywordList in
		if isKeyword then "<span class=\"cpp_keyword\">"^keyword^"</span>"
		else

		(* Function matching *)
		if List.exists (fun fi -> (String.compare fi.funname keyword) = 0) !functionInfoList then begin
			let funInfo = List.find (fun fi -> (String.compare fi.funname keyword) = 0) !functionInfoList in
			"<span class=\"cpp_function\"><a href=\"javascript:showFunctionInfo("^(string_of_int funInfo.funid)^");\">"^keyword^"</a></span>"
		end
		else
		keyword	
	in

	let rec syntaxHighlighter line =
		let getCharType character = if ((character = '#') || ((Char.code character >= Char.code '0') && (Char.code character <= Char.code '9')) || ((Char.code (Char.lowercase character) >= Char.code 'a') && (Char.code (Char.lowercase character) <= Char.code 'z'))) then 1 else 0 in
		let currentType = ref 0 in
		let currentStr = ref "" in
		let currentResult = ref "" in
		let lineStr = line in

		(* Single line comments and strings *)
		let firstCommentChar = try Str.search_forward (Str.regexp_string "//") lineStr 0 with Not_found -> -1 in
		let firstStringChar = try Str.search_forward (Str.regexp_string "\"") lineStr 0 with Not_found -> -1 in
		let secondStringChar = try Str.search_forward (Str.regexp_string "\"") lineStr (firstStringChar+1) with Not_found -> -1 in
		if (firstCommentChar >= 0) && ((firstCommentChar <= firstStringChar) || (firstStringChar = -1)) then (syntaxHighlighter (String.sub lineStr 0 firstCommentChar))^"<span class=\"cpp_comment\">"^(String.sub lineStr firstCommentChar ((String.length lineStr)-firstCommentChar))^"</span>"
		else if (firstStringChar >= 0) && (secondStringChar >= 0) then (syntaxHighlighter (String.sub lineStr 0 firstStringChar))^"<span class=\"cpp_stringDQ\">"^(String.sub lineStr firstStringChar ((secondStringChar-firstStringChar)+1))^"</span>"^(syntaxHighlighter (try (String.sub lineStr (secondStringChar+1) ((String.length lineStr)-secondStringChar-1)) with Invalid_argument s -> ""))
		else 

		(* Preprocessor matchings *)
		let searchForString s = try Str.search_forward (Str.regexp_string s) lineStr 0 with Not_found -> -1 in
		let firstPrepChar = List.fold_left (fun cur ss -> if cur >= 0 then cur else searchForString ss) (0-1) cppPreprocessorList in
		if firstPrepChar >= 0 then "<span class=\"cpp_preprocessor\">"^(replaceHtmlTags lineStr)^"</span>"
		else begin

		(* Divide line into keywords and non-keyword characters *)
		for i = 0 to (String.length line) do
			let letterType = if (i = (String.length line)) then 0 else getCharType (String.get line i) in
			if (letterType <> !currentType) then begin
				if !currentType = 1 then currentResult := !currentResult ^ (keywordHighlighter !currentStr);
				currentType := letterType;
				currentStr := ""
			end;
			if (i < (String.length line)) then
				if !currentType = 0 then currentResult := !currentResult ^ replaceHtmlTags (String.make 1 (String.get line i))
			   	else currentStr := !currentStr ^ replaceHtmlTags (String.make 1 (String.get line i))
			
		done;
		!currentResult
		end
	in

	let prepareLine line = 		
		syntaxHighlighter line
	in

	let createLine line = 
		if ((!currentLine mod 50000) = 0) then fprintf outchan "</div>\n<div id=\"linecontainer%i\" style=\"\">\n" (!currentLine/50000) else ();
		if (!currentLine = 1) then fprintf outchan "<div id=\"linecontainer%i\">\n" (!currentLine/50000) else ();
		
		let isAnalyzed = isLineAnalyzed !currentLine in
		let colorString = (
			if ((Array.get !lineInfo !currentLine) >= 2) then "color: red;"
			else "") 
		in
		let fontString = (
			if isLineDeadcode !currentLine then "color: #909090;"
			else "") 
		in
		let styleString = "style=\""^colorString^fontString^"\" " in
		let linkStart = if (isAnalyzed = true) then "<a "^styleString^"href=\"javascript:showLine('"^shortFilename^"',"^(string_of_int !currentLine)^");\">" else "" in
		let linkEnd = if (isAnalyzed = true) then "</a>" else "" in
		fprintf outchan "      <div id=\"line%i\" class=\"lt%i\"><pre> %s%i:%s %s</pre></div>\n" !currentLine (!currentLine mod 2) linkStart !currentLine linkEnd (prepareLine line);
		currentLine := !currentLine + 1;
		()
	in
	
	List.iter (fun lc -> createLine lc) (List.rev !lines);
	();;

let createGlobalMenu outchan shortFilename filename cilFile =
	(* Functions *)
	fprintf outchan "<div><span class=\"toggle entrydir\" >Functions</span><div class=\"entrydircontent\">\n";
	let iterFunctions global =
		match global with
		  GFun (fundec,loc) -> 
		  	if ((String.compare filename loc.file) = 0) then fprintf outchan "%s <a href=\"javascript: ScrollToLine(%i); showLine('%s',%i);\" style=\"color: #606060;\">(Line %i)</a><br/>\n" fundec.svar.vname loc.line shortFilename loc.line loc.line else ()
		| _ -> ()
	in
	List.iter (iterFunctions) cilFile.globals;
	fprintf outchan "</div></div>\n";

	(* Variables *)
	fprintf outchan "<div><span class=\"toggle entrydir\" >Variables</span><div class=\"entrydircontent\">\n";
	let iterVars global =
		match global with
		  GVar (varinfo,initinfo,loc) -> 
		  	if ((String.compare filename loc.file) = 0) then fprintf outchan "%s <a href=\"javascript:ScrollToLine(%i); showLine('%s',%i);\" style=\"color: #606060;\">(Line %i)</a><br/>\n" varinfo.vname loc.line shortFilename loc.line loc.line else ()
		| _ -> ()
	in
	List.iter (iterVars) cilFile.globals;
	fprintf outchan "</div></div>\n";

	(* Types *)
	fprintf outchan "<div><span class=\"toggle entrydir\" >Types</span><div class=\"entrydircontent\">\n";
	let iterTypes global =
		match global with
		  GType (typeinfo,loc) -> 
		  	if ((String.compare filename loc.file) = 0) then fprintf outchan "%s <a href=\"javascript:ScrollToLine(%i); showLine('%s',%i);\" style=\"color: #606060;\">(Line %i)</a><br/>\n" typeinfo.tname loc.line shortFilename loc.line loc.line else ()
		| _ -> ()
	in
	List.iter (iterTypes) cilFile.globals;
	fprintf outchan "</div></div>\n";

	(* Enums *)
	fprintf outchan "<div><span class=\"toggle entrydir\" >Enums</span><div class=\"entrydircontent\">\n";
	let iterEnums global =
		match global with
		  GEnumTag (enuminfo,loc) -> 
		  	if ((String.compare filename loc.file) = 0) then fprintf outchan "%s <a href=\"javascript:ScrollToLine(%i); showLine('%s',%i);\" style=\"color: #606060;\">(Line %i)</a><br/>\n" enuminfo.ename loc.line shortFilename loc.line loc.line else ()
		| _ -> ()
	in
	List.iter (iterEnums) cilFile.globals;
	fprintf outchan "</div></div>\n";

	();;

let createGlobalsTable outchan gtable =
	let trid = ref 0 in

	let rec getTableEntryCode xmlNode =
		if (List.length (Xml.children xmlNode) = 0) then "<div>"^(Xml.attrib xmlNode "text")^"</div>"
		else begin
			let foldedStr = Xml.fold (fun s n -> s^(getTableEntryCode n) ) "" xmlNode in
			"<div><span class=\"toggle entrydir\" >"^( if (String.compare (Xml.attrib xmlNode "text") "mapping") = 0 then "Mapping" else "Node")^"</span><div style=\"display: none;\" class=\"entrydircontent\">\n"^foldedStr^"</div></div>\n";

		end
	in
	let parseTdNode tdnode =
		try
		Xml.iter (fun child -> fprintf outchan "<td>%s</td>\n" (getTableEntryCode child) ) tdnode;
		with Xml.Not_element _ -> fprintf outchan "%s\n" (Xml.to_string tdnode);
		()
	in
	let parseTrNode trnode = 
		fprintf outchan "<tr style=\"background-color: %s;\">" (if (!trid == 0) then "#D0D0D0" else "#F0F0F0");
		trid := ((!trid)+1) mod 2;
		Xml.iter (fun tdnode -> parseTdNode tdnode ) trnode;
		fprintf outchan "</tr>";
	in
	let parseGlobalsXmlFile xmlfile =
		Xml.iter (fun trnode -> parseTrNode trnode ) xmlfile;
	in
	fprintf outchan "<table>";
	List.iter (fun xmlfile -> parseGlobalsXmlFile xmlfile ) (Lazy.force gtable);
	fprintf outchan "</table>";
	();;

let generateCodeFile fileEntry (file: Cil.file) gtable =
	let shortFilename = Filename.basename fileEntry.filename in

	(* Create output file *)
	let outputChannel = open_out (Filename.concat "result" (shortFilename^".html")) in

	(* Write first html part *)
	let firstPartA = Str.global_replace (Str.regexp_string "%filename%") fileEntry.filename htmlTemp_BasePartOne in
	let firstPartB = Str.global_replace (Str.regexp_string "%shortfilename%") shortFilename firstPartA in
	fprintf outputChannel "%s" firstPartB;

	(* Create list of globals *)
	createGlobalMenu outputChannel shortFilename fileEntry.filename file;

	(* Write second first part *)
	fprintf outputChannel "%s" htmlTemp_BasePartOneSecond;

	(* Create global table *)
	createGlobalsTable outputChannel gtable;

	(* Write third first part *)
	fprintf outputChannel "%s" htmlTemp_BasePartOneThird;	

	(* Write code lines *)
	createCodeLines outputChannel shortFilename fileEntry.lines fileEntry.lineInfo fileEntry.deadcodeInfo;

	(* Write forth first part *)
	fprintf outputChannel "%s" htmlTemp_BasePartOneForth;
	createHtmlWarningsListBox outputChannel fileEntry;

	(* Write forth first part *)
	fprintf outputChannel "%s" htmlTemp_BasePartOneFifth;
	createDeadcodeListBox outputChannel fileEntry;

	(* Write second part *)
	fprintf outputChannel "%s" htmlTemp_BasePartTwo;

	(* Write function infos *)
	createHtmlFunctionInfo outputChannel file;

	(* Write function infos *)
	createHtmlWarningsInfo outputChannel fileEntry;

	(* Write third part *)
	fprintf outputChannel "%s" htmlTemp_BasePartThree;
	close_out outputChannel;
	();;

(* Read code lines from file into a list *)
let readCodeLines filename lines = 
	let chan_code = open_in filename in
	try
		while true; do
			lines := input_line chan_code :: !lines;
		done;
	with End_of_file -> close_in chan_code;
	();;

(* === print_fmt : html output === *)
let print_html chan xmlNode (file: Cil.file) gtable = 
	printf "[HTML-Output] Create html files ...\n";	
	(*printf "Start: %f \n" (Unix.time ());*)
	(*List.iter (fun xmlfile -> printf "GTABLE:\n%s\n" (Xml.to_string_fmt xmlfile)) (Lazy.force gtable);*)

	flush_all ();

	(* fileList contains all analyzed filenames, the output file handle, .. *)
	let fileList = ref [] in

	(* Creates an entry in fileList *)
	let createFileListEntry lineFile = 
		let shortFilename = Filename.basename lineFile in

		(* Read code lines from file *)
		let linesContent = ref [] in
		readCodeLines lineFile linesContent;	
    
		(* Create analysis data directory *)
		let dirn = (Filename.concat "result" (shortFilename^"_data")) in
		try (Unix.mkdir dirn 0o777) with _ -> (); ;

		(* Create analysis files *)
		let lineCount = (List.length !linesContent) in
		let outlist = ref [] in
		for i = 0 to (lineCount / 1000) do begin
			let outchan = open_out (dirn^"/analysis"^(string_of_int i)^".html") in
			outlist := !outlist @ [outchan];
			fprintf outchan "%s" htmlTemp_AnalysisFilePartOne;
		end done;

		(* Return new fileList entry *)		
		{filename = lineFile ; analysis_out = outlist; lines = linesContent ; lineInfo = ref (Array.make (lineCount+1) 0) ; deadcodeInfo = ref (Array.make (lineCount+1) 0)}
	in

	(* Finds an entry in fileList *)
	let getFileListEntry lineFile = 
		try
		List.find (fun entry -> (String.compare entry.filename lineFile) == 0) !fileList
		with Not_found -> begin
			let newEntry = createFileListEntry lineFile in
			fileList := newEntry :: !fileList;
			newEntry;
		end
	in

	(* Process xml node of an analysis line *)
	let rec processAnalysisXmlNode outchan xmlParentNode =
		if (List.length (Xml.children xmlParentNode) = 0) then fprintf outchan "%s<br>\n" (Xml.attrib xmlParentNode "text")
		else begin
			(* Tree node directory start *)
			fprintf outchan "<div><span class=\"toggle entrydir\" >%s</span><div class=\"entrydircontent\">\n" (Xml.attrib xmlParentNode "text");

			(* Process children *)
			Xml.iter (fun x -> processAnalysisXmlNode outchan x) xmlParentNode;

			(* Tree node directory end *)
			fprintf outchan "</div></div>\n";
		end
	in

	(* Processes each line in the xml file *)
	let processAnalysisLineEntry xmlNodeLine = 
		let lineFile = Xml.attrib xmlNodeLine "file" in
		let lineNo = int_of_string (Xml.attrib xmlNodeLine "line") in
		let fileListEntry = (getFileListEntry lineFile) in

		Array.set !(fileListEntry.lineInfo) lineNo 1;

		(* Get analysis output channel *)
		let outchan = List.nth !(fileListEntry.analysis_out) (lineNo/1000) in

		(* Print start line div container *)
		fprintf outchan "     <div id=\"analysis_line%i\" style=\"display: none;\">\n" lineNo;

		(* Print analysis data *)
		List.iter (processAnalysisXmlNode outchan) (Xml.children xmlNodeLine);	

		(* Print end line div container *)
		fprintf outchan "     </div>\n";

		(* Find deadcode : Empty Value Leaf *)
		let checkForEmptyValueLeaf xmlNode = 
			if ((String.compare (Xml.attrib xmlNode "text") "Value") == 0) then
				if ((String.compare (Xml.tag xmlNode) "Leaf") == 0) then Array.set !(fileListEntry.deadcodeInfo) lineNo ((Array.get !(fileListEntry.deadcodeInfo) lineNo)+2)
				else Array.set !(fileListEntry.deadcodeInfo) lineNo (-100)
		in
		List.iter (fun x -> List.iter checkForEmptyValueLeaf (Xml.children x)) (Xml.children xmlNodeLine);
	in

	(* Create function infos *)
	createFunctionInfoList file;

	(* Walk through the analysis lines in the xml file *)
	Xml.iter (fun x -> processAnalysisLineEntry x) xmlNode;

	(* Last line is no deadcode : hack *)
	List.iter (fun fileEntry -> (Array.set !(fileEntry.deadcodeInfo) ((Array.length !(fileEntry.deadcodeInfo))-1) (-1))) !fileList;

	(* Add warning values to the lineInfo array *)
	List.iter (fun (filename,line,msg) -> Array.set !((getFileListEntry filename).lineInfo) line 2) !htmlGlobalWarningList;

	(* Generate main html files with the code lines *)
	List.iter (fun fileEntry -> generateCodeFile fileEntry file gtable) !fileList;

	(* Close analysis data files *)
	let closeAnalysisFile chan =
		fprintf chan "%s" htmlTemp_AnalysisFilePartTwo;
		close_out chan
	in
	List.iter (fun fileEntry -> List.iter (fun chan -> closeAnalysisFile chan) !(fileEntry.analysis_out) ) !fileList;

	(*printf "End: %f \n" (Unix.time ());*)
	;;

