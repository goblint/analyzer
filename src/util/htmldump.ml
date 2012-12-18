open Pretty
open Printf;;
open Xml;;
open Cil;;
open Unix;;

let htmlLocalWarningList : (string*int*string) list ref = ref []
let htmlGlobalWarningList : (string*int*string) list ref = ref []
let warningLineNumberList = ref []

let htmlTemp_BasePartOne = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n<head>\n  <title>%filename%</title>\n  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n\n  
<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"></link>\n
<script type=\"text/javascript\" src=\"script.js\"></script>\n
</head>\n\n<body onload=\"onLoad();\">\n\n  <div style=\"border-bottom: 1px solid #808080;\">\n    <div style=\"border-bottom: 1px solid #000000; color: #5060C0; background-color: #F0F0F0; font-size: 16px; padding: 2px; font-weight: bold;\">\n      Filename: %filename%\n    </div>\n   </div>\n\n   <noscript><span style=\"padding: 10px; font-size: 18px; border: 2px solid Red;\">Javascript is <b>not</b> enabled!</noscript>\n\n  <div id=\"leftWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Analysis</div>\n    <div id=\"leftWindowContent\" class=\"mywindow_content\">\n      \n      <div id=\"analysis_line0\">\n        No line selected\n      </div>\n\n      <div id=\"dynamicanalysis\">\n\n      </div>\n    </div>\n  </div>\n\n  <div id=\"codeWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Source code</div>\n    <div id=\"codeWindowContent\" class=\"mywindow_content\">\n\n";;
let htmlTemp_BasePartTwo = "      <div id=\"lastline\" class=\"linetype0\" style=\"border-top: 1px solid #C0C0C0;\"><pre></pre></div>\n    </div>\n  </div>\n\n  <div id=\"rightWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Empty <a style=\"text-decoration: none; color: #000080;\" href=\"javascript:hideWindow('rightWindow');\">[X]</a></div>\n    <div id=\"rightWindowContent\" class=\"mywindow_content\">\n      Currently no content\n    </div>\n  </div>\n\n  <div id=\"bottomWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\"><span id=\"title_function_info\" style=\"display: none;\">Function Info</span><span id=\"title_warning_info\" style=\"display: none;\">Warning Info</span><a style=\"text-decoration: none; color: #000080;\" href=\"javascript:hideWindow('bottomWindow');\">[X]</a></div>\n    <div id=\"bottomWindowContent\" class=\"mywindow_content\">\n\n";;
let htmlTemp_BasePartThree = "    </div>\n  </div>\n\n</body>\n</html>\n\n";;

(* Some lists for syntax highlighting *)
let cppPreprocessorList = ["#endif";"#ifdef";"#undef";"#else";"include";"#define"];;
let cppKeywordList = ["int";"return";"void";"char";"short";"long";"unsigned";"if";"else";"switch";"default";"while";"do";"for";"typedef";"class";"struct";"sizeof";"case"];;

type entryType = {filename : string ; analysis_out : out_channel list ref} ;;
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

let createHtmlWarningsInfo outchan cilFile = 
	let htmlprintWarningBox lineno = 
		fprintf outchan "<div id=\"warning_info%i\" style=\"display: none;\">" lineno;
		let lineWarnings = List.filter (fun (_,line,_) -> line==lineno) !htmlLocalWarningList in
		List.iter (fun (filename,line,msg) -> fprintf outchan "%s <br/>\n" msg) lineWarnings;
		List.iter (fun (filename,line,msg) -> fprintf outchan "%s <br/>\n" msg) !htmlGlobalWarningList;
		fprintf outchan "</div>"
	in
	List.iter htmlprintWarningBox !warningLineNumberList;;

let createCodeLines outchan filename xmlNode lineCount = 
	let shortFilename = Filename.basename filename in
	let currentLine = ref 1 in

	let isLineAnalyzed lineNo = true in

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
		let isAnalyzed = isLineAnalyzed !currentLine in
		let linkColor = (if (List.exists (fun l -> (l = !currentLine)) !warningLineNumberList) then "red" else "black") in
		let linkStart = if (isAnalyzed = true) then "<a style=\"color: "^linkColor^"\" href=\"javascript:showLine('"^shortFilename^"',"^(string_of_int !currentLine)^");\">" else "" in
		let linkEnd = if (isAnalyzed = true) then "</a>" else "" in
		fprintf outchan "      <div id=\"line%i\" class=\"linetype%i\"><pre> %s%i:%s %s</pre></div>\n" !currentLine (!currentLine mod 2) linkStart !currentLine linkEnd (prepareLine line);
		currentLine := !currentLine + 1;
		()
	in
	
	(* Open input code file and create lines *)
	let chan_code = open_in filename in
	try
		while true; do
			createLine (input_line chan_code);
			lineCount := !lineCount + 1;
		done;
	with End_of_file -> close_in chan_code;
	();;

(* === print_fmt : html output === *)
let print_html chan xmlNode (file: Cil.file) = 
	printf "[HTML-Output] Create html files ...\n";	
	printf "Start: %f \n" (Unix.time ());
	flush_all ();

	(* fileList contains all analyzed filenames, the output file handle, .. *)
	let fileList = ref [] in

	(* Creates an entry in fileList *)
	let createFileListEntry lineFile = 
		let shortFilename = Filename.basename lineFile in
		let lineCount = ref 0 in

		(* Create output file *)
		let outputChannel = open_out (Filename.concat "result" (shortFilename^".html")) in

		(* Write first html part *)
		let firstPartA = Str.global_replace (Str.regexp_string "%filename%") lineFile htmlTemp_BasePartOne in
		let firstPartB = Str.global_replace (Str.regexp_string "%shortfilename%") shortFilename firstPartA in
		fprintf outputChannel "%s" firstPartB;

		(* Write code lines *)
		createCodeLines outputChannel lineFile xmlNode lineCount;

		(* Write second part *)
		fprintf outputChannel "%s" htmlTemp_BasePartTwo;

		(* Write function infos *)
		createHtmlFunctionInfo outputChannel file;

		(* Write warning infos *)
		createHtmlWarningsInfo outputChannel file;

		(* Write third part *)
		fprintf outputChannel "%s" htmlTemp_BasePartThree;
		close_out outputChannel;
    
		(* Create analysis data directory *)
		let dirn = (Filename.concat "result" (shortFilename^"_data")) in
		try (Unix.mkdir dirn 0o777) with _ -> (); ;

		(* Create analysis files *)
		let outlist = ref [] in
		for i = 0 to (!lineCount / 1000) do outlist := !outlist @ [open_out (dirn^"/analysis"^(string_of_int i)^".html")] done;

		(* Return new fileList entry *)		
		{filename = lineFile ; analysis_out = outlist}
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

		(* Get analysis output channel *)
		let outchan = List.nth !(fileListEntry.analysis_out) (lineNo/1000) in

		(* Print start line div container *)
		fprintf outchan "     <div id=\"analysis_line%i\" style=\"display: none;\">\n" lineNo;

		(* Print analysis data *)
		List.iter (processAnalysisXmlNode outchan) (Xml.children xmlNodeLine);	

		(* Print end line div container *)
		fprintf outchan "     </div>\n";
	in

	(* Create function infos *)
	createFunctionInfoList file;

	(* Create warning line number list *)
	warningLineNumberList := List.fold_left (fun l (filename,line,msg) -> if List.mem line l then l else l@[line]) [] (!htmlLocalWarningList @ !htmlGlobalWarningList);

	(* Walk through the analysis lines in the xml file *)
	Xml.iter (fun x -> processAnalysisLineEntry x) xmlNode;

	printf "End: %f \n" (Unix.time ());
	;;

