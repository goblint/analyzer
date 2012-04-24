
open Printf;;
open Xml;;
open Cil;;
open Unix;;

(* Reads a text file into a list (each line is a list entry) *)
let readFileLines filename = 
	let lines = ref [] in
	let chan = open_in filename in
	try
	  while true; do
	    lines := input_line chan :: !lines
	  done; []
	with End_of_file -> close_in chan;
	List.rev !lines;;


let htmlTemp_BasePartOne = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n<head>\n  <title>%filename%</title>\n  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n\n  <style type=\"text/css\">\n    <!--\n    html , body {\n      background-color: #FFFFFF;\n      margin: 0;\n      padding: 0;\n    }\n  \n    .mywindow \n    {\n        border: 1px solid #C0C0C0;\n        position: fixed;\n        overflow: hidden;\n        width: auto;\n        height: auto;\n        background-color: #F0F0F0;\n        overflow: hidden;\n    }\n  \n    .mywindow_header \n    {\n        background-color: #E0E0E0;\n        border-bottom: 1px solid #C0C0C0;\n        color: #4050A0;\n        font-family: Arial;\n        font-weight: bold;\n        font-size: 16px;\n        padding: 2px;\n    }\n  \n    .mywindow_content \n    {\n        overflow: scroll;\n        padding: 0px;\n        width: 0px;\n        height: 0px;\n    }\n  \n    #leftWindowContent\n    {\n      background-color: #F0F0F0;\n      padding: 0px;\n      margin-right: 0px;\n      font-size: 14px;\n      font-family: Arial,Monospace;     \n    }  \n \n    #codeWindowContent\n    {\n      background-color: #F8F8F8;\n    }\n        \n    #codeWindowContent a \n    {\n      text-decoration: none;\n      color: #000000;\n      font-weight: bold;\n    }\n        \n    #codeWindowContent a:hover \n    {\n      text-decoration: underline;\n      color: #0000C0;\n    }\n        \n    #codeWindowContent pre \n    {\n      padding: 0px;\n      margin: 0px;\n    }\n  \n    .linetype0\n    {\n      background-color: #F0F0F0;\n    }\n        \n    .linetype1\n    {\n      background-color: #F8F8F8;\n    }\n  \n    .cpp_datatype\n    {\n      color: blue;\n    }\n        \n    .cpp_keyword\n    {\n      color: blue;            \n    }\n\n    .cpp_preprocessor\n    {\n      color: purple;            \n    }\n        \n    .cpp_stringDQ\n    {\n      color: red;            \n    }\n        \n    .cpp_stringSQ\n    {\n      color: red;            \n    }\n        \n    .cpp_comment\n    {\n      color: Green;\n    }\n\n    .cpp_function\n    {\n    }\n\n    .cpp_function a\n    {\n      color: Green;\n      text-decoration: none;\n      font-weight: normal;\n    }\n    -->\n  </style>\n\n  <script type=\"text/javascript\">\n\n    var loadedPage = -1;\n    var requestedLine = 0;\n    var lineSelected = 0;\n    var functionSelected = 0;\n    var dynamicLoading = false;\n\n    // Try to create and initialize a HttpObject\n    var xmlHttpObject = false;\n\n    if (typeof XMLHttpRequest != 'undefined') \n    {\n        xmlHttpObject = new XMLHttpRequest();\n    }\n    if (!xmlHttpObject) \n    {\n        try \n        {\n            xmlHttpObject = new ActiveXObject(\"Msxml2.XMLHTTP\");\n        }\n        catch(e) \n        {\n            try \n            {\n                xmlHttpObject = new ActiveXObject(\"Microsoft.XMLHTTP\");\n            }\n            catch(e) \n            {\n                xmlHttpObject = null;\n            }\n        }\n    }\n\n    function htmlHandleContent()\n    {\n        if (xmlHttpObject.readyState == 4)\n        {\n            document.getElementById('dynamicanalysis').innerHTML = xmlHttpObject.responseText;\n            MakeLineVisible(requestedLine);\n         }\n    }\n\n    function htmlLoadContent(i)\n    {\n        xmlHttpObject.open('get','%shortfilename%_data/analysis'+i+'.html');\n        xmlHttpObject.onreadystatechange = htmlHandleContent;\n        xmlHttpObject.send(null);\n        return false;\n    }\n\n    function resizeAll() {\n      var leftWindow = document.getElementById('leftWindow');\n      var leftWindowContent = document.getElementById('leftWindowContent');\n      var rightWindow = document.getElementById('rightWindow');\n      var rightWindowContent = document.getElementById('rightWindowContent');\n      var bottomWindow = document.getElementById('bottomWindow');\n      var bottomWindowContent = document.getElementById('bottomWindowContent');\n      var codeWindow = document.getElementById('codeWindow');\n      var codeWindowContent = document.getElementById('codeWindowContent');\n\n      rightWindow.style.display = 'none';\n\n      var codeWindowTop = 35;\n      var leftWindowWidth = 300;\n      var rightWindowWidth = 250;\n      if (rightWindow.style.display == 'none') rightWindowWidth = 0;\n      var bottomWindowHeight = 130;\n      if (bottomWindow.style.display == 'none') bottomWindowHeight = 0;\n      var bottomWindowTop = (window.innerHeight - bottomWindowHeight - 35);\n      var codeWindowHeight = bottomWindowTop - codeWindowTop - 35;\n      var codeWindowWidth = (window.innerWidth - (leftWindowWidth + rightWindowWidth + 2 + 2 + 10 + 10 + 10 + 10));\n\n      leftWindow.style.top = codeWindowTop + 'px';\n      leftWindow.style.left = '10px';\n      leftWindowContent.style.width = leftWindowWidth + 'px';\n      leftWindowContent.style.height = codeWindowHeight + 'px';\n\n      rightWindow.style.top = codeWindowTop + 'px';\n      rightWindow.style.left = (window.innerWidth - rightWindowWidth - 2 - 10) + 'px';\n      rightWindowContent.style.width = rightWindowWidth + 'px';\n      rightWindowContent.style.height = codeWindowHeight + 'px';\n\n      bottomWindow.style.top = bottomWindowTop + 'px';\n      bottomWindow.style.left = '10px';\n      bottomWindowContent.style.width = (window.innerWidth - 20) + 'px';\n      bottomWindowContent.style.height = bottomWindowHeight + 'px';\n\n      codeWindow.style.top = codeWindowTop + 'px';\n      codeWindow.style.left = (leftWindowWidth + 2 + 10 + 10) + 'px';\n      codeWindowContent.style.width = codeWindowWidth + 'px';\n      codeWindowContent.style.height = codeWindowHeight + 'px';\n    }\n\n    function onLoad() {\n      resizeAll();\n      window.onresize = resizeAll;\n    }\n\n    function MakeLineVisible(i) {\n      var el;\n      el = document.getElementById('analysis_line' + lineSelected);\n      if (el != null) el.style.display = 'none';\n      \n      document.getElementById('analysis_line' + i).style.display = '';\n      if (lineSelected != 0) {\n        if ((lineSelected % 2) == 0) document.getElementById('line'+lineSelected).style.background = '#F0F0F0';\n        if ((lineSelected % 2) == 1) document.getElementById('line'+lineSelected).style.background = '#F8F8F8';\n        lineSelected = 0;\n      }\n      if (i != 0) {\n        lineSelected = i;\n        document.getElementById('line'+lineSelected).style.background = '#FFFFD8';        \n      }\n\n    }\n\n    function showLine(i) {\n      var remainder = i % 1000;\n      var reqPage = ( i - remainder ) / 1000;\n\n      if (reqPage != loadedPage) {\n        requestedLine = i;\n        htmlLoadContent(reqPage);\n        loadedPage = reqPage;\n      }\n      else {\n        MakeLineVisible(i);\n      }\n    }\n\n    function showFunctionInfo(i) {\n      var el;\n      el = document.getElementById('function_info' + functionSelected);\n      if (el != null) el.style.display = 'none';\n      document.getElementById('function_info' + i).style.display = '';\n      functionSelected = i;\n    }\n\n    function changeContentVisibility(e) {\n      if (e.parentNode.childNodes[1].style.display == 'none') {\n        e.parentNode.childNodes[1].style.display = '';\n        e.firstChild.nodeValue = '-' + e.firstChild.nodeValue.substr(1, e.firstChild.nodeValue.length);\n      }\n      else {\n        e.parentNode.childNodes[1].style.display = 'none';\n        e.firstChild.nodeValue = '+' + e.firstChild.nodeValue.substr(1, e.firstChild.nodeValue.length);\n      }\n    }\n\n    function hideWindow(windowName) {\n      document.getElementById(windowName).style.display = 'none';\n      resizeAll();\n    }\n  </script>\n</head>\n\n<body onload=\"onLoad();\">\n\n  <div style=\"border-bottom: 1px solid #808080;\">\n    <div style=\"border-bottom: 1px solid #000000; color: #5060C0; background-color: #F0F0F0; font-size: 16px; padding: 2px; font-weight: bold;\">\n      Filename: %filename%\n    </div>\n   </div>\n\n   <noscript><span style=\"padding: 10px; font-size: 18px; border: 2px solid Red;\">Javascript is <b>not</b> enabled!</noscript>\n\n  <div id=\"leftWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Analysis</div>\n    <div id=\"leftWindowContent\" class=\"mywindow_content\">\n      \n      <div id=\"analysis_line0\">\n        No line selected\n      </div>\n\n      <div id=\"dynamicanalysis\">\n\n      </div>\n    </div>\n  </div>\n\n  <div id=\"codeWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Source code</div>\n    <div id=\"codeWindowContent\" class=\"mywindow_content\">\n\n";;
let htmlTemp_BasePartTwo = "      <div id=\"lastline\" class=\"linetype0\" style=\"border-top: 1px solid #C0C0C0;\"><pre></pre></div>\n    </div>\n  </div>\n\n  <div id=\"rightWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Empty <a style=\"text-decoration: none; color: #000080;\" href=\"javascript:hideWindow('rightWindow');\">[X]</a></div>\n    <div id=\"rightWindowContent\" class=\"mywindow_content\">\n      Currently no content\n    </div>\n  </div>\n\n  <div id=\"bottomWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Function info<a style=\"text-decoration: none; color: #000080;\" href=\"javascript:hideWindow('bottomWindow');\">[X]</a></div>\n    <div id=\"bottomWindowContent\" class=\"mywindow_content\">\n\n";;
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

let createCodeLines outchan filename xmlNode lineCount = 
	let lines = readFileLines filename in
	let currentLine = ref 1 in

	let analyzedLineList = Xml.fold (fun l xmlChild -> l @ [int_of_string (Xml.attrib xmlChild "line")]) [] xmlNode in
	let isLineAnalyzed lineNo = List.exists (fun elem -> (elem = lineNo)) analyzedLineList in

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
		let linkStart = if (isAnalyzed = true) then "<a href=\"javascript:showLine("^(string_of_int !currentLine)^");\">" else "" in
		let linkEnd = if (isAnalyzed = true) then "</a>" else "" in
		fprintf outchan "      <div id=\"line%i\" class=\"linetype%i\"><pre> %s%i:%s %s</pre></div>\n" !currentLine (!currentLine mod 2) linkStart !currentLine linkEnd (prepareLine line);
		currentLine := !currentLine + 1;
		()
	in
	lineCount := List.length lines;
	List.iter (createLine) lines;;

(* === print_fmt : html output === *)
let print_html chan xmlNode (file: Cil.file) = 
	printf "[HTML-Output] Create html files ...\n";

	(* fileList contains all analyzed filenames, the output file handle, .. *)
	let fileList = ref [] in

	(* Creates an entry in fileList *)
	let createFileListEntry lineFile = 
		let shortFilename = Filename.basename lineFile in
		let lineCount = ref 0 in

		(* Create output file *)
		let outputChannel = open_out (shortFilename^".html") in

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

		(* Write third part *)
		fprintf outputChannel "%s" htmlTemp_BasePartThree;
		close_out outputChannel;

		(* Create analysis data directory *)
		try (Unix.mkdir (shortFilename^"_data") 0o777) with _ -> (); ;

		(* Create analysis files *)
		let outlist = ref [] in
		for i = 0 to (!lineCount / 1000) do outlist := !outlist @ [open_out (shortFilename^"_data/analysis"^(string_of_int i)^".html")] done;

		(* Return new fileList entry *)		
		{filename = lineFile ; analysis_out = outlist}
	in

	(* Finds an entry in fileList *)
	let getFileListEntry lineFile = 
		try
		List.find (fun entry -> (String.compare entry.filename lineFile) == 0) !fileList
		with Not_found -> begin
			let newEntry = createFileListEntry lineFile in
			fileList := !fileList @ [newEntry];
			newEntry;
		end
	in

	(* Process xml node of an analysis line *)
	let rec processAnalysisXmlNode outchan xmlParentNode =
		if (List.length (Xml.children xmlParentNode) = 0) then fprintf outchan "%s<br>\n" (Xml.attrib xmlParentNode "text")
		else begin
			(* Tree node directory start *)
			fprintf outchan "<div style=\"margin-left: 0px;\" class=\"entrydir\"><span style=\"font-weight: bold; cursor: pointer;\" onmousedown=\"changeContentVisibility(this);\">-%s</span><div style=\"margin-left: 15px;\" class=\"entrydircontent\">\n" (Xml.attrib xmlParentNode "text");

			(* Process childs *)
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
		(*fprintf outchan "       Line %i\n" lineNo;*)
		processAnalysisXmlNode outchan (List.hd (Xml.children xmlNodeLine));		

		(* Print end line div container *)
		fprintf outchan "     </div>\n";
	in

	(* Create function infos *)
	createFunctionInfoList file;

	(* Walk through the analysis lines in the xml file *)
	Xml.iter (fun x -> processAnalysisLineEntry x) xmlNode;
	;;


