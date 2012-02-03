
open Printf;;
open Xml;;
open Cil;;
open Unix;;

let myCilFile = ref Cil.dummyFile;;

(* Some lists for syntax highlighting *)
let cppPreprocessorList = ["#endif";"#ifdef";"#undef";"#else";"include";"#define"];;
let cppKeywordList = ["int";"return";"void";"char";"short";"long";"unsigned";"if";"else";"switch";"default";"while";"do";"for";"typedef";"class";"struct";"sizeof";"case"];;

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

(* Reads a textfile *)
let readFileText filename = 
	let lines = readFileLines filename in
        String.concat "\n" lines;;

(* Writes a textfile *)
let saveFileText filename text = 
	let chan = open_out filename in
        output_string chan text;
	close_out chan;;

(* HTML templates /* ToDo : extra file! */ *)
let htmlTemp_Base = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n<head>\n  <title>%filename%</title>\n  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n\n  <style type=\"text/css\">\n    <!--\n    html , body {\n      background-color: #FFFFFF;\n      margin: 0;\n      padding: 0;\n    }\n  \n    .mywindow \n    {\n        border: 1px solid #C0C0C0;\n        position: fixed;\n        overflow: hidden;\n        width: auto;\n        height: auto;\n        background-color: #F0F0F0;\n        overflow: hidden;\n    }\n  \n    .mywindow_header \n    {\n        background-color: #E0E0E0;\n        border-bottom: 1px solid #C0C0C0;\n        color: #4050A0;\n        font-family: Arial;\n        font-weight: bold;\n        font-size: 16px;\n        padding: 2px;\n    }\n  \n    .mywindow_content \n    {\n        overflow: scroll;\n        padding: 0px;\n        width: 0px;\n        height: 0px;\n    }\n  \n    #leftWindowContent\n    {\n      background-color: #F0F0F0;\n      padding: 0px;\n      margin-right: 0px;\n      font-size: 14px;\n      font-family: Arial,Monospace;     \n    }  \n \n    #codeWindowContent\n    {\n      background-color: #F8F8F8;\n    }\n        \n    #codeWindowContent a \n    {\n      text-decoration: none;\n      color: #000000;\n      font-weight: bold;\n    }\n        \n    #codeWindowContent a:hover \n    {\n      text-decoration: underline;\n      color: #0000C0;\n    }\n        \n    #codeWindowContent pre \n    {\n      padding: 0px;\n      margin: 0px;\n    }\n  \n    .linetype0\n    {\n      background-color: #F0F0F0;\n    }\n        \n    .linetype1\n    {\n      background-color: #F8F8F8;\n    }\n  \n    .cpp_datatype\n    {\n      color: blue;\n    }\n        \n    .cpp_keyword\n    {\n      color: blue;            \n    }\n\n    .cpp_preprocessor\n    {\n      color: purple;            \n    }\n        \n    .cpp_stringDQ\n    {\n      color: red;            \n    }\n        \n    .cpp_stringSQ\n    {\n      color: red;            \n    }\n        \n    .cpp_comment\n    {\n      color: Green;\n    }\n\n    .cpp_function\n    {\n    }\n\n    .cpp_function a\n    {\n      color: Green;\n      text-decoration: none;\n      font-weight: normal;\n    }\n    -->\n  </style>\n\n  <script type=\"text/javascript\">\n\n    var loadedPage = -1;\n    var requestedLine = 0;\n    var lineSelected = 0;\n    var functionSelected = 0;\n    var dynamicLoading = false;\n\n    // Try to create and initialize a HttpObject\n    var xmlHttpObject = false;\n\n    if (typeof XMLHttpRequest != 'undefined') \n    {\n        xmlHttpObject = new XMLHttpRequest();\n    }\n    if (!xmlHttpObject) \n    {\n        try \n        {\n            xmlHttpObject = new ActiveXObject(\"Msxml2.XMLHTTP\");\n        }\n        catch(e) \n        {\n            try \n            {\n                xmlHttpObject = new ActiveXObject(\"Microsoft.XMLHTTP\");\n            }\n            catch(e) \n            {\n                xmlHttpObject = null;\n            }\n        }\n    }\n\n    function htmlHandleContent()\n    {\n        if (xmlHttpObject.readyState == 4)\n        {\n            document.getElementById('dynamicanalysis').innerHTML = xmlHttpObject.responseText;\n            MakeLineVisible(requestedLine);\n         }\n    }\n\n    function htmlLoadContent(i)\n    {\n        xmlHttpObject.open('get','%shortfilename%_data/analysis'+i+'.html');\n        xmlHttpObject.onreadystatechange = htmlHandleContent;\n        xmlHttpObject.send(null);\n        return false;\n    }\n\n    function resizeAll() {\n      var leftWindow = document.getElementById('leftWindow');\n      var leftWindowContent = document.getElementById('leftWindowContent');\n      var rightWindow = document.getElementById('rightWindow');\n      var rightWindowContent = document.getElementById('rightWindowContent');\n      var bottomWindow = document.getElementById('bottomWindow');\n      var bottomWindowContent = document.getElementById('bottomWindowContent');\n      var codeWindow = document.getElementById('codeWindow');\n      var codeWindowContent = document.getElementById('codeWindowContent');\n\n      rightWindow.style.display = 'none';\n\n      var codeWindowTop = 35;\n      var leftWindowWidth = 300;\n      var rightWindowWidth = 250;\n      if (rightWindow.style.display == 'none') rightWindowWidth = 0;\n      var bottomWindowHeight = 130;\n      if (bottomWindow.style.display == 'none') bottomWindowHeight = 0;\n      var bottomWindowTop = (window.innerHeight - bottomWindowHeight - 35);\n      var codeWindowHeight = bottomWindowTop - codeWindowTop - 35;\n      var codeWindowWidth = (window.innerWidth - (leftWindowWidth + rightWindowWidth + 2 + 2 + 10 + 10 + 10 + 10));\n\n      leftWindow.style.top = codeWindowTop + 'px';\n      leftWindow.style.left = '10px';\n      leftWindowContent.style.width = leftWindowWidth + 'px';\n      leftWindowContent.style.height = codeWindowHeight + 'px';\n\n      rightWindow.style.top = codeWindowTop + 'px';\n      rightWindow.style.left = (window.innerWidth - rightWindowWidth - 2 - 10) + 'px';\n      rightWindowContent.style.width = rightWindowWidth + 'px';\n      rightWindowContent.style.height = codeWindowHeight + 'px';\n\n      bottomWindow.style.top = bottomWindowTop + 'px';\n      bottomWindow.style.left = '10px';\n      bottomWindowContent.style.width = (window.innerWidth - 20) + 'px';\n      bottomWindowContent.style.height = bottomWindowHeight + 'px';\n\n      codeWindow.style.top = codeWindowTop + 'px';\n      codeWindow.style.left = (leftWindowWidth + 2 + 10 + 10) + 'px';\n      codeWindowContent.style.width = codeWindowWidth + 'px';\n      codeWindowContent.style.height = codeWindowHeight + 'px';\n    }\n\n    function onLoad() {\n      resizeAll();\n      window.onresize = resizeAll;\n    }\n\n    function MakeLineVisible(i) {\n      var el;\n      el = document.getElementById('analysis_line' + lineSelected);\n      if (el != null) el.style.display = 'none';\n      \n      document.getElementById('analysis_line' + i).style.display = '';\n      if (lineSelected != 0) {\n        if ((lineSelected % 2) == 0) document.getElementById('line'+lineSelected).style.background = '#F0F0F0';\n        if ((lineSelected % 2) == 1) document.getElementById('line'+lineSelected).style.background = '#F8F8F8';\n        lineSelected = 0;\n      }\n      if (i != 0) {\n        lineSelected = i;\n        document.getElementById('line'+lineSelected).style.background = '#FFFFD8';        \n      }\n\n    }\n\n    function showLine(i) {\n      var remainder = i % 1000;\n      var reqPage = ( i - remainder ) / 1000;\n\n      if (reqPage != loadedPage) {\n        requestedLine = i;\n        htmlLoadContent(reqPage);\n        loadedPage = reqPage;\n      }\n      else {\n        MakeLineVisible(i);\n      }\n    }\n\n    function showFunctionInfo(i) {\n      var el;\n      el = document.getElementById('function_info' + functionSelected);\n      if (el != null) el.style.display = 'none';\n      document.getElementById('function_info' + i).style.display = '';\n      functionSelected = i;\n    }\n\n    function changeContentVisibility(e) {\n      if (e.parentNode.childNodes[2].style.display == 'none') {\n        e.parentNode.childNodes[2].style.display = '';\n        e.firstChild.nodeValue = '-' + e.firstChild.nodeValue.substr(1, e.firstChild.nodeValue.length);\n      }\n      else {\n        e.parentNode.childNodes[2].style.display = 'none';\n        e.firstChild.nodeValue = '+' + e.firstChild.nodeValue.substr(1, e.firstChild.nodeValue.length);\n      }\n    }\n\n    function hideWindow(windowName) {\n      document.getElementById(windowName).style.display = 'none';\n      resizeAll();\n    }\n  </script>\n</head>\n\n<body onload=\"onLoad();\">\n\n  <div style=\"border-bottom: 1px solid #808080;\">\n    <div style=\"border-bottom: 1px solid #000000; color: #5060C0; background-color: #F0F0F0; font-size: 16px; padding: 2px; font-weight: bold;\">\n      Filename: %filename%\n    </div>\n   </div>\n\n   <noscript><span style=\"padding: 10px; font-size: 18px; border: 2px solid Red;\">Javascript is <b>not</b> enabled!</noscript>\n\n  <div id=\"leftWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Analysis</div>\n    <div id=\"leftWindowContent\" class=\"mywindow_content\">\n      \n      <div id=\"analysis_line0\">\n        No line selected\n      </div>\n\n      <div id=\"dynamicanalysis\">\n\n      </div>\n    </div>\n  </div>\n\n  <div id=\"codeWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Source code</div>\n    <div id=\"codeWindowContent\" class=\"mywindow_content\">\n      %codelines%\n      <div id=\"lastline\" class=\"linetype0\" style=\"border-top: 1px solid #C0C0C0;\"><pre></pre></div>\n    </div>\n  </div>\n\n  <div id=\"rightWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Empty <a style=\"text-decoration: none; color: #000080;\" href=\"javascript:hideWindow('rightWindow');\">[X]</a></div>\n    <div id=\"rightWindowContent\" class=\"mywindow_content\">\n      Currently no content\n    </div>\n  </div>\n\n  <div id=\"bottomWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Function info<a style=\"text-decoration: none; color: #000080;\" href=\"javascript:hideWindow('bottomWindow');\">[X]</a></div>\n    <div id=\"bottomWindowContent\" class=\"mywindow_content\">\n      %functioninfos%\n    </div>\n  </div>\n\n</body>\n</html>\n";;
let htmlTemp_Codeline = "      <div id=\"line%lineid%\" class=\"linetype%linetype%\"><pre> %linkstart%%lineid%:%linkend% %code%</pre></div>\n\n";;
let htmlTemp_DivListEntry = "      <div id=\"analysis_line%lineid%\" style=\"display: none;\">\n        %divcontent%\n      </div>\n\n";;
let htmlTemp_ListElement = "        <div style=\"margin-left: 0px;\" class=\"entrydir\"><span style=\"font-weight: bold; cursor: pointer;\" onmousedown=\"changeContentVisibility(this);\">-%listtitle%</span>\n          <div style=\"margin-left: 15px;\" class=\"entrydircontent\">\n            %listcontent%\n          </div>\n        </div>\n\n";;
let htmlTemp_FunListEntry = "      <div id=\"function_info%globalid%\" style=\"display: none;\">\n        Name: %funname%<br/>\n	Line %funline%<br/>\n	File: %funfile%\n      </div>\n\n";;


(* Replaces < and > with special html codes *)
let replaceHtmlTags lineStr =
	let a = Str.global_replace (Str.regexp_string "<") "&lt;" lineStr in
	let b = Str.global_replace (Str.regexp_string ">") "&gt;" a in
	b;;

(* Returns a formatted code line with html tags *)
let rec getShLine lineStr =
	(* Single line comments and strings *)
	let firstCommentChar = try Str.search_forward (Str.regexp_string "//") lineStr 0 with Not_found -> -1 in
	let firstStringChar = try Str.search_forward (Str.regexp_string "\"") lineStr 0 with Not_found -> -1 in
	let secondStringChar = try Str.search_forward (Str.regexp_string "\"") lineStr (firstStringChar+1) with Not_found -> -1 in
	if (firstCommentChar >= 0) && ((firstCommentChar <= firstStringChar) || (firstStringChar = -1)) then (getShLine (String.sub lineStr 0 firstCommentChar))^"<span class=\"cpp_comment\">"^(String.sub lineStr firstCommentChar ((String.length lineStr)-firstCommentChar))^"</span>"
	else if (firstStringChar >= 0) && (secondStringChar >= 0) then (getShLine (String.sub lineStr 0 firstStringChar))^"<span class=\"cpp_stringDQ\">"^(String.sub lineStr firstStringChar ((secondStringChar-firstStringChar)+1))^"</span>"^(getShLine (try (String.sub lineStr (secondStringChar+1) ((String.length lineStr)-secondStringChar-1)) with Invalid_argument s -> ""))
	else 

	(* Preprocessor matchings *)
	let searchForString s = try Str.search_forward (Str.regexp_string s) lineStr 0 with Not_found -> -1 in
	let firstPrepChar = List.fold_left (fun cur ss -> if cur >= 0 then cur else searchForString ss) (0-1) cppPreprocessorList in
	if firstPrepChar >= 0 then "<span class=\"cpp_preprocessor\">"^(replaceHtmlTags lineStr)^"</span>"
	else

	(* Keyword matching *)
	let (firstKeywordChar,keyword_id) = List.fold_left (fun (curpos,curid) ss -> if curpos >= 0 then (curpos,curid) else ((searchForString ss),curid+1) ) ((0-1),(0-1)) cppKeywordList in
	if firstKeywordChar >= 0 then 
		let keywordLen = String.length (List.nth cppKeywordList keyword_id) in
		let followingChar = try (String.get lineStr (firstKeywordChar+keywordLen)) with Invalid_argument s -> '*' in
		if (followingChar = ' ') || (followingChar = ',') || (followingChar = ';') || (followingChar = '.') || (followingChar = ':') || (followingChar = '[') || (followingChar = '{') || (followingChar = '*') then
			(getShLine (String.sub lineStr 0 firstKeywordChar))^"<span class=\"cpp_keyword\">"^(String.sub lineStr firstKeywordChar keywordLen)^"</span>"^(getShLine (try (String.sub lineStr (firstKeywordChar+keywordLen) ((String.length lineStr)-firstKeywordChar-keywordLen) ) with Invalid_argument s -> "" ))
		else
			(getShLine (String.sub lineStr 0 (firstKeywordChar+keywordLen-1)))^(getShLine (try (String.sub lineStr (firstKeywordChar+keywordLen-1) ((String.length lineStr)-firstKeywordChar-keywordLen+1) ) with Invalid_argument s -> "" ))
	
	else

	(* Function matching *)
	let findFunc s global =
		if (String.length s) > 0 then s
		else
		match global with
		  GFun (fundec,loc) ->
			let pos = searchForString fundec.svar.vname in
			let functionNameLen = String.length fundec.svar.vname in
			let followingChar = try (String.get lineStr (pos+functionNameLen)) with Invalid_argument s -> '_' in
			if (pos >= 0) && ((followingChar = '(') || (followingChar = ' ') || (followingChar = ',') || (followingChar = ')')) then				
				(getShLine (String.sub lineStr 0 pos))^"<span class=\"cpp_function\"><a href=\"javascript:showFunctionInfo("^(string_of_int fundec.svar.vid)^");\">"^(String.sub lineStr pos functionNameLen)^"</a></span>"^(getShLine (try (String.sub lineStr (pos+functionNameLen) ((String.length lineStr)-pos-functionNameLen) ) with Invalid_argument s -> "" )
		)	else ""
		| _ -> ""
	in
	let res = List.fold_left findFunc "" (!myCilFile.globals) in
	if (String.length res) > 0 then res
	else
	
	(* No matching highlighting text *)
	replaceHtmlTags lineStr;;


(* Create html code lines from a source file *)
let createCodeLines filename xmlNode lineCount = 
	let lines = readFileLines filename in
	let isLineAnalyzed lineid = Xml.fold (fun b xmlChild -> (b || ((String.compare (Xml.attrib xmlChild "line") (string_of_int lineid)) = 0)) ) false xmlNode in
	let prepareCodeLine lineStr = getShLine lineStr	in
	let createCodeLine lineStr lineid = 
		let getReplaceText lineid = if ((isLineAnalyzed lineid) = true) then "<a href=\"javascript:showLine(%lineid%);\">" else "" in 
		let x = Str.global_replace (Str.regexp_string "%linkstart%") (getReplaceText lineid) htmlTemp_Codeline in
		let y = Str.global_replace (Str.regexp_string "%linkend%") (if ((isLineAnalyzed lineid) = true) then "</a>" else "") x in
 		let a = Str.global_replace (Str.regexp_string "%lineid%") (string_of_int lineid) y in
		let b = Str.global_replace (Str.regexp_string "%linetype%") (string_of_int (lineid mod 2)) a in
		Str.global_replace (Str.regexp_string "%code%") (prepareCodeLine lineStr) b
        in 
	let a,_ = (List.fold_left (fun (s,id) line -> (s^(createCodeLine line id),id+1)) ("",1) lines) in
	lineCount := List.length lines;
	a;;

(* Create analysis data tree view *)
let createAnalysisData shortFilename filename xmlNode = 
        (try (Unix.mkdir (shortFilename^"_data") 0o777) with _ -> ()  );
	let saveAnalysisFile pageid dataValue = 
		saveFileText (shortFilename^"_data/analysis"^(string_of_int pageid)^".html") dataValue
	in	
	let createLineAnalysisContent lineid = 
		let rec addListEntry x =
			if (List.length (Xml.children x) = 0) then (Xml.attrib x "text")^"<br>"
			else 
				let m = Xml.fold (fun s child -> s^(addListEntry child)) "" x in
				if ((String.compare (Xml.attrib x "text") "Analyses") == 0) || ((String.compare (Xml.attrib x "text") "mapping") == 0) then m
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
	let datafolder pagelists xmlChild = 
                let lineidint = int_of_string (Xml.attrib xmlChild "line") in
                let pageid = (lineidint/1000) in
                let analysisString = createLineAnalysis (Xml.attrib xmlChild "line") in

                let foundlist = List.filter (fun (id,s) -> if id = pageid then true else false ) pagelists in
                if (List.length foundlist) = 0 then pagelists@[(pageid,ref analysisString)]
                else ( let (_,strref) = (List.hd foundlist) in (strref := (!strref)^analysisString); pagelists)
	in
	let data = Xml.fold datafolder [] xmlNode in
        List.iter (fun (id,s) -> saveAnalysisFile id !s) data;;

(* Create function infos *)
let createFunctionInfos file =
        let createFunctionLine fundec = 
                let addArgumentStr s vi = 
                        let news = s^vi.vname^" " in
                         match vi.vtype with
                            TVoid attr -> "void "^news
                          | TInt (ikind,attr) -> "int(?) "^news
                          | TFloat (ikind,attr) -> "float(?) "^news
                          | TPtr (typ,attr) -> "Unknown *"^news
                          | TNamed (typinfo,attr) -> typinfo.tname^" "^news
                          | _ -> "Unknown "^news
                in
                fundec.svar.vname^" ( "^(List.fold_left addArgumentStr "" fundec.sformals)^")";
                
        in
	let createFunctionInfoEntry fundec loc = 
		let a = Str.global_replace (Str.regexp_string "%globalid%") (string_of_int fundec.svar.vid) htmlTemp_FunListEntry in
		let b = Str.global_replace (Str.regexp_string "%funname%") (createFunctionLine fundec) a in
		let c = Str.global_replace (Str.regexp_string "%funline%") (string_of_int loc.line) b in
		let d = Str.global_replace (Str.regexp_string "%funfile%") loc.file c in
		d
	in
	let foldGlobals s global =
		match global with
		  GFun (fundec,loc) -> s^(createFunctionInfoEntry fundec loc)
		| _ -> s
	in
	let res = List.fold_left foldGlobals "" (file.globals) in
	res


(* Create html output file for one .c source file *)
let createHtmlFile filename xmlNode =
	let shortFilename = Filename.basename filename in
        printf "[HTML output] Creating html file (%s.html) ... \n" shortFilename;
	let htmlStr = ref "" in
	let lineCount = ref 0 in 

	(* HTML title *)
	htmlStr := Str.global_replace (Str.regexp_string "%filename%") filename htmlTemp_Base;

        (* Short filename *)
	htmlStr := Str.global_replace (Str.regexp_string "%shortfilename%") shortFilename !htmlStr;

	(* Code lines *)
        printf "[HTML output] Adding code lines ...\n";
	htmlStr := Str.global_replace (Str.regexp_string "%codelines%") (createCodeLines filename xmlNode lineCount) !htmlStr;

	(* Line count *)
	htmlStr := Str.global_replace (Str.regexp_string "%linecount%") (string_of_int !lineCount) !htmlStr;

	(* Function infos *)
	htmlStr := Str.global_replace (Str.regexp_string "%functioninfos%") (createFunctionInfos !myCilFile) !htmlStr;

	(* Save html file *)
	saveFileText (shortFilename^".html") !htmlStr;

        (* Create analysis data *)
        printf "[HTML output] Create analysis data ...\n";
	createAnalysisData shortFilename filename xmlNode;;


(* === print_fmt : html output === *)
let print_html chan xmlNode (file: Cil.file) = 
	myCilFile := file;

	(* Create filename list *)
	let filenameList = Xml.fold (fun l xmlChild -> if (List.exists (fun e -> (String.compare e (Xml.attrib xmlChild "file")) = 0) l) then l else l@[Xml.attrib xmlChild "file"]) [] xmlNode in

	(* Crate html output for each filename *)
	List.iter (fun fileName -> createHtmlFile fileName xmlNode) filenameList;;


