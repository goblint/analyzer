(*  todo: ids to markers *)
open Pretty
open Printf;;
open Xml;;
open Cil;;
open Unix;;
open Htmlutil

(** Format string for markers.  *)
let marker_s n id l : ('a, 'c, 'd) format = 
  match l with
    | "v" -> "<span class=\"marker marker_%s var\" onclick=\"marker_click('%s')\"></span>" 
    | "m" -> "<span class=\"marker marker_%s message\" onclick=\"marker_click('%s')\"></span>"
    | "e" -> "<span class=\"marker marker_%s error\" onclick=\"marker_click('%s')\">!</span>"
    | _   -> "<span class=\"marker marker_%s\" onclick=\"marker_click('%s')\">@</span>"

(** output of markers *)
let marker n id l ch = fprintf ch (marker_s n id l) id id
let marker_str n id l = sprintf (marker_s n id l) id id

(** Hash table from locations. *)
module MS = Hashtbl.Make (Basetype.ProgLines)  

(** Write html marker code inside of [Pretty] printed document for any location. *)
let pretty_markers msm msv loc =
  let rec pretty_markers () = function
    | [] -> nil
    | (n,f,id,l)::mks -> 
      text (marker_str n id l) ++ pretty_markers () mks
  in
  let mrk_m = 
    try pretty_markers () (MS.find msm loc) 
    with Not_found -> nil 
  in
  let mrk_v = 
    try pretty_markers () (MS.find msv loc) 
    with Not_found -> nil 
  in
  if mrk_m==nil && mrk_v==nil 
  then nil 
  else markup (text "%%%$$" ++ mrk_m ++ text "$%$" ++ mrk_v ++ text "$$%%%")
      
(** Cil printer class that also prints message/error/variable markers. *)
class markerCilPrinterClass (msm : (int*string*string*string) list MS.t) 
                            (msv : (int*string*string*string) list MS.t) : cilPrinter =
object (self)
  inherit defaultCilPrinterClass as super    
  method pStmtKind stmt () knd  = 
    match knd with 
      | Instr _ |	Return _ |	If _ |	Loop _ ->  
          pretty_markers msm msv (get_stmtLoc knd) ++ super#pStmtKind stmt () knd
      | _ -> super#pStmtKind stmt () knd
  method pVDecl () f = pretty_markers msm msv f.vdecl ++ super#pVDecl () f
  method pLineDirective ?forcefile:bool  _ = Pretty.text ""
end

(** Marker tables, that are filled during post-processing of local results & messages. *)
let marker_table_m = MS.create 100 
let marker_table_v = MS.create 100 

(** A cil printer that uses the class & tables from above. *)
let markerCilPrinter = new markerCilPrinterClass marker_table_m marker_table_v

(** A printf style printer that does some dirty substitutions for html output.  *)
let printVerbatim str ch =
  let f = function
    | '&' -> fprintf ch "&amp;"
    | '<' -> fprintf ch "&lt;"
    | '>' -> fprintf ch "&gt;"
    | ' ' -> fprintf ch "&nbsp;"
    | '\t' -> fprintf ch "&nbsp;&nbsp;&nbsp;&nbsp;"
    | c -> fprintf ch "%c" c
  in
  String.iter f str

(** Takes a file name, and sorted marker lists to output a table for the code-view.  *)
let printCodeWithMarkers fn m_mks v_mks ch =
  let m_mks, v_mks = ref m_mks, ref v_mks in
  let rec print_mks fn li mks ch = 
    match !mks with
      | (n,f,id,l) :: rst when fn<>f -> 
          mks := rst; 
          print_mks fn li mks ch
      | (n,f,id,l) :: rst when fn=f && li>=n ->
          mks := rst;
          (marker n id l <:> print_mks fn li mks) ch
      | rst -> ()
  in
  let fch = open_in fn in
  let rec handleLines li = 
    try 
      let ln = input_line fch in
      tag "tr" 
      begin
        tag "td" (print_mks fn li m_mks) <:>
        tag "td" (print_mks fn li v_mks) <:>
        tag "td" ~tp:["class","line"] (num li)             <:>
        tag "td" (printVerbatim ln)
      end 
      <:>
      handleLines (li+1)
    with End_of_file -> fun _ -> ()
  in    
    tag "div" ~tp:["id",fn]
    begin
      tag "table" 
      begin
        tag "tr" 
        begin
          tag "th" (str "msg") <:> tag "th" (str "var") <:> 
          tag "th" ~tp:["class","line"] (str "#") <:> tag "th" ~tp:["class","th_code"] (str "code")
        end <:>
        handleLines 1 
      end
    end ch;
    close_in_noerr fch
  
(*
(** this prints markers inline, but we do not have that precise information *)
  let printCodeWithMarkers ch fn mks =
  let fch = open_in fn in
  let printVerbatim ch =
    let f = function
      | '&' -> fprintf ch "&amp;"
      | '<' -> fprintf ch "&lt;"
      | '>' -> fprintf ch "&gt;"
      | ' ' -> fprintf ch "&nbsp;"
      | '\t' -> fprintf ch "&nbsp;&nbsp;"
      | c -> fprintf ch "%c" c
    in
    String.iter f 
  in
  let printEndline ch () = 
    fprintf ch "<br />\n" 
  in
  let printMarker ch l c m n = 
    match n with
      | "v" -> fprintf ch "<span class=\"marker var\" onclick='marker_click(\"%s\")'>[v]</span>" m 
      | "m" -> fprintf ch "<span class=\"marker message\" onclick='marker_click(\"%s\")'>[m]</span>" m 
      | "e" -> fprintf ch "<span class=\"marker error\" onclick='marker_click(\"%s\")'>[!]</span>" m 
      | _ -> fprintf ch "<span class=\"marker\" onclick='marker_click(\"%s\")'>[@]</span>" m 
  in
  let rec printLine mks ln li col = 
    match mks with
      | (l,c,m,n) :: mks when l = li -> 
          let s1 = try String.sub ln 0 c with Invalid_argument("String.sub") -> "" in
          let s2 = try String.sub ln c (String.length ln-c) with Invalid_argument("String.sub") -> ln in
          fprintf ch "%a" printVerbatim s1;
          printMarker ch l c m n;
          printLine mks s2 li c            
      | _ -> fprintf ch "%a%a" printVerbatim ln printEndline (); mks
  in
  let rec handleLine li mks  = 
    let ln = input_line fch in
    let mks = printLine mks ln li 1 in
    handleLine (li+1) mks
  in    
  try handleLine 1 mks
  with End_of_file -> close_in_noerr fch
*)

(** Some more functions that do char substitutions  *)
let escape_pcd_char ch = function
  | '&' -> fprintf ch "&amp;"
  | '<' -> fprintf ch "&lt;"
  | '>' -> fprintf ch "&gt;"
  | ' ' -> fprintf ch " "
	| '\''-> fprintf ch "&apos;"
	| '"' -> fprintf ch "&quot;"
  | x   -> fprintf ch "%c" x
let escape_attr_char ch = function
  | '"'  -> fprintf ch "\\\""
  | '\\' -> fprintf ch "\\\\"
  | x    -> fprintf ch "%c" x

(** Something more feature-complete is in Batteries, but I do not want to rewrite all of this for batteries. *)
let rec print_list b f ch = function
  | [] -> ()
  | x::xs -> 
      let s = if b then " " else "" in
      fprintf ch "%s%a%a" s f x (print_list true f) xs
    
(** Print one attribute.  *)
let print_attr ch (an,av) =  
  fprintf ch "%s=\"" an;
  String.iter (escape_attr_char ch) av;
  fprintf ch "\""

(** Just print the damn xml! Who needs it in string form?  *)
let rec xml_printer ch = function
  | Element (tg,ats,[]) ->
    fprintf ch "<%s%a></%s>" tg (print_list true print_attr) ats tg
  | Element (tg,ats,cs) ->
    fprintf ch "<%s%a>%a</%s>" tg (print_list true print_attr) ats (print_list false xml_printer) cs tg
  | PCData x -> String.iter (escape_pcd_char ch) x


let conv_loc_attr attr =
  let file = List.assoc "file" attr in
  let line = List.assoc "line" attr in
  let node = List.assoc "node" attr in
  ["class","loc"
  ;"id","mark_"^file^"_"^line^"_"^(string_of_int (Hashtbl.hash node))]
  
(** Convert from the fabolus vesal-xml-format to html. *)
let rec conv_one = function 
  | Element ("Loc", attr, cs) -> 
      let node = Element ("div",["class","node_descr"],[PCData (List.assoc "node" attr)]) in
      let rest = Element ("div",["class","node_data"],List.map conv_one cs) in
      Element ("div",conv_loc_attr attr, [node;rest])
  | Element ("Node", attr, []) -> 
      PCData (List.assoc "text" attr)
  | Element ("Node", attr, cs) -> 
      let status = if List.assoc "text" attr = "Context" then " toggle_off" else "" in
      let n_text = PCData (List.assoc "text" attr) in
      let n_toggle = Element ("span",["class","toggle node"^status], [n_text]) in
      let n_cont = Element ("div",["class","node content"], List.map conv_one cs) in
      Element ("div",[],[n_toggle;n_cont])
  | Element ("Leaf", attr, []) -> 
      Element ("div", [], [PCData (List.assoc "text" attr)])
  | Element (x, a, cs) -> Element (x, a, List.map conv_one cs)
  | x -> x
  
(** Who cares!? *)
let xmlToHtml x =
  match x with
    | Element ("Analysis",_,cs) -> List.map conv_one cs
    | _ -> failwith "Cannot convert analysis result! Is it corrupted?"

(**  print the local result in html format *)  
let printLocalResults xml ch =
  List.iter (fun x -> fprintf ch "%a\n" xml_printer x) (xmlToHtml xml)

(**  print the global result in html format *)  
let printGlobalResults gxmls ch =
  List.iter (fun x -> fprintf ch "%a\n" xml_printer (conv_one x)) gxmls

(** Crawl through xml and record Loc nodes, generate markers.  *)
let get_markers xml = 
  let locs = function  
    | Element ("Loc", attr, _) -> 
        let line = List.assoc "line" attr in
        let file = List.assoc "file" attr in
        let node = List.assoc "node" attr in
        let mrkr = (int_of_string line, file, "mark_"^file^"_"^line^"_"^(string_of_int (Hashtbl.hash node)),"v") in
        let loc = {line=int_of_string line; file=file; byte=0} in
        let old = try MS.find marker_table_v loc with Not_found -> [] in
        MS.add marker_table_v loc (mrkr::old);
        mrkr
    | _ -> failwith "cannot find markers in the result"
  in
  match xml with
    | Element ("Analysis",_,cs) -> List.map locs cs
    | _ -> []
    
(** compare function for markers, needed for sorting  *)
let marker_leq (x1,_,_,_) (y1,_,_,_) = compare x1 y1 

(** Generate messages, that [Messages] has hopefully been storing for us, and stores them in [marker_table_m]. *)
let gen_messages () = 
  let f loc v (m,rst) = 
    let typ = if List.exists (fun (x,_) -> x="e") v then "e" else "m" in
    let id = "msg_"^string_of_int loc.line^"_"^loc.file in
    let g (_,msg) rst =
      tag "div" (str msg) <:> rst
    in
    let mrkr = (loc.line,loc.file,id,typ) in
    let old = try MS.find marker_table_m loc with Not_found -> [] in
    let lable = tag "div" ~tp:["class","node_descr"] (str "messeges at "<:>str loc.file<:>str":"<:>num loc.line) in
    let data = tag "div" ~tp:["class","node_data"] (List.fold_right g v (fun _ -> ())) in
    MS.add marker_table_m loc (mrkr::old);
    mrkr::m, tag "div" ~tp:["id",id] (lable <:> data) <:> rst
  in
  Hashtbl.fold f Messages.xml_warn ([],fun _ -> ())

(** Another function that crawls through xml. This one stores file names that we associate with some solver var.  *)
let get_file_list xml =
  let find_files fs = function 
    | Element ("Loc",ats,_) -> 
        let nf = List.assoc "file" ats in
        if List.mem nf fs then fs else nf::fs 
    | _ -> fs
  in
  match xml with 
    | Element ("Analysis",_,cs) -> List.fold_left find_files [] cs
    | _ -> []

(** This dirty little function generates the combined file table with markers. *)
let postprocessed file ch =
  let fname = "postprocessed.temp" in
  let ch_o = open_out fname in
  Cil.dumpFile markerCilPrinter ch_o "t.c" file;
  close_out ch_o;
  let rx = Str.regexp "\\(.*\\)%%%\\$\\$\\(.*\\)\\$%\\$\\(.*\\)\\$\\$%%%\\(.*\\)" in 
  let fch = open_in fname in
  let match_rx ln =
    let group n = try Str.matched_group n ln with Not_found | Invalid_argument("Str.matched_group") -> "" in
    if Str.string_match rx ln 0 then
      `Matches (group 1, group 2, group 3, group 4)
    else 
      `Fails
    in
  let rec handleLines li = 
    try begin
      let ln = input_line fch in
      match match_rx ln with 
      | `Matches (pr,m,v,su) ->
          tag "tr" 
          begin
            tag "td" (str m) <:>
            tag "td" (str v) <:>
            tag "td"  ~tp:["class","line"] (num li)             <:>
            tag "td" (printVerbatim pr <:> printVerbatim su)
          end
          <:>
          handleLines (li+1)
      | `Fails ->
          tag "tr" 
          begin
            tag "td" (str "") <:>
            tag "td" (str "") <:>
            tag "td"  ~tp:["class","line"] (num li)             <:>
            tag "td" (printVerbatim ln)
          end
          <:>
          handleLines (li+1)
    end with End_of_file -> fun _ -> ()
  in    
    tag "div" ~tp:["id","postprocessed"]
    begin
      tag "table" 
      begin
        tag "tr" 
        begin
          tag "th" (str "msg") <:> tag "th" (str "var") <:> 
          tag "th" ~tp:["class","line"] (str "#") <:> tag "th" ~tp:["class","th_code"] (str "code")
        end <:>
        handleLines 1 
      end
    end ch;
    close_in_noerr fch;
    Sys.remove fname
    
  
(** Generate the main html file. Right now this contains everything. Not good! *)
let printMainHtmlFile file xml gxmls =
  let ret = fun x -> () in
  let ch = open_out "test.html" in
  let markers, msg_content = gen_messages () in
  let m_markers = List.stable_sort marker_leq markers in
  let v_markers = List.stable_sort marker_leq (get_markers xml) in
  let files = get_file_list xml in
  let sel typ id name = tag "span" ~tp:["class",typ^" "^typ^"_"^id;"onclick",typ^"_click('"^id^"')"] (str name) in
  let file_selector = List.fold_right (fun fn -> (<:>) (sel "file_link" fn fn)) files ret in
  let content = List.fold_right (fun fn -> (<:>) (printCodeWithMarkers fn m_markers v_markers)) files ret in
  let menubar = 
    str "Files:" <:>
    file_selector <:> 
    sel "file_link" "postprocessed" "combined" <:> 
    tag "span" ~tp:["id","glob_button"]
      (sel "marker" "globals" "globals")
  in
  let html = 
    tag "html" 
    begin
      tag "head"
      begin
        tag "meta" ~tp:["http-equiv","Content-type";"content","text/html;charset=UTF-8"] (str "")<:>
        tag "link" ~tp:["rel","stylesheet";"type","text/css";"href","style.css"] (str "")
      end <:>
      tag "body"
      begin
        tag "div" ~tp:["id","menubar"] menubar <:>
        tag "div" ~tp:["id","info"] (str "test") <:>
        tag "div" ~tp:["id","code"] (str "test") <:>
        tag "div" ~tp:["id","result";"style","display:none;"] (printLocalResults xml) <:>
        tag "div" ~tp:["id","globals";"style","display:none;"] (printGlobalResults gxmls) <:>
        tag "div" ~tp:["id","messages";"style","display:none;"] msg_content <:>
        tag "div" ~tp:["id","files";"style","display:none;"] (content <:> postprocessed file) <:>
        tag "script" ~tp:["type","text/javascript";"src","script.js"] (str "")
      end  
    end 
  in 
  html ch;
  close_out_noerr ch

(** This is the main function that should generate the whole result in html format *)
let printFiles (file:file) xml gxmls =
  let css_ch = open_out "style.css" in
  fprintf css_ch "%s" Stylesheet.css_string;
  close_out css_ch;
  let js_ch = open_out "script.js" in
  fprintf js_ch "%s" Script.script_str;
  close_out js_ch;
  printMainHtmlFile file xml gxmls

(* ------------------- Cut here! ------------------- *)  


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


let htmlTemp_BasePartOne = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n<head>\n  <title>%filename%</title>\n  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n\n  
<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"></link>\n
<script type=\"text/javascript\" src=\"script.js\"></script>\n
</head>\n\n<body onload=\"onLoad();\">\n\n  <div style=\"border-bottom: 1px solid #808080;\">\n    <div style=\"border-bottom: 1px solid #000000; color: #5060C0; background-color: #F0F0F0; font-size: 16px; padding: 2px; font-weight: bold;\">\n      Filename: %filename%\n    </div>\n   </div>\n\n   <noscript><span style=\"padding: 10px; font-size: 18px; border: 2px solid Red;\">Javascript is <b>not</b> enabled!</noscript>\n\n  <div id=\"leftWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Analysis</div>\n    <div id=\"leftWindowContent\" class=\"mywindow_content\">\n      \n      <div id=\"analysis_line0\">\n        No line selected\n      </div>\n\n      <div id=\"dynamicanalysis\">\n\n      </div>\n    </div>\n  </div>\n\n  <div id=\"codeWindow\" class=\"mywindow\">\n    <div class=\"mywindow_header\">Source code</div>\n    <div id=\"codeWindowContent\" class=\"mywindow_content\">\n\n";;
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
	let shortFilename = Filename.basename filename in
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
		let linkStart = if (isAnalyzed = true) then "<a href=\"javascript:showLine('"^shortFilename^"',"^(string_of_int !currentLine)^");\">" else "" in
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
			fileList := !fileList @ [newEntry];
			newEntry;
		end
	in

	(* Process xml node of an analysis line *)
	let rec processAnalysisXmlNode outchan xmlParentNode =
		if (List.length (Xml.children xmlParentNode) = 0) then fprintf outchan "%s<br>\n" (Xml.attrib xmlParentNode "text")
		else begin
			(* Tree node directory start *)
			fprintf outchan "<div><span class=\"toggle entrydir\" >%s</span><div class=\"entrydircontent\">\n" (Xml.attrib xmlParentNode "text");

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
		List.iter (processAnalysisXmlNode outchan) (Xml.children xmlNodeLine);		

		(* Print end line div container *)
		fprintf outchan "     </div>\n";
	in

	(* Create function infos *)
	createFunctionInfoList file;

	(* Walk through the analysis lines in the xml file *)
	Xml.iter (fun x -> processAnalysisLineEntry x) xmlNode;
	;;


