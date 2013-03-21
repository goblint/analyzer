
let htmlTemp_BasePartOne = 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
<head>
  <title>%filename%</title>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
  <link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"></link>
  <script type=\"text/javascript\" src=\"script.js\"></script>
</head>
<body onload=\"onLoad();\">
  <div style=\"border-bottom: 1px solid #808080;\">
    <div style=\"border-bottom: 1px solid #000000; color: #5060C0; background-color: #F0F0F0; font-size: 16px; padding: 2px; font-weight: bold;\">
      Filename: %filename%
    </div>
   </div>
   <noscript><span style=\"padding: 10px; font-size: 18px; border: 2px solid Red;\">Javascript is <b>not</b> enabled!</noscript>
  <div id=\"leftWindow\" class=\"mywindow\">
    <div class=\"mywindow_header\">
      <a href=\"javascript:showLeftTab(0);\"><div class=\"tabheader\">Analysis</div></a>
      <a href=\"javascript:showLeftTab(1);\"><div class=\"tabheader\">Decls</div></a>
      <a href=\"javascript:showLeftTab(2);\"><div class=\"tabheader\">Globals</div></a>
      <div style=\"clear: both;\"></div>
    </div>
    <div id=\"leftWindowContent\" >
      <div id=\"analysisbox\" style=\"width: 100%; height: 100%;\">      
        <div id=\"analysis_noselection\">
          No line selected
        </div>
        <iframe id=\"analysisiframe\" style=\"display: none;\" class=\"iframebox\">
        </iframe>
      </div>
      <div id=\"declsbox\" style=\"display: none;\">
";;

let htmlTemp_BasePartOneSecond = 
"    </div>
    <div id=\"globalsbox\" style=\"display: none; width: 100%; height: 100%; overflow: scroll;\">
";;

let htmlTemp_BasePartOneThird = "</div>
    </div>
  </div>
  <div id=\"codeWindow\" class=\"mywindow\">
    <div class=\"mywindow_header\">
      <a href=\"javascript:showMainTab(0);\"><div class=\"tabheader\">Source Code</div></a>
      <a href=\"javascript:showMainTab(1);\"><div class=\"tabheader\">Warnings</div></a>
      <a href=\"javascript:showMainTab(2);\"><div class=\"tabheader\">Deadcode</div></a>
      <div style=\"clear: both;\"></div>
    </div>
    <div id=\"codeWindowContent\" class=\"mywindow_content\">
      <div id=\"codelineBox\">
";;

let htmlTemp_BasePartOneForth = 
"        <div id=\"lastline\" class=\"linetype0\" style=\"border-top: 1px solid #C0C0C0;\"><pre></pre></div>
      </div>
    </div>
    <div id=\"warningListBox\" style=\"display: none;\">
";;

let htmlTemp_BasePartOneFifth = 
"    </div>
    <div id=\"deadcodeListBox\" style=\"display: none;\">
";;

let htmlTemp_BasePartTwo =
"    </div>
  </div>
  <div id=\"rightWindow\" class=\"mywindow\">
    <div class=\"mywindow_header\">Empty <a style=\"text-decoration: none; color: #000080;\" href=\"javascript:hideWindow('rightWindow');\">[X]</a></div>
    <div id=\"rightWindowContent\" class=\"mywindow_content\">
      Currently no content
    </div>
  </div>
  <div id=\"bottomWindow\" class=\"mywindow\">
    <div class=\"mywindow_header\"><span id=\"title_function_info\" style=\"display: none;\">Function Info</span><span id=\"title_warning_info\" style=\"display: none;\">Warning Info</span><a style=\"text-decoration: none; color: #000080;\" href=\"javascript:hideWindow('bottomWindow');\">[X]</a></div>
    <div id=\"bottomWindowContent\" class=\"mywindow_content\">
";;

let htmlTemp_BasePartThree = 
"    </div>
  </div>
</body>
</html>
";;

let htmlTemp_AnalysisFilePartOne =
"<html>
  <head>
    <link rel=\"stylesheet\" href=\"../style.css\" type=\"text/css\"></link>
    <script type=\"text/javascript\" src=\"../script.js\"></script>
  </head>
  <body id=\"analysisfilebody\" onload=\"OnAnalysisFileLoaded();\">
";;

let htmlTemp_AnalysisFilePartTwo =
"  </body>
</html>
";;

