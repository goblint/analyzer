let js_string = 
"
var loadedPage = -1;
var requestedLine = 0;
var lineSelected = 0;
var functionSelected = 0;
var warningSelected = 0;
var dynamicLoading = false;


function init_toggle(e) {
  e.firstChild.nodeValue = \"▿ \"+e.firstChild.nodeValue;
  e.onmousedown = function ch(t) { 
      e.firstChild.nodeValue = '◿' + e.firstChild.nodeValue.substr(1, e.firstChild.nodeValue.length) ;
  };
  e.onmouseup = function ch(t) {toggleVisibility(e, e.title);};
}

function init_toggle_old(e) {
  e.firstChild.nodeValue = \"▿ \"+e.firstChild.nodeValue;
  e.onmousedown = function ch(t) { 
      e.firstChild.nodeValue = '◿' + e.firstChild.nodeValue.substr(1, e.firstChild.nodeValue.length) ;
  };
  e.onmouseup = function ch(t) {changeContentVisibility(e);};
}

function init_all() {
  var els = document.getElementsByClassName('toggle');
  for (var i=0; i < els.length; i++) {
    init_toggle_old(els[i]);
  }

  var els = document.getElementsByClassName('toggle_ttl');
  for (var i=0; i < els.length; i++) {
    init_toggle(els[i]);
  }
  
  els = document.getElementsByClassName('toggle_ttl off');
  for (var i=0; i < els.length; i++) {
    toggleVisibility(els[i],els[i].title);
  }
}

// Try to create and initialize a HttpObject
var xmlHttpObject = false;

if (typeof XMLHttpRequest != 'undefined') 
{
    xmlHttpObject = new XMLHttpRequest();
}
if (!xmlHttpObject) 
{
    try 
    {
        xmlHttpObject = new ActiveXObject(\"Msxml2.XMLHTTP\");
    }
    catch(e) 
    {
        try 
        {
            xmlHttpObject = new ActiveXObject(\"Microsoft.XMLHTTP\");
        }
        catch(e) 
        {
            xmlHttpObject = null;
        }
    }
}

function htmlHandleContent()
{
    if (xmlHttpObject.readyState == 4)
    {
        document.getElementById('dynamicanalysis').innerHTML = xmlHttpObject.responseText;
        var els =document.getElementById('dynamicanalysis').getElementsByClassName('toggle');
        for (var i=0; i < els.length; i++) {
          init_toggle_old(els[i]);
        }
        MakeLineVisible(requestedLine);
     }     
}

function htmlLoadContent(sfile,i)
{
    xmlHttpObject.open('get',sfile+'_data/analysis'+i+'.html');
    xmlHttpObject.onreadystatechange = htmlHandleContent;
    xmlHttpObject.send(null);
    return false;
}

function resizeAll() {
  var leftWindow = document.getElementById('leftWindow');
  var leftWindowContent = document.getElementById('leftWindowContent');
  var rightWindow = document.getElementById('rightWindow');
  var rightWindowContent = document.getElementById('rightWindowContent');
  var bottomWindow = document.getElementById('bottomWindow');
  var bottomWindowContent = document.getElementById('bottomWindowContent');
  var codeWindow = document.getElementById('codeWindow');
  var codeWindowContent = document.getElementById('codeWindowContent');

  rightWindow.style.display = 'none';

  var codeWindowTop = 35;
  var leftWindowWidth = 300;
  var rightWindowWidth = 250;
  if (rightWindow.style.display == 'none') rightWindowWidth = 0;
  var bottomWindowHeight = 130;
  if (bottomWindow.style.display == 'none') bottomWindowHeight = 0;
  var bottomWindowTop = (window.innerHeight - bottomWindowHeight - 35);
  var codeWindowHeight = bottomWindowTop - codeWindowTop - 35;
  var codeWindowWidth = (window.innerWidth - (leftWindowWidth + rightWindowWidth + 2 + 2 + 10 + 10 + 10 + 10));

  leftWindow.style.top = codeWindowTop + 'px';
  leftWindow.style.left = '10px';
  leftWindowContent.style.width = leftWindowWidth + 'px';
  leftWindowContent.style.height = codeWindowHeight + 'px';

  rightWindow.style.top = codeWindowTop + 'px';
  rightWindow.style.left = (window.innerWidth - rightWindowWidth - 2 - 10) + 'px';
  rightWindowContent.style.width = rightWindowWidth + 'px';
  rightWindowContent.style.height = codeWindowHeight + 'px';

  bottomWindow.style.top = bottomWindowTop + 'px';
  bottomWindow.style.left = '10px';
  bottomWindowContent.style.width = (window.innerWidth - 20) + 'px';
  bottomWindowContent.style.height = bottomWindowHeight + 'px';

  codeWindow.style.top = codeWindowTop + 'px';
  codeWindow.style.left = (leftWindowWidth + 2 + 10 + 10) + 'px';
  codeWindowContent.style.width = codeWindowWidth + 'px';
  codeWindowContent.style.height = codeWindowHeight + 'px';
}

function onLoad() {
  resizeAll();
  window.onresize = resizeAll;

  init_all();
}

function MakeLineVisible(i) {
  var el;
  el = document.getElementById('analysis_line' + lineSelected);
  if (el != null) el.style.display = 'none';
      
  var analysis_line = document.getElementById('analysis_line' + i);
  if(analysis_line) analysis_line.style.display = '';
  if (lineSelected != 0) {
    if ((lineSelected % 2) == 0) document.getElementById('line'+lineSelected).style.background = '#F0F0F0';
    if ((lineSelected % 2) == 1) document.getElementById('line'+lineSelected).style.background = '#F8F8F8';
    lineSelected = 0;
  }
  if (i != 0) {
    lineSelected = i;
    document.getElementById('line'+lineSelected).style.background = '#FFFFD8';        
  }

  showWarning(i);
}

function hideWarning() {
  var el3;
  el3 = document.getElementById('warning_info' + warningSelected);
  if (el3 != null) {
    warningSelected = 0;
    el3.style.display = 'none';
    document.getElementById('title_warning_info').style.display = 'none';
  }
}

function showWarning(i) {
  hideWarning();
  hideFunctionInfo();

  var el;
  el = document.getElementById('warning_info' + i);
  if (el != null) {
    el.style.display = '';
    document.getElementById('title_warning_info').style.display = '';
  }
  warningSelected = i;
}

function showLine(sfile,i) {
  var remainder = i % 1000;
  var reqPage = ( i - remainder ) / 1000;

  if (reqPage != loadedPage) {
    requestedLine = i;
    htmlLoadContent(sfile,reqPage);
    loadedPage = reqPage;
  }
  else {
    MakeLineVisible(i);
  }
}

function hideFunctionInfo()  {
  var el3;
  el3 = document.getElementById('function_info' + functionSelected);
  if (el3 != null) {
    functionSelected = 0;
    el3.style.display = 'none';
    document.getElementById('title_function_info').style.display = 'none';
  }
}

function showFunctionInfo(i) {
  hideWarning();
  hideFunctionInfo();

  var el2;
  el2 = document.getElementById('function_info' + i);
  if (el2 != null) {
    el2.style.display = '';
    document.getElementById('title_function_info').style.display = '';
  }
  functionSelected = i;
}

function changeContentVisibility(e) {
  if (e.parentNode.children[1].style.display == 'none') {
    e.parentNode.children[1].style.display = '';
    e.firstChild.nodeValue = '▿' + e.firstChild.nodeValue.substr(1, e.firstChild.nodeValue.length) ;
  }
  else {
    e.parentNode.children[1].style.display = 'none';
    e.firstChild.nodeValue = '▹' + e.firstChild.nodeValue.substr(1, e.firstChild.nodeValue.length);
  }
}
function toggleVisibility(e,title) {
  var el = document.getElementById(title);
  if (el.style.display == 'none') {
    el.style.display = '';
    e.firstChild.nodeValue = '▿' + e.firstChild.nodeValue.substr(1, e.firstChild.nodeValue.length) ;
  }
  else {
    el.style.display = 'none';
    e.firstChild.nodeValue = '▹' + e.firstChild.nodeValue.substr(1, e.firstChild.nodeValue.length);
  }
}

function hideWindow(windowName) {
  document.getElementById(windowName).style.display = 'none';
  resizeAll();
}

function showLeftTab(no) {
  if (no == 0) {
    document.getElementById('analysisbox').style.display = '';
    document.getElementById('globalsbox').style.display = 'none';
    document.getElementById('warningsbox').style.display = 'none';
  }
  if (no == 1) {
    document.getElementById('analysisbox').style.display = 'none';
    document.getElementById('globalsbox').style.display = '';
    document.getElementById('warningsbox').style.display = 'none';
  }
  if (no == 2) {
    document.getElementById('analysisbox').style.display = 'none';
    document.getElementById('globalsbox').style.display = 'none';
    document.getElementById('warningsbox').style.display = '';
  }
}

function ScrollToLine(line) {
  document.getElementById('codeWindowContent').scrollTop = (line-1)*document.getElementById('line1').clientHeight;

}

"
