var selectColor = "#fcf"
var fileData = ""

function getURLParameter(name) {
  return decodeURIComponent((new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)').exec(location.search)||[,""])[1].replace(/\+/g, '%20'))||null
}

function get_firstchild(n)
{
  x=n.firstChild;
  while (x.nodeType!=1) {
    x=x.nextSibling;
  }
  return x;
}


function toggleVisibility(e) {
  var ech = e.firstChild;
  if (e.children[1].style.display == 'none') {
    e.children[1].style.display = '';
    ech.nodeValue = '+' + ech.nodeValue.substr(1, ech.nodeValue.length) ;
  }
  else {
    e.children[1].style.display = 'none';
    ech.nodeValue = '-' + ech.nodeValue.substr(1, ech.nodeValue.length);
  }
}

function init_toggle(e) {
  var ech = get_firstchild(e);
  e.firstChild.nodeValue = "+ "+e.firstChild.nodeValue;
  ech.onmousedown = function ch(t) { 
      var ech = get_firstchild(e);
      ech.nodeValue = '*' + ech.nodeValue.substr(1, ech.nodeValue.length) ;
  };
  ech.onmouseup = function ch(t) {toggleVisibility(e);};
  ech.style.cursor = "pointer";
}


function init_all() {
  var file = getURLParameter("file");
  if (file!=""){
    var jsonName = "../files/"+file+".json" ;
    $.getJSON(jsonName,
      function f(data) {
        fileData = data;
        var node = $(".node-wrap");
        var node_id = node.attr("href");
        node.attr("href","../frame.html?file="+getURLParameter("file")+"&fun="+fileData.functions[node_id]+"&node="+node_id);
      });
  }
  var els = document.getElementsByClassName('toggle');
  for (var i=0; i < els.length; i++) {
    init_toggle(els[i]);
  }
  var els = document.getElementsByClassName('toggle off');
  for (var i=0; i < els.length; i++) {
    toggleVisibility(els[i]);
  }
}


var old_color = ""
var old_node = ""
function show_info(x){
  $("#data-frame").empty();
  $("#data-frame").append("<iframe class=\"borderless fill\" id=\"data-frame0\" src=\"nodes/"+x+".xml?file="+getURLParameter("file")+"\"></iframe>");
  if (old_node != "")
    old_node.attr("fill",old_color);
  old_node = $("#a_"+x+" a :first-child");
  old_color = old_node.attr("fill");
  $("#a_"+x+" a :first-child").attr("fill",selectColor);
}


function warn_toggle(e,b){
  if (b) {
    e.css("background-color","#d22");
    e.css("border-color","#700");
  } else {
    e.css("background-color","transparent");
    e.css("border-color","transparent");
  }
}

var old_line = 0;
function select_line(n,xs) {
  $("#line"+old_line).css("background-color","transparent");
  $("#line"+n).css("background-color",selectColor);
  old_line = n;
  $("#data-frame").empty();
  for (var i=0; i<xs.length; i++){
    $("#data-frame").append("<iframe class=\"borderless fill\" onload='javascript:resizeIframe(this)' id=\"data-frame"+i+"\" src=\"nodes/"+xs[i]+".xml?file="+getURLParameter("file")+"\"></iframe>");
  };
  $(".inline-warning").remove();
  var ws = fileData.warnings[n];
  if (ws != null){
    for (var i = 0; i < ws.length; i++ ) {
      $("#line"+n).append("<iframe class=\"inline-warning\" onload='javascript:resizeIframe(this)' id=\"line"+n+"_warn"+i+"\" src=\"warn/"+ws[i]+".xml\"></iframe>");
    } 
  }
}

function resizeIframe(obj){
  setTimeout(function () {
    obj.style.height = 0;
    obj.style.height = (obj.contentWindow.document.body.scrollHeight+20) + 'px';
  },100);
}

function init_source(){
  var ws = fileData.warnings;
  for (var n in fileData.warnings) {
    warn_toggle($("#line"+n+" .source-line-warn"),true);
  } 
  var ds = fileData.data;
  for (var n in ds) {
    $("#line"+n+" .source-line-nr").css("font-weight","bold");
    $("#line"+n).click((function (q,n){
      return function() {select_line(n,q);}
    }) (ds[n],n));
  } 
  var dc = fileData.dead;
  for (var i=0; i<dc.length; i++) {
    $("#line"+dc[i]+" .source-line-nr").css("color","#900");    
  }
}

function init_frames(){
  $('#file-button').text(getURLParameter("file"));
  $('#file-button').attr("href","frame.html?file="+getURLParameter("file"));
  if (getURLParameter("fun")==null){
    $('#file-view-frame-div').load("files/"+getURLParameter("file")+'.html');
    $('#function-button').css("display","none");
    $.getJSON("files/"+getURLParameter("file")+'.json',
      function f(data) {
        fileData = data;
        init_source();        
      });
  } else {
    $('#file-view-frame-div').load("cfgs/"+getURLParameter("file")+"/"+getURLParameter("fun")+'.svg', function f(){svgPanZoom.init();});
    $('#function-button').text(getURLParameter("fun"));
    $('#function-button').attr("href","frame.html?fun="+getURLParameter("fun")+"&file="+getURLParameter("file"));
  }
  
  if (getURLParameter("node")!= null)
    setTimeout(function () {
      show_info(getURLParameter("node"));
    },100);
    
  
  
  $('#dragbar').mousedown(function(e){
      e.preventDefault();
      $(document).mousemove(function(e){
        $('#data-frame-div').css("width",e.pageX-22);
        $('#file-view-frame-div').css("left",e.pageX+10);
        $('#dragbar').css("left",e.pageX-8);
     })
  });
  $(document).mouseup(function(e){
     $(document).unbind('mousemove');
     });
}


