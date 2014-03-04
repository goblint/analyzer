var selectColor = "#fcf"

function getURLParameter(name) {
  return decodeURIComponent((new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)').exec(location.search)||[,""])[1].replace(/\+/g, '%20'))||null
}

 function findFirstSvg(callback) {
    var i, candidateSvg, foundSvg;
    var candidateSvg = document.querySelector('svg');
    if (!!candidateSvg) {
      foundSvg = candidateSvg;
      callback(foundSvg);
    }

    var candidateObjectElements = document.querySelectorAll('object');
    i = 0;
    do {
      i += 1;
      getSvg('object:nth-of-type(' + i + ')', function(err, candidateSvg) {
        if (!!candidateSvg) {
          foundSvg = candidateSvg;
          callback(foundSvg);
        }
      });
    } while (i < candidateObjectElements.length);

    var candidateEmbedElements = document.querySelectorAll('embed');
    i = 0;
    do {
      i += 1;
      getSvg('embed:nth-of-type(' + i + ')', function(err, candidateSvg) {
        if (!!candidateSvg) {
          foundSvg = candidateSvg;
          callback(foundSvg);
        }
      });
    } while (i < candidateEmbedElements.length);

    // TODO add a timeout
  }

function getSvg(selector, callback) {
    var target, err, svg;
    if (!selector) {
      if(typeof console !== "undefined") {
        console.warn('No selector specified for getSvg(). Using first svg element found.');
      }
      target = findFirstSvg(function(svg) {
        if (!svg) {
          err = new Error('No SVG found in this document.');
        }
        if (!!callback) {
          callback(err, svg);
        }
        else {
          if (!svg) {
            throw err;
          }
          return svg;
        }
      });
    }
    else {
      target = document.querySelector(selector);
      if (!!target) {
        if (target.tagName.toLowerCase() === 'svg') {
          svg = target;
        }
        else {
          if (target.tagName.toLowerCase() === 'object') {
            svg = target.contentDocument.documentElement;
          }
          else {
            if (target.tagName.toLowerCase() === 'embed') {
              svg = target.getSVGDocument().documentElement;
            }
            else {
              if (target.tagName.toLowerCase() === 'img') {
                throw new Error('Cannot script an SVG in an "img" element. Please use an "object" element or an in-line SVG.');
              }
              else {
                throw new Error('Cannot get SVG.');
              }
            }
          }
        }
      }
      if (!svg) {
        err = new Error('No SVG found in this document.');
      }

      if (!!callback) {
        callback(err, svg);
      }
      else {
        if (!svg) {
          throw err;
        }
        return svg;
      }
    }
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
    var cHeight = e.children[1].offsetHeight;
    document.body.style.height = document.body.offsetHeight + cHeight;
  } else {
    var cHeight = e.children[1].offsetHeight;
    document.body.style.height = document.body.offsetHeight - cHeight;
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
    var node = $(".node-wrap");
    var node_id = node.attr("href");
    node.attr("href","../frame.html?file="+getURLParameter("file")+"&fun="+fileData[getURLParameter("file")].functions[node_id]+"&node="+node_id);

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
  window.parent.postMessage("load:clear","*");
  window.parent.postMessage("load:1:"+x,"*");

  if (old_node != "")
    old_node.setAttribute("fill",old_color);

  var a = document.getElementById("file-view-frame");
  var svgDoc = a.contentDocument;
  var svgItem = svgDoc.getElementById("a_"+x);
  old_node = get_firstchild(get_firstchild(svgItem));

  old_color = old_node.getAttribute("fill");
  old_node.setAttribute("fill",selectColor);
}

function show_info_self(x){
  window.postMessage("load:clear","*");
  window.postMessage("load:1:"+x,"*");
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

  window.parent.postMessage("load:clear","*");
  $("#data-frame").empty();
  for (var i=0; i<xs.length; i++){
    window.parent.postMessage("load:"+i+":"+xs[i],"*");
  };

  $(".inline-warning").remove();
  var ws = fileData[getURLParameter("file")].warnings[n];
  if (ws != null){
    for (var i = 0; i < ws.length; i++ ) {
      var newid = "line"+n+"_warn"+i;
      $("#line"+n).append("<iframe class=\"inline-warning\" scrolling=\"no\" id=\""+newid+"\" src=\"../warn/"+ws[i]+".xml\"></iframe>");
      $("#"+newid).iFrameResize({log:false});
    }
  }
}



function init_source(){
  var curFD = fileData[getURLParameter("file")];
  var ws = curFD.warnings;
  for (var n in curFD.warnings) {
    warn_toggle($("#line"+n+" .source-line-warn"),true);
  } 
  var ds = curFD.data;
  for (var n in ds) {
    $("#line"+n+" .source-line-nr").css("font-weight","bold");
    $("#line"+n).click((function (q,n){
      return function() {select_line(n,q);}
    }) (ds[n],n));
  } 
  var dc = curFD.dead;
  for (var i=0; i<dc.length; i++) {
    $("#line"+dc[i]+" .source-line-nr").css("color","#900");    
  }
  var line = getURLParameter("line");
  if (line) {
    select_line(line,ds[line]);
  }
}

function loadMessage(event){
  if (event.data == "load:clear") {
    $("#data-frame-div").empty();
  } else {
    var xs = event.data.split(":")
    if (xs.length==3 && xs[0]=="load")  {
      var newid = "data-frame"+xs[1];
      $("#data-frame-div").append("<iframe class=\"borderless fillW\" scrolling=\"no\" id=\""+newid+"\" src=\"nodes/"+xs[2]+".xml?file="+getURLParameter("file")+"\"></iframe>");
      $("#"+newid).iFrameResize({log:false});
    }
  }
}


function svgLoaded(){
  findFirstSvg(function (svg){
    $(svg).attr("width","100%");
    $(svg).attr("height","100%");
    $(svg).attr("id","svgObject");
    svg.ownerDocument.defaultView.show_info = show_info;
  })
  svgPanZoom.init();
}

function init_frames(){
  window.addEventListener("message", loadMessage, false);

  $('#file-button').text(getURLParameter("file"));
  $('#file-button').attr("href","frame.html?file="+getURLParameter("file"));
  if (getURLParameter("fun")==null){
    $('#file-view-frame-div').append("<iframe id=\"file-view-frame\" class=\"borderless fill\"></iframe>");
    $('#file-view-frame').attr("src","files/"+getURLParameter("file")+'.html?file='+getURLParameter("file"));

    $('#function-button').css("display","none");
    $('#function-slash').css("display","none");
  } else {
    $('#file-view-frame-div').append("<object type=\"image/svg+xml\" id=\"file-view-frame\" class=\"borderless fill\">does not work</object>");
    var href = "cfgs/"+getURLParameter("file")+"/"+getURLParameter("fun")+'.svg';
    $('#file-view-frame').attr("onload", "javascript:svgLoaded()");
    $('#file-view-frame').attr("data", href);

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
  $(document).mouseleave(function(e){
     $(document).unbind('mousemove');
     });
}


