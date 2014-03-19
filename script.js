var selectColor = "#fcf"

function getURLParameter(name) {
  return decodeURIComponent((new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)').exec(location.search)||[,""])[1].replace(/\+/g, '%20'))||null
}

function get_firstchild(n){
  x=n.firstChild;
  while (x.nodeType!=1) {
    x=x.nextSibling;
  }
  return x;
}

function getNodeWithValue(e){
	if (e.nodeValue==null)
		return e.firstChild;
	else
		return e;
}


// toggle foldables
function toggleVisibility(e) {
  var ech = getNodeWithValue(e.firstChild);
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

// intialize foldables
function init_toggle(e) {
  var ech = get_firstchild(e);
  getNodeWithValue(e.firstChild).nodeValue = "+ "+getNodeWithValue(e.firstChild).nodeValue;
  ech.onmouseup = function ch(t) {toggleVisibility(e);};
  ech.style.cursor = "pointer";
}

// prepare the data node by initializing the tree structure
function init_node() {
	// make all nodes of class 'toggle' foldable
  var els = document.getElementsByClassName('toggle');
  for (var i=0; i < els.length; i++) {
    init_toggle(els[i]);
  }

  // close all 'toggle off' nodes
  var els = document.getElementsByClassName('toggle off');
  for (var i=0; i < els.length; i++) {
    toggleVisibility(els[i]);
  }
}


// select the node from the svg graph
var old_color = ""
var old_node = ""
function show_info(x){
	// clear data nodes
  window.postMessage("load:clear","*");
  // show data for the selected color
  window.postMessage("load:1:"+x,"*");

  // color the nodes
  if (old_node != "")
    old_node.attr("fill",old_color);
  old_node = $("#a_"+x+" a :first-child");
  old_color = old_node.attr("fill");
  $("#a_"+x+" a :first-child").attr("fill",selectColor);
}

// show local nodes --- called by the "globals" button
function show_info_self(x){
  window.postMessage("load:clear","*");
  window.postMessage("load:1:"+x,"*");
}



// select lines from the code-listing
var old_line = 0;
function select_line(n,xs,ys) {
	// update background color to show selected line
  $("#ln"+old_line).css("background-color","transparent");
  $("#ln"+n).css("background-color",selectColor);
  old_line = n;

	// load data nodes by posting messages
  window.parent.postMessage("load:clear","*");
  for (var i = 0; i < xs.length; i++ ) {
    window.parent.postMessage("load:"+i+":"+xs[i],"*");
  }

	// show warnings if present
  $(".inline-warning").remove();
  for (var i = 0; i < ys.length; i++ ) {
    var newid = "ln"+n+"_warn"+i;
    $("#ln"+n).append("<iframe class=\"inline-warning\" scrolling=\"no\" id=\""+newid+"\" src=\"../warn/"+ys[i]+".xml\"></iframe>");
    $("#"+newid).iFrameResize({log:false});
  }
}


// messages to handle data nodes
function loadMessage(event){
	// clear event to remove all displayed nodes
  if (event.data == "load:clear") {
    $("#data-frame-div").empty();
  } else {
    // in case it is "load:<x>:<y>", show node y, as the x-th data node
    var xs = event.data.split(":")
    if (xs.length==3 && xs[0]=="load")  {
      var newid = "data-frame"+xs[1];
      $("#data-frame-div").append("<iframe class=\"borderless fillW\" scrolling=\"no\" id=\""+newid+"\" src=\"nodes/"+xs[2]+".xml?file="+getURLParameter("file")+"\"></iframe>");
      $("#"+newid).iFrameResize({log:false});
    }
  }
}

// highlight the selected line if possible
function init_file(){
  if (getURLParameter("line") != null)
    $("#ln"+getURLParameter("line")).click();

}

// used on load of frames.html
function init_frames(){
  // wait for events from file listings or such
  window.addEventListener("message", loadMessage, false);

  // fix breadcrumbs: file
  $('#file-button').text(getURLParameter("file"));
  $('#file-button').attr("href","frame.html?file="+getURLParameter("file"));

	// if we know the function then show the graph, else try to show the whole file
  if (getURLParameter("file")!=null) {
    if (getURLParameter("fun")!=null) {
		  // we know the function so we can show the graph
      $('#file-view-frame-div').load("cfgs/"+encodeURIComponent(encodeURIComponent(getURLParameter("file")))+"/"+getURLParameter("fun")+'.svg',
          function f(){
              $("#file-view-frame-div svg").attr("width","100%");
              $("#file-view-frame-div svg").attr("height","100%");
              svgPanZoom.init({selector:"#file-view-frame-div svg"});
          });
      $('#function-button').text(getURLParameter("fun"));
      $('#function-button').attr("href","frame.html?fun="+getURLParameter("fun")+"&file="+getURLParameter("file"));
    } else {
		  // we know the file only, so we show the file listing
      $('#file-view-frame-div').empty();
      $('#file-view-frame-div').append("<iframe class=\"borderless fill\"src=\"files/"+encodeURIComponent(encodeURIComponent(getURLParameter("file")))+".xml?line="+getURLParameter("line")+"\"></iframe>");

      // fix breadcrumbs: function -- not avaliable
      $('#function-button').css("display","none");
      $('#function-slash').css("display","none");
    }
  }

  // if a node is given as an argument, show it
  if (getURLParameter("node")!= null)
    setTimeout(function () {
      show_info(getURLParameter("node"));
    },100);
    
  
  // initialize the drag-bar
  $('#dragbar').mousedown(function(e){
      e.preventDefault();
      $('body').mousemove(function(e){
	      $('#dragbar').css("left",e.pageX-8);
        $('#data-frame-div').css("width",e.pageX-22);
        $('#file-view-frame-div').css("left",e.pageX+10);
     })
  });
  $('body').mouseup(function(e){
    $('body').unbind('mousemove');
  });
}



