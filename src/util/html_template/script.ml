let script_str =
"
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

function reinit_toggle_mouse(e) {
  e.onmousedown = function ch(t) {
      e.firstChild.nodeValue = '◿' + e.firstChild.nodeValue.substr(1, e.firstChild.nodeValue.length) ;
  };
  e.onmouseup = function ch(t) {changeContentVisibility(e);};
}

function init_toggle(e) {
  e.firstChild.nodeValue = \"▿ \"+e.firstChild.nodeValue;
  reinit_toggle_mouse(e)
}


var current_marker = \"\"
function marker_click(id){
  var els = document.getElementsByClassName('marker_'+current_marker);
  for(var i=0; i<els.length; i++){
    els[i].className=els[i].className.substr(0,els[i].className.length-9);
  }
  current_marker = id;
  var els = document.getElementsByClassName('marker_'+id);
  for(var i=0; i<els.length; i++){
    els[i].className=els[i].className+\" selected\";
  }

  document.getElementById('info').innerHTML = document.getElementById(id).innerHTML;

  /* actually one should only reinit things inside 'info' */
  var els = document.getElementsByClassName('toggle');
  for (var i=0; i < els.length; i++) {
    reinit_toggle_mouse(els[i]);
  }
}

var current_file = \"\"
function file_link_click(id){
  var els = document.getElementsByClassName('file_link_'+current_file);
  for(var i=0; i<els.length; i++){
    els[i].className=els[i].className.substr(0,els[i].className.length-9);
  }
  current_file = id;
  var els = document.getElementsByClassName('file_link_'+id);
  for(var i=0; i<els.length; i++){
    els[i].className=els[i].className+\" selected\";
  }

  document.getElementById('code').innerHTML = document.getElementById(id).innerHTML;
}


function init_all() {
  var els = document.getElementsByClassName('toggle');
  for (var i=0; i < els.length; i++) {
    init_toggle(els[i]);
  }
  var els = document.getElementsByClassName('toggle_off');
  for (var i=0; i < els.length; i++) {
    changeContentVisibility(els[i]);
  }

  file_link_click(document.getElementById('files').firstChild.id);
  marker_click(\"globals\");
}
init_all();

"