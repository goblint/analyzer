let css_string =
  "
html , body {
  font-family:Georgia,Serif;
  background-color: #FFFFFF;
  margin: 0;
  padding: 0;
}

/* no space between table rows/columns */
table {
  border-collapse:collapse;
}

/* header style */
th {
  vertical-align: top;
  padding:4pt;
  background-color:rgb(155,206,233) ;
  border-bottom:1px solid #000;
  border-top:1px solid #000;
  text-align:left;
}


/* code section */
#code {
  background-color:rgb(235,230,216);
  z-index:1;
  position:absolute;
  width:100%;
  height:93%;
  overflow: scroll;
  padding-top:12pt;
}
#code table {
  width:100%;
}
.th_code{
  width:99%;
}

/* line numbering */
td.line {
  font-weight: bold;
  font-family:\"Courier New\", monospace;
  text-align:right;
  padding-left:15pt;
  padding-right:5pt;
  font-size:90%;
}
th.line {
  text-align:right;
}



/* markers in the code section & menubar */
.marker, .file_link {
  cursor:pointer;
  border-style:solid;
  border-width:1px;
  border-radius:16pt;
  min-width:26pt;
  min-height:26pt;
  padding:0px 7px;
  margin:2pt;
  background-color:rgb(215,215,215);
  font-size:80%;
}
.marker:hover, .file_link:hover {
  background-color:rgb(155,206,233) ;
}
.selected{
  background-color:rgb(155,206,233) ;
}


/* info section */
#info {
  background-color:#FFF;
  border-radius:10pt;
  top:20%;
  right:0px;
  position:absolute;
  z-index:2  ;
  width:40%;
  height:60%;
  overflow: scroll;
  border-style:solid;
  border-width:1px;
}
#info td,#info th{
  padding-left:8pt;
  padding-right:8pt;
}
#info table{
  margin-top:8pt;
  margin-bottom:8pt;
  width:100%;
}

/* menu section */
#menubar{
  border-bottom:solid;
  border-width:2px;
  background-color:rgb(242,116,60);
  padding:5pt;
  height:14pt;
}
#glob_button{
  margin:2pt;
  float:right;
}

/* foldable treeview elements */
.node_data {
  padding:8pt;
}
.node_descr {
  padding:4pt;
  font-weight: bold;
  background-color:rgb(155,206,233) ;
  border-bottom:1px solid #000;
  border-top:1px solid #000;
  margin-top:8pt;
  margin-bottom:8pt;
}
.content{
	margin-left: 15px;
}

"
