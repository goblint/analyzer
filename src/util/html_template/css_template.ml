let css_string = 
"
html , body {
  font-family:Georgia,Serif;
  margin: 0;
  padding: 0;
}
  
.mywindow 
{
    border: 1px solid #C0C0C0;
    position: fixed;
    overflow: hidden;
    width: auto;
    height: auto;
    background-color: #F0F0F0;
    overflow: hidden;
}
  
.mywindow_header 
{
    background-color: #E0E0E0;
    border-bottom: 1px solid #C0C0C0;
    color: #4050A0;
    font-family: Arial;
    font-weight: bold;
    font-size: 16px;
    padding: 2px;
}
  
.mywindow_content 
{
    overflow: scroll;
    padding: 0px;
    width: 0px;
    height: 0px;
}

.iframebox {
    overflow: scroll;
    border: 0px;
    width: 100%;
    height: 100%;
}

#analysisfilebody {
  width: 100%;
  height: 100%;
  background-color: #F0F0F0;
  font-size: 14px;
  font-family: Arial,Monospace;     
}
  
#leftWindowContent
{
  background-color: #F0F0F0;
  padding: 0px;
  margin-right: 0px;
  font-size: 14px;
  font-family: Arial,Monospace;     
}  
 
#codeWindowContent
{
  background-color: #F8F8F8;
}
        
#codeWindowContent a 
{
  text-decoration: none;
  color: #000000;
  font-weight: bold;
}
        
#codeWindowContent a:hover 
{
  text-decoration: underline;
  color: #0000C0;
}
        
#codeWindowContent pre 
{
  padding: 0px;
  margin: 0px;
}
  
.lt0
{
  background-color: #F0F0F0;
}
        
.lt1
{
  background-color: #F8F8F8;
}
  
.cpp_datatype
{
  color: blue;
}
        
.cpp_keyword
{
  color: blue;            
}

.cpp_preprocessor
{
  color: purple;            
}
        
.cpp_stringDQ
{
  color: red;            
}
        
.cpp_stringSQ
{
  color: red;            
}
        
.cpp_comment
{
  color: Green;
}

.cpp_function
{
}

.cpp_function a
{
  color: Green;
  text-decoration: none;
  font-weight: normal;
}

td, th { 
  vertical-align: top; 
  padding:4pt;
}

th {
  border-bottom:1px solid #000;
  border-top:1px solid #000;
  text-align:left;
}

table {
  border-collapse:collapse;
}

.toggle, .toggle_ttl {
  cursor: pointer;
}

.amenu td {
  border-bottom:1px solid #ccc;
}

.mmenu td {
  border-bottom:1px solid #ccc;
  border-top:1px solid #ccc;
}

#vers table {
  display:inline-block;
  vertical-align:top;
}

#vers, #detail, #ana, #gprop {
  width:70%;
  margin: 0px auto;
}

#date {
  text-align:right;
}

.entrydir {
	margin-left: 0px;
  font-weight: bold; 
  cursor: pointer;
}
.entrydircontent {
	margin-left: 15px;
}

.tabheader {
	float: left;
	margin-left: 2px;
	padding: 2px;
	border: 1px solid #C0C0C0;
	background-color: #F8F8F8;
	font-size: 14px;
}

"
