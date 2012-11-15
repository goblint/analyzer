%{
(* 	Header - Ocaml declarations (Ocaml code) *)

open OilUtil

%}

%token RBRACK LBRACK RCURL LCURL SEMICOLON COLON COMMA ASSIGN DOTS EOF
%token CPU IMPLEMENTATION OIL_VERSION
%token AUTO NO_DEFAULT WITH_AUTO
%token BOOLEAN STRING ENUM FLOAT INT64 UINT64 INT32 UINT32
%token <string> OIL_STRING 
%token <int> NUMBER
%token <float> OIL_FLOAT
%token <string> OBJECT
%token <string> OBJECT_TYPE
%token <bool> BOOL
%token <string> NAME 

%start file
%type <(string*string*(OilUtil.param_t list)) list> file

%%
/*(* 	Grammar rules *) */

file			  : oil_version implementation_definition application_definition EOF		{$3}
			  | oil_version implementation_definition application_definition		{$3}

/* version details */
oil_version 		  : /* empty definition */							{}
			  | OIL_VERSION ASSIGN OIL_STRING description SEMICOLON 			{}
;

/* implementation definitions */
implementation_definition : /* empty implementation definition */					{}
			  | IMPLEMENTATION NAME LCURL implementation_spec_list RCURL description SEMICOLON {}
;
implementation_spec_list  : implementation_spec								{}
			  | implementation_spec_list implementation_spec				{}
;
implementation_spec	  : OBJECT LCURL implementation_list RCURL description SEMICOLON		{}
;

implementation_list 	  :  /* empty list */								{}
			  | implementation_def implementation_list 					{}
;
implementation_def	  : impl_attr_def								{}
			  | impl_ref_def								{}
;
description		  :  /* empty definition */							{}
			  | COLON OIL_STRING								{}

impl_attr_def		  : UINT32	auto_specifier number_range attribute_name multiple_specifier default_number description SEMICOLON 	{}
			  | INT32 	auto_specifier number_range attribute_name multiple_specifier default_number description SEMICOLON 	{}
			  | UINT64	auto_specifier number_range attribute_name multiple_specifier default_number description SEMICOLON 	{}
			  | INT64 	auto_specifier number_range attribute_name multiple_specifier default_number description SEMICOLON 	{}
			  | FLOAT	auto_specifier float_range attribute_name multiple_specifier default_float description SEMICOLON 	{}
			  | ENUM	auto_specifier enumeration attribute_name multiple_specifier default_name description SEMICOLON 	{}
			  | STRING	auto_specifier attribute_name multiple_specifier default_string description SEMICOLON 	{}
			  | BOOLEAN	auto_specifier bool_values attribute_name multiple_specifier default_bool description SEMICOLON 	{}
;
auto_specifier 		  : /* empty definition */							{}
			  | WITH_AUTO									{}
;
number_range		  : /* empty definition */							{}
			  | LBRACK NUMBER DOTS NUMBER RBRACK						{}
			  | LBRACK number_list RBRACK							{}
;
number_list		  : NUMBER 									{}
			  | number_list COMMA NUMBER 							{}
;
default_number		  : /* empty definition */ 							{}
			  | ASSIGN NUMBER 								{}
			  | ASSIGN NO_DEFAULT 								{}
			  | ASSIGN AUTO									{}
;

float_range		   : /* empty definition */							{}
			   | LBRACK OIL_FLOAT ".." OIL_FLOAT RBRACK						{}
;
default_float		   : /* empty definition */							{}
			   | ASSIGN OIL_FLOAT 								{}
			   | ASSIGN NO_DEFAULT 								{}
			   | ASSIGN AUTO								{}
;
enumeration		   : LBRACK enumerator_list RBRACK						{}
;
enumerator_list		   : enumerator									{}
			   | enumerator_list COMMA enumerator						{}
;
enumerator		   : NAME impl_parameter_list description					{}
;
impl_parameter_list 	  : /* empty definition */ 							{}
			  | LCURL implementation_list RCURL 						{}
;
bool_values		   : /* empty definition */							{}
			   | LBRACK BOOL impl_parameter_list description COMMA BOOL impl_parameter_list description RBRACK {}
;
default_name		   : /* empty definition */							{}
			   | ASSIGN STRING								{}
			   | ASSIGN NO_DEFAULT								{}
			   | ASSIGN AUTO								{}
;
default_string		   : /* empty definition */							{}
			   | ASSIGN STRING								{}
			   | ASSIGN NO_DEFAULT 								{}
			   | ASSIGN AUTO								{}
;
default_bool		   : /* empty definition */							{}
			   | ASSIGN BOOL								{}
			   | ASSIGN NO_DEFAULT 								{}
			   | ASSIGN AUTO								{}
;
impl_ref_def 		  : OBJECT_TYPE reference_name multiple_specifier description SEMICOLON		{}
;
reference_name 		  : NAME 									{}
			  | OBJECT									{}
;
multiple_specifier	  : /* empty definition */							{}
			  | LBRACK RBRACK								{}
;

/* appplication definitions */
application_definition	  : CPU NAME LCURL object_definition_list RCURL description SEMICOLON		{$4}
			  | object_definition_list							{$1}
;

/* this is the part we are actually interessted in */
object_definition_list	  : /* empty definition */ 							{[]}
			  | object_definition object_definition_list 					{$1 :: $2}
;
object_definition	  : OBJECT NAME description SEMICOLON						{($1, $2, []) }
			  | OBJECT NAME LCURL parameter_list RCURL description SEMICOLON			{($1, $2, $4)}
;
parameter_list		  : /* empty definition */							{[]}
			  | parameter parameter_list							{$1::$2}
;
parameter		  : attribute_name ASSIGN attribute_value description SEMICOLON 			{($1,$3)}
;
attribute_name		  : NAME									{$1}
			  | OBJECT									{$1}
;
params			  : /* empty definition */ 							{None}
			  | LCURL parameter_list RCURL 							{Some $2}
;
attribute_value 	  : NAME params									{ Name ($1,$2)}
			  | BOOL params									{ Bool ($1,$2)}
			  | NUMBER									{ Int $1}
			  | OIL_FLOAT									{ Float $1}
			  | OIL_STRING									{ String $1}
			  | AUTO									{Auto}
;



%%

let parse_error s = print_endline s

(* 	Trailer - Additional Ocaml code (Ocaml code) *)