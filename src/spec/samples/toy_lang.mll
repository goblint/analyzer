(* scanner for a toy language *)

{
  open Printf
}

let digit = ['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9']*

rule toy_lang = parse
  | digit+ as inum
  	{ printf "integer: %s (%d)\n" inum (int_of_string inum);
	  toy_lang lexbuf
	}
  | digit+ '.' digit* as fnum
  	{ printf "float: %s (%f)\n" fnum (float_of_string fnum);
	  toy_lang lexbuf
	}
  | "if"
  | "then"
  | "begin"
  | "end"
  | "let"
  | "in"
  | "function" as word
  	{ printf "keyword: %s\n" word;
	  toy_lang lexbuf
	}
  | id as text
  	{ printf "identifier: %s\n" text;
	  toy_lang lexbuf
	}
  | '+'
  | '-'
  | '*'
  | '/' as op
  	{ printf "operator: %c\n" op;
	  toy_lang lexbuf
	}
  | '{' [^ '\n']* '}'	{ toy_lang lexbuf }	(* eat up one-line comments *)
  | [' ' '\t' '\n']	{ toy_lang lexbuf }	(* eat up whitespace *)
  | _ as c
  	{ printf "Unrecognized character: %c\n" c;
	  toy_lang lexbuf
	}
  | eof		{ }

{
  let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    toy_lang lexbuf

  let _ = Printexc.print main ()
}