(* TRACING STUFF. A rewrite of Cil's tracing framework which is too slow for the
 * large domains we output. The original code generated the document object
 * even when the subsystem is not activated. *)

open Pretty
open Cil

module Strs = Set.Make (String)

let current_loc = ref locUnknown
let trace_sys = ref Strs.empty
let activated = ref Strs.empty
let active_dep = Hashtbl.create 9 
let tracevars = ref ([]: string list)
let tracelocs = ref ([]: int list)

let addsystem sys = trace_sys := Strs.add sys !trace_sys
let activate (sys:string) (subsys: string list): unit = 
  let subs = List.fold_right Strs.add subsys (Strs.add sys Strs.empty) in 
    activated := Strs.union !activated subs;
    Hashtbl.add active_dep sys subs
let deactivate (sys:string): unit =
  activated := Strs.diff !activated (try Hashtbl.find active_dep sys with Not_found -> print_endline ("WTF? " ^ sys); Strs.empty)

let indent_level = ref 0
let traceIndent () = indent_level := !indent_level + 2
let traceOutdent () = indent_level := !indent_level - 2

(* Parses a format string to generate a nop-function of the correct type. *)
let mygprintf (format : ('a, unit, doc, 'b) format4) : 'a =
  let format = (Obj.magic format : string) in
  let flen    = String.length format in
  let fget    = String.unsafe_get format in
  let rec literal acc i = 
    let rec skipChars j = 
      if j >= flen || (match fget j with '%' | '@' | '\n' -> true | _ -> false) then
        collect nil j
      else
        skipChars (succ j)
    in
    skipChars (succ i)
  and collect (acc: doc) (i: int) = 
    if i >= flen then begin
      Obj.magic (()) 
    end else begin
      let c = fget i in
      if c = '%' then begin
        let j = skip_args (succ i) in
        match fget j with
          '%' -> literal acc j 
	| ',' -> collect acc (succ j)
        | 's' | 'c' | 'd' | 'i' | 'o' | 'x' | 'X' | 'u'
        | 'f' | 'e' | 'E' | 'g' | 'G' | 'b' | 'B' -> 
            Obj.magic(fun b -> collect nil (succ j))
	| 'L' | 'l' | 'n' -> Obj.magic(fun n -> collect nil (succ (succ j)))
        | 'a' -> Obj.magic(fun pprinter arg -> collect nil (succ j))
        | 't' -> Obj.magic(fun pprinter -> collect nil (succ j))
        | c -> invalid_arg ("dprintf: unknown format %s" ^ String.make 1 c)
      end else if c = '@' then begin
        if i + 1 < flen then begin
          match fget (succ i) with
            '[' | ']' | '!' | '?' | '^' | '@' -> collect nil (i + 2)
          | '<' | '>' -> collect nil (i + 1)
          | c -> invalid_arg ("dprintf: unknown format @" ^ String.make 1 c)
        end else
          invalid_arg "dprintf: incomplete format @"
      end else if c = '\n' then begin
        collect nil (i + 1)
      end else
        literal acc i
    end
  and skip_args j =
    match String.unsafe_get format j with
      '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
    | c -> j
  in
  collect nil 0

let traceTag (sys : string) : Pretty.doc =
  let rec ind (i : int) : string = if (i <= 0) then "" else " " ^ (ind (i-1)) in
    (text ((ind !indent_level) ^ "%%% " ^ sys ^ ": "))

let printtrace sys d: unit = 
  fprint stderr 80 ((traceTag sys) ++ d); 
  flush stderr 

let gtrace always f sys var ?loc do_subsys fmt = 
  let cond = 
    (Strs.mem sys !activated || always && Strs.mem sys !trace_sys) &&
    match var,loc with 
      | Some s, Some l -> (!tracevars = [] || List.mem s !tracevars) &&
                          (!tracelocs = [] || List.mem l !tracelocs)
      | Some s, None   -> (!tracevars = [] || List.mem s !tracevars)
      | None  , Some l -> (!tracelocs = [] || List.mem l !tracelocs)
      | _ -> true
  in
  if cond then begin
    do_subsys ();
    gprintf (f sys) fmt
  end else
    mygprintf fmt

let trace sys ?var fmt = gtrace true printtrace sys var (fun x -> x) fmt

let tracel sys ?var fmt = 
  let loc = !current_loc in
  let docloc sys doc = 
    printtrace sys (dprintf "(%s:%d)" loc.file loc.line ++ indent 2 doc);
  in
    gtrace true docloc sys var ~loc:loc.line (fun x -> x) fmt

let tracei (sys:string) ?var ?(subsys=[]) fmt =  
  let f sys d = printtrace sys d; traceIndent () in
  let g () = activate sys subsys in
    gtrace true f sys var g fmt

let tracec sys fmt = gtrace false printtrace sys None (fun x -> x) fmt

let traceu sys fmt =  
  let f sys d = printtrace sys d; traceOutdent () in
  let g () = deactivate sys in
    gtrace false f sys None g fmt


let traceli sys ?var ?(subsys=[]) fmt = 
  let loc = !current_loc in
  let g () = activate sys subsys in
  let docloc sys doc: unit = 
    printtrace sys (dprintf "(%s:%d)" loc.file loc.line ++ indent 2 doc);
    traceIndent ()
  in
    gtrace true docloc sys var ~loc:loc.line g fmt
