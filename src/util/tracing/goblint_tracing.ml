(** Nested tracing system for debugging. *)

(* TRACING STUFF. A rewrite of Cil's tracing framework which is too slow for the
 * large domains we output. The original code generated the document object
 * even when the subsystem is not activated. *)

open Goblint_std
open Goblint_parallel
open GoblintCil

module Strs = Set.Make (String)


let tracing = Goblint_build_info.dune_profile = "trace"

let current_loc = ref locUnknown
let next_loc    = ref locUnknown
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
  activated := Strs.diff !activated (try Hashtbl.find active_dep sys with Not_found -> Goblint_logs.Logs.error "Missing tracing active_dep for %s" sys; Strs.empty)

let indent_level = ref 0
let traceIndent () = indent_level := !indent_level + 2
let traceOutdent () = indent_level := !indent_level - 2

let traceTag (sys : string) : string =
  let rec ind (i : int) : string = if (i <= 0) then "" else " " ^ (ind (i-1)) in
  (ind !indent_level) ^ "%%% " ^ sys ^ ": "

let trace_mutex = GobMutex.create ()

let printtrace sys (s: string): unit =
  GobMutex.lock trace_mutex;
  Printf.eprintf "%s%s\n%!" (traceTag sys) s;
  GobMutex.unlock trace_mutex

let gtrace always f sys var ?loc do_subsys fmt =
  let cond =
    (Strs.mem sys !activated || always && Strs.mem sys !trace_sys) &&
    (* TODO: allow file, column in tracelocs? *)
    match var,loc with
    | Some s, Some l -> (!tracevars = [] || List.mem s !tracevars) &&
                        (!tracelocs = [] || List.mem l.line !tracelocs)
    | Some s, None   -> (!tracevars = [] || List.mem s !tracevars)
    | None  , Some l -> (!tracelocs = [] || List.mem l.line !tracelocs)
    | _ -> true
  in
  if cond then begin
    do_subsys ();
    let buf = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer buf in
    Format.kfprintf (fun ppf ->
        Format.pp_print_flush ppf ();
        f sys (Buffer.contents buf)
      ) ppf fmt
  end else
    Format.ifprintf Format.err_formatter fmt

let trace sys ?var fmt = gtrace true printtrace sys var ignore fmt

(* trace*
 * l: include location
 * i: indent after print, optionally activate subsys for sys
 * u: outdent after print, deactivate subsys of sys
 * c: continue/normal print w/o indent-change
*)

let tracei (sys:string) ?var ?(subsys=[]) fmt =
  let f sys s = printtrace sys s; traceIndent () in
  let g () = activate sys subsys in
  gtrace true f sys var g fmt

let tracec sys fmt = gtrace false printtrace sys None ignore fmt

let traceu sys fmt =
  let f sys s = printtrace sys s; traceOutdent () in
  let g () = deactivate sys in
  gtrace true f sys None g fmt
