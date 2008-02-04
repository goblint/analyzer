(* 
 * Copyright (c) 2005-2007,
 *     * University of Tartu
 *     * Vesal Vojdani <vesal.vojdani@gmail.com>
 *     * Kalmer Apinis <kalmera@ut.ee>
 *     * Jaak Randmets <jaak.ra@gmail.com>
 *     * Toomas Römer <toomasr@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 * 
 *     * Neither the name of the University of Tartu nor the names of its
 *       contributors may be used to endorse or promote products derived from
 *       this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

open Cil
open Pretty
module GU = Goblintutil

exception Bailure of string
let bailwith s = raise (Bailure s)

let tracing = false  (* Hopefully, when set to false optimizations will kick in *)

let soundness = ref true
let warn_out = ref stdout

let get_out name alternative = match !GU.dump_path with
  | Some path -> open_out (Filename.concat path (name ^ ".out"))
  | _ -> alternative

let current_loc = GU.current_loc

let print_msg msg loc = 
  if !Goblintutil.eclipse then 
    Printf.printf "WARNING /-/ %s /-/ %d /-/ %s\n%!" loc.file loc.line msg
  else
    Printf.fprintf !warn_out "%s (%s:%d)\n%!" msg loc.file loc.line

let print_group group_name errors =
  if !Goblintutil.eclipse then
    List.iter (fun (msg,loc) -> print_msg (group_name ^ ", " ^ msg) loc) errors
  else
    let f (msg,loc): doc = Pretty.dprintf "%s (%s:%d)" msg loc.file loc.line in
      ignore (Pretty.fprintf !warn_out "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)

let warn_urgent msg = 
  soundness := false;
  print_msg msg (!current_loc)

let warn_all msg = 
  if !GU.debug then 
    print_msg msg (!current_loc)
  else if !soundness then 
    print_msg ("No longer sound. " ^ msg) !current_loc;
  soundness := false

let warn_str_hashtbl = Hashtbl.create 10
let warn_lin_hashtbl = Hashtbl.create 10

let warn msg = 
  if (Hashtbl.mem warn_str_hashtbl msg == false) then
    begin
      warn_all msg;
      Hashtbl.add warn_str_hashtbl msg true
    end

let warn_each msg = 
  let loc = !current_loc in
    if (Hashtbl.mem warn_lin_hashtbl (msg,loc) == false) then
      begin
	warn_all msg;
	Hashtbl.add warn_lin_hashtbl (msg,loc) true
      end

let debug msg =
  if !GU.debug then warn msg

let trace = Trace.trace
let tracei = Trace.tracei
let traceu = Trace.traceu

let tracel sys doc = 
  let loc = !current_loc in
  let docloc = text loc.file ++ text ":" ++ num loc.line ++ line ++ indent 2 doc in
    Trace.trace sys docloc

let traceli sys doc =
  let loc = !current_loc in
  let doc = text loc.file ++ text ":" ++ num loc.line ++ line ++ indent 2 doc in
    Trace.tracei sys doc

