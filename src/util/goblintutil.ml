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

(** Globally accessible flags and utility functions. *)

open Cil

(** when goblin is in debug mode *)
let debug = ref false 

(** Outputs information about what the goblin is doing *)
let verbose = ref false

(** prints the CFG on [getCFG] *)
let cfg_print = ref false 

(** analyze all the functions in the program, rather than just main *)
let allfuns = ref false
let nonstatic = ref false

(** name of the main / init function *)
let mainfun = ref "main"

(** name of the exit function, just additionally spawned ... *)
let exitfun = ref ([]: string list)

(** Whether a main function has been found. *)
let has_main = ref false

(** print information about all globals, not just races *)
let allglobs = ref false

(** an optional path to dump all output *)
let dump_path = ref (None : string option)

(** has any threads have been spawned *)
let multi_threaded = ref false

(** should globals be side-effected early *)
let earlyglobs = ref false

(** Will terminate on a collapsed array --- for debugging. *)
let die_on_collapse = ref false

(** hack to use a special integer to denote synchronized array-based locking *)
let inthack = Int64.of_int (-19012009)

(** The file where everything is output *)
let out = ref stdout

type result_style =
  | None (** Do not print any output except warnings *)
  | State (** Only output the state of main function *)
  | Indented (** Output indented XML *)
  | Compact (** Output compact XML, for Eclipse plugin *)
  | Pretty (** Pretty-printed text outpu *)

(** The specified result style *)
let result_style = ref None

(** Is the goblin Eclipse Plugin calling the analyzer? *)
let eclipse = ref false

(** Analyzing Device Drivers? *)
let kernel = ref false

(** Enable region-based warnings. *)
let regions = ref false

(** Length of summary description in XML output *)
let summary_length = 80

(** Do we need to print CIL's temporary variables? *)
let show_temps = ref true

(** If we want to display functions that are not called *)
let print_uncalled = ref false

(** A very nice imperative hack to get the current location. This can be
  * referenced from within any transfer function. *)
let current_loc = ref locUnknown

let escape (x:string):string =
  let esc_1 = Str.global_replace (Str.regexp "&") "&amp;" x in
  let esc_2 = Str.global_replace (Str.regexp "<") "&lt;" esc_1 in
  let esc_3 = Str.global_replace (Str.regexp ">") "&gt;" esc_2 in
  let esc_4 = Str.global_replace (Str.regexp "\"") "&quot;" esc_3 in
    esc_4

let trim (x:string): string = 
  let len = String.length x in
    if x.[len-1] = ' ' then String.sub x 0 (len-1) else x


(** Creates a directory and returns the absolute path **)
let create_dir name = 
  let dirName = if Filename.is_relative name then Filename.concat (Unix.getcwd ()) name else name in
  (* The directory should be writable to group and user *)
  let dirPerm = 0o770 in
  let _ = 
    try
      Unix.mkdir dirName dirPerm
    with Unix.Unix_error(err, ctx1, ctx2) as ex -> 
      (* We can discared the EEXIST, we are happy to use the existing directory *)
      if err != Unix.EEXIST then begin
        (* Hopefully will be friendly enough :) *)
        print_endline ("Error, " ^ (Unix.error_message err));
        raise ex
      end
  in
    dirName
