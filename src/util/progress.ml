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

module M = Messages
module GU = Goblintutil

(* Compile time constants: *)
let tracking = false
let n = 2

(* Auxialiary data structures: *)
let current_n = ref 64
let reached_loc_hashtbl = Hashtbl.create 1001

let track_with (notify: int -> unit): unit = 
  let loc = !GU.current_loc in
  if (Hashtbl.mem reached_loc_hashtbl loc) then
    begin
      let visited_count = Hashtbl.find reached_loc_hashtbl loc in
      (* Need this because calling notify will trigger demand-driven evaluation
       * of other nodes, so we have to update the real current_n before! *)
      let the_current_n = !current_n in
	visited_count := (succ !visited_count);
	if (!visited_count > the_current_n) then
	  begin
	    current_n := the_current_n * n;
	    notify the_current_n
	  end
    end
  else
    Hashtbl.add reached_loc_hashtbl loc (ref 1)

let track () = 
  let msg n = M.warn_all ("Line visited more than " ^ string_of_int n ^ " times.") in
    track_with msg

let show_subtask (subt:string) (len:int) =
  Printf.printf "PROGRESS /-/ SUBTASK /-/ %s /-/ %d\n%!" subt len


let show_add_work (len:int) =
  Printf.printf "PROGRESS /-/ MORE WORK /-/ %d\n%!" len


let add_work_count = ref 0 
let add_work_acc = ref 0
let show_add_work_buf (len:int) =
  if !add_work_count > (2) && (abs !add_work_acc) > 2 then begin
      show_add_work (len + !add_work_acc);
      add_work_count := 0;
      add_work_acc := 0
    end 
  else begin
      add_work_count := !add_work_count + 1;
      add_work_acc   := !add_work_acc   + len
    end      


let show_worked (len:int) =
  Printf.printf "PROGRESS /-/ WORKED /-/ %d\n%!" len


let worked_count = ref 0 
let worked_acc = ref 0 
let show_worked_buf (len:int) =
  if !worked_count > (2) && (abs !worked_acc) > 2 then begin
      show_worked (len + !worked_acc);
      worked_count := 0;
      worked_acc := 0
    end 
  else begin
      worked_count := !worked_count + 1;
      worked_acc   := !worked_acc   + len
    end
      
