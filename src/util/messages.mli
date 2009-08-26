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

exception Bailure of string
(** Raise this when you want to bail out of doing any analysis. This will
  * continue the analysis with a warning and leaving the state untouched. *)

val bailwith: string -> 'a
(** Behaves like [failwith], but with bailures that will not terminate the
  * analysis. *)

val warn_out : out_channel ref
val get_out: string -> out_channel -> out_channel

val print_msg: string -> location -> unit
(** Prints a message and adds the given location information. *)

val print_group: string -> (string * location) list -> unit
(** Prints a group of warning with a title and individual messages and given
  * location information. *)

val soundness: bool ref
(** The soundness of the current analysis. We begin with sound analyses, but if
  * we can't keep soundness, we try to continue and maybe find some bugs. *)

val warnings: bool ref
(** Turns on printing of soundness warnings. *)

val write: string -> unit
(** Print out a message, does not affect soundness. *)

val report: string -> unit
(** Print out a message, does not affect soundness. One message is 
  * printed per line of code. *)

val warn: string -> unit
(** Prints a warning and adds the source code location where the warning
  * occured. This should also be called from within transfer functions. 
  * Same message is printed only once. *)

val warn_each: string -> unit
(** Prints a warning and adds the source code location where the warning
  * occured. This should also be called from within transfer functions. 
  * One message is printed per line of code. *)

val warn_all: string -> unit
(** Prints a warning and adds the source code location where the warning
  * occured. This should also be called from within transfer functions. *)

val warn_urgent: string -> unit
(** Prints a warning no matter what. And sets soundness to false. *)

val debug: string -> unit
(** Prints a debugging warning with location. *)

val tracing: bool
(** Static flag to turn off tracing (improves performance) *)

val trace: string -> Pretty.doc -> unit
(** A wrapper around {!Trace.trace}. *)

val tracei: string -> Pretty.doc -> unit
(** A wrapper around {!Trace.tracei}. *)

val traceu: string -> Pretty.doc -> unit
(** A wrapper around {!Trace.traceu}. *)

val tracel: string -> Pretty.doc -> unit
(** Like {!Analyses.trace}, but adds the location information to the message. *)

val traceli: string -> Pretty.doc -> unit
(** Like {!Analyses.tracei}, but adds the location information to the message. *)
