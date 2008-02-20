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

(** Abstract domain for addresses, the abstraction of l-values. *)
open Cil
open Pretty

module type S = 
sig
  include Lattice.S
  type idx (** The integer abstract domain for array indexes. *)
  type field (** The abstract representation of field names. *)
  type heap (** The heab representation. *)
  
  val from_var: varinfo -> t
  (** Creates an address from variable. *)  
  val from_var_offset: (varinfo * (idx,field) Lval.offs) -> t
  (** Creates an address from a variable and offset, will fail on a top element! *) 
  val to_var_may: t -> varinfo list
  val to_var_must: t -> varinfo list
  (** Strips the varinfo out of the address representation. *)
  val get_type: t -> typ
  (** Finds the type of the address location. *)

(*  val from_heap: heap -> t*)
(*  (** Creates an address from a heap representation. *) *)
(*  val from_offset_heap: (heap * (idx,field) offs) -> t*)
(*  (** Creates an address from a heap representation and offset. *) *)
end

module AddressSet (Idx: Lattice.S): 
sig
  include SetDomain.S with type elt = Lval.Lval(Idx).t 
  type idx = Idx.t
  type field = fieldinfo
  val null_ptr: unit -> t
  (* Creates a null pointer address*)
  val str_ptr: unit -> t
  (* Creates a string pointer address*)
  val from_var: varinfo -> t
  (** Creates an address from variable. *)  
  val from_var_offset: (varinfo * (field,idx) Lval.offs) -> t
  (** Creates an address from a variable and offset. *) 
  val to_var_may: t -> varinfo list
  val to_var_must: t -> varinfo list
  (** Strips the varinfo out of the address representation. *)
  val get_type: t -> typ
  (** Finds the type of the address location. *)
end
