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

(** Abstract domains for mappings. *)
open Pretty

module type S =
sig
  include Lattice.S
  type key (** The type of the map keys. *)
  type value (** The type of the values. *)

  val add: key -> value -> t -> t
  val remove: key -> t -> t
  val find: key -> t -> value
  val mem: key -> t -> bool
  val iter: (key -> value -> unit) -> t -> unit
  val map: (value -> value) -> t -> t
  val fold: (key -> value -> 'a -> 'a) -> t -> 'a -> 'a

  val add_list: (key * value) list -> t -> t
  (** Adds a list of key-value pairs to the mapping.*)
  val add_list_set: key list -> value -> t -> t
  (** Sets all the keys to the given value. Useful for setting many variables to
    * top or bot. *)
  val add_list_fun: key list -> (key -> value) -> t -> t
  (** Adds a list of key-value pairs to the mapping according to a given
    * function. *)
  val filter_class: int -> t -> t
  (** Returns a map with only the mappings from the keys of the given class
    * according to the {!Groupable.classsify} function. *)

  val for_all: (key -> value -> bool) -> t -> bool
  val map2: (value -> value -> value) -> t -> t -> t
  val long_map2: (value -> value -> value) -> t -> t -> t
end
(** Signature for Map Domains. *)

module type Groupable = 
sig
  include Printable.S
  val classify: t -> int
  val class_name: int -> string
end
(** Argument signature for the mapping functor that should enable the grouping
  * of the output. *)

module MapBot (Domain: Groupable) (Range: Lattice.S): S with
  type key = Domain.t and type value = Range.t
(** Creates a mapping with [bot] as the default value. *)

module MapTop (Domain: Groupable) (Range: Lattice.S): S with
  type key = Domain.t and type value = Range.t
(** Creates a mapping with [top] as the default value. *)
