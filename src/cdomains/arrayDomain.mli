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

(** Abstract domains representing arrays. *)

open Pretty
module type S = 
sig
  include Lattice.S
  type idx (** The abstract domain used to index on arrays. *)
  type value (** The abstract domain of values stored in the array. *)

  val get: t -> idx -> value
  (** Returns the element residing at the given index. *)
  val set: t -> idx -> value -> t
  (** Returns a new abstract value, where the given index is replaced with the
    * given element. *)
  val make: int -> value -> t
  (** [make l e] creates an abstract representation of an array of length [l]
    * containing the element [e]. *)
  val length: t -> int option
  (** returns length of array if known *)
end

module Trivial (Val: Lattice.S) (Idx: Lattice.S): S with type value = Val.t and type idx = Idx.t
(** This functor creates a trivial single cell representation of an array. The
  * indexing type is taken as a parameter to satisfy the type system, it is not
  * used in the implementation. *)


module NativeArray (Base: Lattice.S) (Idx: IntDomain.S): S with type value = Base.t and type idx = Idx.t 
(** Stores values in a real array *)

module NativeArrayEx (Base: Lattice.S) (Idx: IntDomain.S): S with type value = Base.t and type idx = Idx.t 
(** Stores values in a real array & has top and bot *)
  
module Collapsing (Base: Lattice.S) (Idx: IntDomain.S): S with type value = Base.t and type idx = Idx.t 
(** Small arrays are reperesented as real arrays, but large as a single cell *)

module MapArray (I: sig val n: int option end) (Base: Lattice.S) (Idx: IntDomain.S) : S with type value = Base.t and type idx = Idx.t 
(** Arrays as maps -- stores definite values in a map. First argument is nr. of items
    to pre-allocate. Does not store array's length. Gives top on non-int indeces *)

module PreciseMapArrayDomain 
  (I:sig val n : int option end) (Base:Lattice.S) (Idx:IntDomain.S) 
  : S with type value = Base.t and type idx = Idx.t 
(** Arrays as maps --  keeps at most I.n items in map plus 
    a magic rest (top) indexed element *)

module LooseMapArrayDomain 
  (I:sig val n : int option end) (Base:Lattice.S) (Idx:IntDomain.S) 
  : S with type value = Base.t and type idx = Idx.t 
(** Arrays as maps --  keeps at some I.n items in map plus 
    a magic rest (top) indexed element *)


