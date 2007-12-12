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

(** A simple worklist equation solver *)

module Make (Var: Hashtbl.HashedType) (Dom: Lattice.S) :
sig
  module HT : Hashtbl.S with type key = Var.t
  type 'a table    = 'a HT.t
  (** A hashtable mapping system variables to ['a] *)

  (** A few type synonyms to make it more readable: *)

  type variable    = Var.t
  (** The variables of constraint system *)
  type domain      = Dom.t
  (** The value domain of the variables *)

  type assignment  = variable -> domain
  (** Assignments from variables to their values *)
  type assignment' = domain table 
  (** data structure representation of an assignment *)
  type rhs         = assignment -> domain 
  (** Right hand sides of an equation in functional form. The rhs is a function
    * that given the current values of the variables will evaluate the
    * expression on the rhs. *)
  type lhs         = variable 
  (** The lhs of each equation is just a system variable *)
  type system     = lhs -> rhs
  (** A system of equations is just a mapping from each lhs to the corresponding
    * rhs *)
  type solution    = assignment'
  (** The solution to a system of constraints is an an assignment of variables
    * satisfying all constraints. We prefer a the data-structure representation
    * of the solution as a hashtable rather than an abstract mapping. *)

  (** And finally... *)

  val solve: system -> variable list -> solution
  (** [solve system xs] solves a [system] of equations in a demand-driven
    * fashion starting from the variables [xs] *)
end
