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

(** Some functions to deal with globals and dependencies. *)
module type Deps =
sig
  module Dom : Lattice.S
  (** analysis state domain (-- used for binding only)*)
    
  module Var : Analyses.VarType
  (** module of local & global variables *)
    
  val get_changed_globals : Dom.t -> Dom.t -> Var.t list
    (** [get_globals x y] returns list of (global) variables that have changed *)
  val filter_globals      : Dom.t -> Dom.t
    (** [filter_globals x] removes all non-global info from [x]. 
     * The usual pattern should be [insert_globals x (func (filter_globals x))]. *)
  val insert_globals      : Dom.t -> Dom.t -> Dom.t
    (** [insert_globals st glob] return a state where globals are taken from [glob] and
     * all other data from [st] *)
  val reset_global_dep    : Dom.t -> Dom.t
    (** [reset_global_dep x] clears global dependency list from [x]*)
  val get_global_dep      : Dom.t -> Var.t list
    (** [get_global_dep x] returns list of (global) dependencies thet have gathered in calculating the state. *)
end
    
(** A very fast demand-driven constraint solver *)
module Make (Dom : Lattice.S) (Deps : Deps with module Dom = Dom) :
sig
  module HT : Hash.S with type key = Deps.Var.t
  type 'a table    = 'a HT.t
  (** A hashtable mapping system variables to ['a] *)

  (** A few type synonyms to make it more readable: *)
  type variable    = Deps.Var.t
  (** The variables of constraint system *)
  type domain      = Dom.t
  (** The value domain of the variables *)
  type forks       = variable list
  (** Forks to be evaluated in parallel *)

  type assignment  = variable -> domain
  (** Assignments from variables to their values *)
  type assignment' = domain table 
  (** data structure representation of an assignment *)
  type rhs         = assignment -> domain * forks
  (** RHS of the constraint in functional form. The rhs is a function that given
    * the current values of the variables will evaluate the rhs expression *)
  type lhs         = variable 
  (** The lhs of each constraint is just a system variable *)
  type system      = lhs -> rhs list 
  (** A system is just a map from variables to the list of rh-sides constraining
    * each variable. *)

  type solution    = assignment'
  (** The solution to a system of constraints is an an assignment of variables
    * satisfying all constraints. We prefer a the data-structure representation
    * of the solution as a hashtable rather than an abstract mapping. *)

  (** And finally... *)
                       
  val solve: system -> variable list -> solution 
  (** [solve system xs] solves a constraint [system] in a demand-driven fashion
    * starting from the variables [xs], also returning global state *)
end 
