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

(** Control Flow Graphs. *)
open Cil

type node = 
  | Statement of stmt  
  (** The statements as identified by CIL *)
  | Function of varinfo  
  (** The variable information associated with the function declaration. *)
(** A node in the Control Flow Graph is either a statement or function. Think of
  * the function node as last node that all the returning nodes point to.  So
  * the result of the function call is contained in the fucntion node. *)

module Node : Hashtbl.HashedType with type t = node
(** The HashedType module for nodes *)

type edge = 
  | Assign of lval * exp  
  (** Assignments lval = exp *)
  | Proc of lval option * exp * exp list 
  (** Function calls of the form lva = fexp (e1, e2, ...) *)
  | Entry of fundec 
  (** Entry edge that relates function declaration to function body. You can use 
    * this to initialize the local variables. *)
  | Ret of exp option * fundec
  (** Return edge is between the return statement, which may optionally contain
    * a return value, and the function. The result of the call is then
    * transfered to the function node! *)
  | Test of exp * bool 
  (** The true-branch or false-branch of a conditional exp *) 
  | Skip 
  (** This is here for historical reasons. I never use Skip edges! *)

type cfg = node -> (edge * node) list
(** The control flow graph is a function that for each node returns the set of
  * edges entering the node and the node each edge originated from. This will
  * only work for forward analyses! *)

val getCFG: file -> cfg
(** Returns the cfg of the given AST. Note that if the variable
  * {!Goblinutil.cfg_print} is set (by the flag "--cfg"), this function will
  * also write to the file cfg.dot *)

val getGlobalInits: file -> (edge * location) list
(** Returns a list of globals and their initializer expressions as [Assign]
  * edges.  We rely on {!Cil.foldLeftCompoundAll} to initialize complicated
  * structures and handle missing initializers.Hopefully, the user doesn't need
  * to worry about initializing globals. *)

val getLoc: node -> location
(** Returns the source location of the given node. For a function, this is the
  * point were the function is defined. *)

val getFun: node -> fundec
(** Returns the function that the given node belongs to. *)

val initfun: fundec
(** All the global initializers are put into this function, so that the assign
  * transfer function will take care of initializers in a uniform way... very
  * nice! *)
