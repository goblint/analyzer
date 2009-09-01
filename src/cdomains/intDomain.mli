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

(** Abstract Domains for integers. These are domains that support the C
  * operations on integer values. *)

module type S =
sig
  include Lattice.S

  (** {b Accessing values of the ADT} *)

  val to_int: t -> int64 option
  (** Return a single integer value if the value is a known constant, otherwise
    * don't return anything. *)
  val of_int: int64 -> t
  (** Transform an integer literal to your internal domain representation. *)
  val is_int: t -> bool
  (** Checks if the element is a definite integer value. If this function
    * returns [true], the above [to_int] should return a real value. *)

  val to_bool: t -> bool option
  (** Give a boolean interpretation of an abstract value if possible, otherwise
    * don't return anything.*)
  val of_bool: bool -> t
  (** Transform a known boolean value to the default internal representation. It
    * should follow C: [of_bool true = of_int 1] and [of_bool false = of_int 0]. *)
  val is_bool: t -> bool
  (** Checks if the element is a definite boolean value. If this function
    * returns [true], the above [to_bool] should return a real value. *)
  val to_excl_list: t -> int64 list option
  (* Gives a list representation of the excluded values if possible. *)
  val of_excl_list: int64 list -> t
  (* Creates a exclusion set from a given list of integers. *)
  val is_excl_list: t -> bool
  (* Checks if the element is an exclusion set. *)
(*  val of_interval: int64 -> int64 -> t*)
  val starting   : int64 -> t
  val ending     : int64 -> t
  val maximal    : t -> int64 option
  val minimal    : t -> int64 option
  
  (** {b Arithmetic operators} *)

  val neg: t -> t
  (** Negating an integer value: [-x] *)
  val add: t -> t -> t
  (** Addition: [x + y] *)
  val sub: t -> t -> t
  (** Subtraction: [x - y] *)
  val mul: t -> t -> t
  (** Multiplication: [x * y] *)
  val div: t -> t -> t
  (** Division: [x / y] *)
  val rem: t -> t -> t
  (** Integer remainder: [x % y] *) 
                       
  (** {b Comparison operators} *)

  val lt: t -> t -> t
  (** Less than: [x < y] *)
  val gt: t -> t -> t
  (** Greater than: [x > y] *)
  val le: t -> t -> t
  (** Less than or equal: [x <= y] *)
  val ge: t -> t -> t
  (** Greater than or equal: [x >= y] *)
  val eq: t -> t -> t
  (** Equal to: [x == y] *)
  val ne: t -> t -> t
  (** Not equal to: [x != y] *)

  (** {b Bit operators} *)

  val bitnot: t -> t
  (** Bitwise not (one's complement): [~x] *)
  val bitand: t -> t -> t
  (** Bitwise and: [x & y] *)
  val bitor : t -> t -> t
  (** Bitwise or: [x | y] *)
  val bitxor: t -> t -> t
  (** Bitwise exclusive or: [x ^ y] *)
  val shift_left : t -> t -> t
  (** Shifting bits left: [x << y] *)
  val shift_right: t -> t -> t
  (** Shifting bits right: [x >> y] *)

  (** {b Logical operators} *)

  val lognot: t -> t
  (** Logical not: [!x] *)
  val logand: t -> t -> t
  (** Logical and: [x && y] *)
  val logor : t -> t -> t
  (** Logical or: [x || y] *)
end
(** The signature of integral value domains. They need to support all integer
  * operations that are allowed in C *)


exception Unknown
(** An exception that can be raised when the result of a computation is unknown.
  * This is caught by lifted domains and will be replaced by top. *)

exception Error
(** An exception that can be raised when an arithmetic error occurs. This is
  * caught by lifted domains and the evaluation will then be set to bot, which
  * signifies an error in computation *)

(** {b Predefined domains} *)

module Integers : S with type t = int64
(** The integers with their natural orderings. Calling [top] and [bot] will
  * raise exceptions. *)

module Flattened : S with type t = [`Top | `Lifted of int64 | `Bot]
(** This is the typical flattened integer domain used in Kildall's constant
  * propagation. *)

module Trier 
: S with type t = [
    | `Excluded of SetDomain.Make(Integers).t
    | `Definite of Integers.t
    | `Bot
    ]
(** The Trier domain. The Flattened integer domain is topped by exclusion sets.
  * Good for analysing branches. *)


(** {b Domain constructors} *)

module Flat (Base: S): S
(** Creates a flat value domain, where all ordering is lost. Arithmetic
  * operations are lifted such that only lifted values can be evaluated
  * otherwise the top/bot is simply propagated with bot taking precedence over
  * top. *)

module Lift (Base: S): S
(** Just like {!Value.Flat} except the order is preserved. *)

module Interval : S
(** Interval domain with int64-s --- use with caution! *)

module IncExcInterval : S with type t = [ | `Excluded of Interval.t| `Included of Interval.t ] 
(** Inclusive and exclusive intervals. Warning: NOT A LATTICE *)

module ManyInts : S 

(** {b Boolean domains} *)

module type BooleansNames = 
sig
  val truename: string (** The name of the [true] abstract value *)
  val falsename: string (** The name of the [false] abstract value *)
end
(** Parameter signature for the [MakeBooleans] functor. *)

module MakeBooleans (Names: BooleansNames): S with type t = bool
(** Creates an abstract domain for integers represented by boolean values. *)

module Booleans: S with type t = bool
(** Boolean abstract domain, where true is output "True" and false is output
  * "False" *)

module None: S with type t = unit
(** Domain with nothing in it. *)
