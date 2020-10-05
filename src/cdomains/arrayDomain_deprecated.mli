(* open ArrayDomain

module NativeArray (Base: Lattice.S) (Idx: IntDomain.S): S with type value = Base.t and type idx = Idx.t
(** Stores values in a real array *)

module NativeArrayEx (Base: Lattice.S) (Idx: IntDomain.S): S with type value = Base.t and type idx = Idx.t
(** Stores values in a real array & has top and bot *)

module Collapsing (Base: Lattice.S) (Idx: IntDomain.S): S with type value = Base.t and type idx = Idx.t
(** Small arrays are reperesented as real arrays, but large as a single cell *)

module MapArray (I: sig val n: int option end) (Base: Lattice.S) (Idx: IntDomain.S) : S with type value = Base.t and type idx = Idx.t
(** Arrays as maps -- stores definite values in a map. First argument is nr. of items
    to pre-allocate. Does not store array's length. Gives top on non-int indices *)

module PreciseMapArrayDomain
  (I:sig val n : int option end) (Base:Lattice.S) (Idx:IntDomain.S)
  : S with type value = Base.t and type idx = Idx.t
(** Arrays as maps --  keeps at most I.n items in map plus
    a magic rest (top) indexed element *)

module LooseMapArrayDomain
  (I:sig val n : int option end) (Base:Lattice.S) (Idx:IntDomain.S)
  : S with type value = Base.t and type idx = Idx.t
(** Arrays as maps --  keeps at some I.n items in map plus
    a magic rest (top) indexed element *) *)
