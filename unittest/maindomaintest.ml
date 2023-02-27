open Goblint_lib
(* open! Defaults (* Enums / ... need initialized conf *) *)
open GoblintCil

module PrintableChar =
struct
  include Printable.Std
  type t = char [@@deriving eq, ord, hash, to_yojson]
  let name () = "char"
  let show x = String.make 1 x

  module P =
  struct
    type nonrec t = t
    let show = show
  end
  include Printable.SimpleShow (P)
end

module ArbitraryLattice = SetDomain.FiniteSet (PrintableChar) (
  struct
    type t = char
    let elems = ['a'; 'b'; 'c'; 'd']
  end
  )

module HoareArbitrary = HoareDomain.Set_LiftTop (ArbitraryLattice) (struct let topname = "Top" end)
module HoareArbitrary_NoTop = HoareDomain.Set (ArbitraryLattice)

let domains: (module Lattice.S) list = [
  (* (module IntDomainProperties.IntegerSet); (* TODO: top properties error *) *)
  (module IntDomain.Lifted); (* not abstraction of IntegerSet *)

  (* TODO: move to intDomains if passing *)
  (module IntDomain.Booleans);

  (* TODO: fix *)
  (* (module IntDomain.Enums); *)
  (* (module IntDomain.IntDomTuple); *)

  (module ArbitraryLattice);
  (module HoareArbitrary);
  (module HoareArbitrary_NoTop);
  (module HoareDomain.MapBot[@alert "-deprecated"] (ArbitraryLattice) (HoareArbitrary));
  (module HoareDomain.MapBot[@alert "-deprecated"] (ArbitraryLattice) (HoareArbitrary_NoTop));
]

let nonAssocDomains: (module Lattice.S) list = []

let intDomains: (module IntDomainProperties.S) list = [
  (module IntDomain.Interval);
  (module IntDomain.Enums);
  (module IntDomain.Congruence);
  (* (module IntDomain.Flattened); *)
  (* (module IntDomain.Interval32); *)
  (* (module IntDomain.Booleans); *)
  (* (module IntDomain.IntDomTuple); *)
]

let nonAssocIntDomains: (module IntDomainProperties.S) list = [
  (module IntDomain.DefExc);
]

(* TODO: make arbitrary ikind part of domain test for better efficiency *)
let ikinds: Cil.ikind list = [
  IChar;
  ISChar;
  IUChar;
  IBool;
  IInt;
  IUInt;
  IShort;
  IUShort;
  ILong;
  IULong;
  ILongLong;
  IULongLong;
]

let testsuite =
  domains
  |> List.concat_map (fun d ->
      let module D = (val d: Lattice.S) in
      let module DP = DomainProperties.All (D) in
      DP.tests
    )
let nonAssocTestsuite =
  nonAssocDomains
  |> List.concat_map (fun d ->
      let module D = (val d: Lattice.S) in
      let module DP = DomainProperties.AllNonAssoc (D) in
      DP.tests
    )

let old_intdomains intDomains =
  BatList.cartesian_product intDomains ikinds
  |> List.map (fun (d, ik) ->
      let module D = (val d: IntDomainProperties.S) in
      let module Ikind = struct let ikind () = ik end in
      (module IntDomainProperties.WithIkind (D) (Ikind): IntDomainProperties.OldSWithIkind)
    )
let intTestsuite =
  old_intdomains intDomains
  |> List.concat_map (fun d ->
      let module D = (val d: IntDomainProperties.OldSWithIkind) in
      let module DP = IntDomainProperties.All (D) in
      DP.tests
    )
let nonAssocIntTestsuite =
  old_intdomains nonAssocIntDomains
  |> List.concat_map (fun d ->
      let module D = (val d: IntDomainProperties.OldSWithIkind) in
      let module DP = IntDomainProperties.AllNonAssoc (D) in
      DP.tests
    )

let all_testsuite = testsuite @ nonAssocTestsuite @ intTestsuite @ nonAssocIntTestsuite
