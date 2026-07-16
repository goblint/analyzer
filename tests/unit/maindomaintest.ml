open Goblint_lib
(* open! Defaults (* Enums / ... need initialized conf *) *)
open GoblintCil

module PrintableChar =
struct
  include Printable.StdLeaf
  type t = char [@@deriving eq, ord, hash, to_yojson]
  let name () = "char"
  let show x = String.make 1 x

  module P =
  struct
    type nonrec t = t
    let show = show
  end
  include Printable.SimpleShow (P)

  let elems = ['a'; 'b'; 'c'; 'd']
end

module ArbitraryLattice = SetDomain.FiniteSet (PrintableChar)

module HoareArbitrary = HoareDomain.Set_LiftTop (ArbitraryLattice) (struct let topname = "Top" end)
module HoareArbitrary_NoTop = HoareDomain.Set (ArbitraryLattice)

let domains: (module Lattice.S) list = [
  (* (module IntDomainProperties.IntegerSet); (* TODO: top properties error *) *)
  (module IntDomain.Lifted); (* not abstraction of IntegerSet *)

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

let intDomains: (module IntDomainProperties.S2) list = [
  (module IntDomain.SOverflowUnlifter(IntDomain.Interval));
  (module IntDomainProperties.MakeS2 (IntDomain.Enums));
  (module IntDomainProperties.MakeS2 (IntDomain.Congruence));
  (module IntDomain.SOverflowUnlifter(IntDomain.IntervalSet));
  (module IntDomain.SOverflowUnlifter(IntDomain.Bitfield));
  (* (module IntDomain.Flattened); *)
  (* (module IntDomain.Booleans); *)
  (* (module IntDomain.IntDomTuple); *)
]

let nonAssocIntDomains: (module IntDomainProperties.S2) list = [
  (module IntDomainProperties.MakeS2 (IntDomain.DefExc));
]

(* TODO: make arbitrary ikind part of domain test for better efficiency *)
let ikinds: Cil.ikind list = [
  IChar;
  ISChar;
  IUChar;
  (* IBool; *) (* see https://github.com/goblint/analyzer/pull/1111 *)
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
  GobList.cartesian_map (fun d ik ->
      let module D = (val d: IntDomainProperties.S2) in
      let module Ikind = struct let ikind () = ik end in
      (module IntDomainProperties.WithIkind (D) (Ikind): IntDomainProperties.OldSWithIkind)
    ) intDomains ikinds
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
