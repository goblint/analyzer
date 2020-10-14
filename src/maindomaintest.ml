open! Defaults (* CircInterval / Enums / ... need initialized conf *)

module type FiniteSetElems =
sig
  type t
  val elems: t list
end

module FiniteSet (E:Printable.S) (Elems:FiniteSetElems with type t = E.t) =
struct
  module E =
  struct
    include E
    let arbitrary () = QCheck.oneofl Elems.elems
  end

  include SetDomain.Make (E)
  let top () = of_list Elems.elems
  let is_top x = equal x (top ())
end

module PrintableChar =
struct
  type t = char [@@deriving to_yojson]
  let name () = "char"
  let short _ x = String.make 1 x

  module P =
  struct
    type t' = t
    let name = name
    let short = short
  end
  include Printable.Std
  include Printable.PrintSimple (P)

  let hash = Char.code
  let equal = Char.equal
end

module ArbitraryLattice = FiniteSet (PrintableChar) (
  struct
    type t = char
    let elems = ['a'; 'b'; 'c'; 'd']
  end
)

let domains: (module Lattice.S) list = [
  (* (module IntDomainProperties.IntegerSet); (* TODO: top properties error *) *)
  (module IntDomain.Lifted); (* not abstraction of IntegerSet *)

  (* TODO: move to intDomains if passing *)
  (module IntDomain.Booleans);

  (* TODO: fix *)
  (* (module IntDomain.CircInterval); *)
  (* (module IntDomain.Enums); *)
  (* (module IntDomain.IntDomTuple); *)

  (module ArbitraryLattice);
  (module SetDomain.Hoare (ArbitraryLattice) (struct let topname = "Top" end));
]

let nonAssocDomains: (module Lattice.S) list = []

let intDomains: (module IntDomain.S) list = [
  (module IntDomain.Flattened);
  (module IntDomain.Interval32);
  (* (module IntDomain.Booleans); *)
  (* (module IntDomain.CircInterval); *)
  (* (module IntDomain.Enums); *)
  (* (module IntDomain.IntDomTuple); *)
]

let nonAssocIntDomains: (module IntDomain.S) list = [
  (module IntDomain.DefExc)
]

let testsuite =
  List.map (fun d ->
      let module D = (val d: Lattice.S) in
      let module DP = DomainProperties.All (D) in
      DP.tests)
    domains
  |> List.flatten
let nonAssocTestsuite =
  List.map (fun d ->
      let module D = (val d: Lattice.S) in
      let module DP = DomainProperties.AllNonAssoc (D) in
      DP.tests)
    nonAssocDomains
  |> List.flatten
let intTestsuite =
  List.map (fun d ->
      let module D = (val d: IntDomain.S) in
      let module DP = IntDomainProperties.All (D) in
      DP.tests)
    intDomains
  |> List.flatten
let nonAssocIntTestsuite =
  List.map (fun d ->
      let module D = (val d: IntDomain.S) in
      let module DP = IntDomainProperties.AllNonAssoc (D) in
      DP.tests)
    nonAssocIntDomains
  |> List.flatten
let () =
  QCheck_runner.run_tests_main ~argv:Sys.argv (testsuite @ nonAssocTestsuite @ intTestsuite @ nonAssocIntTestsuite)
