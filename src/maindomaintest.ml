(* open Defaults (* CircInterval needs initialized conf *) *)

module ArbitraryLattice =
struct
  module E =
  struct
    type t = char [@@deriving to_yojson]
    let short _ x = String.make 1 x
    module P =
    struct
      type t' = t
      let name () = "ArbitraryLattice element"
      let short = short
    end
    include Printable.Std
    include Printable.PrintSimple (P)

    let hash = Char.code
    let equal = Char.equal
    let arbitrary () = QCheck.oneofl ['a'; 'b'; 'c'; 'd']
  end

  include SetDomain.Make (E)

  let top () = of_list ['a'; 'b'; 'c'; 'd']
  let is_top x = equal x (top ())
end

let domains: (module Lattice.S) list = [
  (* (module IntDomainProperties.IntegerSet); (* TODO: top properties error *) *)
  (module IntDomain.Lifted); (* not abstraction of IntegerSet *)

  (* TODO: move to intDomains if passing *)
  (module IntDomain.Interval32);
  (module IntDomain.Booleans);

  (* TODO: fix *)
  (* (module IntDomain.CircInterval); *)
  (* (module IntDomain.DefExc); *)
  (* (module IntDomain.Enums); *)
  (* (module IntDomain.IntDomTuple); *)

  (module ArbitraryLattice);
  (module SetDomain.Hoare (ArbitraryLattice) (struct let topname = "Top" end));
]

let intDomains: (module IntDomain.S) list = [
  (module IntDomain.Flattened);
  (* (module IntDomain.Interval32); *)
  (* (module IntDomain.Booleans); *)
  (* (module IntDomain.CircInterval); *)
  (* (module IntDomain.DefExc); *)
  (* (module IntDomain.Enums); *)
  (* (module IntDomain.IntDomTuple); *)
]

let testsuite =
  List.map (fun d ->
      let module D = (val d: Lattice.S) in
      let module DP = DomainProperties.All (D) in
      DP.tests)
    domains
  |> List.flatten
let intTestsuite =
  List.map (fun d ->
      let module D = (val d: IntDomain.S) in
      let module DP = IntDomainProperties.All (D) in
      DP.tests)
    intDomains
  |> List.flatten

let () =
  QCheck_runner.run_tests_main ~argv:Sys.argv (testsuite @ intTestsuite)