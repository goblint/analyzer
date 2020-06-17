open Defaults (* CircInterval needs initialized conf *)

let domains: (module Lattice.S) list = [
  (module IntDomainProperties.IntegerSet); (* TODO: top properties error *)
  (module IntDomain.Lifted); (* not abstraction of IntegerSet *)
]

let intDomains: (module IntDomain.S) list = [
  (module IntDomain.Flattened);
  (module IntDomain.Interval32);
  (module IntDomain.Booleans);
  (module IntDomain.CircInterval);
  (module IntDomain.DefExc);
  (module IntDomain.Enums);
  (module IntDomain.IntDomTuple)
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