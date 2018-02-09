open QCheck

module DomainTest (D: Printable.S) =
struct
  (* Wrapper for Test.make: prepends domain name to required name *)
  let make ?count ?long_factor ?max_gen ?max_fail ?small ~name arb law =
    let full_name = D.name () ^ " " ^ name in
    Test.make ?count ?long_factor ?max_gen ?max_fail ?small ~name:full_name arb law
end

module DomainProperties (D: Lattice.S) =
struct
  include DomainTest (D)

  let arb = D.arbitrary ()

  let leq_refl = make ~name:"leq refl" (arb) (fun x -> D.leq x x)
  let leq_trans = make ~name:"leq trans" (triple arb arb arb) (fun (a, b, c) -> (D.leq a b && D.leq b c) ==> D.leq a c)

  let tests = [
    leq_refl;
    leq_trans
  ]
end

let domains: (module Lattice.S) list = [
  (module IntDomain.Integers);
  (module IntDomain.Flattened)
]

let testsuite =
  List.map (fun d ->
      let module D = (val d: Lattice.S) in
      let module DP = DomainProperties (D) in
      DP.tests)
    domains
  |> List.flatten

let () =
  QCheck_runner.run_tests_main ~argv:Sys.argv testsuite