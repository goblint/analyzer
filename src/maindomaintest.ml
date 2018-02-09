open QCheck

module DomainProperties (D: Lattice.S) =
struct
  (* Wrapper for Test.make: prepends domain name to required name *)
  let make ?count ?long_factor ?max_gen ?max_fail ?small ~name arb law =
    let full_name = D.name () ^ " " ^ name in
    Test.make ?count ?long_factor ?max_gen ?max_fail ?small ~name:full_name arb law

  let arb = D.arbitrary ()

  let leq_refl = make ~name:"leq refl" (arb) (fun x -> D.leq x x)
  let leq_trans = make ~name:"leq trans" (triple arb arb arb) (fun (a, b, c) -> (D.leq a b && D.leq b c) ==> D.leq a c)

  let tests = [
    leq_refl;
    leq_trans
  ]
end

module DP = DomainProperties (IntDomain.Flattened)
let testsuite = DP.tests

let () =
  QCheck_runner.run_tests_main ~argv:Sys.argv testsuite