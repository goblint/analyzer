open QCheck

module DomainProperties (D: Lattice.S) =
struct
  let arb = D.arbitrary ()

  let leq_refl = Test.make ~name:"leq refl" (arb) (fun x -> D.leq x x)
  let leq_trans = Test.make ~name:"leq trans" (triple arb arb arb) (fun (a, b, c) -> (D.leq a b && D.leq b c) ==> D.leq a c)

  let tests = [
    leq_refl;
    leq_trans
  ]
end

module DP = DomainProperties (IntDomain.Flattened)
let testsuite = DP.tests

let () =
  QCheck_runner.run_tests_main ~argv:Sys.argv testsuite