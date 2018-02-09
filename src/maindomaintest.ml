open QCheck

module D = IntDomain.Integers
let arb = D.arbitrary ()
let testsuite = [
  Test.make ~name:"leq refl" (arb) (fun x -> D.leq x x);
  Test.make ~name:"leq trans" (triple arb arb arb) (fun (a, b, c) -> (D.leq a b && D.leq b c) ==> D.leq a c)
]

let () =
  QCheck_runner.run_tests_main ~argv:Sys.argv testsuite