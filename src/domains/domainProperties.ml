open QCheck

module DomainTest (D: Printable.S) =
struct
  (* Shorthand for domain arbitrary *)
  let arb = D.arbitrary ()

  (* Wrapper for Test.make: prepends domain name to required name *)
  let make ?count ?long_factor ?max_gen ?max_fail ?small ~name arb law =
    let full_name = D.name () ^ " " ^ name in
    Test.make ?count ?long_factor ?max_gen ?max_fail ?small ~name:full_name arb law
end

module type S =
sig
  val tests: Test.t list
end

module Leq (D: Lattice.S): S =
struct
  include DomainTest (D)

  let leq_refl = make ~name:"leq refl" (arb) (fun x -> D.leq x x)
  let leq_trans = make ~name:"leq trans" (triple arb arb arb) (fun (a, b, c) -> (D.leq a b && D.leq b c) ==> D.leq a c)

  let tests = [
    leq_refl;
    leq_trans
  ]
end

module Join (D: Lattice.S): S =
struct
  include DomainTest (D)

  let join_leq = make ~name:"join leq" (pair arb arb) (fun (x, y) -> let j = D.join x y in D.leq x j && D.leq y j)
  let join_comm = make ~name:"join comm" (pair arb arb) (fun (x, y) -> D.equal (D.join x y) (D.join y x))

  let tests = [
    join_leq;
    join_comm
  ]
end

module All (D: Lattice.S): S =
struct
  module L = Leq (D)
  module J = Join (D)

  let tests = L.tests @ J.tests
end