open QCheck

module DomainTest (D: Printable.S) =
struct
  (* Shorthand for domain arbitrary *)
  let arb = try D.arbitrary () with
    | Failure(s) -> failwith (D.name () ^ ": " ^ s)
  (* let arb = D.arbitrary () *)

  (* Wrapper for Test.make: prepends domain name to required name *)
  let make ?count ?(long_factor=100) ?max_gen ?max_fail ?small ~name arb law =
    let full_name = D.name () ^ " " ^ name in
    Test.make ?count ~long_factor ?max_gen ?max_fail ?small ~name:full_name arb law
end

module type S =
sig
  val tests: Test.t list
end

module Leq (D: Lattice.S): S =
struct
  include DomainTest (D)

  let leq_refl = make ~name:"leq refl" (arb) (fun a -> D.leq a a)
  let leq_trans = make ~name:"leq trans" (triple arb arb arb) (fun (a, b, c) -> (D.leq a b && D.leq b c) ==> D.leq a c)
  let leq_antisym = make ~name:"leq antisym" (pair arb arb) (fun (a, b) -> (D.leq a b && D.leq b a) ==> D.equal a b)

  let tests = [
    leq_refl;
    leq_trans;
    leq_antisym
  ]
end

module Join (D: Lattice.S): S =
struct
  include DomainTest (D)

  let join_leq = make ~name:"join leq" (pair arb arb) (fun (a, b) -> let j = D.join a b in D.leq a j && D.leq b j)
  let join_assoc = make ~name:"join assoc" (triple arb arb arb) (fun (a, b, c) -> D.equal (D.join (D.join a b) c) (D.join a (D.join b c)))
  let join_comm = make ~name:"join comm" (pair arb arb) (fun (a, b) -> D.equal (D.join a b) (D.join b a))
  let join_idem = make ~name:"join idem" (arb) (fun a -> D.equal (D.join a a) a)
  let join_abs = make ~name:"join abs" (pair arb arb) (fun (a, b) -> D.equal (D.join a (D.meet a b)) a)

  let tests = [
    join_leq;
    join_assoc;
    join_comm;
    join_idem;
    join_abs
  ]
end

module Meet (D: Lattice.S): S =
struct
  include DomainTest (D)

  let meet_leq = make ~name:"meet leq" (pair arb arb) (fun (a, b) -> let m = D.meet a b in D.leq m a && D.leq m b)
  let meet_assoc = make ~name:"meet assoc" (triple arb arb arb) (fun (a, b, c) -> D.equal (D.meet (D.meet a b) c) (D.meet a (D.meet b c)))
  let meet_comm = make ~name:"meet comm" (pair arb arb) (fun (a, b) -> D.equal (D.meet a b) (D.meet b a))
  let meet_idem = make ~name:"meet idem" (arb) (fun a -> D.equal (D.meet a a) a)
  let meet_abs = make ~name:"meet abs" (pair arb arb) (fun (a, b) -> D.equal (D.meet a (D.join a b)) a)

  let tests = [
    meet_leq;
    meet_assoc;
    meet_comm;
    meet_idem;
    meet_abs
  ]
end

module Bot (D: Lattice.S): S =
struct
  include DomainTest (D)

  let bot_leq = make ~name:"bot leq" (arb) (fun a -> D.leq (D.bot ()) a)
  let bot_is_bot = make ~name:"bot is_bot" (arb) (fun a -> D.is_bot a = (D.equal a (D.bot ())))
  let bot_join = make ~name:"bot join" (arb) (fun a -> D.equal (D.join a (D.bot ())) a)

  let tests = [
    bot_leq;
    bot_is_bot;
    bot_join
  ]
end

module Top (D: Lattice.S): S =
struct
  include DomainTest (D)

  let top_leq = make ~name:"top leq" (arb) (fun a -> D.leq a (D.top ()))
  let top_is_top = make ~name:"top is_top" (arb) (fun a -> D.is_top a = (D.equal a (D.top ())))
  let top_meet = make ~name:"top meet" (arb) (fun a -> D.equal (D.meet a (D.top ())) a)

  let tests = [
    top_leq;
    top_is_top;
    top_meet
  ]
end

module Connect (D: Lattice.S): S =
struct
  include DomainTest (D)

  let connect_join = make ~name:"connect join" (pair arb arb) (fun (a, b) -> D.leq a b = (D.equal (D.join a b) b))
  let connect_meet = make ~name:"connect meet" (pair arb arb) (fun (a, b) -> D.leq a b = (D.equal (D.meet a b) a))

  let tests = [
    connect_join;
    connect_meet
  ]
end

module All (D: Lattice.S): S =
struct
  module L = Leq (D)
  module J = Join (D)
  module M = Meet (D)
  module B = Bot (D)
  module T = Top (D)
  module C = Connect (D)

  let tests = L.tests @ J.tests @ M.tests @ B.tests @ T.tests @ C.tests
end