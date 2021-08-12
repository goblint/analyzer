(* dune exec bench/deriving/benchEq.exe -- -a *)

open Benchmark
open Benchmark.Tree


let () =
  let equal_manual_primitive ((x1: int), (y1: string)) ((x2: int), (y2: string)) = x1 = x2 && y1 = y2 in (* manual type annotations to be monomorphic like deriving *)
  let equal_deriving_primitive = [%eq: int * string] in

  let equal_manual_module ((x1: int), (y1: string)) ((x2: int), (y2: string)) = Int.equal x1 x2 && String.equal y1 y2 in
  let equal_deriving_module = [%eq: Int.t * String.t] in

  register (
    "pair" @>>> [
        "snd" @> lazy (
            let args = ((1, "foo"), (1, "bar")) in
            throughputN 1 [
              ("manual_primitive", Batteries.uncurry equal_manual_primitive, args);
              ("deriving_primitive", Batteries.uncurry equal_deriving_primitive, args);
              ("manual_module", Batteries.uncurry equal_manual_module, args);
              ("deriving_module", Batteries.uncurry equal_deriving_module, args);
            ]
          );
        "fst" @> lazy (
            let args = ((1, "foo"), (2, "bar")) in
            throughputN 1 [
              ("manual_primitive", Batteries.uncurry equal_manual_primitive, args);
              ("deriving_primitive", Batteries.uncurry equal_deriving_primitive, args);
              ("manual_module", Batteries.uncurry equal_manual_module, args);
              ("deriving_module", Batteries.uncurry equal_deriving_module, args);
            ]
          );
      ]
  )

let () =
  run_global ()
