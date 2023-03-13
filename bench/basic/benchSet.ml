(* dune exec bench/basic/benchSet.exe -- -a *)

open Benchmark
open Benchmark.Tree

module IS = Set.Make (Int)

let set1 = IS.of_seq (Seq.init 1024 (fun i -> i))
let set2 = IS.of_seq (Seq.init 1024 (fun i -> i + 1))
let set3 = IS.of_seq (Seq.init 1024 (fun i -> i - 1))

let equal1 (x, y) = IS.equal x y
let equal2 (x, y) = IS.for_all (fun i -> IS.exists (Int.equal i) y) x

let equal1' (x, y) = IS.cardinal x = IS.cardinal y && IS.equal x y
let equal2' (x, y) = IS.cardinal x = IS.cardinal y && IS.for_all (fun i -> IS.exists (Int.equal i) y) x


let () =
  register (
    "equal" @>>> [
      "1-1" @> lazy (
        let args = (set1, set1) in
        throughputN 1 [
          ("equal1", equal1, args);
          ("equal2", equal2, args);
          ("equal1'", equal1', args);
          ("equal2'", equal2', args);
        ]
      );
      "1-2" @> lazy (
        let args = (set1, set2) in
        throughputN 1 [
          ("equal1", equal1, args);
          ("equal2", equal2, args);
          ("equal1'", equal1', args);
          ("equal2'", equal2', args);
        ]
      );
      "1-3" @> lazy (
        let args = (set1, set3) in
        throughputN 1 [
          ("equal1", equal1, args);
          ("equal2", equal2, args);
          ("equal1'", equal1', args);
          ("equal2'", equal2', args);
        ]
      );
    ]
  )

let () =
  run_global ()
