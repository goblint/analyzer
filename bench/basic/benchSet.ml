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


let map1 (f, x) = IS.map f x
let map2 (f, x) =
  let add_to_it e x = IS.add (f e) x in
  IS.fold add_to_it x IS.empty

let () =
  register (
    "map" @>>> [
      "inc" @> lazy (
        let args = ((fun x -> x + 1), set1) in
        throughputN 1 [
          ("map1", map1, args);
          ("map2", map2, args);
        ]
      );
      "flip" @> lazy (
        let args = ((fun x -> 2048 - x), set1) in
        throughputN 1 [
          ("map1", map1, args);
          ("map2", map2, args);
        ]
      );
      "const" @> lazy (
        let args = ((fun x -> 42), set1) in
        throughputN 1 [
          ("map1", map1, args);
          ("map2", map2, args);
        ]
      );
      "shuffle" @> lazy (
        let args = ((fun x -> (31 * x + 42) mod 37), set1) in
        throughputN 1 [
          ("map1", map1, args);
          ("map2", map2, args);
        ]
      );
    ]
  )


let () =
  run_global ()
