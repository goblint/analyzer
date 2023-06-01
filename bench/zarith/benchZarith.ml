(** Benchmark calculation of powers of 2 with Zarith.
    Originally for https://github.com/goblint/cil/pull/115.

    dune exec bench/zarith/benchZarith.exe -- -a *)

open Benchmark
open Benchmark.Tree


let () =
  let pow2_pow n = Z.pow (Z.of_int 2) n in
  let pow2_lsl n = Z.shift_left Z.one n in


  register (
    "pow2" @>>> [
      "8" @> lazy (
        let arg = 8 in
        throughputN 1 [
          ("pow", pow2_pow, arg);
          ("lsl", pow2_lsl, arg);
        ]
      );
      "16" @> lazy (
        let arg = 16 in
        throughputN 1 [
          ("pow", pow2_pow, arg);
          ("lsl", pow2_lsl, arg);
        ]
      );
      "32" @> lazy (
        let arg = 32 in
        throughputN 1 [
          ("pow", pow2_pow, arg);
          ("lsl", pow2_lsl, arg);
        ]
      );
      "64" @> lazy (
        let arg = 64 in
        throughputN 1 [
          ("pow", pow2_pow, arg);
          ("lsl", pow2_lsl, arg);
        ]
      );
    ]
  )


let () =
  run_global ()
