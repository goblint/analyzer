(* ocamlbuild -pkg benchmark equality_bench.native && ./equality_bench.native *)

open Benchmark

(* Q: does the compiler actually try == first for = or is it a good idea to always do `a == b || a = b` instead of `a = b`? *)

let xs = [1;2;3;4;5]

let f1 x = x = xs
let f2 x = x == xs
let f3 x = x == xs || x = xs

let () =
  let res = throughputN ~repeat:3 5 ["=", f1, xs;
                                     "==", f2, xs;
                                     "== || =", f3, xs] in
  print_newline();
  tabulate res

(* $ ocamlbuild -pkg benchmark equality_bench.native && ./equality_bench.native
Throughputs for "=", "==", "== || =" each running 3 times for at least 5 CPU seconds:
      =:  5.25 WALL ( 5.24 usr +  0.01 sys =  5.24 CPU) @ 18748174.76/s (n=98296624)
          5.24 WALL ( 5.23 usr +  0.01 sys =  5.24 CPU) @ 18774200.12/s (n=98296624)
          5.25 WALL ( 5.24 usr +  0.01 sys =  5.24 CPU) @ 18745439.63/s (n=98296624)
     ==: (Estimated time for each run: 2m 10s)
          5.15 WALL ( 5.26 usr +  0.01 sys =  5.27 CPU) @ 4301970183.11/s (n=22661423804)
          5.04 WALL ( 5.03 usr +  0.01 sys =  5.04 CPU) @ 4025109409.74/s (n=20294899502)
          5.06 WALL ( 5.05 usr +  0.00 sys =  5.06 CPU) @ 3907251381.76/s (n=19758028590)
== || =:  5.19 WALL ( 5.19 usr +  0.00 sys =  5.20 CPU) @ 995641661.26/s (n=5172673053)
          5.26 WALL ( 5.24 usr +  0.01 sys =  5.25 CPU) @ 985885687.02/s (n=5172673053)
          5.67 WALL ( 5.63 usr +  0.02 sys =  5.65 CPU) @ 915706978.79/s (n=5172673053)

                Rate                  = == || =      ==
      =   18755938+-    12313/s      --    -98%   -100%
== || =  965744776+- 33825253/s   5049%      --    -76%
     == 4078110325+-157171762/s  21643%    322%      --
*)
