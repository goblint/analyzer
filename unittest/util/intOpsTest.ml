open OUnit2
open Goblint_lib

(* If the first operand of a div is negative, Zarith rounds the result away from zero.
    We thus always transform this into a division with a non-negative first operand. *)
let old_div a b = if Z.lt a Z.zero then Z.neg (Z.ediv (Z.neg a) b) else Z.ediv a b

(* Z.erem computes the Euclidian Modulus, but what we want here is the remainder, as returned by mod on ints
   -1 rem 5 == -1, whereas -1 Euclid-Mod 5 == 4 *)
let old_rem a b = Z.sub a (Z.mul b (old_div a b))

let test_bigint_div =
  QCheck.(Test.make ~name:"div"
    (pair MyCheck.Arbitrary.big_int MyCheck.Arbitrary.big_int)
    (fun (x, y) ->
      assume (Z.compare y Z.zero <> 0);
      Z.equal (Z.div x y) (old_div x y)
    ))

let test_bigint_rem =
  QCheck.(Test.make ~name:"rem"
    (pair MyCheck.Arbitrary.big_int MyCheck.Arbitrary.big_int)
    (fun (x, y) ->
      assume (Z.compare y Z.zero <> 0);
      Z.equal (Z.rem x y) (old_rem x y)
    ))

let tests =
  "intOpsTest" >::: [
    "bigint" >::: QCheck_ounit.to_ounit2_test_list [
      test_bigint_div;
      test_bigint_rem;
    ]
  ]
