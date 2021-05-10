(* ocamlbuild -pkg batteries physical_equality.native && ./physical_equality.native *)

open Batteries;;

(* Q: When do we lose physical equality? *)
Set.empty =  Set.empty;;
(* - : bool = true *)
Set.empty == Set.empty;;
(* - : bool = true *)
let x = Set.(add 1 empty);;
(* val x : int BatSet.t = <abstr> *)
let y = Set.(add 1 empty);;
(* val y : int BatSet.t = <abstr> *)
x =  y;;
(* - : bool = true *)
x == y;;
(* - : bool = false *)
type t1 = A | B of int;;
A == A;;
(* - : bool = true *)
B 1 == B 1;;
(* - : bool = false *)
(* A: For any constructor with arguments that cannot be statically computed.
 * Natively compiled the following will print true, but false in utop. *)
print_bool (B (2*3-5) == B (2-1));;
let x = read_int () in
let y = read_int () in
Printf.printf "x: %d, y: %d, B x == B y: %b, B x == B x: %b\n" x y (B x == B y) (B x == B x);; (* false and false for inputs x = y *)

(* Q: Does = use == first? *)
type t2 = R of t2;;
(* type t2 = R of t2 *)
let rec c = R c;;
(* val c : t2 = R <cycle> *)
c == c;;
(* - : bool = true *)
(* c = c;; (* does not terminate! *) *)
(* A: No, otherwise the above would terminate. Why doesn't it use it? *)
