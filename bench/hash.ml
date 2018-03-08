open Batteries
open Printf

let n_hash = ref 0
let n_equal = ref 0

module Var = struct
  type node = int
  type context = (int, unit) Map.t
  type t = node * context

  let hash x = incr n_hash; Hashtbl.hash x
  let equal x y = incr n_equal; x = y

  (* let of_int x = x, Map.add x () Map.empty *)
  let of_int x = x, Map.empty
end

module Solver = struct
  module H = Hashtbl.Make (Var)

  let h = H.create 13

  let find v =
    ignore (try H.find h v with _ -> ())

  let add v =
    H.add h v ()

  let solve i =
    (* printf "solve %d\n" i; *)
    let v = Var.of_int i in
    (* let _ = try H.find h v with _ -> () in *)
    H.add h v ()
end

let time ?(n=1) title f =
  let times = if n = 1 then "" else " (" ^ string_of_int n ^ "x)" in
  printf "Start %s%s\n" title times;
  let t = Unix.gettimeofday () in
  for i = 1 to n do
    ignore (f i)
  done;
  printf "Done in %f ns\n" ((Unix.gettimeofday () -. t) *. 1000. *. 1000.)

let () =
  time ~n:1000 "find" (Solver.find % Var.of_int);
  printf "n_hash = %d\nn_equal = %d\n" !n_hash !n_equal;
  n_hash := 0; n_equal := 0;
  time ~n:1000 "add" (Solver.add % Var.of_int);
  printf "n_hash = %d\nn_equal = %d\n" !n_hash !n_equal;
  n_hash := 0; n_equal := 0;
  time ~n:1000 "find" (Solver.find % Var.of_int);
  printf "n_hash = %d\nn_equal = %d\n" !n_hash !n_equal;
  n_hash := 0; n_equal := 0;
