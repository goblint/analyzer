(* ocamlbuild -use-ocamlfind src/afl.native *)
(* afl-fuzz -i input -o output ./afl.native *)
open BatteriesExceptionless

let test (module X : IntDomain.S) =
  let open X in
  let read () =
    try match read_line () with
      | "bot" -> bot ()
      | "top" -> top ()
      | x -> Int64.of_string x |> of_int
    with _ -> exit 0
  in
  let x = read () in
  let y = read () in

  assert (leq (bot ()) x);
  assert (leq x (top ()));

  assert (leq x (join x y));
  assert (leq y (join x y));

  assert (leq (meet x y) x);
  assert (leq (meet x y) y);

  assert (leq y (widen x y));
  assert (leq (narrow x y) x);

  assert (leq (join x y) (widen x y));
  assert (leq (narrow x y) (meet x y));

  assert (not (leq x y && leq y x) || equal x y)

let () =
  test (module IntDomain.Interval32)
