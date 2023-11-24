(** call [f], with [r] temporarily set to [x] *)
let wrap r x =
  let x0 = !r in
  r := x;
  Fun.protect ~finally:(fun () -> r := x0)
