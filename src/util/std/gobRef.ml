(** call [f], with [r] temporarily set to [x] *)
let wrap_old r x =
  let x0 = !r in
  r := x;
  Fun.protect ~finally:(fun () -> r := x0)

let wrap r x =
  let x0 = Domain.DLS.get r in
  Domain.DLS.set r x;
  Fun.protect ~finally:(fun () -> Domain.DLS.set r x0)
