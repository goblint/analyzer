(** call [f], with the ordinary ref [r] temporarily set to [x]

    [r] is shared by every domain, so this save/restore only holds if at most one
    domain can reach [r]. If several can, they interleave and the last one to
    restore wins. For state a parallel solver may touch, use {!wrap} instead. *)
let wrap_ref r x =
  let x0 = !r in
  r := x;
  Fun.protect ~finally:(fun () -> r := x0)

(** call [f], with the domain-local [k] temporarily set to [x]

    Each domain saves and restores its own value, so this stays correct however
    many domains run [f] at once. *)
let wrap k x =
  let x0 = Domain.DLS.get k in
  Domain.DLS.set k x;
  Fun.protect ~finally:(fun () -> Domain.DLS.set k x0)
