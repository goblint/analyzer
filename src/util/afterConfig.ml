(** Hooks which run after the runtime configuration is fully loaded. *)

let callbacks = ref []

let register callback =
  callbacks := callback :: !callbacks

let run () =
  List.iter (fun callback -> callback ()) !callbacks;
  callbacks := []