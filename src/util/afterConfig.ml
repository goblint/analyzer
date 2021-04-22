let callbacks = ref []

let register callback =
  callbacks := callback :: !callbacks

let run () =
  List.iter (fun callback -> callback ()) !callbacks;
  callbacks := []