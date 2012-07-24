module Self = Plugin.Register
    (struct
       let name = "Goblint"
       let shortname = "goblint"
       let help = "The Goblint Concurrent C Analyzer."
     end)

(** Register the new Frama-C option "-hello". *)
module Enabled =
  Self.False
    (struct
       let option_name = "-goblint"
       let help = "Run Goblint!"
       let kind = `Correctness
     end)

let print () = Self.result "Goblint running!"

(** The code below is not mandatory: you can ignore it in a first reading. It
    provides an API for the plug-in, so that the function [run] is callable by
    another plug-in and journalized: first, each plug-in can call [Dynamic.get
    "Hello.run" (Datatype.func Datatype.unit Datatype.unit)] in order to call [run]
    and second, each call to [run] is written in the Frama-C journal. *)
(*let print =*)
(*  Dynamic.register*)
(*    ~plugin:"Hello"*)
(*    "run"*)
(*    ~journalize:true*)
(*    (Datatype.func Datatype.unit Datatype.unit)*)
(*    print*)

(** Print 'Hello World!' whenever the option is set. *)
let run () =  if Enabled.get () then print ()

(** Register the function [run] as a main entry point. *)
let () = Db.Main.extend run
