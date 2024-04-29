(** Exception utilities. *)

(** Filter for catching and swallowing all exceptions.
    This avoids swallowing some very important exceptions that shouldn't be caught to begin with. *)
let catch_all_filter = function
  | Out_of_memory
  | Stack_overflow -> false (* OCaml runtime *)
  | Assert_failure _
  | Match_failure _
  | Undefined_recursive_module _ -> false (* Broken program *)
  | Sys.Break -> false (* Ctrl-C *)
  | Timeout.Timeout -> false (* Goblint timeout *)
  | _ -> true

(* TODO: can't move to goblint.std because Timeout.Timeout *)
