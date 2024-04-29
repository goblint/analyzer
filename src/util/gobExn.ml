let catch_all_filter = function
  | Out_of_memory
  | Stack_overflow -> false (* OCaml runtime *)
  | Assert_failure _
  | Match_failure _
  | Undefined_recursive_module _ -> false (* Broken program *)
  | Sys.Break -> false (* Ctrl-C *)
  | Timeout.Timeout -> false (* Goblint timeout *)
  | _ -> true
