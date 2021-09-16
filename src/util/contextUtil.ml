open Cil

let hasAttribute s al =
  List.exists (function
      | Attr ("goblint_context", args) when List.exists (function
          | AStr s' when s = s' -> true
          | _ -> false
        ) args -> true
      | _ -> false
    ) al
