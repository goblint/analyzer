open Cil

let hasAttribute s al =
  List.exists (function
      | Attr ("goblint_context", args) when List.exists (function
          | AStr s' when s = s' -> true
          | _ -> false
        ) args -> true
      | _ -> false
    ) al

let shouldRemove ~removeOption ~removeAttr ~keepAttr fd =
  let al = fd.svar.vattr in
  match GobConfig.get_bool removeOption, hasAttribute removeAttr al, hasAttribute keepAttr al with
  | _, true, true ->
    failwith (Printf.sprintf "ContextUtil.shouldRemove: conflicting context attributes %s and %s on %s" removeAttr keepAttr (CilType.Fundec.show fd))
  | _, false, true
  | false, false, false ->
    false
  | true, _, false
  | _, true, false ->
    true
