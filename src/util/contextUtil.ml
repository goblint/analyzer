open Cil

let has_attribute s al =
  List.exists (function
      | Attr ("goblint_context", args) when List.exists (function
          | AStr s' when s = s' -> true
          | _ -> false
        ) args -> true
      | _ -> false
    ) al

let should_remove ~keepOption ~removeAttr ~keepAttr fd =
  let al = fd.svar.vattr in
  match GobConfig.get_bool keepOption, has_attribute removeAttr al, has_attribute keepAttr al with
  | _, true, true ->
    failwith (Printf.sprintf "ContextUtil.should_remove: conflicting context attributes %s and %s on %s" removeAttr keepAttr (CilType.Fundec.show fd))
  | _, false, true
  | true, false, false ->
    false
  | false, _, false
  | _, true, false ->
    true
