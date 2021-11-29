open Cil

(** Definition of Goblint specific user defined C attributes **)

type attribute =
  | GobContext
  | GobPrecision

let attribute_to_string = function
  | GobContext -> "goblint_context"
  | GobPrecision -> "goblint_precision"

let has_attribute s1 s2 al =
  List.exists (function
      | Attr (s1', args) when s1 = s1' && List.exists (function
          | AStr s2' when s2 = s2' -> true
          | _ -> false
        ) args -> true
      | _ -> false
    ) al

let should_keep ~isAttr ~keepOption ~removeAttr ~keepAttr fd =
  let al = fd.svar.vattr in
  let s = attribute_to_string isAttr in
  match GobConfig.get_bool keepOption, has_attribute s removeAttr al, has_attribute s keepAttr al with
  | _, true, true ->
    failwith (Printf.sprintf "ContextUtil.should_remove: conflicting context attributes %s and %s on %s" removeAttr keepAttr (CilType.Fundec.show fd))
  | _, false, true
  | true, false, false ->
    true
  | false, _, false
  | _, true, false ->
    false
