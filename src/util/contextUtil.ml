open GoblintCil

(** Definition of Goblint specific user defined C attributes and their alternatives via options **)

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

let has_option s1 s2 fd =
  List.mem s2 (GobConfig.get_string_list ("annotation." ^ s1 ^ "." ^ fd.svar.vname))

let should_keep ~isAttr ~keepOption ~removeAttr ~keepAttr fd =
  let al = fd.svar.vattr in
  let s = attribute_to_string isAttr in
  let has_annot a = has_option s a fd || has_attribute s a al in
  match GobConfig.get_bool keepOption, has_annot removeAttr, has_annot keepAttr with
  | _, true, true ->
    failwith (Printf.sprintf "ContextUtil.should_remove: conflicting context attributes %s and %s on %s" removeAttr keepAttr (CilType.Fundec.show fd))
  | _, false, true
  | true, false, false ->
    true
  | false, _, false
  | _, true, false ->
    false
