(* Copied literally from json-data-encoding's Json_repr.Yojson. *)
module Yojson = struct
  type value = Yojson.Safe.t (* Only this type is modified to refer to current version of Yojson. *)

  (* The following functions need to be copied from Json_repr.Yojson.
     The versions in Json_repr cannot be called because its interface has already fixed the value type.

     However, we need them in their fully polymorphic form.
     This allows the same implementations to be reused for Yojson 2 and 3 without conditional compilation.
     With Yojson 3, the `Tuple and `Variant cases are simply never used. *)

  let view = function
    | `Intlit i -> `String i
    | `Tuple l -> `A l
    | `Variant (label, Some x) -> `A [`String label; x]
    | `Variant (label, None) -> `String label
    | `Assoc l -> `O l
    | `List l -> `A l
    | `Int i -> `Float (float i)
    | `Float f -> `Float f
    | `String s -> `String s
    | `Null -> `Null
    | `Bool b -> `Bool b

  let repr = function
    | `O l -> `Assoc l
    | `A l -> `List l
    | `Bool b -> `Bool b
    | `Float f -> `Float f
    | `String s -> `String s
    | `Null -> `Null

  let repr_uid = Json_repr.repr_uid ()
end
