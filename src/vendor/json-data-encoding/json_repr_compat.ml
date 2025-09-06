module Yojson = struct
  type value = Yojson.Safe.t

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
