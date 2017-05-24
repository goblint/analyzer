open BatteriesExceptionless

let rec bson_of_yojson = let open Bson in function
    | `Assoc x -> create_doc_element (List.fold_left (fun doc (k,v) -> add_element k (bson_of_yojson v) doc) empty x)
    | `Bool x -> create_boolean x
    | `Float x -> create_double x
    | `Int x -> create_int64 (Int64.of_int x)
    | `Intlit x -> create_int64 (Int64.of_string x)
    | `List x -> create_list (List.map bson_of_yojson x)
    | `Null -> create_null ()
    | `String x -> create_string x
    | `Tuple x -> failwith "TODO Tuple"
    | `Variant x -> failwith "TODO Variant"

let connect () = Mongo.create_local_default "goblint" "test"

let insert db (loc,node,v) =
  let x = Bson.(
      add_element "location" (bson_of_yojson loc) empty |>
      add_element "node" (bson_of_yojson node) |>
      add_element "states" (bson_of_yojson v))
  in
  Mongo.insert db [x]