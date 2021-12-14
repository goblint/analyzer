open Json_encoding
open Json_schema

(* TODO: deduplicate with GobConfig *)
(** The ultimate convenience function for writing values. *)
let one_quote = Str.regexp "\'"
let parse_goblint_json s =
  try
    let s' = Str.global_replace one_quote "\"" s in
    let v = Yojson.Safe.from_string s' in
    v
  with Yojson.Json_error _ ->
    `String s

(* Convert old Defaults options to schema. *)

type defaults = (string * (DefaultsCategory.category * string * string)) list

let rec element_category_of_defaults_json (defaults: defaults) (json: Yojson.Safe.t) (path: string): element * DefaultsCategory.category =
  let element_defaults kind = (* element with metadata from defaults *)
    let name = BatString.lchop path in (* chop initial . *)
    let (category, description, default) = List.assoc name defaults in
    let default_json = parse_goblint_json default in
    let element = { (element kind) with
      title = Some name;
      description = Some description;
      default = Some (Json_repr.repr_to_any (module Json_repr.Yojson) default_json);
    }
    in
    (element, category)
  in
  match json with
  | `String s ->
    element_defaults @@ String string_specs
  | `Bool b ->
    element_defaults @@ Boolean
  | `Int i ->
    element_defaults @@ Integer numeric_specs
  | `List xs ->
    let element_element = match path with
      | ".phases" -> element (Id_ref "") (* refer to entire schema itself *)
      | _ -> element (String string_specs) (* all other lists contain strings *)
    in
    element_defaults @@ Monomorphic_array (element_element, array_specs)
  | `Assoc xs ->
    let category = ref None in
    let properties = List.map (fun (key, value) ->
        let (field_element, field_category) = element_category_of_defaults_json defaults value (path ^ "." ^ key) in
        category := Some field_category; (* category from defaults for object description *)
        (key, field_element, false, None)
      ) xs
    in
    let category = Option.get !category in
    let element = element @@ Object {
        object_specs with
        properties;
        additional_properties = None; (* forbid additional properties *)
      }
    in
    let element = match BatString.index_after_n '.' 2 path with
      | exception Not_found when path = ".interact" || path = "" -> (* interact isn't Std or is root *)
        element
      | exception Not_found -> (* category *)
        { element with
          title = Some (DefaultsCategory.show_category category);
          description = Some (DefaultsCategory.catDescription category);
        }
      | _ -> (* inner object, not category *)
        element
    in
    (element, category)
  | _ ->
    failwith (Format.asprintf "element_category_of_defaults_json: %a" Yojson.Safe.pp json)

let convert_schema opts json =
  try
    let sch = create @@ {(fst @@ element_category_of_defaults_json opts json "") with id = Some ""} in (* add id to make create defs check happy, doesn't get outputted apparently *)
    JsonSchema2.global_schema := sch;
    (* Format.printf "schema: %a\n" Json_schema.pp sch; *)
    (* Format.printf "schema2: %a\n" (Yojson.Safe.pretty_print ~std:true) (JS.to_json sch) *)
    Yojson.Safe.pretty_to_channel (Stdlib.open_out "schema.json") (JsonSchema2.JS.to_json sch);
    let sch_req = create @@ {(JsonSchema2.require_all (root sch)) with id = Some ""} in
    Yojson.Safe.pretty_to_channel (Stdlib.open_out "schema_require.json") (JsonSchema2.JS.to_json sch_req);
    let defaults = JsonSchema2.create_defaults (root sch) in
    Yojson.Safe.pretty_to_channel (Stdlib.open_out "defaults.json") defaults;
    (* let defaults2 = JE.construct ~include_default_fields:`Always (encoding_of_schema sch) () in *)
    (* erase construct fails *)
    (* Yojson.Safe.pretty_to_channel (Stdlib.open_out "defaults2.json") defaults2; *)
    ()
  with (Json_schema.Dangling_reference u as e) ->
    Json_schema.print_error Format.err_formatter e

let () =
  let defaults = List.map (fun (c, (n, (desc, def))) -> (n, (c, desc, def))) !Defaults.registrar in (* transform for assoc list lookup by name *)
  convert_schema defaults !GobConfig.json_conf
