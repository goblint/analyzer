open Json_encoding
open Json_schema

(** The ultimate convenience function for writing values. *)
let one_quote = Str.regexp "\'"
let parse_goblint_json s =
  try
    let s' = Str.global_replace one_quote "\"" s in
    let v = Yojson.Safe.from_string s' in
    v
  with Yojson.Json_error _ ->
    `String s

let rec convert_schema' (json: Yojson.Safe.t) opts (prefix: string): element * DefaultsCategory.category =
  let element' ekind =
    let (cat, (desc, def)) = List.assoc (BatString.lchop prefix) opts in
    (* let name = BatString.lchop @@ Filename.extension prefix in *)
    ({(element ekind) with title = Some (BatString.lchop prefix); description = Some desc; default = Some (Json_repr.repr_to_any (module Json_repr.Yojson) (parse_goblint_json def))}, cat)
  in
  match json with
  | `String s ->
    element' @@ String string_specs
  | `Bool b ->
    element' @@ Boolean
  | `Int i ->
    element' @@ Integer numeric_specs
  | `List xs ->
    let element_schema = match prefix with
      | ".phases" -> element (Id_ref "") (* refer to entire schema itself *)
      | _ -> element (String string_specs)
    in
    element' @@ Monomorphic_array (element_schema, array_specs)
  | `Assoc xs ->
    let cat' = ref None in
    let properties = List.map (fun (key, value) ->
        let (inner, cat) = convert_schema' value opts (prefix ^ "." ^ key) in
        cat' := Some cat;
        (key, inner, false, None)
      ) xs
    in
    let cat = Option.get !cat' in
    let el = match BatString.index_after_n '.' 2 prefix with
      | exception Not_found when prefix = ".interact" || prefix = "" -> (* isn't Std; is root *)
        element @@ Object { object_specs with properties; additional_properties = None}
      | exception Not_found ->
        {(element @@ Object { object_specs with properties; additional_properties = None}) with title = Some (DefaultsCategory.show_category cat); description = Some (DefaultsCategory.catDescription cat)}
      | _ ->
        element @@ Object { object_specs with properties; additional_properties = None}
    in
    (el, cat)
  | _ -> failwith (Format.asprintf "convert_schema': %a" Yojson.Safe.pp json)

let convert_schema json opts =
  try
    let sch = create @@ {(fst @@ convert_schema' json opts "") with id = Some ""} in (* add id to make create defs check happy, doesn't get outputted apparently *)
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
  convert_schema !GobConfig.json_conf @@ List.map (fun (c, (n, p)) -> (n, (c, p))) !Defaults.registrar
