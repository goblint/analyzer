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
  let name = BatString.lchop path in (* chop initial . *)
  let element_defaults kind = (* element with metadata from defaults *)
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
      | exception Not_found when path = ".interact" -> (* interact isn't Std *)
        { element with
          title = Some name;
        }
      | exception Not_found when path = "" -> (* is root *)
        element
      | exception Not_found -> (* category *)
        { element with
          title = Some (DefaultsCategory.show_category category);
          description = Some (DefaultsCategory.catDescription category);
        }
      | _ -> (* inner object, not category *)
        { element with
          title = Some name;
        }
    in
    (element, category)
  | _ ->
    failwith (Format.asprintf "element_category_of_defaults_json: %a" Yojson.Safe.pp json)

let schema_of_defaults_json defaults json =
  let (element, _) = element_category_of_defaults_json defaults json "" in
  JsonSchema.create_schema element


(* let defaults_schema =
  let defaults = List.map (fun (c, (n, (desc, def))) -> (n, (c, desc, def))) !Defaults.registrar in (* transform for assoc list lookup by name *)
  schema_of_defaults_json defaults !GobConfig.json_conf

let () =
  Yojson.Safe.pretty_to_channel (Stdlib.open_out "options.schema.json") (JsonSchema.schema_to_yojson defaults_schema) *)

let schema_of_yojson json =
  (* workaround for json-data-encoding not handling recursive root reference correctly *)
  (* remove the reference before parsing, hack it back afterwards *)
  let json = JsonSchema.JQ.replace [`Field "properties"; `Field "phases"; `Field "items"] (`Assoc []) json in
  let schema = JsonSchema.schema_of_yojson json in (* definitions_path doesn't work, "definitions" field still hardcoded *)
  let element = Json_schema.root schema in
  let element = match element with
    | { kind = Object ({properties; _} as object_specs); _} ->
      let rec modify = function
        | [] -> assert false
        | ("phases", ({ Json_schema.kind = Monomorphic_array (_, array_specs); _} as field_element), required, unknown) :: props ->
          ("phases", {field_element with Json_schema.kind = Monomorphic_array (Json_schema.element (Id_ref ""), array_specs)}, required, unknown) :: props
        | prop :: props ->
          prop :: modify props
      in
      {element with kind = Object {object_specs with properties = modify properties}}
    | _ ->
      assert false
  in
  JsonSchema.create_schema element

let schema =
  (* schema_of_yojson (Yojson.Safe.from_file "options.schema.json") *)
  schema_of_yojson (Yojson.Safe.from_string [%blob "options.schema.json"])

let require_all = JsonSchema.schema_require_all schema

let defaults = JsonSchema.schema_defaults schema

let () =
  (* Yojson.Safe.pretty_to_channel (Stdlib.open_out "options.require-all.schema.json") (JsonSchema.schema_to_yojson require_all); *)
  (* Yojson.Safe.pretty_to_channel (Stdlib.open_out "options.defaults.json") defaults; *)
  ()

let rec element_paths (element: element): string list =
  match element.kind with
  | String _
  | Boolean
  | Integer _
  | Number _
  | Monomorphic_array _ ->
    [""]
  | Object object_specs ->
    List.concat_map (fun (name, field_element, _, _) ->
        List.map (fun path -> name ^ "." ^ path) (element_paths field_element)
      ) object_specs.properties
  | _ ->
    Format.printf "%a\n" Json_schema.pp (create element);
    failwith "element_paths"

let schema_paths (schema: schema): string list =
  element_paths (root schema)
  |> List.map BatString.rchop (* remove trailing '.' *)

let paths = schema_paths schema

let rec pp_options ~levels ppf (element: element) =
  match element.kind with
  | String _
  | Boolean
  | Integer _
  | Number _
  | Monomorphic_array _ ->
    (* Format.fprintf ppf "%s: %s (%a)" (Option.get element.title) (Option.get element.description) (Yojson.Safe.pretty_print ~std:false) (Json_repr.any_to_repr (module Json_repr.Yojson) (Option.get element.default)) *)
    (* Yojson screws up box indentation somehow... *)
    Format.fprintf ppf "%s: %s (%s)" (Option.get element.title) (Option.get element.description) (Yojson.Safe.to_string (Json_repr.any_to_repr (module Json_repr.Yojson) (Option.get element.default)))
  | Object object_specs when levels > 0 ->
    let properties = List.filter (fun (name, field_element, _, _) ->
        match field_element.kind with
        | Object _ when levels = 1 -> false (* avoid empty lines with --options *)
        | _ -> true
      ) object_specs.properties
    in
    let pp_property ppf (name, field_element, _, _) =
      Format.fprintf ppf "%a" (pp_options ~levels:(levels - 1)) field_element
    in
    begin match element.title with
      | Some title ->
        Format.fprintf ppf "@[<v 0>%s:@,  @[<v 0>%a@]@]" title (Format.pp_print_list pp_property) properties
      | None ->
        Format.fprintf ppf "@[<v 0>%a@]" (Format.pp_print_list pp_property) properties
    end
  | Object _ ->
    ()
  | _ ->
    failwith "pp_options"

let print_options () =
  Format.printf "%a\n" (pp_options ~levels:1) (root schema)

let print_all_options () =
  Format.printf "%a\n" (pp_options ~levels:max_int) (root schema)
