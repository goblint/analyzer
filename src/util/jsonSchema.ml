open Prelude

module JS = Json_schema.Make (Json_repr.Yojson)
module JE = Json_encoding.Make (Json_repr.Yojson)
module JQ = Json_query.Make (Json_repr.Yojson)

(* copied & modified from json_encoding.ml *)
let unexpected kind expected =
  let kind =
    match Json_repr.from_yojson kind with
    | `O [] -> "empty object"
    | `A [] -> "empty array"
    | `O _ -> "object"
    | `A _ -> "array"
    | `Null -> "null"
    | `String _ -> "string"
    | `Float _ -> "number"
    | `Bool _ -> "boolean"
  in
  Json_encoding.Cannot_destruct ([], Json_encoding.Unexpected (kind, expected))

let schema_to_yojson schema = JS.to_json schema
let schema_of_yojson json = JS.of_json json

let erase: type t. t Json_encoding.encoding -> unit Json_encoding.encoding = fun encoding -> Json_encoding.conv (fun _ -> failwith "erase construct") (fun _ -> ()) encoding

let rec encoding_of_schema_element (top: unit Json_encoding.encoding) (schema_element: Json_schema.element): unit Json_encoding.encoding =
  let open Json_encoding in
  match schema_element.kind with
  | Any -> unit
  | String string_specs ->
    begin match schema_element.enum with
      | None ->
        erase string
      | Some enum ->
        enum
        |> List.map (fun value ->
            match Json_repr.any_to_repr (module Json_repr.Yojson) value with
            | `String value -> (value, ())
            | _ -> failwith "encoding_of_schema_element: string_enum"
          )
        |> string_enum
    end
  | Boolean -> erase bool
  | Integer numeric_specs -> erase int
  | Monomorphic_array (el, array_specs) ->
    erase @@ array (encoding_of_schema_element top el)
  | Id_ref "" ->
    top
  | Object object_specs ->
    let properties_encoding = List.fold_left (fun acc (name, element, required, _) ->
        let field =
          if required then
            req name (encoding_of_schema_element top element)
          else
            dft name (encoding_of_schema_element top element) ()
        in
        erase @@ merge_objs acc (obj1 field)
      ) empty object_specs.properties
    in
    begin match object_specs.additional_properties with
      | Some additional_properties ->
        let additional_encoding = encoding_of_schema_element top additional_properties in
        JE.custom (fun _ -> failwith "erase construct") (function
            | `Assoc fields ->
              let is_properties_field (name, _) = List.exists (fun (name', _, _, _) -> name = name') object_specs.properties in
              let (properties_fields, additional_fields) = List.partition is_properties_field fields in
              JE.destruct properties_encoding (`Assoc properties_fields);
              List.iter (fun (name, value) ->
                  try
                    JE.destruct additional_encoding value
                  with Cannot_destruct (path, err) ->
                    raise (Cannot_destruct (`Field name :: path, err))
                ) additional_fields
            | j ->
              raise (unexpected j "object")
          ) ~schema:(Json_schema.create schema_element)
      | None ->
        properties_encoding
    end
  | _ -> failwith (Format.asprintf "encoding_of_schema_element: %a" Json_schema.pp (Json_schema.create schema_element))

let encoding_of_schema (schema: Json_schema.schema): unit Json_encoding.encoding =
  let root = Json_schema.root schema in
  Json_encoding.mu "" (fun top -> encoding_of_schema_element top root)

open Json_schema

let rec element_defaults ?additional_field (element: element): Yojson.Safe.t =
  match element.default with
  | Some default ->
    Json_repr.any_to_repr (module Json_repr.Yojson) default
  | None ->
    begin match element.kind with
      | Object object_specs ->
        let additional = match additional_field, object_specs.additional_properties with
          | Some additional_field, Some additional_properties ->
            (* create additional field with the additionalProperties default value for lookup in GobConfig *)
            [(additional_field, element_defaults ~additional_field additional_properties)]
          | _, _ ->
            []
        in
        `Assoc (additional @ List.map (fun (name, field_element, _, _) ->
            (name, element_defaults ?additional_field field_element)
          ) object_specs.properties)
      | _ ->
        Format.printf "%a\n" Json_schema.pp (create element);
        failwith "element_defaults"
    end

let schema_defaults ?additional_field (schema: schema): Yojson.Safe.t =
  element_defaults ?additional_field (root schema)

let create_schema element =
  create @@ { element with id = Some "" } (* add id to make create defs check happy for phases Id_ref, doesn't get outputted apparently *)

let rec element_require_all (element: element): element =
  let kind' = match element.kind with
    | String _
    | Boolean
    | Id_ref _
    | Integer _
    | Number _ -> element.kind
    | Monomorphic_array (element_element, array_specs) ->
      let array_specs' =
        { array_specs with
          additional_items = Option.map element_require_all array_specs.additional_items;
        }
      in
      Monomorphic_array (element_require_all element_element, array_specs')
    | Object object_specs ->
      let properties' = List.map (fun (name, field_element, required, unknown) ->
          (name, element_require_all field_element, true, unknown) (* change required to true *)
        ) object_specs.properties
      in
      Object { object_specs with properties = properties' }
    | _ ->
      Format.printf "%a\n" Json_schema.pp (create element);
      failwith "element_require_all"
  in
  { element with kind = kind' }

let schema_require_all (schema: schema): schema =
  create_schema (element_require_all (root schema))


module type Schema =
sig
  val schema: schema
end

module Validator (Schema: Schema) =
struct
  let schema_encoding = encoding_of_schema Schema.schema

  (** Raises [Json_encoding.Cannot_destruct] if invalid. *)
  let validate_exn json = JE.destruct schema_encoding json

  (* TODO: bool-returning validate? *)
end


let () = Printexc.register_printer (function
    | Json_encoding.Unexpected _
    | Json_encoding.No_case_matched _
    | Json_encoding.Bad_array_size _
    | Json_encoding.Missing_field _
    | Json_encoding.Unexpected_field _
    | Json_encoding.Bad_schema _
    | Json_encoding.Cannot_destruct _ as exn ->
      let msg = Format.asprintf "Json_encoding: %a" (Json_encoding.print_error ?print_unknown:None) exn in
      Some msg
    | Json_schema.Cannot_parse _
    | Json_schema.Dangling_reference _
    | Json_schema.Bad_reference _
    | Json_schema.Unexpected _
    | Json_schema.Duplicate_definition _ as exn ->
      let msg = Format.asprintf "Json_schema: %a" (Json_encoding.print_error ?print_unknown:None) exn in
      Some msg
    | Json_query.Illegal_pointer_notation _
    | Json_query.Unsupported_path_item _
    | Json_query.Cannot_merge _ as exn ->
      let msg = Format.asprintf "Json_query: %a" (Json_encoding.print_error ?print_unknown:None) exn in
      Some msg
    | _ -> None (* for other exceptions *)
  )
