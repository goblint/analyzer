open Prelude

module JS = Json_schema.Make (Json_repr.Yojson)
module JE = Json_encoding.Make (Json_repr.Yojson)

let default_schema = {schema|
{ "id"              : "root"
, "type"            : "object"
, "required"        : ["outfile", "includes", "kernel_includes", "custom_includes", "kernel-root", "justcil", "justcfg", "printstats", "verify", "mainfun", "exitfun", "otherfun", "allglobs", "keepcpp", "tempDir", "cppflags", "kernel", "dump_globs", "result", "solver", "allfuns", "nonstatic", "colors", "g2html"]
, "additionalProperties" : false
, "properties" :
  { "ana" :
    { "type"            : "object"
    , "additionalProperties" : true
    , "required"        : []
    }
  , "sem"               : {}
  , "incremental"       : {}
  , "trans"             : {}
  , "phases"            : {}
  , "annotation" :
    { "type"            : "object"
    , "additionalProperties" : true
    , "required"        : []
    }
  , "exp" :
    { "type"            : "object"
    , "additionalProperties" : true
    , "required"        : []
    }
  , "dbg" :
    { "type"            : "object"
    , "additionalProperties" : true
    , "required"        : []
    }
  , "questions" :
    { "file"            : ""
    }
  , "outfile"         : {}
  , "includes"        : {}
  , "kernel_includes" : {}
  , "custom_includes" : {}
  , "kernel-root"     : {}
  , "justcil"         : {}
  , "justcfg"         : {}
  , "printstats"      : {}
  , "verify"          : {}
  , "mainfun"         : {}
  , "exitfun"         : {}
  , "otherfun"        : {}
  , "allglobs"        : {}
  , "keepcpp"         : {}
  , "tempDir"         :
    { "type"            : "string"
    }
  , "cppflags"        : {}
  , "kernel"          : {}
  , "dump_globs"      : {}
  , "result"          :
    { "type"            : "string"
    }
  , "solver"          : {}
  , "comparesolver"   : {}
  , "solverdiffs"     : {}
  , "allfuns"         : {}
  , "nonstatic"       : {}
  , "colors"          : {}
  , "g2html"          : {}
  , "interact"        : {}
  , "save_run"        : {}
  , "load_run"        : {}
  , "compare_runs"    : {}
  , "warn_at"         : {}
  , "warn"              :
    { "type"            : "object"
    , "additionalProperties" : true
    , "required"        : ["foo"]
    }
  , "gobview"         : {}
  }
}|schema}

let schema = JS.of_json (Yojson.Safe.from_string default_schema)

let erase: type t. t Json_encoding.encoding -> unit Json_encoding.encoding = fun encoding -> Json_encoding.conv (fun _ -> failwith "asd") (fun _ -> ()) encoding

let rec encoding_of_schema_element (schema_element: Json_schema.element): unit Json_encoding.encoding =
  let open Json_encoding in
  match schema_element.kind with
  | Any -> unit
  | String string_specs -> erase string
  | Object object_specs ->
    List.fold_left (fun acc (name, element, required, _) ->
        let field =
          if required then
            req name (encoding_of_schema_element element)
          else
            dft name (encoding_of_schema_element element) ()
        in
        erase @@ merge_objs acc (obj1 field)
      ) (Option.map_default encoding_of_schema_element empty object_specs.additional_properties) object_specs.properties
  | _ -> failwith (Format.asprintf "encoding_of_schema_element: %a" Json_schema.pp (Json_schema.create schema_element))

let encoding_of_schema (schema: Json_schema.schema): unit Json_encoding.encoding =
  encoding_of_schema_element (Json_schema.root schema)

let validate json =
  match JE.destruct (encoding_of_schema schema) json with
    | _ -> ()
    | exception (Json_encoding.Cannot_destruct _ as e) ->
      Format.printf "validate: %a\n" (Json_encoding.print_error ?print_unknown:None) e;
      failwith "JsonSchema2.validate"


(** raise when you cannot parse the path *)
exception PathParseError

(** raise when there is an type error *)
exception ConfTypeError

(** Type of the index *)
type index = Int of int  (** and integer *)
           | App         (** prepend to the list *)
           | Rem         (** remove from the list *)
           | New         (** create a new list *)

(** Type of the path *)
type path  = Here                    (** we are there *)
           | Select of string * path (** we need to select an field *)
           | Index  of index  * path (** we need to select an array index *)

open Printf

(** Path printing. *)
let rec print_path' ch = function
  | Here -> ()
  | Select (s,p)    -> fprintf ch ".%s%a"  s print_path' p
  | Index (Int i,p) -> fprintf ch "[%d]%a" i print_path' p
  | Index (App ,p) -> fprintf ch "[+]%a"    print_path' p
  | Index (Rem ,p) -> fprintf ch "[-]%a"    print_path' p
  | Index (New  ,p) -> fprintf ch "[*]%a"    print_path' p


(** Parse an index. *)
let parse_index s =
  try if s = "+" then App
    else if s = "*" then New
    else if s = "-" then Rem
    else Int (int_of_string s)
  with Failure _ -> raise PathParseError

(** Helper function [split c1 c2 xs] that splits [xs] on [c1] or [c2] *)
let split c1 c2 xs =
  let l = String.length xs in
  let rec split' i =
    if i<l then begin
      if xs.[i]=c1 || xs.[i]=c2 then
        (String.sub xs 0 i, String.sub xs i (l-i))
      else
        split' (i+1)
    end else
      (xs,"")
  in
  split' 0

(** Parse a string path. *)
let rec parse_path' (s:string) : path =
  if String.length s = 0 then Here else
    match s.[0] with
    | '.' ->
      let fld, pth = split '.' '[' (String.lchop s) in
      Select (fld, parse_path' pth)
    | '[' ->
      let idx, pth = String.split (String.lchop s) "]" in
      Index (parse_index idx, parse_path' pth)
    | _ -> raise PathParseError

(** Parse a string path, but you may ignore the first dot. *)
let parse_path (s:string) : path =
  let s = String.trim s in
  try
    if String.length s = 0 then Here else begin
      let fld, pth = split '.' '[' s in
      if fld = ""
      then parse_path' pth
      else Select (fld, parse_path' pth)
    end
  with PathParseError ->
    eprintf "Error: Couldn't parse the json path '%s'\n%!" s;
    failwith "parsing"

open Json_encoding

let convert_opt name desc def: unit encoding =
  let rec convert_path = function
    | Select (s, Here) ->
      obj1 @@ dft ~title:name ~description:desc s unit () (* TODO: correct type *)
    | Select (s, path') ->
      let obj' = convert_path path' in
      obj1 @@ dft s obj' ()
    | _ -> assert false
  in
  let path = parse_path name in
  convert_path path

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

let rec convert_schema' (json: Yojson.Safe.t) opts (prefix: string): element =
  let element' ekind =
    let (desc, def) = List.assoc (BatString.lchop prefix) opts in
    (* let name = BatString.lchop @@ Filename.extension prefix in *)
    {(element ekind) with title = Some (BatString.lchop prefix); description = Some desc; default = Some (Json_repr.repr_to_any (module Json_repr.Yojson) (parse_goblint_json def))}
  in
  match json with
  | `String s ->
    element' @@ String string_specs
  | `Bool b ->
    element' @@ Boolean
  | `Int i ->
    element' @@ Number numeric_specs
  | `List xs ->
    let element_schema = match prefix with
      | ".phases" -> element (Id_ref "") (* refer to entire schema itself *)
      | _ -> element (String string_specs)
    in
    element' @@ Monomorphic_array (element_schema, array_specs)
  | `Assoc xs ->
    let properties = List.map (fun (key, value) ->
        (key, convert_schema' value opts (prefix ^ "." ^ key), false, None)
      ) xs
    in
    element @@ Object { object_specs with properties; additional_properties = None }
  | _ -> failwith (Format.asprintf "convert_schema': %a" Yojson.Safe.pp json)

let convert_schema json opts =
  try
    let sch = create @@ {(convert_schema' json opts "") with id = Some ""} in (* add id to make create defs check happy, doesn't get outputted apparently *)
    (* Format.printf "schema: %a\n" Json_schema.pp sch; *)
    (* Format.printf "schema2: %a\n" (Yojson.Safe.pretty_print ~std:true) (JS.to_json sch) *)
    Yojson.Safe.pretty_to_channel (Stdlib.open_out "schema.json") (JS.to_json sch)
  with (Json_schema.Dangling_reference u as e) ->
    Json_schema.print_error Format.err_formatter e
