(**
   New, untyped, path-based configuration subsystem.

   {v
  path' ::== \epsilon              (*  *)
           | . <field-name> path'  (* field access *)
           | [ <index-nr> ] path'  (* array index access *)
           | [ + ] path'           (* cons to array *)
           | [ - ] path'           (* cons away from array *)
           | [ * ] path'           (* reset array *)

  path ::==              path'     (*  *)
          | <field_name> path'     (* you can leave out the first dot *)
  v}

   All functions [failwith] on error. Warnings are generated in [verbose] mode.

   There is a "conf" [trace] option that traces setting.
*)

open Prelude
open Tracing
open Printf

exception ConfigError of string

let building_spec = ref false


module Validator = JsonSchema.Validator (struct let schema = Options.schema end)
module ValidatorRequireAll = JsonSchema.Validator (struct let schema = Options.require_all end)

(** The type for [gobConfig] module. *)
module type S =
sig
  (** Get JSON value at a given path. *)
  val get_json : string -> Yojson.Safe.t

  (** Directly set a JSON value; the result must conform to the schema. *)
  val set_json : string -> Yojson.Safe.t -> unit

  (** Equivalent to [get_json ""]. *)
  val get_conf : unit -> Yojson.Safe.t

  (** Equivalent to [set_conf ""]. *)
  val set_conf : Yojson.Safe.t -> unit

  (** Functions to query conf variable of type int. *)
  val get_int    : string -> int

  (** Functions to modify conf variables of type int. *)
  val set_int    : string -> int    -> unit

  (** Functions to query conf variable of type bool. *)
  val get_bool   : string -> bool

  (** Functions to modify conf variables of type bool. *)
  val set_bool   : string -> bool   -> unit

  (** Functions to query conf variable of type string. *)
  val get_string : string -> string

  (** Functions to modify conf variables of type string. *)
  val set_string : string -> string -> unit

  (** Functions to modify conf variables by trying to parse the value.
      The second argument must be valid Json except single quotes represent double quotes. *)
  val set_auto   : string -> string -> unit

  (** Get a list of values *)
  val get_list : string -> Yojson.Safe.t list

  (** Get a list of strings *)
  val get_string_list : string -> string list

  (** Set a list of values *)
  val set_list : string -> Yojson.Safe.t list -> unit

  (** Write the current configuration to [filename] *)
  val write_file: Fpath.t -> unit

  (** Merge configurations from a file with current. *)
  val merge_file : Fpath.t -> unit

  (** Merge configurations from a JSON object with current. *)
  val merge : Yojson.Safe.t -> unit

  (** Check whether modification of configuration is currently allowed. *)
  val is_immutable : unit -> bool

  (** Run the given computation with modification to configuration disabled. *)
  val with_immutable_conf : (unit -> 'a) -> 'a
end

(** The implementation of the [gobConfig] module. *)
module Impl : S =
struct
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


  let show_path p =
    let rec helper = function
      | Here             -> []
      | Select (s, p)    -> Printf.sprintf ".%s" s :: helper p
      | Index (Int i, p) -> Printf.sprintf "[%d]" i :: helper p
      | Index (App, p)   -> "[+]" :: helper p
      | Index (Rem, p)   -> "[-]" :: helper p
      | Index (New, p)   -> "[*]" :: helper p
    in
    String.concat "" (helper p)

  (** raise when an attempt is made to modify the configuration while it is immutable *)
  exception Immutable of path

  let () =
    Printexc.register_printer @@
    function
    | Immutable p -> Some (Printf.sprintf "Immutable(%s)" (show_path p))
    | _ -> None

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

  (** Parse an index. *)
  let parse_index s =
    try if s = "+" then App
      else if s = "*" then New
      else if s = "-" then Rem
      else Int (int_of_string s)
    with Failure _ -> raise PathParseError

  (** Parse a string path. *)
  let rec parse_path' (s:string) : path =
    if String.length s = 0 then Here else
      match s.[0] with
      | '.' ->
        let fld, pth = split '.' '[' (String.lchop s) in
        Select (fld, parse_path' pth)
      | '[' ->
        let idx, pth = String.split (String.lchop s) ~by:"]" in
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
      Logs.error "Error: Couldn't parse the json path '%s'\n%!" s;
      failwith "parsing"

  (** Here we store the actual configuration. *)
  let json_conf : Yojson.Safe.t ref = ref `Null

  (** Helper function to print the conf using [printf "%t"] and alike. *)
  let print ch : unit =
    GobYojson.print ch !json_conf
  let write_file filename = File.with_file_out (Fpath.to_string filename) print

  (** Main function to receive values from the conf. *)
  let rec get_value (o : Yojson.Safe.t) pth =
    match o, pth with
    | o, Here -> o
    | `Assoc m, Select (key,pth) ->
      begin
        try get_value (List.assoc key m) pth
        with Not_found ->
        try get_value (List.assoc Options.defaults_additional_field m) pth (* if schema specifies additionalProperties, then use the default from that *)
        with Not_found -> raise ConfTypeError
      end
    | `List a, Index (Int i, pth) ->
      begin
        try get_value (List.at a i) pth
        with Invalid_argument _ -> raise ConfTypeError
      end
    | _, _ -> raise ConfTypeError

  (** Recursively create the value for some new path. *)
  let rec create_new v = function
    | Here -> v
    | Select (key,pth) -> `Assoc [(key,create_new v pth)]
    | Index (_, pth) -> `List [create_new v pth]

  (** Helper function to decide if types in the json conf have changed. *)
  let json_type_equals x y =
    match x, y with
    | `String _, `String _
    | `Int _, `Int _
    | `Int _, `Intlit _
    | `Intlit _, `Int _
    | `Intlit _, `Intlit _
    | `Assoc _, `Assoc _
    | `List  _, `List  _
    | `Bool _ , `Bool _
    | `Null    , `Null     -> true
    (* TODO: other Yojson cases *)
    | _                  -> false

  exception TypeError of Yojson.Safe.t * Yojson.Safe.t

  let () = Printexc.register_printer (function
      | TypeError (o, new_v) ->
        Some (sprintf2 "GobConfig.Impl.TypeError (%a, %a)" GobYojson.print o GobYojson.print new_v)
      | _ -> None (* for other exceptions *)
    )

  (** (Global) flag to disallow modification of the configuration. *)
  let immutable = ref false

  let set_immutable = (:=) immutable

  let is_immutable () = !immutable

  let with_immutable_conf f =
    (* allow nesting *)
    if is_immutable () then f ()
    else (
      set_immutable true;
      Fun.protect ~finally:(fun () -> set_immutable false) f
    )

  (** The main function to write new values into the conf. Use [set_value] to properly invalidate cache and check immutability. *)
  let unsafe_set_value v o orig_pth =
    let rec set_value v o pth =
      match o, pth with
      | `Assoc m, Select (key,pth) ->
        let rec modify = function
          | [] ->
            [(key, create_new v pth)] (* create new key, validated by schema *)
          | (key', v') :: kvs when key' = key ->
            (key, set_value v v' pth) :: kvs
          | (key', v') :: kvs ->
            (key', v') :: modify kvs
        in
        `Assoc (modify m)
      | `List a, Index (Int i, pth) ->
        `List (List.modify_at i (fun o -> set_value v o pth) a)
      | `List a, Index (App, pth) ->
        `List (a @ [create_new v pth])
      | `List a, Index (Rem, pth) ->
        let original_list = a in
        let excluded_elem = create_new v pth in
        let filtered_list =
          List.filter (fun elem ->
              match (elem, excluded_elem) with
              | (`String s1, `String s2) -> not (String.equal s1 s2)
              | (_, _) -> failwith "At the moment it's only possible to remove a string from an array."
            ) original_list
        in
        `List filtered_list
      | `List _, Index (New, pth) ->
        `List [create_new v pth]
      | `Null, _ ->
        create_new v pth
      | _ ->
        let new_v = create_new v pth in
        if not (json_type_equals o new_v) then
          raise (TypeError (o, new_v));
        new_v
    in
    o := set_value v !o orig_pth;
    Validator.validate_exn !json_conf

  (** Helper function for reading values. Handles error messages. *)
  let get_path_string f st =
    try
      let st = String.trim st in
      let x = get_value !json_conf (parse_path st) in
      if tracing then trace "conf-reads" "Reading '%s', it is %a.\n" st GobYojson.pretty x;
      try f x
      with Yojson.Safe.Util.Type_error (s, _) ->
        Logs.error "The value for '%s' has the wrong type: %s\n" st s;
        failwith "get_path_string"
    with ConfTypeError ->
      Logs.Batteries.error "Cannot find value '%s' in\n%t\nDid You forget to add default values to options.schema.json?\n"
        st print;
      failwith "get_path_string"
  let get_json : string -> Yojson.Safe.t = get_path_string Fun.id
  let get_conf () = get_json ""

  (** Convenience functions for reading values. *)
  (* memoize for each type with BatCache: *)
  let memo gen = BatCache.make_ht ~gen ~init_size:5 (* uses hashtable; fine since our options are bounded *)
  let memog f = memo @@ get_path_string f

  let memo_int    = memog Yojson.Safe.Util.to_int
  let memo_bool   = memog Yojson.Safe.Util.to_bool
  let memo_string = memog Yojson.Safe.Util.to_string
  let memo_list   = memog Yojson.Safe.Util.to_list

  let drop_memo ()  =
    (* The explicit polymorphism is needed to make it compile *)
    let drop:'a. (string,'a) BatCache.manual_cache -> _ = fun m ->
      let r = m.enum () in
      BatEnum.force r; BatEnum.iter (fun (k,v) -> m.del k) r
    in
    drop memo_int; drop memo_bool; drop memo_string; drop memo_list

  let wrap_get f x =
    (* self-observe options, which Spec construction depends on *)
    if !building_spec && Tracing.tracing then Tracing.trace "config" "get during building_spec: %s\n" x;
    (* TODO: blacklist such building_spec option from server mode modification since it will have no effect (spec is already built) *)
    f x

  let get_int    = wrap_get memo_int.get
  let get_bool   = wrap_get memo_bool.get
  let get_string = wrap_get memo_string.get
  let get_list   = wrap_get memo_list.get
  let get_string_list = List.map Yojson.Safe.Util.to_string % get_list

  (** Helper functions for writing values. *)

  (** Sets a value, preventing changes when the configuration is immutable and invalidating the cache. *)
  let set_value v o pth =
    if is_immutable () then raise (Immutable pth);
    drop_memo ();
    unsafe_set_value v o pth

  (** Helper function for writing values. Handles the tracing. *)
  let set_path_string st v =
    if tracing then trace "conf" "Setting '%s' to %a.\n" st GobYojson.pretty v;
    set_value v json_conf (parse_path st)

  let set_json st j =
    (* can't validate before updating:
       JSON inserted somewhere else than the root doesn't need to conform to the schema *)
    set_path_string st j;
    ValidatorRequireAll.validate_exn !json_conf
  let set_conf = set_json ""

  (** Convenience functions for writing values. *)
  let set_int    st i = set_path_string st (`Int i)
  let set_bool   st i = set_path_string st (`Bool i)
  let set_string st i = set_path_string st (`String i)
  let set_list   st l = set_path_string st (`List l)

  (** The ultimate convenience function for writing values. *)
  let one_quote = Str.regexp "\'"
  let set_auto st s =
    try
      try
        let s' = Str.global_replace one_quote "\"" s in
        let v = Yojson.Safe.from_string s' in
        set_path_string st v
      with Yojson.Json_error _ | TypeError _ ->
        set_string st s
    with e ->
      Logs.error "Cannot set %s to '%s'.\n" st s;
      raise e

  let merge json =
    Validator.validate_exn json;
    set_conf (GobYojson.merge !json_conf json)

  (** Merge configurations form a file with current. *)
  let merge_file fn =
    let cwd = Fpath.v (Sys.getcwd ()) in
    let config_dirs = cwd :: Goblint_sites.conf in
    let file = List.find_map_opt (fun custom_include_dir ->
        let path = Fpath.append custom_include_dir fn in
        if Sys.file_exists (Fpath.to_string path) then
          Some path
        else
          None
      ) config_dirs
    in
    match file with
    | Some fn ->
      let v = Yojson.Safe.from_channel % BatIO.to_input_channel |> File.with_file_in (Fpath.to_string fn) in
      merge v;
      if tracing then trace "conf" "Merging with '%a', resulting\n%a.\n" GobFpath.pretty fn GobYojson.pretty !json_conf
    | None -> raise (Sys_error (Printf.sprintf "%s: No such file or diretory" (Fpath.to_string fn)))
end

include Impl

let () = set_conf Options.defaults
