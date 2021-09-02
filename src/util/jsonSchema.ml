(** A simpler schema than http://json-schema.org *)

open Prelude
open Json

(** type of a [jvalue] *)
type jtype =
  | JBool | JInt | JNum | JNull | JString
  | JArray of jarray
  | JObj   of jobj

(** a schema for a [jvalue] *)
and jschema =
  {         sid      : string option (** identificator *)
  ;         sdescr   : string option (** description  *)
  ;         stype    : jtype  option (** a type with possibly extra information *)
  ;         sdefault : Yojson.Safe.t option (** the default value *)
  ; mutable saddenum : jschema list  (** additional schemata that were loaded separately *)
  }

(** extra schema information for an array [jvalue] *)
and jarray =
  { sitem : jschema (** the schema for all array elements *)
  }

(** extra schema information for an object [jvalue] *)
and jobj  =
  { sprops           : (string * jschema) list (** properties *)
  ; spatternprops    : (string * jschema) list (** regular expression properties *)
  ; sadditionalprops : bool                    (** are all properties acounted in the schema *)
  ; srequired        : string list             (** list of required properties *)
  }

(** An exception that indicates that some [jschema] is was invalid. *)
exception JsonSchemaMalformed of string

(** An exception that indicates that some [jvalue] was not valid according to some [jschema]. *)
exception JsonMalformed       of string

(** Translate a [jschema] to a [jvalue]. *)
let rec toJson (s:jschema) : jvalue =
  let typeToJson fields = function
    | JBool    -> fields := ("type",Build.string "boolean") :: !fields
    | JInt     -> fields := ("type",Build.string "integer") :: !fields
    | JNum     -> fields := ("type",Build.string "number" ) :: !fields
    | JNull    -> fields := ("type",Build.string "null"   ) :: !fields
    | JString  -> fields := ("type",Build.string "string" ) :: !fields
    | JArray x ->
      fields := ("type",Build.string "string") :: !fields;
      fields := ("item",toJson x.sitem) :: !fields
    | JObj   x ->
      let prop  = List.map (fun (x,y) -> (x,toJson y)) x.sprops in
      let pprop = List.map (fun (x,y) -> (x,toJson y)) x.spatternprops in
      fields := ("type"             , Build.string "object"                           ) :: !fields;
      fields := ("properties"       , Build.objekt prop                               ) :: !fields;
      fields := ("patternproperties", Build.objekt pprop                              ) :: !fields;
      fields := ("patternProperties", Build.bool x.sadditionalprops                   ) :: !fields;
      fields := ("required"         , Build.array @@ List.map Build.string x.srequired) :: !fields
  in
  let fields = ref [] in
  Option.may (fun id -> fields := ("id"         ,Build.string id) :: !fields) s.sid;
  Option.may (fun ds -> fields := ("description",Build.string ds) :: !fields) s.sdescr;
  Option.may (fun de -> fields := ("default"    ,de             ) :: !fields) (Option.map Json.of_yojson s.sdefault);
  Option.may (fun t  -> typeToJson fields t                                 ) s.stype;
  Build.objekt !fields

(** Collect all ids from the [jschema]. *)
let collectIds (s:jschema) : string list =
  let acc = ref [] in
  let rec f_sc s =
    let add_id id =
      if List.mem id !acc
      then raise (JsonSchemaMalformed "collectIds")
      else acc := id :: !acc
    in
    Option.may add_id s.sid;
    List.iter f_sc s.saddenum;
    Option.may f_ty s.stype
  and f_ty = function
    | JBool | JInt | JNum  | JNull | JString  -> ()
    | JArray a -> f_sc a.sitem
    | JObj   o ->
      List.iter (f_sc % snd) o.sprops;
      List.iter (f_sc % snd) o.spatternprops;
  in f_sc s; !acc

(** Call to [validate s v] validates the [jvalue] [v] in the [jschema] [s].
    Invalidness is communicated using the exception [JsonMalformed]. *)
let validate (s:jschema) (v:Yojson.Safe.t) : unit =
  let matches x y = Str.string_match (Str.regexp x) y 0 in
  let rec assoc_regex k = function
    | [] -> raise Not_found
    | (k',c)::xs when matches k' k -> c
    | _::xs -> assoc_regex k xs
  in
  let err n r = raise (JsonMalformed ("Json Validate: "^n^" is not "^r)) in
  let rec f_sc n v s =
    Option.may (f_ty n v) s.stype;
    List.iter  (f_sc n v) s.saddenum
  and f_ty (n:string) (v:Yojson.Safe.t) (t:jtype) =
    match (t,v) with
    | JBool, `Bool _ -> ()
    | JBool,_      -> err n "of type boolean."
    | JInt, `Int _
    | JInt, `Intlit _ -> ()
    | JInt, _      -> err n "of type integer."
    | JNum, `Int _
    | JNum, `Intlit _
    | JNum, `Float _ -> ()
    | JNum, _      -> err n "of type number."
    | JNull, `Null  -> ()
    | JNull, _     -> err n "of type null."
    | JString, `String _ -> ()
    | JString, _   -> err n "of type string."
    | JArray a, `List b -> List.iter (fun x -> f_sc ("element of "^n) x a.sitem) b
    | JArray _, _  -> err n "of type array."
    | JObj o, `Assoc i ->
      List.iter (one_map o) i;
      let r = List.filter (not % flip List.mem_assoc i) o.srequired in
      if r<>[] then err (List.hd r) "present."
    | JObj o, _    -> err n "of type object."
  and one_map o (k, v) =
    try
      f_sc k v (List.assoc k o.sprops)
    with Not_found ->
    try
      f_sc k v (assoc_regex k o.spatternprops)
    with Not_found ->
      if not o.sadditionalprops then err k "in the mapping."
  in
  f_sc "root" v s

(** Convert a [jvalue] to a [jschema] if possible. Raise [JsonSchemaMalformed] otherwise. *)
let rec fromJson (jv: Yojson.Safe.t) : jschema =
  let open Yojson.Safe.Util in
  let jarrayFromJson jv =
    let item = try jv |> member "item" with Yojson.Safe.Util.Type_error _ -> raise (JsonSchemaMalformed "jarrayFromJson") in
    { sitem = fromJson item }
  in
  let jobjFromJson jv =
    let addit =
      try jv |> member "additionalProps" |> to_bool
      with Yojson.Safe.Util.Type_error _ -> raise (JsonSchemaMalformed "jobjFromJson.addit")
    in
    let req   =
      try jv |> member "required" |> to_list |> List.map to_string
      with Yojson.Safe.Util.Type_error _ -> []
    in
    let props =
      try jv |> member "properties" |> to_assoc |> List.map (Tuple2.map2 fromJson)
      with Yojson.Safe.Util.Type_error _ -> []
    in
    let pprops =
      try jv |> member "patternProperties" |> to_assoc |> List.map (Tuple2.map2 fromJson)
      with Yojson.Safe.Util.Type_error _ -> []
    in
    { sprops           = props
    ; spatternprops    = pprops
    ; sadditionalprops = addit
    ; srequired        = req
    }
  in
  let typeFromJson jv =
    let typ = try Some (jv |> member "type" |> to_string) with Yojson.Safe.Util.Type_error _ -> None in
    Option.bind typ @@ fun typ ->
    match typ with
    | "string"  -> Some JString
    | "boolean" -> Some JBool
    | "integer" -> Some JInt
    | "number"  -> Some JNum
    | "null"    -> Some JNull
    | "array"   -> Some (JArray (jarrayFromJson jv))
    | "object"  -> Some (JObj   (jobjFromJson   jv))
    | _ -> raise (JsonSchemaMalformed "typeFromJson")
  in
  let id     = try Some (jv |> member "id" |> to_string) with Yojson.Safe.Util.Type_error _ -> None in
  let descr  = try Some (jv |> member "description" |> to_string) with Yojson.Safe.Util.Type_error _ -> None in
  let def    = jv |> member "default" |> to_option Fun.id in
  let typ    = typeFromJson jv in
  let r =
    { sid      = id
    ; sdescr   = descr
    ; stype    = typ
    ; sdefault = def
    ; saddenum = []
    }
  in
  Option.may (validate r) def; r

(** Call to [addenum x y] extends [x] with [y] at the id [Option.get y.sid] *)
let addenum (r:jschema) (l:jschema) =
  let addingJson s id l =
    let rec f_sc s =
      if Some id = s.sid then
        s.saddenum <- l :: s.saddenum
      else begin
        List.iter f_sc s.saddenum;
        Option.may f_ty s.stype
      end
    and f_ty = function
      | JBool | JInt | JNum  | JNull | JString  -> ()
      | JArray a -> f_sc a.sitem
      | JObj   o ->
        List.iter (f_sc % snd) o.sprops;
        List.iter (f_sc % snd) o.spatternprops;
    in f_sc r
  in
  match l.sid with
  | None -> raise (JsonSchemaMalformed "addenum")
  | Some id ->
    if not @@ List.mem id @@ collectIds r then
      raise (JsonSchemaMalformed "addenum")
    else
      addingJson r id l
