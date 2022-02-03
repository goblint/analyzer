open GobConfig
module GU = Goblintutil

module Category = MessageCategory


module Severity =
struct
  type t =
    | Error
    | Warning
    | Info
    | Debug
    | Success
  [@@deriving eq, hash, show { with_path = false }]

  let should_warn e =
    let to_string = function
      | Error -> "error"
      | Warning -> "warning"
      | Info -> "info"
      | Debug -> "debug"
      | Success -> "success"
    in
    get_bool ("warn." ^ (to_string e))

  let to_yojson x = `String (show x)
end

module Piece =
struct
  type t = {
    loc: CilType.Location.t option; (* only *_each warnings have this, used for deduplication *)
    text: string;
    context: (Obj.t [@equal fun x y -> Hashtbl.hash (Obj.obj x) = Hashtbl.hash (Obj.obj y)] [@hash fun x -> Hashtbl.hash (Obj.obj x)] [@to_yojson fun x -> `Int (Hashtbl.hash (Obj.obj x))]) option; (* TODO: this equality is terrible... *)
  } [@@deriving eq, hash, to_yojson]

  let text_with_context {text; context; _} =
    match context with
    | Some context when GobConfig.get_bool "dbg.warn_with_context" -> text ^ " in context " ^ string_of_int (Hashtbl.hash context) (* TODO: this is kind of useless *)
    | _ -> text
end

module MultiPiece =
struct
  type group = {group_text: string; pieces: Piece.t list} [@@deriving eq, hash, to_yojson]
  type t =
    | Single of Piece.t
    | Group of group
  [@@deriving eq, hash, to_yojson]

  let to_yojson = function
    | Single piece -> Piece.to_yojson piece
    | Group group -> group_to_yojson group
end

module Tag =
struct
  type t =
    | Category of Category.t
    | CWE of int
  [@@deriving eq, hash]

  let pp ppf = function
    | Category category -> Format.pp_print_string ppf (Category.show category)
    | CWE n -> Format.fprintf ppf "CWE-%d" n

  let should_warn = function
    | Category category -> Category.should_warn category
    | CWE _ -> false (* TODO: options for CWEs? *)

  let to_yojson = function
    | Category category -> `Assoc [("Category", Category.to_yojson category)]
    | CWE n -> `Assoc [("CWE", `Int n)]
end

module Tags =
struct
  type t = Tag.t list [@@deriving eq, hash, to_yojson]

  let pp =
    let pp_tag_brackets ppf tag = Format.fprintf ppf "[%a]" Tag.pp tag in
    Format.pp_print_list ~pp_sep:GobFormat.pp_print_nothing pp_tag_brackets

  let should_warn tags = List.exists Tag.should_warn tags
end

module Message =
struct
  type t = {
    tags: Tags.t;
    severity: Severity.t;
    multipiece: MultiPiece.t;
  } [@@deriving eq, hash, to_yojson]

  let should_warn {tags; severity; _} =
    Tags.should_warn tags && Severity.should_warn severity
end

module Table =
struct
  module MH = Hashtbl.Make (Message)

  let messages_table = MH.create 113 (* messages without order for quick mem lookup *)
  let messages_list = ref [] (* messages with reverse order (for cons efficiency) *)

  let mem = MH.mem messages_table

  let add m =
    MH.replace messages_table m ();
    messages_list := m :: !messages_list

  let to_yojson () =
    [%to_yojson: Message.t list] (List.rev !messages_list) (* reverse to get in addition order *)
end


let formatter = ref Format.std_formatter
let () = AfterConfig.register (fun () ->
    if !formatter == Format.std_formatter && MessageUtil.colors_on Unix.stdout then
      GobFormat.pp_set_ansi_color_tags !formatter
  )

let xml_file_name = ref ""



(*Warning files*)
let warn_race = ref stdout
let warn_safe = ref stdout
let warn_higr = ref stdout
let warn_higw = ref stdout
let warn_lowr = ref stdout
let warn_loww = ref stdout

let init_warn_files () =
  warn_race := (open_out "goblint_warnings_race.txt");
  warn_safe := (open_out "goblint_warnings_safe.txt");
  warn_higr := (open_out "goblint_warnings_highreadrace.txt");
  warn_higw := (open_out "goblint_warnings_highwriterace.txt");
  warn_lowr := (open_out "goblint_warnings_lowreadrace.txt");
  warn_loww := (open_out "goblint_warnings_lowwriterace.txt")

let get_out name alternative = match get_string "dbg.dump" with
  | "" -> alternative
  | path -> open_out (Filename.concat path (name ^ ".out"))




let print ?(ppf= !formatter) (m: Message.t) =
  let severity_stag = match m.severity with
    | Error -> "red"
    | Warning -> "yellow"
    | Info -> "blue"
    | Debug -> "white" (* non-bright white is actually some gray *)
    | Success -> "green"
  in
  let pp_prefix = Format.dprintf "@{<%s>[%a]%a@}" severity_stag Severity.pp m.severity Tags.pp m.tags in
  let pp_piece ppf piece =
    let pp_loc ppf = Format.fprintf ppf " @{<violet>(%a)@}" CilType.Location.pp in
    Format.fprintf ppf "@{<%s>%s@}%a" severity_stag (Piece.text_with_context piece) (Format.pp_print_option pp_loc) piece.loc
  in
  let pp_multipiece ppf = match m.multipiece with
    | Single piece ->
      pp_piece ppf piece
    | Group {group_text; pieces} ->
      Format.fprintf ppf "@{<%s>%s:@}@,@[<v>%a@]" severity_stag group_text (Format.pp_print_list pp_piece) pieces
  in
  Format.fprintf ppf "@[<v 2>%t %t@]\n%!" pp_prefix pp_multipiece


let add m =
  if !GU.should_warn then (
    if Message.should_warn m && not (Table.mem m) then (
      print m;
      Table.add m
    )
  )

(** Adapts old [print_group] to new message structure.
    Don't use for new (group) warnings. *)
let msg_group_race_old severity group_name errors =
  let m = Message.{tags = [Category Race]; severity; multipiece = Group {group_text = group_name; pieces = List.map (fun (s, loc) -> Piece.{loc = Some loc; text = s; context = None}) errors}} in
  add m;

  if (get_bool "ana.osek.warnfiles") then
    let print ~out = print ~ppf:(Format.formatter_of_out_channel out) in
    match (String.sub group_name 0 6) with
    | "Safely" -> print ~out:!warn_safe m
    | "Datara" -> print ~out:!warn_race m
    | "High r" -> print ~out:!warn_higr m
    | "High w" -> print ~out:!warn_higw m
    | "Low re" -> print ~out:!warn_lowr m
    | "Low wr" -> print ~out:!warn_loww m
    | _ -> ()

let current_context: Obj.t option ref = ref None (** (Control.get_spec ()) context, represented type: (Control.get_spec ()).C.t *)

let msg_context () =
  if GobConfig.get_bool "dbg.warn_with_context" then
    !current_context
  else
    None (* avoid identical messages from multiple contexts without any mention of context *)

let msg severity ?loc:(loc= !Tracing.current_loc) ?(tags=[]) ?(category=Category.Unknown) fmt =
  let finish doc =
    let text = Pretty.sprint ~width:max_int doc in
    add {tags = Category category :: tags; severity; multipiece = Single {loc = Some loc; text; context = msg_context ()}}
  in
  Pretty.gprintf finish fmt

let msg_noloc severity ?(tags=[]) ?(category=Category.Unknown) fmt =
  let finish doc =
    let text = Pretty.sprint ~width:max_int doc in
    add {tags = Category category :: tags; severity; multipiece = Single {loc = None; text; context = msg_context ()}}
  in
  Pretty.gprintf finish fmt

let msg_group severity ?(tags=[]) ?(category=Category.Unknown) fmt =
  let finish doc msgs =
    let group_text = Pretty.sprint ~width:max_int doc in
    let piece_of_msg (doc, loc) =
      let text = Pretty.sprint ~width:max_int doc in
      Piece.{loc; text; context = None}
    in
    add {tags = Category category :: tags; severity; multipiece = Group {group_text; pieces = List.map piece_of_msg msgs}}
  in
  Pretty.gprintf finish fmt

(* must eta-expand to get proper (non-weak) polymorphism for format *)
let warn ?loc = msg Warning ?loc
let warn_noloc ?tags = msg_noloc Warning ?tags
let error ?loc = msg Error ?loc
let error_noloc ?tags = msg_noloc Error ?tags
let info ?loc = msg Info ?loc
let info_noloc ?tags = msg_noloc Info ?tags
let debug ?loc = msg Debug ?loc
let debug_noloc ?tags = msg_noloc Debug ?tags
let success ?loc = msg Success ?loc
let success_noloc ?tags = msg_noloc Success ?tags

include Tracing
