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
    [@@deriving eq, show { with_path = false }]

  let hash x = Hashtbl.hash x (* variants, so this is fine *)

  let should_warn e =
    let to_string = function
      | Error -> "error"
      | Warning -> "warning"
      | Info -> "info"
      | Debug -> "debug"
      | Success -> "success"
    in
    get_bool ("warn." ^ (to_string e))
end

module Piece =
struct
  type t = {
    loc: CilType.Location.t option; (* only *_each warnings have this, used for deduplication *)
    text: string;
    context: (Obj.t [@equal fun x y -> Hashtbl.hash (Obj.obj x) = Hashtbl.hash (Obj.obj y)]) option; (* TODO: this equality is terrible... *)
    print_loc: CilType.Location.t [@equal fun _ _ -> true]; (* all warnings have this, not used for deduplication *)
  } [@@deriving eq]

  let hash {loc; text; context; print_loc} =
    7 * BatOption.map_default CilType.Location.hash 1 loc + 9 * Hashtbl.hash text + 11 * BatOption.map_default (fun c -> Hashtbl.hash (Obj.obj c)) 1 context

  let with_context msg = function
    | Some ctx when GobConfig.get_bool "dbg.warn_with_context" -> msg ^ " in context " ^ string_of_int (Hashtbl.hash ctx) (* TODO: this is kind of useless *)
    | _ -> msg

  let show {loc; text; context; print_loc} =
    with_context text context
end

module MultiPiece =
struct
  type t =
    | Single of Piece.t
    | Group of {group_text: string; pieces: Piece.t list}
    [@@ deriving eq]

  let hash = function
    | Single piece -> Piece.hash piece
    | Group {group_text; pieces} ->
      Hashtbl.hash group_text + 3 * (List.fold_left (fun xs x -> xs + Piece.hash x) 996699 pieces) (* copied from Printable.Liszt *)
end

module Tag =
struct
  type t =
    | Category of Category.t
    | CWE of int
    [@@deriving eq]

  let hash = function
    | Category category -> Category.hash category
    | CWE n -> n

  let show = function
    | Category category -> Category.show category
    | CWE n -> "[CWE-" ^ string_of_int n ^ "]"

  let should_warn = function
    | Category category -> Category.should_warn category
    | CWE _ -> false (* TODO: options for CWEs? *)
end

module Tags =
struct
  type t = Tag.t list [@@deriving eq]

  let hash tags = List.fold_left (fun xs x -> xs + Tag.hash x) 996699 tags (* copied from Printable.Liszt *)

  let show tags = List.fold_left (fun acc tag -> acc ^ Tag.show tag) "" tags

  let should_warn tags = List.exists Tag.should_warn tags
end

module Message =
struct
  type t = {
    tags: Tags.t;
    severity: Severity.t;
    multipiece: MultiPiece.t;
  } [@@deriving eq]

  let should_warn {tags; severity; _} =
    Tags.should_warn tags && Severity.should_warn severity

  let hash {tags; severity; multipiece} =
    3 * Tags.hash tags + 7 * MultiPiece.hash multipiece + 13 * Severity.hash severity
end

module MH = Hashtbl.Make (Message)
let messages_table = MH.create 113 (* messages without order for quick mem lookup *)
let messages_list = ref [] (* messages with reverse order (for cons efficiency) *)


let formatter = ref Format.std_formatter
let () = AfterConfig.register (fun () ->
    if !formatter == Format.std_formatter && MessageUtil.colors_on () then
      GobFormat.pp_set_ansi_color_tags !formatter
  )

let tracing = Config.tracing
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
  let pp_prefix = Format.dprintf "@{<%s>[%s]%s@}" severity_stag (Severity.show m.severity) (Tags.show m.tags) in
  let pp_piece ppf piece =
    Format.fprintf ppf "@{<%s>%s@} @{<violet>(%a)@}" severity_stag (Piece.show piece) CilType.Location.pp piece.print_loc
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
    if Message.should_warn m && not (MH.mem messages_table m) then (
      print m;
      MH.replace messages_table m ();
      messages_list := m :: !messages_list
    )
  )

(** Adapts old [print_group] to new message structure.
    Don't use for new (group) warnings. *)
let warn_group_old group_name errors =
  let m = Message.{tags = [Category Unknown]; severity = Warning; multipiece = Group {group_text = group_name; pieces = List.map (fun (s, loc) -> Piece.{loc = Some loc; text = s; context = None; print_loc = loc}) errors}} in
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


let msg severity ?(tags=[]) ?(category=Category.Unknown) text =
  add {tags = Category category :: tags; severity; multipiece = Single {loc = None; text; context = !current_context; print_loc = !Tracing.current_loc}}

let msg_each severity ?loc:(loc= !Tracing.current_loc) ?(tags=[]) ?(category=Category.Unknown) text =
  add {tags = Category category :: tags; severity; multipiece = Single {loc = Some loc; text; context = !current_context; print_loc = loc}}

let warn = msg Warning
let warn_each = msg_each Warning
(* TODO: error? *)
let error_each = msg_each Error
(* TODO: info *)
let debug = msg Debug
let debug_each = msg_each Debug
(* TODO: success? *)
let success_each = msg_each Success

include Tracing
