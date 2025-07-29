(** Messages (e.g. warnings) presented to the user about the program from the analysis. *)

module Pretty = GoblintCil.Pretty

open GobConfig

module Category = MessageCategory

open GobResult.Syntax


module Severity =
struct
  type t =
    | Error
    | Warning
    | Info
    | Debug
    | Success
  [@@deriving eq, hash, show { with_path = false }, enum]
  (* TODO: fix ord Error: https://github.com/ocaml-ppx/ppx_deriving/issues/254 *)

  let compare x y = Stdlib.compare (to_enum x) (to_enum y)

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
  let of_yojson = function
    | `String "Error" -> Result.Ok Error
    | `String "Warning" -> Result.Ok Warning
    | `String "Info" -> Result.Ok Info
    | `String "Debug" -> Result.Ok Debug
    | `String "Success" -> Result.Ok Success
    | _ -> Result.Error "Messages.Severity.of_yojson"
end

module Location =
struct
  type t =
    | Node of Node0.t (** Location identified by a node. Strongly preferred, because output location updates incrementally. *)
    | CilLocation of CilType.Location.t (** Location identified by a literal CIL location. Strongly discouraged, because not updated incrementally. *)
  [@@deriving eq, ord, hash]

  let to_cil = function
    | Node node -> UpdateCil0.getLoc node (* use incrementally updated location *)
    | CilLocation loc -> loc

  let to_yojson x = CilType.Location.to_yojson (to_cil x)
  let of_yojson x =
    let+ loc = CilType.Location.of_yojson x in
    CilLocation loc
end

module Piece =
struct
  let context_to_yojson context = `Assoc [("tag", `Int (ControlSpecC.tag context))]

  type t = {
    loc: Location.t option; (* only *_each warnings have this, used for deduplication *)
    text: string;
    context: (ControlSpecC.t [@to_yojson context_to_yojson] [@of_yojson fun x -> Result.Error "ControlSpecC"]) option;
  } [@@deriving eq, ord, hash, yojson]

  let text_with_context {text; context; _} =
    match context with
    | Some context when GobConfig.get_bool "dbg.warn_with_context" -> text ^ " in context " ^ string_of_int (ControlSpecC.tag context) (* TODO: this is kind of useless *)
    | _ -> text
end

module MultiPiece =
struct
  type group = {
    group_text: string;
    group_loc: Location.t option;
    pieces: Piece.t list;
  } [@@deriving eq, ord, hash, yojson]
  type t =
    | Single of Piece.t
    | Group of group
  [@@deriving eq, ord, hash, yojson]

  let to_yojson = function
    | Single piece -> Piece.to_yojson piece
    | Group group -> group_to_yojson group

  let of_yojson = function
    | (`Assoc l) as json when List.mem_assoc "group_text" l ->
      let+ group = group_of_yojson json in
      Group group
    | json ->
      let+ piece = Piece.of_yojson json in
      Single piece
end

module Tag =
struct
  type t =
    | Category of Category.t
    | CWE of int
  [@@deriving eq, ord, hash]

  let pp ppf = function
    | Category category -> Format.pp_print_string ppf (Category.show category)
    | CWE n -> Format.fprintf ppf "CWE-%d" n

  let should_warn = function
    | Category category -> Category.should_warn category
    | CWE _ -> false (* TODO: options for CWEs? *)

  let to_yojson = function
    | Category category -> `Assoc [("Category", Category.to_yojson category)]
    | CWE n -> `Assoc [("CWE", `Int n)]

  let of_yojson = function
    | `Assoc [("Category", category)] ->
      let+ category = Category.of_yojson category in
      Category category
    | `Assoc [("CWE", `Int n)] -> Result.Ok (CWE n)
    | _ -> Result.Error "Messages.Tag.of_yojson"
end

module Tags =
struct
  type t = Tag.t list [@@deriving eq, ord, hash, yojson]

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
  } [@@deriving eq, ord, hash, yojson]
end

module Table =
struct
  module MH = Hashtbl.Make (Message)

  let messages_table = MH.create 113 (* messages without order for quick mem lookup *)
  let messages_list = ref [] (* messages with reverse order (for cons efficiency) *)

  let mem = MH.mem messages_table

  let add_hook: (Message.t -> unit) ref = ref (fun _ -> ())

  let add m =
    MH.replace messages_table m ();
    messages_list := m :: !messages_list;
    !add_hook m

  let to_list () =
    List.rev !messages_list (* reverse to get in addition order *)

  let to_yojson () =
    [%to_yojson: Message.t list] (to_list ())
end


let formatter = ref Format.std_formatter
let () = AfterConfig.register (fun () ->
    if !formatter == Format.std_formatter && MessageUtil.colors_on Unix.stdout then
      GobFormat.pp_set_ansi_color_tags !formatter
  )

let xml_file_name = ref ""

(** The file where everything is output *)
let out = ref stdout

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
  let pp_loc ppf = Format.fprintf ppf " @{<violet>(%a)@}" CilType.Location.pp in
  let pp_loc ppf loc =
    Format.fprintf ppf "%a" (Format.pp_print_option pp_loc) (Option.map Location.to_cil loc)
  in
  let pp_piece ppf piece =
    Format.fprintf ppf "@{<%s>%s@}%a" severity_stag (Piece.text_with_context piece) pp_loc piece.loc
  in
  let pp_quote ppf (loc: GoblintCil.location) =
    let lines = BatFile.lines_of loc.file in
    BatEnum.drop (loc.line - 1) lines; (* nosemgrep: batenum-module *)
    let lines = BatEnum.take (loc.endLine - loc.line + 1) lines in (* nosemgrep: batenum-module *)
    let lines = BatList.of_enum lines in (* nosemgrep: batenum-of_enum *)
    match lines with
    | [] -> assert false
    | [line] ->
      let prefix = BatString.slice ~last:(loc.column - 1) line in
      let middle = BatString.slice ~first:(loc.column - 1) ~last:(loc.endColumn - 1) line in
      let suffix = BatString.slice ~first:(loc.endColumn - 1) line in
      Format.fprintf ppf "%s@{<turquoise>%s@}%s" prefix middle suffix
    | first :: rest ->
      begin match BatList.split_at (List.length rest - 1) rest with
        | (middles, [last]) ->
          let first_prefix = BatString.slice ~last:(loc.column - 1) first in
          let first_middle = BatString.slice ~first:(loc.column - 1) first in
          let last_middle = BatString.slice ~last:(loc.endColumn - 1) last in
          let last_suffix = BatString.slice ~first:(loc.endColumn - 1) last in
          Format.fprintf ppf "%s@{<turquoise>%a@}%s" first_prefix (Format.pp_print_list Format.pp_print_string) (first_middle :: middles @ [last_middle]) last_suffix
        | _ -> assert false
      end
  in
  let pp_quote ppf loc =
    if get_bool "warn.quote-code" then (
      let pp_cut_quote ppf = Format.fprintf ppf "@,@[<v 0>%a@,@]" pp_quote in
      (Format.pp_print_option pp_cut_quote) ppf (Option.map Location.to_cil loc)
    )
  in
  let pp_piece ppf piece = Format.fprintf ppf "%a%a" pp_piece piece pp_quote piece.loc in
  let pp_multipiece ppf = match m.multipiece with
    | Single piece ->
      pp_piece ppf piece
    | Group {group_text; group_loc; pieces} ->
      let pp_piece2 ppf = Format.fprintf ppf "@[<v 2>%a@]" pp_piece in (* indented box for quote *)
      Format.fprintf ppf "@{<%s>%s:@}%a%a@,@[<v>%a@]" severity_stag group_text pp_loc group_loc pp_quote group_loc (Format.pp_print_list pp_piece2) pieces
  in
  Format.fprintf ppf "@[<v 2>%t %t@]\n%!" pp_prefix pp_multipiece


let add m =
  if not (Table.mem m) then (
    if not (get_bool "warn.deterministic") then
      print m;
    Table.add m
  )

let final_table: unit Table.MH.t = Table.MH.create 13

let add_final m =
  Table.MH.replace final_table m ()

let finalize () =
  if get_bool "warn.deterministic" then (
    !Table.messages_list
    |> List.sort Message.compare
    |> List.iter print
  );
  Table.MH.to_seq_keys final_table
  |> List.of_seq
  |> List.sort Message.compare
  |> List.iter (fun m ->
      print m;
      Table.add m
    )

let current_context: ControlSpecC.t option ref = ref None

let msg_context () =
  if GobConfig.get_bool "dbg.warn_with_context" then
    !current_context
  else
    None (* avoid identical messages from multiple contexts without any mention of context *)

let msg severity ?loc ?(tags=[]) ?(category=Category.Unknown) fmt =
  if !AnalysisState.should_warn && Severity.should_warn severity && (Category.should_warn category || Tags.should_warn tags) then (
    let finish doc =
      let text = GobPretty.show doc in
      let loc = match loc with
        | Some node -> Some node
        | None -> Option.map (fun node -> Location.Node node) !Node0.current_node
      in
      add {tags = Category category :: tags; severity; multipiece = Single {loc; text; context = msg_context ()}}
    in
    Pretty.gprintf finish fmt
  )
  else
    GobPretty.igprintf () fmt

let msg_noloc severity ?(tags=[]) ?(category=Category.Unknown) fmt =
  if !AnalysisState.should_warn && Severity.should_warn severity && (Category.should_warn category || Tags.should_warn tags) then (
    let finish doc =
      let text = GobPretty.show doc in
      add {tags = Category category :: tags; severity; multipiece = Single {loc = None; text; context = None}}
    in
    Pretty.gprintf finish fmt
  )
  else
    GobPretty.igprintf () fmt

let msg_group severity ?loc ?(tags=[]) ?(category=Category.Unknown) fmt =
  if !AnalysisState.should_warn && Severity.should_warn severity && (Category.should_warn category || Tags.should_warn tags) then (
    let finish doc msgs =
      let group_text = GobPretty.show doc in
      let piece_of_msg (doc, loc) =
        let text = GobPretty.show doc in
        Piece.{loc; text; context = None}
      in
      add {tags = Category category :: tags; severity; multipiece = Group {group_text; group_loc = loc; pieces = List.map piece_of_msg msgs}}
    in
    Pretty.gprintf finish fmt
  )
  else
    GobPretty.igprintf (fun msgs -> ()) fmt

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

let msg_final severity ?(tags=[]) ?(category=Category.Unknown) fmt =
  if !AnalysisState.should_warn then (
    let finish doc =
      let text = GobPretty.show doc in
      add_final {tags = Category category :: tags; severity; multipiece = Single {loc = None; text; context = None}}
    in
    Pretty.gprintf finish fmt
  )
  else
    GobPretty.igprintf () fmt


include Goblint_tracing

open Pretty

let tracel sys ?var fmt =
  let loc = !current_loc in
  let docloc sys doc =
    printtrace sys (dprintf "(%a)@?" CilType.Location.pretty loc ++ indent 2 doc);
  in
  gtrace true docloc sys var ~loc ignore fmt

let traceli sys ?var ?(subsys=[]) fmt =
  let loc = !current_loc in
  let g () = activate sys subsys in
  let docloc sys doc: unit =
    printtrace sys (dprintf "(%a)" CilType.Location.pretty loc ++ indent 2 doc);
    traceIndent ()
  in
  gtrace true docloc sys var ~loc g fmt
