open GobConfig
module GU = Goblintutil

type array_oob =
  | PastEnd
  | BeforeStart
  | Unknown
  [@@deriving eq]

type undefined_behavior =
  | ArrayOutOfBounds of array_oob
  | NullPointerDereference
  | UseAfterFree
  [@@deriving eq]

type behavior =
  | Undefined of undefined_behavior
  | Implementation
  | Machine
  [@@deriving eq]

type integer = Overflow | DivByZero [@@deriving eq]

type cast = TypeMismatch [@@deriving eq]

type category =
  | Assert
  | Behavior of behavior
  | Integer of integer
  | Race
  | Cast of cast
  | Unknown
  | Analyzer
  [@@deriving eq]

module Category =
struct
  type t = category [@@deriving eq]

  let hash x = Hashtbl.hash x (* nested variants, so this is fine *)

  module Behavior =
  struct
    type t = behavior

    let create (e: t): category = Behavior e
    let undefined e: category = create @@ Undefined e
    let implementation (): category = create @@ Implementation
    let machine (): category = create @@ Machine

    module Undefined =
    struct
      type t = undefined_behavior

      let create (e: t): category = undefined e
      let array_out_of_bounds e: category = create @@ ArrayOutOfBounds e
      let nullpointer_dereference (): category = create @@ NullPointerDereference
      let use_after_free (): category = create @@ UseAfterFree

      module ArrayOutOfBounds =
      struct
        type t = array_oob

        let create (e: t): category = array_out_of_bounds e
        let past_end (): category = create PastEnd
        let before_start (): category = create BeforeStart
        let unknown (): category = create Unknown

        let from_string_list (s: string list): category =
          match s with
          | [] -> Unknown
          | h :: t -> match h with
            | "past_end" -> past_end ()
            | "before_start" -> before_start ()
            | "unknown" -> unknown ()
            | _ -> Unknown

        let show (e: t): string =
          match e with
          | PastEnd -> "PastEnd]" ^ " Index is past the end of the array."
          | BeforeStart -> "BeforeStart]" ^ " Index is before start of the array."
          | Unknown -> "Unknown]" ^ " Not enough information about index."
      end

      let from_string_list (s: string list): category =
        match s with
        | [] -> Unknown
        | h :: t -> match h with
          | "array_out_of_bounds" -> ArrayOutOfBounds.from_string_list t
          | "nullpointer_dereference" -> nullpointer_dereference ()
          | "use_after_free" -> use_after_free ()
          | _ -> Unknown

      let show (e: t): string =
        match e with
        | ArrayOutOfBounds e -> "ArrayOutOfBounds > "^(ArrayOutOfBounds.show e)
        | NullPointerDereference -> "NullPointerDereference]"
        | UseAfterFree -> "UseAfterFree]"
    end

    let from_string_list (s: string list): category =
      match s with
      | [] -> Unknown
      | h :: t -> ();match h with
        | "undefined" -> Undefined.from_string_list t
        | "implementation" -> implementation ()
        | "machine" -> machine ()
        | _ -> Unknown

    let show (e: t): string =
      match e with
      | Undefined u -> "Undefined > "^(Undefined.show u)
      | Implementation -> "Implementation > "
      | Machine -> "Machine > "
  end

  module Integer =
  struct
    type t = integer

    let create (e: t): category = Integer e
    let overflow (): category = create Overflow
    let div_by_zero (): category = create DivByZero

    let from_string_list (s: string list): category =
      match s with
      | [] -> Unknown
      | h :: t -> ();match h with
        | "overflow" -> overflow ()
        | "div_by_zero" -> div_by_zero ()
        | _ -> Unknown

    let show (e: t): string =
      match e with
      | Overflow -> "Overflow]"
      | DivByZero -> "DivByZero]"
  end

  module Cast =
  struct
    type t = cast

    let create (e: t): category = Cast e
    let type_mismatch (): category = create TypeMismatch

    let from_string_list (s: string list): category =
      match s with
      | [] -> Unknown
      | h :: t -> ();match h with
        | "type_mismatch" -> type_mismatch ()
        | _ -> Unknown

    let show (e: t): string =
      match e with
      | TypeMismatch -> "TypeMismatch]"
  end

  let should_warn e =
    let to_string e =
      match e with
      | Assert -> "assert"
      | Behavior _ -> "behavior"
      | Integer _ -> "integer"
      | Race -> "race"
      | Cast _ -> "cast"
      | Unknown -> "unknown"
      | Analyzer -> "analyzer"
    in get_bool ("warn." ^ (to_string e))

  let show e =
    match e with
    | Assert -> "[Assert]"
    | Behavior x -> "[Behavior > " ^ (Behavior.show x)
    | Integer x -> "[Integer > " ^ (Integer.show x)
    | Race -> "[Race]"
    | Cast x -> "[Cast > " ^ (Cast.show x)
    | Unknown -> "[Unknown]"
    | Analyzer -> "[Analyzer]"

  let from_string_list (s: string list) =
    match s with
    | [] -> Unknown
    | h :: t -> match h with
      | "assert" -> Assert
      | "behavior" -> Behavior.from_string_list t
      | "integer" -> Integer.from_string_list t
      | "race" -> Race
      | "cast" -> Cast.from_string_list t
      | "analyzer" -> Analyzer
      | _ -> Unknown
end


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

  let show = function
    | Single piece -> Piece.show piece
    | Group {group_text; pieces} ->
      List.fold_left (fun acc piece -> acc ^ "\n  " ^ Piece.show piece) group_text pieces
end

module Message =
struct
  type t = {
    category: Category.t; (* TODO: make list of tags *)
    severity: Severity.t;
    multipiece: MultiPiece.t;
  } [@@deriving eq]

  let should_warn {category; severity; _} =
    Category.should_warn category && Severity.should_warn severity

  let hash {category; severity; multipiece} =
    3 * Category.hash category + 7 * MultiPiece.hash multipiece + 13 * Severity.hash severity

  let show {category; severity; multipiece} =
    let msg = "[" ^ Severity.show severity ^ "]" ^ (Category.show category)^" "^ MultiPiece.show multipiece in
    msg
end

module MH = Hashtbl.Make (Message)
let messages_table = MH.create 113 (* messages without order for quick mem lookup *)
let messages_list = ref [] (* messages with reverse order (for cons efficiency) *)


let warn_out = ref stdout
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

let colors_on () = (* use colors? *)
  let c = get_string "colors" in
  c = "always" || c = "auto" && Unix.(isatty stdout)

let colorize ?on:(on=colors_on ()) msg =
  let colors = [("gray", "30"); ("red", "31"); ("green", "32"); ("yellow", "33"); ("blue", "34");
                ("violet", "35"); ("turquoise", "36"); ("white", "37"); ("reset", "0;00")] in
  let replace (color,code) =
    let modes = [(fun x -> x), "0" (* normal *); String.uppercase_ascii, "1" (* bold *)] in
    List.fold_right (fun (f,m) -> Str.global_replace (Str.regexp ("{"^f color^"}")) (if on then "\027["^m^";"^code^"m" else "")) modes
  in
  let msg = List.fold_right replace colors msg in
  msg^(if on then "\027[0;0;00m" else "") (* reset at end *)


let print ?(out= !warn_out) (m: Message.t) =
  let show_piece piece =
    Piece.show piece ^ " {violet}(" ^ CilType.Location.show piece.print_loc ^ ")"
  in
  let severity_color = match m.severity with
    | Error -> "{red}"
    | Warning -> "{yellow}"
    | Info -> "{blue}"
    | Debug -> "{white}" (* non-bright white is actually some gray *)
    | Success -> "{green}"
  in
  let prefix = severity_color ^ "[" ^ Severity.show m.severity ^ "]" ^ Category.show m.category in
  match m.multipiece with
  | Single piece ->
    Printf.fprintf out "%s\n%!" (colorize @@ prefix ^ " " ^ show_piece piece)
  | Group {group_text; pieces} ->
    Printf.fprintf out "%s\n%!" (colorize @@ prefix ^ " " ^ List.fold_left (fun acc piece -> acc ^ "\n  " ^ severity_color ^ show_piece piece) (group_text ^ ":") pieces)

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
  let m = Message.{category = Unknown; severity = Warning; multipiece = Group {group_text = group_name; pieces = List.map (fun (s, loc) -> Piece.{loc = Some loc; text = s; context = None; print_loc = loc}) errors}} in
  add m;

  if (get_bool "ana.osek.warnfiles") then
    match (String.sub group_name 0 6) with
    | "Safely" -> print ~out:!warn_safe m
    | "Datara" -> print ~out:!warn_race m
    | "High r" -> print ~out:!warn_higr m
    | "High w" -> print ~out:!warn_higw m
    | "Low re" -> print ~out:!warn_lowr m
    | "Low wr" -> print ~out:!warn_loww m
    | _ -> ()

let current_context: Obj.t option ref = ref None (** (Control.get_spec ()) context, represented type: (Control.get_spec ()).C.t *)


let msg severity ?(category=Unknown) text =
  add {category; severity; multipiece = Single {loc = None; text; context = !current_context; print_loc = !Tracing.current_loc}}

let msg_each severity ?loc:(loc= !Tracing.current_loc) ?(category=Unknown) text =
  add {category; severity; multipiece = Single {loc = Some loc; text; context = !current_context; print_loc = loc}}

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
