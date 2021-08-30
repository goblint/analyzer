open Cil
open Pretty
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

type warning =
  | Behavior of behavior
  | Integer of integer
  | Race
  | Cast of cast
  | Unknown
  | Analyzer
  [@@deriving eq]

module Warning =
struct
  type t = warning [@@deriving eq]

  let hash x = Hashtbl.hash x (* nested variants, so this is fine *)

  module Behavior =
  struct
    type t = behavior

    let create (e: t): warning = Behavior e
    let undefined e: warning = create @@ Undefined e
    let implementation (): warning = create @@ Implementation
    let machine (): warning = create @@ Machine

    module Undefined =
    struct
      type t = undefined_behavior

      let create (e: t): warning = undefined e
      let array_out_of_bounds e: warning = create @@ ArrayOutOfBounds e
      let nullpointer_dereference (): warning = create @@ NullPointerDereference
      let use_after_free (): warning = create @@ UseAfterFree

      module ArrayOutOfBounds =
      struct
        type t = array_oob

        let create (e: t): warning = array_out_of_bounds e
        let past_end (): warning = create PastEnd
        let before_start (): warning = create BeforeStart
        let unknown (): warning = create Unknown

        let from_string_list (s: string list): warning =
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

      let from_string_list (s: string list): warning =
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

    let from_string_list (s: string list): warning =
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

    let create (e: t): warning = Integer e
    let overflow (): warning = create Overflow
    let div_by_zero (): warning = create DivByZero

    let from_string_list (s: string list): warning =
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

    let create (e: t): warning = Cast e
    let type_mismatch (): warning = create TypeMismatch

    let from_string_list (s: string list): warning =
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
      | Behavior _ -> "behavior"
      | Integer _ -> "integer"
      | Race -> "race"
      | Cast _ -> "cast"
      | Unknown -> "unknown"
      | Analyzer -> "analyzer"
    in get_bool ("warn." ^ (to_string e))

  let show e =
    match e with
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
    [@@deriving eq, show { with_path = false }]

  let hash x = Hashtbl.hash x (* variants, so this is fine *)

  let should_warn e =
    let to_string = function
      | Error -> "error"
      | Warning -> "warning"
      | Info -> "info"
      | Debug -> "debug"
    in
    get_bool ("warn." ^ (to_string e))
end

module Message =
struct
  type t = {
    warn_type: Warning.t; (* TODO: make list of tags *)
    severity: Severity.t;
    loc: CilType.Location.t option; (* only *_each warnings have this, used for deduplication *)
    text: string;
    context: (Obj.t [@equal fun x y -> Hashtbl.hash (Obj.obj x) = Hashtbl.hash (Obj.obj y)]) option; (* TODO: this equality is terrible... *)
    print_loc: CilType.Location.t [@equal fun _ _ -> true]; (* all warnings have this, not used for deduplication *)
  } [@@deriving eq]

  let should_warn {warn_type; severity; _} =
    Warning.should_warn warn_type && Severity.should_warn severity

  let hash {warn_type; severity; loc; text; context; print_loc} =
    3 * Warning.hash warn_type + 7 * BatOption.map_default CilType.Location.hash 1 loc + 9 * Hashtbl.hash text + 11 * BatOption.map_default (fun c -> Hashtbl.hash (Obj.obj c)) 1 context + 13 * Severity.hash severity

  let with_context msg = function
    | Some ctx when GobConfig.get_bool "dbg.warn_with_context" -> msg ^ " in context " ^ string_of_int (Hashtbl.hash ctx) (* TODO: this is kind of useless *)
    | _ -> msg

  let show {warn_type; severity; loc; text; context; print_loc} =
    let msg = "[" ^ Severity.show severity ^ "]" ^ (Warning.show warn_type)^(if text != "" then " "^text else "") in
    let msg = with_context msg context in
    msg
end

module MH = Hashtbl.Make (Message)
let messages_table = MH.create 113 (* messages without order for quick mem lookup *)
let messages_list = ref [] (* messages with reverse order (for cons efficiency) *)


exception Bailure of string
let bailwith s = raise (Bailure s)

let warning_table : [`text of string * location | `group of string * ((string * location) list)] list ref = ref []
let warn_out = ref stdout
let tracing = Config.tracing
let xml_file_name = ref ""

let push_warning w =
  warning_table := w :: !warning_table


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

let print_msg msg loc =
  let msgc = colorize msg in
  let msg  = colorize ~on:false msg in
  push_warning (`text (msg, loc));
  let color = if colors_on () then "{violet}" else "" in
  let s = Printf.sprintf "%s %s(%s)" msgc color (CilType.Location.show loc) in
  Printf.fprintf !warn_out "%s\n%!" (colorize s)


let print_group group_name errors =
  (* Add warnings to global warning list *)
  push_warning (`group (group_name, errors));
  let f (msg,loc): doc = Pretty.dprintf "%s (%a)" msg CilType.Location.pretty loc in
  if (get_bool "ana.osek.warnfiles") then begin
    match (String.sub group_name 0 6) with
    | "Safely" -> ignore (Pretty.fprintf !warn_safe "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "Datara" -> ignore (Pretty.fprintf !warn_race "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "High r" -> ignore (Pretty.fprintf !warn_higr "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "High w" -> ignore (Pretty.fprintf !warn_higw "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "Low re" -> ignore (Pretty.fprintf !warn_lowr "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "Low wr" -> ignore (Pretty.fprintf !warn_loww "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | _ -> ()
  end;
  ignore (Pretty.fprintf !warn_out "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)


let warn_all m =
  if !GU.should_warn then (
    if Message.should_warn m && not (MH.mem messages_table m) then (
      print_msg (Message.show m) m.print_loc;
      MH.replace messages_table m ();
      messages_list := m :: !messages_list
    )
  )

let current_context: Obj.t option ref = ref None (** (Control.get_spec ()) context, represented type: (Control.get_spec ()).C.t *)

let msg_internal severity ~msg (warning: Warning.t) =
  warn_all {warn_type = warning; severity; loc = None; text = msg; context = !current_context; print_loc = !Tracing.current_loc}

let msg_internal_with_loc severity ?loc:(loc= !Tracing.current_loc) ~msg (warning: Warning.t) =
  warn_all {warn_type = warning; severity; loc = Some loc; text = msg; context = !current_context; print_loc = loc}

let warn_internal = msg_internal Warning
let warn_internal_with_loc = msg_internal_with_loc Warning

let warn ~msg ?warning:(warning=Unknown) () =
  warn_internal ~msg warning

let warn_each ?loc ~msg ?warning:(warning=Unknown) () =
  warn_internal_with_loc ?loc ~msg warning

let error_internal_with_loc = msg_internal_with_loc Error

let error_each ?loc ~msg ?warning:(warning=Unknown) () =
  error_internal_with_loc ?loc ~msg warning

let debug_internal = msg_internal Debug
let debug_internal_with_loc = msg_internal_with_loc Debug

let debug msg =
  debug_internal ~msg @@ Unknown

let debug_each msg =
  debug_internal_with_loc ~msg @@ Unknown

include Tracing
