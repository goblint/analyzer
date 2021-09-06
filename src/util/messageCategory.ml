open GobConfig

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

type t = category [@@deriving eq]

let hash x = Hashtbl.hash x (* nested variants, so this is fine *)

module Behavior =
struct
  type t = behavior

  let create (e: t): category = Behavior e
  let undefined e: category = create @@ Undefined e
  let implementation: category = create @@ Implementation
  let machine: category = create @@ Machine

  module Undefined =
  struct
    type t = undefined_behavior

    let create (e: t): category = undefined e
    let array_out_of_bounds e: category = create @@ ArrayOutOfBounds e
    let nullpointer_dereference: category = create @@ NullPointerDereference
    let use_after_free: category = create @@ UseAfterFree

    module ArrayOutOfBounds =
    struct
      type t = array_oob

      let create (e: t): category = array_out_of_bounds e
      let past_end: category = create PastEnd
      let before_start: category = create BeforeStart
      let unknown: category = create Unknown

      let from_string_list (s: string list): category =
        match s with
        | [] -> Unknown
        | h :: t -> match h with
          | "past_end" -> past_end
          | "before_start" -> before_start
          | "unknown" -> unknown
          | _ -> Unknown

      let path_show (e: t) =
        match e with
        | PastEnd -> ["PastEnd"]
        | BeforeStart -> ["BeforeStart"]
        | Unknown -> ["Unknown"]
    end

    let from_string_list (s: string list): category =
      match s with
      | [] -> Unknown
      | h :: t -> match h with
        | "array_out_of_bounds" -> ArrayOutOfBounds.from_string_list t
        | "nullpointer_dereference" -> nullpointer_dereference
        | "use_after_free" -> use_after_free
        | _ -> Unknown

    let path_show (e: t) =
      match e with
      | ArrayOutOfBounds e -> "ArrayOutOfBounds" :: ArrayOutOfBounds.path_show e
      | NullPointerDereference -> ["NullPointerDereference"]
      | UseAfterFree -> ["UseAfterFree"]
  end

  let from_string_list (s: string list): category =
    match s with
    | [] -> Unknown
    | h :: t -> match h with
      | "undefined" -> Undefined.from_string_list t
      | "implementation" -> implementation
      | "machine" -> machine
      | _ -> Unknown

  let path_show (e: t) =
    match e with
    | Undefined u -> "Undefined" :: Undefined.path_show u
    | Implementation -> ["Implementation"]
    | Machine -> ["Machine"]
end

module Integer =
struct
  type t = integer

  let create (e: t): category = Integer e
  let overflow: category = create Overflow
  let div_by_zero: category = create DivByZero

  let from_string_list (s: string list): category =
    match s with
    | [] -> Unknown
    | h :: t -> match h with
      | "overflow" -> overflow
      | "div_by_zero" -> div_by_zero
      | _ -> Unknown

  let path_show (e: t) =
    match e with
    | Overflow -> ["Overflow"]
    | DivByZero -> ["DivByZero"]
end

module Cast =
struct
  type t = cast

  let create (e: t): category = Cast e
  let type_mismatch: category = create TypeMismatch

  let from_string_list (s: string list): category =
    match s with
    | [] -> Unknown
    | h :: t -> match h with
      | "type_mismatch" -> type_mismatch
      | _ -> Unknown

  let path_show (e: t) =
    match e with
    | TypeMismatch -> ["TypeMismatch"]
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

let path_show e =
  match e with
  | Assert -> ["Assert"]
  | Behavior x -> "Behavior" :: Behavior.path_show x
  | Integer x -> "Integer" :: Integer.path_show x
  | Race -> ["Race"]
  | Cast x -> "Cast" :: Cast.path_show x
  | Unknown -> ["Unknown"]
  | Analyzer -> ["Analyzer"]

let show x = String.concat " > " (path_show x)

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

let to_yojson x = `List (List.map (fun x -> `String x) (path_show x))
