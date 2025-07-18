open GobConfig

type array_oob =
  | PastEnd
  | BeforeStart
  | Unknown
[@@deriving eq, ord, hash]

type undefined_behavior =
  | ArrayOutOfBounds of array_oob
  | NullPointerDereference
  | UseAfterFree
  | MemoryOutOfBoundsAccess
  | DoubleFree
  | InvalidMemoryDeallocation
  | MemoryLeak
  | Uninitialized
  | DoubleLocking
  | Other
[@@deriving eq, ord, hash]

type behavior =
  | Undefined of undefined_behavior
  | Implementation
  | Machine
[@@deriving eq, ord, hash]

type integer = Overflow | DivByZero [@@deriving eq, ord, hash]

type cast = TypeMismatch [@@deriving eq, ord, hash]

type category =
  | Assert
  | Behavior of behavior
  | Call
  | Integer of integer
  | Float
  | Race
  | Deadlock
  | Cast of cast
  | Deadcode
  | Unknown
  | Analyzer
  | Unsound
  | Imprecise
  | Witness
  | Program
  | Termination
[@@deriving eq, ord, hash]

type t = category [@@deriving eq, ord, hash]

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
    let memory_out_of_bounds_access: category = create @@ MemoryOutOfBoundsAccess
    let double_free: category = create @@ DoubleFree
    let invalid_memory_deallocation: category = create @@ InvalidMemoryDeallocation
    let memory_leak: category = create @@ MemoryLeak
    let uninitialized: category = create @@ Uninitialized
    let double_locking: category = create @@ DoubleLocking
    let other: category = create @@ Other

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
        | "memory_out_of_bounds_access" -> memory_out_of_bounds_access
        | "double_free" -> double_free
        | "invalid_memory_deallocation" -> invalid_memory_deallocation
        | "uninitialized" -> uninitialized
        | "double_locking" -> double_locking
        | "other" -> other
        | _ -> Unknown

    let path_show (e: t) =
      match e with
      | ArrayOutOfBounds e -> "ArrayOutOfBounds" :: ArrayOutOfBounds.path_show e
      | NullPointerDereference -> ["NullPointerDereference"]
      | UseAfterFree -> ["UseAfterFree"]
      | MemoryOutOfBoundsAccess -> ["MemoryOutOfBoundsAccess"]
      | DoubleFree -> ["DoubleFree"]
      | InvalidMemoryDeallocation -> ["InvalidMemoryDeallocation"]
      | MemoryLeak -> ["MemoryLeak"]
      | Uninitialized -> ["Uninitialized"]
      | DoubleLocking -> ["DoubleLocking"]
      | Other -> ["Other"]
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
    | Call -> "call"
    | Integer _ -> "integer"
    | Float -> "float"
    | Race -> "race"
    | Deadlock -> "deadlock"
    | Cast _ -> "cast"
    | Deadcode -> "deadcode"
    | Unknown -> "unknown"
    | Analyzer -> "analyzer"
    | Unsound -> "unsound"
    | Imprecise -> "imprecise"
    | Witness -> "witness"
    | Program -> "program"
    | Termination -> "termination"
    (* Don't forget to add option to schema! *)
  in get_bool ("warn." ^ (to_string e))

let path_show e =
  match e with
  | Assert -> ["Assert"]
  | Behavior x -> "Behavior" :: Behavior.path_show x
  | Call -> ["Call"]
  | Integer x -> "Integer" :: Integer.path_show x
  | Float -> ["Float"]
  | Race -> ["Race"]
  | Deadlock -> ["Deadlock"]
  | Cast x -> "Cast" :: Cast.path_show x
  | Deadcode -> ["Deadcode"]
  | Unknown -> ["Unknown"]
  | Analyzer -> ["Analyzer"]
  | Unsound -> ["Unsound"]
  | Imprecise -> ["Imprecise"]
  | Witness -> ["Witness"]
  | Program -> ["Program"]
  | Termination -> ["Termination"]

let show x = String.concat " > " (path_show x)

let behaviorName = function
  |Machine -> "Machine";
  |Implementation -> "Implementation"
  |Undefined u -> match u with
    |NullPointerDereference -> "NullPointerDereference"
    |UseAfterFree -> "UseAfterFree"
    |MemoryOutOfBoundsAccess -> "MemoryOutOfBoundsAccess"
    |DoubleFree -> "DoubleFree"
    |InvalidMemoryDeallocation -> "InvalidMemoryDeallocation"
    |MemoryLeak -> "MemoryLeak"
    |Uninitialized -> "Uninitialized"
    |DoubleLocking -> "DoubleLocking"
    |Other -> "Other"
    | ArrayOutOfBounds aob -> match aob with
      | PastEnd -> "PastEnd"
      | BeforeStart -> "BeforeStart"
      | Unknown -> "Unknown Aob"
let categoryName = function
  | Assert -> "Assert"
  | Call -> "Call"
  | Race -> "Race"
  | Deadlock -> "Deadlock"
  | Cast x -> "Cast"
  | Deadcode -> "Deadcode"
  | Unknown -> "Unknown"
  | Analyzer -> "Analyzer"
  | Unsound -> "Unsound"
  | Imprecise -> "Imprecise"
  | Witness -> "Witness"
  | Program -> "Program"

  | Behavior x -> behaviorName x
  | Integer x -> (match x with
      | Overflow -> "Overflow";
      | DivByZero -> "DivByZero")
  | Float -> "Float"
  | Termination -> "Termination"


let from_string_list (s: string list) =
  match s with
  | [] -> Unknown
  | h :: t -> match h with
    | "assert" -> Assert
    | "behavior" -> Behavior.from_string_list t
    | "call" -> Call
    | "integer" -> Integer.from_string_list t
    | "float" -> Float
    | "race" -> Race
    | "deadlock" -> Deadlock
    | "cast" -> Cast.from_string_list t
    | "deadcode" -> Deadcode
    | "analyzer" -> Analyzer
    | "unsound" -> Unsound
    | "imprecise" -> Imprecise
    | "witness" -> Witness
    | "program" -> Program
    | "termination" -> Termination
    | _ -> Unknown

let to_yojson x = `List (List.map (fun x -> `String x) (path_show x))
let of_yojson = function
  | `List l ->
    l
    |> List.map Yojson.Safe.Util.to_string
    |> from_string_list
    |> Result.ok
  | _ -> Result.Error "MessageCategory.of_yojson"
