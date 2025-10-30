(** YAML witness format version. *)

type t =
  | V2_0
  | V2_1
[@@deriving ord, enum]

let show = function
  | V2_0 -> "2.0"
  | V2_1 -> "2.1"

let of_string = function
  | "2.0" -> V2_0
  | "2.1" -> V2_1
  | _ -> invalid_arg "YamlWitnessVersion.of_string"

let of_option () =
  of_string (GobConfig.get_string "witness.yaml.format-version")

let min x y =
  Option.get (of_enum (Int.min (to_enum x) (to_enum y)))

let max x y =
  Option.get (of_enum (Int.max (to_enum x) (to_enum y)))
