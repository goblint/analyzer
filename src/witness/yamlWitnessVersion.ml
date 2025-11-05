(** YAML witness format version. *)

type t =
  | V2_0
  | V2_1
  | V2_1_Goblint
[@@deriving ord, enum]

let show = function
  | V2_0 -> "2.0"
  | V2_1 -> "2.1"
  | V2_1_Goblint -> "2.1-goblint"

include Printable.SimpleShow (
  struct
    type nonrec t = t
    let show = show
  end
  )

let of_string = function
  | "2.0" -> V2_0
  | "2.1" -> V2_1
  | "2.1-goblint" -> V2_1_Goblint
  | _ -> invalid_arg "YamlWitnessVersion.of_string"

let of_option () =
  of_string (GobConfig.get_string "witness.yaml.format-version")

let min x y =
  Option.get (of_enum (Int.min (to_enum x) (to_enum y)))

let max x y =
  Option.get (of_enum (Int.max (to_enum x) (to_enum y)))
