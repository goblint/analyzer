type t =
  | V2_0
  | V2_1
  | V2_1_Goblint
  | V2_2 (* TODO: not proper order *)
  | V2_2_Goblint
  | V2_3 (* TODO: not proper order *)
  | V2_3_Goblint
[@@deriving ord, enum]

let show = function
  | V2_0 -> "2.0"
  | V2_1 -> "2.1"
  | V2_1_Goblint -> "2.1-goblint"
  | V2_2 -> "2.2"
  | V2_2_Goblint -> "2.2-goblint"
  | V2_3 -> "2.3"
  | V2_3_Goblint -> "2.3-goblint"

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
  | "2.2" -> V2_2
  | "2.2-goblint" -> V2_2_Goblint
  | "2.3" -> V2_3
  | "2.3-goblint" -> V2_3_Goblint
  | _ -> invalid_arg "YamlWitnessVersion.of_string"

let of_option () =
  of_string (GobConfig.get_string "witness.yaml.format-version")

let min x y =
  Option.get (of_enum (Int.min (to_enum x) (to_enum y)))

let max x y =
  Option.get (of_enum (Int.max (to_enum x) (to_enum y)))
