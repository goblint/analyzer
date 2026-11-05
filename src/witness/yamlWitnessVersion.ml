module Version =
struct
  type t =
    | V2_0
    | V2_1
    | V2_2
    | V2_3
  [@@deriving ord, enum]

  let show = function
    | V2_0 -> "2.0"
    | V2_1 -> "2.1"
    | V2_2 -> "2.2"
    | V2_3 -> "2.3"

  let leq x y = compare x y <= 0

  let meet x y =
    Option.get (of_enum (Int.min (to_enum x) (to_enum y)))

  let join x y =
    Option.get (of_enum (Int.max (to_enum x) (to_enum y)))
end

module Goblint =
struct
  include BoolDomain.MayBool

  let show = function
    | false -> ""
    | true -> "-goblint"
end

type t = {
  version: Version.t;
  goblint: Goblint.t;
}
[@@deriving leq, meet, join]

let v2_0 = {version = V2_0; goblint = false}
let v2_1 = {version = V2_1; goblint = false}
let v2_1_goblint = {version = V2_1; goblint = true}
let v2_2 = {version = V2_2; goblint = false}
let v2_2_goblint = {version = V2_2; goblint = true}
let v2_3 = {version = V2_3; goblint = false}
let v2_3_goblint = {version = V2_3; goblint = true}

let show {version; goblint} = Version.show version ^ Goblint.show goblint

include Printable.SimpleShow (
  struct
    type nonrec t = t
    let show = show
  end
  )

(* TODO: more general parsing? *)
let of_string = function
  | "2.0" -> v2_0
  | "2.1" -> v2_1
  | "2.1-goblint" -> v2_1_goblint
  | "2.2" -> v2_2
  | "2.2-goblint" -> v2_2_goblint
  | "2.3" -> v2_3
  | "2.3-goblint" -> v2_3_goblint
  | _ -> invalid_arg "YamlWitnessVersion.of_string"

let of_option () =
  of_string (GobConfig.get_string "witness.yaml.format-version")
