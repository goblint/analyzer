type t =
  | Cil of Basetype.Variables.t (* Basetype not CilType because of custom printing *)
[@@deriving eq, ord, hash, show]

let name () = "var"

include Printable.Std

let show (Cil x) = Basetype.Variables.show x
let pretty () (Cil x) = Basetype.Variables.pretty () x
let printXml f (Cil x) = Basetype.Variables.printXml f x
let to_yojson (Cil x) = Basetype.Variables.to_yojson x

let tag (Cil x) = Basetype.Variables.tag x (* for Patricia maps *)
let relift (Cil x) = Cil (Basetype.Variables.relift x)

let typ (Cil x) = x.vtype


type group = Basetype.Variables.group [@@deriving ord, show { with_path = false }]
let to_group (Cil x) = Basetype.Variables.to_group x
