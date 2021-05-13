module GU = Goblintutil
open Cil
open Deriving.Cil
open Pretty

module type S =
sig
  include Printable.S
end

module Std =
struct
  include Printable.Std

  let pretty_diff () (_, _) = nil
end

module Varinfo: S with type t = varinfo =
struct
  include Std

  type t = varinfo

  let name () = "varinfo"

  (* Identity *)
  let equal x y = x.vid = y.vid
  let compare x y = compare x.vid y.vid
  let hash x = x.vid - 4773

  (* Output *)
  let show x = x.vname
  let pretty () x = Pretty.text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (show x))
  let to_yojson x = `String x.vname
end