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
  (* let hash x = Hashtbl.hash x.vid *)

  (* Output *)
  let show x = x.vname
  let pretty () x = Pretty.text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (show x))
  let to_yojson x = `String x.vname
end

module Stmt: S with type t = stmt =
struct
  include Std

  type t = stmt

  let name () = "stmt"

  (* Identity *)
  let equal x y = x.sid = y.sid
  let compare x y = compare x.sid y.sid
  let hash x = Hashtbl.hash x.sid * 97

  (* Output *)
  let pretty () x = dn_stmt () x
  let show x = Pretty.sprint ~width:max_int (pretty () x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (show x))
  let to_yojson x = `String (show x)
end

module Fundec: S with type t = fundec =
struct
  include Std

  type t = fundec

  let name () = "fundec"

  (* Identity *)
  let equal x y = x.svar.vid = y.svar.vid
  let compare x y = compare x.svar.vid y.svar.vid
  let hash x = x.svar.vid * 3

  (* Output *)
  let show x = x.svar.vname
  let pretty () x = Pretty.text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (show x))
  let to_yojson x = `String (show x)
end