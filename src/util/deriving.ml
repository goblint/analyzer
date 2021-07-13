type json = Yojson.Safe.t
module Stdlib = struct (* since ocaml 4.07, Stdlib (which includes Pervasives) is opened by default, since 4.08 Pervasives is deprectated in favor of just Stdlib *)
  type 'a ref = [%import: 'a Stdlib.ref] [@@deriving yojson, show]
end
module Map = struct
  (* include all of Map but Make *)
  include (Map : module type of Map with module Make := Map.Make)
  (* define Map.Make with to_yojson *)
  module Make (K : sig include OrderedType val to_yojson : t -> json end) = struct
    include Map.Make (K)
    let to_yojson poly_v x = [%to_yojson: (K.t * 'v) list] poly_v (bindings x)
  end
end
module Cil = struct
  open Cil

  (* To fix this properly, the types above should be annotated with sth like @to_yojson to give a custom function to  *)
  (* create json from them. This is however currently not supported by ppx_derving. This should work in the meanwhile *)
  (* see also https://github.com/ocaml-ppx/ppx_deriving/issues/184 *)

  let pp_varinfo fmt v = Format.fprintf fmt "%s" v.vname
  let show_varinfo v = v.vname
end
