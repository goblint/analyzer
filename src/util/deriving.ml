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
module Pretty = struct
  include Pretty
  let doc_to_yojson x = assert false
  let pp_doc x = assert false
end
module Cil = struct
  type location = [%import: Cil.location]
  and stmt = [%import: Cil.stmt]
  and stmtkind = [%import: Cil.stmtkind]
  and label = [%import: Cil.label]
  and instr = [%import: Cil.instr]
  and exp = [%import: Cil.exp]
  and block = [%import: Cil.block]
  and lval = [%import: Cil.lval]
  and lhost = [%import: Cil.lhost]
  and varinfo = [%import: Cil.varinfo]
  and offset = [%import: Cil.offset]
  and attributes = [%import: Cil.attributes]
  and attribute = [%import: Cil.attribute]
  and constant = [%import: Cil.constant]
  and typ = [%import: Cil.typ]
  and unop = [%import: Cil.unop]
  and binop = [%import: Cil.binop]
  and initinfo = [%import: Cil.initinfo]
  and storage = [%import: Cil.storage]
  and fieldinfo = [%import: Cil.fieldinfo]
  and attrparam = [%import: Cil.attrparam]
  and ikind = [%import: Cil.ikind]
  and fkind = [%import: Cil.fkind]
  and enuminfo = [%import: Cil.enuminfo]
  and typeinfo = [%import: Cil.typeinfo]
  and compinfo = [%import: Cil.compinfo]
  and init = [%import: Cil.init]
  and typsig = [%import: Cil.typsig]
  and fundec = [%import: Cil.fundec]

  (* To fix this properly, the types above should be annotated with sth like @to_yojson to give a custom function to  *)
  (* create json from them. This is however currently not supported by ppx_derving. This should work in the meanwhile *)
  (* see also https://github.com/ocaml-ppx/ppx_deriving/issues/184 *)
  let rec fundec_to_yojson (x:fundec) = varinfo_to_yojson x.svar
  and fieldinfo_to_yojson (f:fieldinfo) = `String (f.fname)
  and varinfo_to_yojson (v:varinfo) = `String(v.vname)
  and exp_to_yojson (l:exp) = `String(Pretty.sprint ~width:80 (Cil.d_exp () l))

  let pp_varinfo fmt v = Format.fprintf fmt "%s" v.vname
  let show_varinfo v = v.vname
end
