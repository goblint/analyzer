(** Ghost variables for YAML witnesses. *)

module Var =
struct
  type t =
    | Locked of LockDomain.Addr.t
    | Multithreaded
  [@@deriving eq, hash]

  let name_varinfo = function
    | Locked (Addr (v, _) as l) ->
      let name =
        if RichVarinfo.BiVarinfoMap.Collection.mem_varinfo v then
          Printf.sprintf "alloc_%s%d" (if v.vid < 0 then "m" else "") (abs v.vid) (* turn minus into valid C name *)
        else
          LockDomain.Addr.show l (* TODO: valid names with interval offsets, etc *)
      in
      name ^ "_locked"
    | Locked _ -> assert false
    | Multithreaded -> "multithreaded"

  let describe_varinfo _ _ = ""

  let typ = function
    | Locked _ -> GoblintCil.intType
    | Multithreaded -> GoblintCil.intType

  let initial = function
    | Locked _ -> GoblintCil.zero
    | Multithreaded -> GoblintCil.zero
end

include Var

module Map = RichVarinfo.BiVarinfoMap.Make (Var)

include Map

let variable_entry ~task x =
  let variable = name_varinfo x in
  let type_ = String.trim (CilType.Typ.show (typ x)) in (* CIL printer puts space at the end of some types *)
  let initial = CilType.Exp.show (initial x) in
  YamlWitness.Entry.ghost_variable ~task ~variable ~type_ ~initial

let update_entry ~task ~node x e =
  let loc = Node.location node in
  let location_function = (Node.find_fundec node).svar.vname in
  let location = YamlWitness.Entry.location ~location:loc ~location_function in
  let variable = name_varinfo x in
  let expression = CilType.Exp.show e in
  YamlWitness.Entry.ghost_update ~task ~location ~variable ~expression
