(** Ghost variables for YAML witnesses. *)

type t =
  | Locked of LockDomain.Addr.t
  | Multithreaded
[@@deriving eq, ord, hash]

let name_varinfo = function
  | Locked (Addr (v, os)) ->
    let name =
      if CilType.Varinfo.equal v LibraryFunctions.verifier_atomic_var then
        "__VERIFIER_atomic"
      else
      if RichVarinfo.BiVarinfoMap.Collection.mem_varinfo v then
        Printf.sprintf "alloc_%s%d" (if v.vid < 0 then "m" else "") (abs v.vid) (* turn minus into valid C name *)
      else
        Basetype.Variables.show v
    in
    let rec offs: LockDomain.Addr.Offs.t -> string = function
      | `NoOffset -> ""
      | `Field (f, os') -> "_" ^ f.fname ^ offs os'
      | `Index (i, os') -> failwith "TODO" (* TODO: valid names with interval offsets, etc *)
    in
    name ^ offs os ^ "_locked"
  | Locked _ -> assert false
  | Multithreaded -> "multithreaded"

let show = name_varinfo

include Printable.SimpleShow (struct
    type nonrec t = t
    let show = show
  end)

let describe_varinfo _ _ = ""

let typ = function
  | Locked _ -> GoblintCil.intType
  | Multithreaded -> GoblintCil.intType

let initial = function
  | Locked _ -> GoblintCil.zero
  | Multithreaded -> GoblintCil.zero
