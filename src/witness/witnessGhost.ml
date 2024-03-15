(** Ghost variables for YAML witnesses. *)

module Var =
struct
  type t =
    | Locked of LockDomain.Addr.t
    | Multithreaded
  [@@deriving eq, hash]

  let name_varinfo = function
    | Locked l -> LockDomain.Addr.show l ^ "_locked" (* TODO: valid C name *)
    | Multithreaded -> "multithreaded"

  (* TODO: define correct types *)
end

include Var

module Map = RichVarinfo.Make (Var)

include Map
