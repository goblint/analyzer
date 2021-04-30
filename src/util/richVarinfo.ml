open Cil

let create_var name = Goblintutil.create_var @@ makeGlobalVar name voidType

let single ~name =
  let vi = lazy (create_var name) in
  fun () ->
    Lazy.force vi

module type S =
sig
  type t
  val map: name:(t -> string) -> ?size:int -> (t -> varinfo)
end

module Make (X: Hashtbl.HashedType) =
struct
  module XH = Hashtbl.Make (X)

  type t = X.t

  let map ~name ?(size=13) =
    let xh = XH.create size in
    fun x ->
      try
        XH.find xh x
      with Not_found ->
        let vi = create_var (name x) in
        XH.replace xh x vi;
        vi
end

module Variables = Make (Basetype.Variables)