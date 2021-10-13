open Cil

let create_var name = Goblintutil.create_var @@ makeGlobalVar name voidType

let single ~name =
  let vi = lazy (create_var name) in
  fun () ->
    Lazy.force vi

module type S =
sig
  type t
  type marshal
  module type VarinfoMap =
  sig
    val to_varinfo : t -> varinfo
    val from_varinfo: varinfo -> t option
    val marshal: marshal
  end
  val map: ?marshal:(marshal option) -> ?size:int -> name:(t -> string) -> unit -> (module VarinfoMap)
end

module Make (X: Hashtbl.HashedType) =
struct
  (* Mapping from X.t to varinfo *)
  module XH = Hashtbl.Make (X)
  (* Mapping from varinfo to X.t *)
  module VH = Hashtbl.Make (CilType.Varinfo)

  type t = X.t
  type marshal = varinfo XH.t * t VH.t

  module type VarinfoMap =
  sig
    val to_varinfo : t -> varinfo
    val from_varinfo: varinfo -> t option
    val marshal: marshal
  end
  let map ?(marshal=None) ?(size=113) ~name ()  =
    let xh, vh = match marshal with
      | Some (xh, vh) -> xh, vh
      | None -> XH.create size, VH.create size
    in
    (module struct
      let to_varinfo x =
        try
          XH.find xh x
        with Not_found ->
          let vi = create_var (name x) in
          XH.replace xh x vi;
          VH.replace vh vi x;
          vi

      let from_varinfo vi =
        VH.find_opt vh vi

      let marshal = xh, vh
    end
    : VarinfoMap)
end

module Variables = Make (Basetype.Variables)
