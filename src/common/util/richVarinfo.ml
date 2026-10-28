open GoblintCil

let create_var name typ = Cilfacade.create_var @@ makeGlobalVar name typ

let single ~name ~typ =
  let vi = lazy (create_var name typ) in
  fun () ->
    Lazy.force vi

module type VarinfoMap =
sig
  type t
  type marshal
  val to_varinfo : t -> varinfo
  val unmarshal: marshal option -> unit
  val marshal: unit -> marshal
  val bindings: unit -> (t * varinfo) list
end

module type G =
sig
  include Hashtbl.HashedType
  val name_varinfo: t -> string
  val typ: t -> typ
end

module type H =
sig
  include G
  val describe_varinfo: varinfo -> t -> string
end

(* The maps are filled during solving, which may run on several domains, hence lock-free hashtables. *)
module Htbl = Saturn.Htbl

module Make (X: G) =
struct
  (* Mapping from X.t to varinfo *)
  type t = X.t
  type marshal = (X.t * varinfo) list

  let create_xh () = Htbl.create ~hashed_type:(module X) ()
  let xh = ref (create_xh ())

  (** Returns the varinfo for [x], creating it if necessary.
      [before_publish] is called with the new varinfo before other domains can find it in [xh].
      If several domains create a varinfo for [x] at the same time, all of them return the one that was published first. *)
  let to_varinfo_helper before_publish x =
    match Htbl.find_opt !xh x with
    | Some vi -> vi
    | None ->
      let vi = create_var (X.name_varinfo x) (X.typ x) in
      before_publish x vi;
      if Htbl.try_add !xh x vi then
        vi
      else
        Htbl.find_exn !xh x

  let to_varinfo x =
    to_varinfo_helper (fun _ _ -> ()) x

  let bindings () = List.of_seq (Htbl.to_seq !xh)

  let marshal = bindings

  let unmarshal = Option.iter (fun loaded ->
      let h = create_xh () in
      List.iter (fun (x, vi) -> ignore (Htbl.try_add h x vi)) loaded;
      xh := h
    )
end

(* module to maintain bidirectional mappings between some type t and varinfo.
   Provides a way to add a description shown to the user for varinfos, based on the associated object of type t.
*)
module BiVarinfoMap =
struct

  module type S =
  sig
    include VarinfoMap
    val from_varinfo: varinfo -> t option
    val mem_varinfo: varinfo -> bool
    val describe_varinfo: varinfo -> t -> string
  end

  (* Collection of all BiVarinfo mappings.
     This collection is queried by functions that output varinfos that might occur in such a mapping. *)
  module Collection =
  struct

    let mappings: (module S) list ref = ref []

    let mem_varinfo (v: varinfo) =
      List.exists (fun (module M: S) -> M.mem_varinfo v) !mappings

    (** Provides a description to be printed with the varinfo *)
    let describe_varinfo (v: varinfo) =
      match List.find_opt (fun (module M: S) -> M.mem_varinfo v) !mappings with
      | None -> failwith "Not a rich varinfo!"
      | Some m ->
        let module Map = (val m) in
        match Map.from_varinfo v with
        | Some x -> Map.describe_varinfo v x
        | None -> failwith "Element not found in mapping that claimed to contain it."

    let register_mapping (m) =
      mappings := m::!mappings
  end

  (** For technical resaons, this functor cannot register the module it creates in [Collection] itself.
      Thus this functor is private to this file, and should only be used through the [Make] defined below. *)
  module PrivateMake (X: H) =
  struct
    module M = Make(X)
    (* Mapping from varinfo to X.t *)
    type t = M.t
    type marshal = M.marshal * (varinfo * t) list
    let create_vh () = Htbl.create ~hashed_type:(module CilType.Varinfo) ()
    let vh = ref (create_vh ())

    let to_varinfo x =
      (* Add to vh before the varinfo becomes visible in xh: whoever gets the varinfo from xh must find it in vh,
         otherwise from_varinfo/mem_varinfo wrongly say that it does not belong to this map (e.g. not a heap variable).
         varinfos are compared by vid, which create_var derives from name and type, so racing creations agree on the key. *)
      let add_to_vh x vi =
        ignore (Htbl.try_add !vh vi x)
      in
      M.to_varinfo_helper add_to_vh x

    let from_varinfo vi =
      Htbl.find_opt !vh vi

    let mem_varinfo v =
      Htbl.mem !vh v

    let describe_varinfo v x =
      X.describe_varinfo v x

    let marshal () =
      M.marshal (), List.of_seq (Htbl.to_seq !vh)

    let unmarshal = function
      | Some (xh_loaded, vh_loaded) ->
        M.unmarshal (Some xh_loaded);
        let h = create_vh () in
        List.iter (fun (vi, x) -> ignore (Htbl.try_add h vi x)) vh_loaded;
        vh := h
      | None -> ()

    let bindings = M.bindings
  end

  (** Create a BiVarinfoMap and register it in the collection *)
  module Make (X: H) =
  struct
    module BiVarinfoMap = PrivateMake(X)
    include BiVarinfoMap
    let () =
      let m = (module BiVarinfoMap: S) in
      Collection.register_mapping m;
  end
end
