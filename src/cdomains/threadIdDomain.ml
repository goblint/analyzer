open Cil

module type S =
sig
  include Printable.S
  include MapDomain.Groupable with type t := t

  val threadinit: varinfo -> multiple:bool -> t
  val to_varinfo: t -> varinfo
  val is_main: t -> bool
  val is_unique: t -> bool

  (** Overapproximates whether the first TID can be involved in the creation fo the second TID*)
  val may_create: t -> t -> bool

  (** Is the first TID a must parent of the second thread. Always false if the first TID is not unique *)
  val is_must_parent: t -> t -> bool

  type marshal
  val init: marshal option -> unit
  val finalize: unit -> marshal

  (** How the created varinfos should be namend. Returned strings should not contain [Cil.location]s, but may contain node ids. *)
  val name_varinfo: t -> string
end

module type Stateless =
sig
  include S

  val threadenter: Node.t -> varinfo -> t
end

module type Stateful =
sig
  include S

  module D: Lattice.S

  val threadenter: t * D.t -> Node.t -> varinfo -> t
  val threadspawn: D.t -> Node.t -> varinfo -> D.t

  (** If it is possible to get a list of unique thread create thus far, get it *)
  val created: t -> D.t -> (t list) option
end


(** Type to represent an abstract thread ID. *)
module FunLoc: Stateless =
struct
  module M = struct
    include Printable.Prod (CilType.Varinfo) (Printable.Option (Node) (struct let name = "no location" end))

    (* Defines how varinfos representing a FunLoc are named.
    The varinfo-name contains node ids, but not their location (for compatibility with incremental analysis) *)
    let name_varinfo = function
      | (f, Some n) -> f.vname ^ "@" ^ Node.show n
      | (f, None) -> f.vname
  end
  include M

  let show = function
    | (f, Some n) -> f.vname ^ "@" ^ (CilType.Location.show (UpdateCil.getLoc n))
    | (f, None) -> f.vname

  include Printable.PrintSimple (
    struct
      type nonrec t = t
      let show = show
    end
  )

  let threadinit v ~multiple: t = (v, None)
  let threadenter l v: t = (v, Some l)

  let is_main = function
    | ({vname = "main"; _}, None) -> true
    | _ -> false

  let is_unique _ = false (* TODO: should this consider main unique? *)
  let may_create _ _ = true
  let is_must_parent _ _ = false

  module VarinfoMap = RichVarinfo.Make (M)
  let to_varinfo = VarinfoMap.to_varinfo
  type marshal = VarinfoMap.marshal
  let init m = VarinfoMap.unmarshal m
  let finalize () = VarinfoMap.marshal ()
end


module Unit (Base: Stateless): Stateful =
struct
  include Base

  module D = Lattice.Unit

  let threadenter _ = threadenter
  let threadspawn () _ _ = ()

  let created _ _ = None
end

module History (Base: Stateless): Stateful =
struct
  module P =
  struct
    include Printable.Liszt (Base)
    (* Prefix is stored in reversed order (main is last) since prepending is more efficient. *)
    let name () = "prefix"
  end
  module S =
  struct
    include SetDomain.Make (Base)
    let name () = "set"
  end
  module M = struct
    include Printable.Prod (P) (S)
    (* Varinfos for histories are named using a string representation based on node ids,
     not locations, for compatibility with incremental analysis.*)
    let name_varinfo ((l, s): t): string =
      let list_name = String.concat "," (List.map Base.name_varinfo l) in
      let set_name = String.concat "," (List.map Base.name_varinfo (S.elements s)) in
      list_name ^ ", {" ^ set_name ^ "}"
  end
  include M

  module D =
  struct
    include S
    let name () = "created"
  end

  let is_unique (_, s) =
    S.is_empty s

  let is_must_parent (p,s) (p',s') =
    if not (S.is_empty s) then
      false
    else
      let cdef_ancestor = P.common_suffix p p' in
      P.equal p cdef_ancestor

  let may_create (p,s) (p',s') =
    S.subset (S.union (S.of_list p) s) (S.union (S.of_list p') s')

  let compose ((p, s) as current) n =
    if BatList.mem_cmp Base.compare n p then (
      (* TODO: can be optimized by implementing some kind of partition_while function *)
      let s' = S.of_list (BatList.take_while (fun m -> not (Base.equal n m)) p) in
      let p' = List.tl (BatList.drop_while (fun m -> not (Base.equal n m)) p) in
      (p', S.add n (S.union s s'))
    )
    else if is_unique current then
      (n :: p, s)
    else
      (p, S.add n s)

  let threadinit v ~multiple =
    let base_tid = Base.threadinit v ~multiple in
    if multiple then
      ([], S.singleton base_tid)
    else
      ([base_tid], S.empty ())

  let threadenter ((p, _ ) as current, cs) (n: Node.t) v =
    let n = Base.threadenter n v in
    let ((p', s') as composed) = compose current n in
    if is_unique composed && S.mem n cs then
      (p, S.singleton n)
    else
      composed

  let created current cs =
    let els = D.elements cs in
    Some (List.map (compose current) els)

  let threadspawn cs l v =
    S.add (Base.threadenter l v) cs

  module VarinfoMap = RichVarinfo.Make (M)
  let to_varinfo = VarinfoMap.to_varinfo

  let is_main = function
    | ([fl], s) when S.is_empty s && Base.is_main fl -> true
    | _ -> false

  type marshal = VarinfoMap.marshal
  let finalize () = VarinfoMap.marshal ()
  let init m = VarinfoMap.unmarshal m
end

module ThreadLiftNames = struct
  let bot_name = "Bot Threads"
  let top_name = "Top Threads"
end
module Lift (Thread: S) =
struct
  include Lattice.Flat (Thread) (ThreadLiftNames)
  let name () = "Thread"
end

module FlagConfiguredTID:Stateful =
struct
  (* Thread IDs with prefix-set history *)
  module H = History(FunLoc)
  (* Plain thread IDs *)
  module P = Unit(FunLoc)

  module D = Lattice.Lift2(H.D)(P.D)(struct let bot_name = "bot" let top_name = "top" end)

  type t = H.t option * P.t option
  type group = H.group option * P.group option
  type marshal = H.marshal option * P.marshal option

  let history_enabled () =
    match GobConfig.get_string "ana.thread.domain" with
    | "plain" -> false
    | "history" -> true
    | s -> failwith @@ "Illegal value " ^ s ^ " for ana.thread.domain"

  let unop oph opp (h,p) = match (h, p) with
    | (Some h, None) -> oph h
    | (None, Some p) -> opp p
    | _ -> failwith "FlagConfiguredTID received a value where not exactly one component is set"

  let unop_to_t oph opp (h,p) = match (h, p) with
    | (Some h, None) -> (Some (oph h), None)
    | (None, Some p) -> (None, Some (opp p))
    | _ -> failwith "FlagConfiguredTID received a value where not exactly one component is set"

  let binop oph opp (h1,p1) (h2,p2) = match (h1, p1), (h2,p2) with
    | (Some h1, None), (Some h2, None) -> oph h1 h2
    | (None, Some p1), (None, Some p2) -> opp p1 p2
    | _ -> failwith "FlagConfiguredTID received a value where not exactly one component is set"

  let threadinit v ~multiple =
    if history_enabled () then
      (Some (H.threadinit v multiple), None)
    else
      (None, Some (P.threadinit v multiple))

  let to_varinfo = unop H.to_varinfo P.to_varinfo
  let is_main = unop H.is_main P.is_main
  let is_unique = unop H.is_unique P.is_unique
  let name_varinfo = unop H.name_varinfo P.name_varinfo

  let may_create = binop H.may_create P.may_create
  let is_must_parent = binop H.is_must_parent P.is_must_parent

  let created x d =
    let lifth x' d' =
      let hres = H.created x' d' in
      match hres with
      | None -> None
      | Some l -> Some (List.map (fun x -> (Some x, None)) l)
    in
    let liftp x' d' =
      let pres = P.created x' d' in
      match pres with
      | None -> None
      | Some l -> Some (List.map (fun x -> (None, Some x)) l)
    in
    match x, d with
    | (Some x', None), `Lifted1 d' -> lifth x' d'
    | (Some x', None), `Bot -> lifth x' (H.D.bot ())
    | (Some x', None), `Top -> lifth x' (H.D.top ())
    | (None, Some x'), `Lifted2 d' -> liftp x' d'
    | (None, Some x'), `Bot -> liftp x' (P.D.bot ())
    | (None, Some x'), `Top -> liftp x' (P.D.top ())
    | _ -> None

  let threadenter x n v =
    match x with
    | ((Some x', None), `Lifted1 d) -> (Some (H.threadenter (x',d) n v), None)
    | ((Some x', None), `Bot) -> (Some (H.threadenter (x',H.D.bot ()) n v), None)
    | ((Some x', None), `Top) -> (Some (H.threadenter (x',H.D.top ()) n v), None)
    | ((None, Some x'), `Lifted2 d) -> (None, Some (P.threadenter (x',d) n v))
    | ((None, Some x'), `Bot) -> (None, Some (P.threadenter (x',P.D.bot ()) n v))
    | ((None, Some x'), `Top) -> (None, Some (P.threadenter (x',P.D.top ()) n v))
    | _ -> failwith "FlagConfiguredTID received a value where not exactly one component is set"

  let threadspawn x n v =
    match x with
    | `Lifted1 x' -> `Lifted1 (H.threadspawn x' n v)
    | `Lifted2 x' -> `Lifted2 (P.threadspawn x' n v)
    | `Bot when history_enabled () -> `Lifted1 (H.threadspawn (H.D.bot ()) n v)
    | `Bot  -> `Lifted2 (P.threadspawn (P.D.bot ()) n v)
    | `Top when history_enabled () -> `Lifted1 (H.threadspawn (H.D.top ()) n v)
    | `Top  -> `Lifted2 (P.threadspawn (P.D.top ()) n v)

  let trace_enabled = false
  let equal = binop H.equal P.equal
  let hash = unop H.hash P.hash
  let compare = binop H.compare P.compare
  let show = unop H.show P.show
  let pretty () = unop (H.pretty ()) (P.pretty ())
  let printXml f = unop (H.printXml f) (P.printXml f)
  let to_yojson = unop H.to_yojson P.to_yojson

  let name () = "FlagConfiguredTID: " ^ if history_enabled () then H.name () else P.name ()
  let invariant _ _ = Invariant.none
  let tag _ = failwith "FlagConfiguredTID: no tag"
  let arbitrary () = failwith "FlagConfiguredTID: no arbitrary"

  let relift = unop_to_t H.relift P.relift
  let show_group = unop H.show_group P.show_group
  let to_group (h,p) = match (h, p) with
    | (Some h, None) ->
      (let r = H.to_group h in
       match r with
       | Some r -> Some (Some r, None)
       | _ -> None)
    | (None, Some p) ->
      (let r = P.to_group p in
       match r with
       | Some r -> Some (None, Some r)
       | _ -> None)
    | _ -> failwith "FlagConfiguredTID received a value where not exactly one component is set"


  let finalize () =
    if history_enabled () then
      (Some (H.finalize ()), None)
    else
      (None, Some (P.finalize ()))

  let init v =
    if history_enabled () then
      match v with
      | None -> H.init None
      | Some (v', None) -> H.init v'
      |_ -> failwith "FlagConfiguredTID received a value where not exactly one component is set"
    else
      match v with
      | None -> P.init None
      | Some (None, v') -> P.init v'
      |_ -> failwith "FlagConfiguredTID received a value where not exactly one component is set"
end

module Thread = FlagConfiguredTID

module ThreadLifted = Lift (Thread)
