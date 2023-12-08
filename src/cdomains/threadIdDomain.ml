(** Thread ID domains. *)

open GoblintCil
open FlagHelper
open BatPervasives

module type S =
sig
  include Printable.S

  val threadinit: varinfo -> multiple:bool -> t
  val is_main: t -> bool
  val is_unique: t -> bool

  (** Overapproximates whether the first TID can be involved in the creation fo the second TID*)
  val may_create: t -> t -> bool

  (** Is the first TID a must parent of the second thread. Always false if the first TID is not unique *)
  val is_must_parent: t -> t -> bool
end

module type Stateless =
sig
  include S

  val threadenter: multiple:bool -> Node.t -> int option -> varinfo -> t
end

module type Stateful =
sig
  include S

  module D: Lattice.S

  val threadenter: multiple:bool -> t * D.t -> Node.t -> int option -> varinfo -> t list
  val threadspawn: multiple:bool -> D.t -> Node.t -> int option -> varinfo -> D.t

  (** If it is possible to get a list of threads created thus far, get it *)
  val created: t -> D.t -> (t list) option
end



(** Type to represent an abstract thread ID. *)
module FunNode: Stateless =
struct
  include
    Printable.Prod
      (CilType.Varinfo) (
      Printable.Option (
        Printable.Prod
          (Node) (
          Printable.Option
            (WrapperFunctionAnalysis0.ThreadCreateUniqueCount)
            (struct let name = "no index" end)))
        (struct let name = "no node" end))

  let show = function
    | (f, Some (n, i)) ->
      f.vname
      ^ "@" ^ (CilType.Location.show (UpdateCil.getLoc n))
      ^ "#" ^ Option.fold ~none:"top" ~some:string_of_int i
    | (f, None) -> f.vname

  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
  )

  let threadinit v ~multiple: t = (v, None)

  let threadenter ~multiple l i v: t =
    if GobConfig.get_bool "ana.thread.include-node" then
      let counter = Option.map (fun x -> if multiple then WrapperFunctionAnalysis0.ThreadCreateUniqueCount.top () else x) i in
      (v, Some (l, counter))
    else
      (v, None)

  let is_main = function
    | ({vname; _}, None) -> List.mem vname @@ GobConfig.get_string_list "mainfun"
    | _ -> false

  let is_unique _ = false (* TODO: should this consider main unique? *)
  let may_create _ _ = true
  let is_must_parent _ _ = false
end


module Unit (Base: Stateless): Stateful =
struct
  include Base

  module D = Lattice.Unit

  let threadenter ~multiple _ n i v = [threadenter ~multiple n i v]
  let threadspawn ~multiple () _ _ _ = ()

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
  include Printable.Prod (P) (S)

  let pretty () (p, s) =
    let p = List.rev p in (* show in "unreversed" order *)
    if S.is_empty s then
      P.pretty () p (* hide empty set *)
    else
      Pretty.dprintf "%a, %a" P.pretty p S.pretty s

  let show x = GobPretty.sprint pretty x

  module D = Lattice.Prod (struct
      include S
      let name () = "created (once)"
    end) (struct
    include S
    let name () = "created (multiple times)"
  end)


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

  let compose ((p, s) as current) ni =
    if BatList.mem_cmp Base.compare ni p then (
      let shared, unique = BatList.span (not % Base.equal ni) p in
      (List.tl unique, S.of_list shared |> S.union s |> S.add ni)
    )
    else if is_unique current then
      (ni :: p, s)
    else
      (p, S.add ni s)

  let threadinit v ~multiple =
    let base_tid = Base.threadinit v ~multiple in
    if multiple then
      ([], S.singleton base_tid)
    else
      ([base_tid], S.empty ())

  let threadenter ~multiple ((p, _ ) as current, (cs,_)) (n: Node.t) i v =
    let ni = Base.threadenter ~multiple n i v in
    let ((p', s') as composed) = compose current ni in
    if is_unique composed && (S.mem ni cs || multiple) then
      [(p, S.singleton ni); composed] (* also respawn unique version of the thread to keep it reachable while thread ID sets refer to it *)
    else
      [composed]

  let created ((p, _ ) as current) (cs, cms) =
    let els = S.elements cs in
    let map_one e =
      let ((p', s') as composed) = compose current e in
      if is_unique composed && S.mem e cms then
        (* Also construct the non-unique version that was spawned as e was encountered multiple times *)
        [(p, S.singleton e); composed]
      else
        [composed]
    in
    Some (List.concat_map map_one els)

  let threadspawn ~multiple (cs,cms) l i v =
    let e = Base.threadenter ~multiple l i v in
    if S.mem e cs then
      (cs, S.add e cms)
    else
      (S.add e cs, if multiple then S.add e cms else cms)

  let is_main = function
    | ([fl], s) when S.is_empty s && Base.is_main fl -> true
    | _ -> false
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
  module H = History(FunNode)
  (* Plain thread IDs *)
  module P = Unit(FunNode)

  include FlagHelper(H)(P)(struct
      let msg = "FlagConfiguredTID received a value where not exactly one component is set"
      let name = "FlagConfiguredTID"
    end)

  module D = Lattice.Lift2(H.D)(P.D)(struct let bot_name = "bot" let top_name = "top" end)

  let history_enabled () =
    match GobConfig.get_string "ana.thread.domain" with
    | "plain" -> false
    | "history" -> true
    | s -> failwith @@ "Illegal value " ^ s ^ " for ana.thread.domain"

  let threadinit v ~multiple =
    if history_enabled () then
      (Some (H.threadinit v ~multiple), None)
    else
      (None, Some (P.threadinit v ~multiple))

  let is_main = unop H.is_main P.is_main
  let is_unique = unop H.is_unique P.is_unique
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

  let threadenter ~multiple x n i v =
    match x with
    | ((Some x', None), `Lifted1 d) -> H.threadenter ~multiple (x',d) n i v |> List.map (fun t -> (Some t, None))
    | ((Some x', None), `Bot) -> H.threadenter ~multiple (x',H.D.bot ()) n i v |> List.map (fun t -> (Some t, None))
    | ((Some x', None), `Top) -> H.threadenter ~multiple (x',H.D.top ()) n i v |> List.map (fun t -> (Some t, None))
    | ((None, Some x'), `Lifted2 d) -> P.threadenter ~multiple (x',d) n i v |> List.map (fun t -> (None, Some t))
    | ((None, Some x'), `Bot) -> P.threadenter ~multiple (x',P.D.bot ()) n i v |> List.map (fun t -> (None, Some t))
    | ((None, Some x'), `Top) -> P.threadenter ~multiple (x',P.D.top ()) n i v |> List.map (fun t -> (None, Some t))
    | _ -> failwith "FlagConfiguredTID received a value where not exactly one component is set"

  let threadspawn ~multiple x n i v =
    match x with
    | `Lifted1 x' -> `Lifted1 (H.threadspawn ~multiple x' n i v)
    | `Lifted2 x' -> `Lifted2 (P.threadspawn ~multiple x' n i v)
    | `Bot when history_enabled () -> `Lifted1 (H.threadspawn  ~multiple (H.D.bot ()) n i v)
    | `Bot  -> `Lifted2 (P.threadspawn  ~multiple (P.D.bot ()) n i v)
    | `Top when history_enabled () -> `Lifted1 (H.threadspawn ~multiple (H.D.top ()) n i v)
    | `Top  -> `Lifted2 (P.threadspawn ~multiple (P.D.top ()) n i v)

  let name () = "FlagConfiguredTID: " ^ if history_enabled () then H.name () else P.name ()
end

type thread =
  | Thread of FlagConfiguredTID.t
  | UnknownThread
[@@deriving eq, ord, hash]

module Thread : Stateful with type t = thread =
struct
  include Printable.Std
  type t = thread [@@deriving eq, ord, hash]

  let name () = "Thread id"
  let pretty () t =
    match t with
    | Thread tid -> FlagConfiguredTID.pretty () tid
    | UnknownThread -> Pretty.text "Unknown thread id"

  let show t =
    match t with
    | Thread tid -> FlagConfiguredTID.show tid
    | UnknownThread -> "Unknown thread id"

  let printXml f t =
    match t with
    | Thread tid -> FlagConfiguredTID.printXml f tid
    | UnknownThread -> BatPrintf.fprintf f "<value>\n<data>\nUnknown thread id\n</data>\n</value>\n"

  let to_yojson t =
    match t with
    | Thread tid -> FlagConfiguredTID.to_yojson tid
    | UnknownThread -> `String "Unknown thread id"

  let relift t =
    match t with
    | Thread tid -> Thread (FlagConfiguredTID.relift tid)
    | UnknownThread -> UnknownThread

  let lift t = Thread t

  let threadinit v ~multiple = Thread (FlagConfiguredTID.threadinit v ~multiple)

  let is_main t =
    match t with
    | Thread tid -> FlagConfiguredTID.is_main tid
    | UnknownThread -> false

  let is_unique t =
    match t with
    | Thread tid -> FlagConfiguredTID.is_unique tid
    | UnknownThread -> false

  let may_create t1 t2 =
    match t1, t2 with
    | Thread tid1, Thread tid2 -> FlagConfiguredTID.may_create tid1 tid2
    | _, _ -> true

  let is_must_parent t1 t2 =
    match t1, t2 with
    | Thread tid1, Thread tid2 -> FlagConfiguredTID.is_must_parent tid1 tid2
    | _, _ -> false

  module D = FlagConfiguredTID.D

  let threadenter ~multiple (t, d) node i v =
    match t with
    | Thread tid -> List.map lift (FlagConfiguredTID.threadenter ~multiple (tid, d) node i v)
    | UnknownThread -> assert false

  let threadspawn = FlagConfiguredTID.threadspawn

  let created t d =
    match t with
    | Thread tid -> Option.map (List.map lift) (FlagConfiguredTID.created tid d)
    | UnknownThread -> None
end

module ThreadLifted = Lift (Thread)
