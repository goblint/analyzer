(** Lockset domains. *)

open struct
  module MvalZ = Mval.Z (* So we can define MustLock using Mval.Z after redefining Mval *)
end

module IndexDomain = ValueDomain.IndexDomain
module Mval = ValueDomain.Mval
module Addr = ValueDomain.Addr

module MustLock =
struct
  include MvalZ

  let semantic_equal_mval ((v, o): t) ((v', o'): Mval.t): bool option =
    if CilType.Varinfo.equal v v' then (
      let index1 = Cilfacade.bytesOffsetOnly v.vtype (Offset.Z.to_cil o) in (* TODO: better way to compute this? as Z.t not int *)
      let index2: IndexDomain.t = ValueDomain.Offs.to_index ~typ:v.vtype o' in
      match IndexDomain.equal_to (Z.of_int index1) index2 with
      | `Eq -> Some true
      | `Neq -> Some false
      | `Top -> None
    )
    else
      Some false

  let of_var v: t = (v, `NoOffset)

  let of_mval ((v, o): Mval.t): t =
    (v, Offset.Poly.map_indices (fun i -> IndexDomain.to_int i |> Option.get) o)

  let of_addr (addr : Addr.t) : t option =
    match addr with
    | Addr mv when Mval.is_definite mv -> Some (of_mval mv)
    | _ -> None

  let to_mval ((v, o): t): Mval.t =
    (v, Offset.Poly.map_indices (IndexDomain.of_int (Cilfacade.ptrdiff_ikind ())) o)
end

module MustLockset =
struct
  include SetDomain.Reverse (SetDomain.ToppedSet (MustLock) (struct let topname = "All mutexes" end))

  let all (): t = `Top
  let is_all (set: t) = set = `Top
end

(* true means exclusive lock and false represents reader lock*)
module RW   = BoolDomain.MayBool (* TODO: name booleans? *)

(* pair Addr and RW; also change pretty printing*)
module MakeRW (P: Printable.S) =
struct
  include Printable.Prod (P) (RW)

  let pretty () (a, write) =
    if write then
      P.pretty () a
    else
      GoblintCil.Pretty.dprintf "read lock %a" P.pretty a

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module MvalRW = MakeRW (Mval)
module AddrRW = MakeRW (Addr)
module MustLockRW = MakeRW (MustLock)

module MustLocksetRW =
struct
  include SetDomain.Reverse (SetDomain.ToppedSet (MustLockRW) (struct let topname = "All mutexes" end))
  let name () = "lockset"

  let add_mval_rw ((mv, rw): MvalRW.t) (set: t) =
    if Addr.Mval.is_definite mv then
      add (MustLock.of_mval mv, rw) set
    else
      set

  let remove_mval_rw ((mv, rw): MvalRW.t) (set: t) =
    if Addr.Mval.is_definite mv then
      remove (MustLock.of_mval mv, rw) set
    else
      filter (fun (ml, _) ->
          MustLock.semantic_equal_mval ml mv = Some false
        ) set

  let mem_mval (mv: Mval.t) (set: t) =
    if Mval.is_definite mv then (
      let ml = MustLock.of_mval mv in
      mem (ml, true) set || mem (ml, false) set
    )
    else
      false

  let remove_mval (mv: Mval.t) set =
    remove_mval_rw (mv, true) (remove_mval_rw (mv, false) set)

  let all (): t = `Top
  let is_all (set: t) = set = `Top

  let to_must_lockset (ls: t): MustLockset.t =
    let f (ml, _) set = MustLockset.add ml set in
    fold f ls (MustLockset.empty ())
end

module MustMultiplicity = struct
  (* the maximum multiplicity which we keep track of precisely *)
  let max_count () = 4

  module Count = Lattice.Reverse (
      Lattice.Chain (
      struct
        let n () = max_count () + 1
        let names x = if x = max_count () then Format.asprintf ">= %d" x else Format.asprintf "%d" x
      end
      )
    )

  include MapDomain.MapTop_LiftBot (MustLock) (Count)

  let name () = "multiplicity"

  let increment v x =
    let current = find v x in
    if current = max_count () then
      x
    else
      add v (current + 1) x

  let increment mv m =
    if Addr.Mval.is_definite mv then
      increment (MustLock.of_mval mv) m
    else
      m

  let decrement v x =
    let current = find v x in
    if current = 0 then
      (x, true)
    else
      (add v (current - 1) x, current - 1 = 0) (* TODO: remove if 0? *)

  let decrement mv m =
    if Addr.Mval.is_definite mv then
      decrement (MustLock.of_mval mv) m
    else
      (* TODO: non-definite should also decrement (to 0)? *)
      fold (fun ml _ (m, rmed) ->
          if MustLock.semantic_equal_mval ml mv = Some false then
            (m, rmed)
          else (
            let (m', rmed') = decrement ml m in
            (m', rmed || rmed')
          )
        ) m (m, false)
end

module MayLocksetNoRW =
struct
  include PreValueDomain.AD
end
