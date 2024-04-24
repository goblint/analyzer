(** Lockset domains. *)

open GoblintCil
open struct
  module MvalZ = Mval.Z (* So we can define MustLock using Mval.Z after redefining Mval *)
end

module IndexDomain = ValueDomain.IndexDomain
module Mval = ValueDomain.Mval
module Addr = ValueDomain.Addr

module MustLock =
struct
  include MvalZ

  let of_mval ((v, o): Mval.t): t =
    (v, Offset.Poly.map_indices (fun i -> IndexDomain.to_int i |> Option.get) o)

  let to_mval ((v, o): t): Mval.t =
    (v, Offset.Poly.map_indices (IndexDomain.of_int (Cilfacade.ptrdiff_ikind ())) o)
end

module Mutexes = SetDomain.ToppedSet (Addr) (struct let topname = "All mutexes" end) (* TODO: AD? *)
module MustLockset =
struct
  include SetDomain.Reverse (SetDomain.ToppedSet (MustLock) (struct let topname = "All mutexes" end))

  let all (): t = `Top
  let is_all (set: t) = set = `Top
end

(* true means exclusive lock and false represents reader lock*)
module RW   = IntDomain.Booleans

(* pair Addr and RW; also change pretty printing*)
module MakeRW (P: Printable.S) =
struct
  include Printable.Prod (P) (RW)

  let pretty () (a, write) =
    if write then
      P.pretty () a
    else
      Pretty.dprintf "read lock %a" P.pretty a

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module AddrRW = MakeRW (Addr)
module MustLockRW = MakeRW (MustLock)

module MustLocksetRW =
struct
  include SetDomain.Reverse (SetDomain.ToppedSet (MustLockRW) (struct let topname = "All mutexes" end))
  let name () = "lockset"

  let add (mv, rw) set =
    if Addr.Mval.is_definite mv then
      add (MustLock.of_mval mv, rw) set
    else
      set

  let remove (mv, rw) set =
    if Addr.Mval.is_definite mv then
      remove (MustLock.of_mval mv, rw) set
    else
      filter (fun (mv', _) ->
          (* TODO: avoid conversion: semantic_equal between Mval and Mval *)
          Mval.semantic_equal mv (MustLock.to_mval mv') = Some false
        ) set

  let mem_rw mv set =
    Mval.is_definite mv && (
      mem (MustLock.of_mval mv, true) set || mem (MustLock.of_mval mv, false) set)

  let remove_rw mv set =
    remove (mv, true) (remove (mv, false) set)

  let all (): t = `Top
  let is_all (set: t) = set = `Top

  let export_locks ls =
    let f (x,_) set = MustLockset.add x set in
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
      fold (fun mv' _ (m, rmed) ->
          (* TODO: avoid conversion: semantic_equal between Mval and Mval *)
          if Mval.semantic_equal mv (MustLock.to_mval mv') = Some false then
            (m, rmed)
          else (
            let (m', rmed') = decrement mv' m in
            (m', rmed || rmed')
          )
        ) m (m, false)
end

module MayLocksetNoRW =
struct
  include PreValueDomain.AD
end

module Symbolic =
struct
  (* TODO: use SetDomain.Reverse *)
  module S = SetDomain.ToppedSet (CilType.Exp) (struct let topname = "All mutexes" end)
  include Lattice.Reverse (S)

  let rec eq_set (ask: Queries.ask) e =
    S.union
      (match ask.f (Queries.EqualSet e) with
       | es when not (Queries.ES.is_bot es) ->
         Queries.ES.fold S.add es (S.empty ())
       | _ -> S.empty ())
      (match e with
       | SizeOf _
       | SizeOfE _
       | SizeOfStr _
       | AlignOf _
       | Const _
       | AlignOfE _
       | UnOp _
       | BinOp _
       | Question _
       | Real _
       | Imag _
       | AddrOfLabel _ -> S.empty ()
       | AddrOf  (Var _,_)
       | StartOf (Var _,_)
       | Lval    (Var _,_) -> S.singleton e
       | AddrOf  (Mem e,ofs) -> S.map (fun e -> AddrOf  (Mem e,ofs)) (eq_set ask e)
       | StartOf (Mem e,ofs) -> S.map (fun e -> StartOf (Mem e,ofs)) (eq_set ask e)
       | Lval    (Mem e,ofs) -> S.map (fun e -> Lval    (Mem e,ofs)) (eq_set ask e)
       | CastE (_,e)           -> eq_set ask e
      )

  let add (ask: Queries.ask) e st =
    let no_casts = S.map Expcompare.stripCastsDeepForPtrArith (eq_set ask e) in
    let addrs = S.filter (function AddrOf _ -> true | _ -> false) no_casts in
    S.union addrs st
  let remove ask e st =
    (* TODO: Removing based on must-equality sets is not sound! *)
    let no_casts = S.map Expcompare.stripCastsDeepForPtrArith (eq_set ask e) in
    let addrs = S.filter (function AddrOf _ -> true | _ -> false) no_casts in
    S.diff st addrs
  let remove_var v st = S.filter (fun x -> not (SymbLocksDomain.Exp.contains_var v x)) st

  let filter = S.filter
  let fold = S.fold

end
