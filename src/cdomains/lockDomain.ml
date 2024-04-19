(** Lockset domains. *)

module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module Exp = CilType.Exp
module IdxDom = ValueDomain.IndexDomain

open GoblintCil

module Mutexes = SetDomain.ToppedSet (Addr) (struct let topname = "All mutexes" end) (* TODO: AD? *)
module Simple = SetDomain.Reverse (Mutexes)
module Priorities = IntDomain.Lifted

module Lockset =
struct

  (* true means exclusive lock and false represents reader lock*)
  module RW   = IntDomain.Booleans

  (* pair Addr and RW; also change pretty printing*)
  module Lock =
  struct
    include Printable.Prod (Addr) (RW)

    let pretty () (a, write) =
      if write then
        Addr.pretty () a
      else
        Pretty.dprintf "read lock %a" Addr.pretty a

    include Printable.SimplePretty (
      struct
        type nonrec t = t
        let pretty = pretty
      end
      )
  end

  include SetDomain.Reverse(SetDomain.ToppedSet (Lock) (struct let topname = "All mutexes" end))
  let name () = "lockset"

  let add ((addr, _) as lock) set =
    match addr with
    | Addr.Addr mv when Addr.Mval.is_definite mv -> (* avoids NULL *)
      add lock set
    | _ ->
      set

  let remove ((addr, _) as lock) set =
    match addr with
    | Addr.Addr mv when Addr.Mval.is_definite mv -> (* avoids NULL *)
      remove lock set
    | _ ->
      filter (fun (addr', _) ->
          Addr.semantic_equal addr addr' = Some false
        ) set

  let export_locks ls =
    let f (x,_) set = Mutexes.add x set in
    fold f ls (Mutexes.empty ())
end

module MayLockset =
struct
  include Lockset
  let leq x y = leq y x
  let join = Lockset.meet
  let meet = Lockset.join
  let top = Lockset.bot
  let bot = Lockset.top
end

module MayLocksetNoRW =
struct
  include PreValueDomain.AD
end
