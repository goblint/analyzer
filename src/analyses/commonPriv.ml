open Prelude.Ana
open Analyses
module Q = Queries

module IdxDom = ValueDomain.IndexDomain


module Locksets =
struct
  module Lock = LockDomain.Addr

  module Lockset =
  struct
    include Printable.Std (* To make it Groupable *)
    include SetDomain.ToppedSet (Lock) (struct let topname = "All locks" end)
    let disjoint s t = is_empty (inter s t)
  end

  let rec conv_offset = function
    | `NoOffset -> `NoOffset
    | `Field (f, o) -> `Field (f, conv_offset o)
    (* TODO: better indices handling *)
    | `Index (_, o) -> `Index (IdxDom.top (), conv_offset o)

  let current_lockset (ask: Q.ask): Lockset.t =
    (* TODO: remove this global_init workaround *)
    if !GU.global_initialization then
      Lockset.empty ()
    else
      let ls = ask.f Queries.CurrentLockset in
      Q.LS.fold (fun (var, offs) acc ->
          Lockset.add (Lock.from_var_offset (var, conv_offset offs)) acc
        ) ls (Lockset.empty ())

  (* TODO: reversed SetDomain.Hoare *)
  module MinLocksets = HoareDomain.Set_LiftTop (Lattice.Reverse (Lockset)) (struct let topname = "All locksets" end) (* reverse Lockset because Hoare keeps maximal, but we need minimal *)
end

module WriteCenteredD =
struct
  open Locksets

  module W =
  struct
    include MapDomain.MapBot_LiftTop (Basetype.Variables) (MinLocksets)
    let name () = "W"
  end

  module P =
  struct
    (* Note different Map order! *)
    (* MapTop because default value in P must be top of MinLocksets,
       as opposed to bottom in W. *)
    include MapDomain.MapTop_LiftBot (Basetype.Variables) (MinLocksets)
    let name () = "P"

    (* TODO: change MinLocksets.exists/top instead? *)
    let find x p = find_opt x p |? MinLocksets.singleton (Lockset.empty ()) (* ensure exists has something to check for thread returns *)
  end
end