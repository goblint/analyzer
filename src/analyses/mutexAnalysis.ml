(** Mutex analysis. *)

module M = Messages
module Addr = ValueDomain.Addr
module Lockset = LockDomain.Lockset
module Mutexes = LockDomain.Mutexes
module LF = LibraryFunctions
open Prelude.Ana
open Analyses
open GobConfig

let big_kernel_lock = LockDomain.Addr.from_var (Goblintutil.create_var (makeGlobalVar "[big kernel lock]" intType))
let console_sem = LockDomain.Addr.from_var (Goblintutil.create_var (makeGlobalVar "[console semaphore]" intType))
let verifier_atomic = LockDomain.Addr.from_var (Goblintutil.create_var (makeGlobalVar "[__VERIFIER_atomic]" intType))

module type SpecParam =
sig
  module G: Lattice.S
  val effect_fun: ?write:bool -> Lockset.t -> G.t
  val check_fun: ?write:bool -> Lockset.t -> G.t
end

(** Mutex analyzer without base --- this is the new standard *)
module MakeSpec (P: SpecParam) =
struct
  include Analyses.IdentitySpec

  (** name for the analysis (btw, it's "Only Mutex Must") *)
  let name () = "mutex"

  (** Add current lockset alongside to the base analysis domain. Global data is collected using dirty side-effecting. *)
  module D = Lockset
  module C = Lockset

  (** We do not add global state, so just lift from [BS]*)
  module G = P.G
  module V = VarinfoV

  let should_join x y = D.equal x y

  let rec conv_offset_inv = function
    | `NoOffset -> `NoOffset
    | `Field (f, o) -> `Field (f, conv_offset_inv o)
    (* TODO: better indices handling *)
    | `Index (_, o) -> `Index (MyCFG.unknown_exp, conv_offset_inv o)



  (** We just lift start state, global and dependency functions: *)
  let startstate v = Lockset.empty ()
  let threadenter ctx lval f args = [Lockset.empty ()]
  let exitstate  v = Lockset.empty ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let non_overlapping locks1 locks2 =
      let intersect = G.join locks1 locks2 in
      G.is_top intersect
    in
    match q with
    | Queries.MayBePublic _ when Lockset.is_bot ctx.local -> false
    | Queries.MayBePublic {global=v; write} ->
      let held_locks: G.t = P.check_fun ~write (Lockset.filter snd ctx.local) in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks ctx.local) then
        false
      else *)
        non_overlapping held_locks (ctx.global v)
    | Queries.MayBePublicWithout _ when Lockset.is_bot ctx.local -> false
    | Queries.MayBePublicWithout {global=v; write; without_mutex} ->
      let held_locks: G.t = P.check_fun ~write (Lockset.remove (without_mutex, true) (Lockset.filter snd ctx.local)) in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if Mutexes.mem verifier_atomic (Lockset.export_locks (Lockset.remove (without_mutex, true) ctx.local)) then
        false
      else *)
         non_overlapping held_locks (ctx.global v)
    | Queries.MustBeProtectedBy {mutex; global; write} ->
      let mutex_lockset = Lockset.singleton (mutex, true) in
      let held_locks: G.t = P.check_fun ~write mutex_lockset in
      (* TODO: unsound in 29/24, why did we do this before? *)
      (* if LockDomain.Addr.equal mutex verifier_atomic then
        true
      else *)
        G.leq (ctx.global global) held_locks
    | Queries.CurrentLockset ->
      let held_locks = Lockset.export_locks (Lockset.filter snd ctx.local) in
      let ls = Mutexes.fold (fun addr ls ->
          match Addr.to_var_offset addr with
          | Some (var, offs) -> Queries.LS.add (var, conv_offset_inv offs) ls
          | None -> ls
        ) held_locks (Queries.LS.empty ())
      in
      ls
    | Queries.MustBeAtomic ->
      let held_locks = Lockset.export_locks (Lockset.filter snd ctx.local) in
      Mutexes.mem verifier_atomic held_locks
    | _ -> Queries.Result.top q

  module A =
  struct
    include D
    let name () = "lock"
    let may_race ls1 ls2 =
      is_empty (join ls1 ls2) (* D is reversed, so join is intersect *)
    let should_print ls = not (is_empty ls)
  end

  let access ctx e vo w =
    if w then
      (* when writing: ignore reader locks *)
      Lockset.filter snd ctx.local
    else
      (* when reading: bump reader locks to exclusive as they protect reads *)
      Lockset.map (fun (x,_) -> (x,true)) ctx.local

  let event ctx e octx =
    match e with
    | Events.Access {var_opt; write} ->
      (*privatization*)
      begin match var_opt with
        | Some v ->
          if not (Lockset.is_bot ctx.local) then
            let ls = Lockset.filter snd ctx.local in
            let el = P.effect_fun ~write ls in
            ctx.sideg v el
        | None -> M.info ~category:Unsound "Write to unknown address: privatization is unsound."
      end;
      ctx.local
    | Events.Lock2 l ->
      let nls = D.add l ctx.local in
      if not (D.equal ctx.local nls) then
        ctx.emit (Lock (fst l));
      nls
    | Events.Unlock2 l ->
      if snd l then
        ctx.emit (Unlock (fst l));
      D.remove l ctx.local
    | _ ->
      ctx.local

end

module MyParam =
struct
  module G = LockDomain.Simple
  let effect_fun ?write:(w=false) ls = Lockset.export_locks ls
  let check_fun = effect_fun
end

module WriteBased =
struct
  module GReadWrite =
  struct
    include LockDomain.Simple
    let name () = "readwrite"
  end
  module GWrite =
  struct
    include LockDomain.Simple
    let name () = "write"
  end
  module G = Lattice.Prod (GReadWrite) (GWrite)
  let effect_fun ?write:(w=false) ls =
    let locks = Lockset.export_locks ls in
    (locks, if w then locks else Mutexes.top ())
  let check_fun ?write:(w=false) ls =
    let locks = Lockset.export_locks ls in
    if w then (Mutexes.bot (), locks) else (locks, Mutexes.bot ())
end

module Spec = MakeSpec (WriteBased)

let _ =
  MCP.register_analysis ~dep:["mutexEvents"; "access"] (module Spec : MCPSpec)
