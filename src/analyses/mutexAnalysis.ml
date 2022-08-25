(** Protecting mutex analysis. Must locksets locally and for globals. *)

module M = Messages
module Addr = ValueDomain.Addr
module Lockset = LockDomain.Lockset
module Mutexes = LockDomain.Mutexes
module LF = LibraryFunctions
open Prelude.Ana
open Analyses


module type SpecParam =
sig
  module G: Lattice.S
  val effect_fun: ?write:bool -> Lockset.t -> G.t
  val check_fun: ?write:bool -> Lockset.t -> G.t
end

module MakeSpec (P: SpecParam) =
struct
  module Arg =
  struct
    module D = Lockset
    module G = P.G
    module V = VarinfoV

    let add ctx l =
      D.add l ctx.local

    let remove ctx l =
      D.remove (l, true) (D.remove (l, false) ctx.local)

    let remove_all ctx =
      (* Mutexes.iter (fun m ->
           ctx.emit (MustUnlock m)
         ) (D.export_locks ctx.local); *)
      (* TODO: used to have remove_nonspecial, which kept v.vname.[0] = '{' variables *)
      D.empty ()
  end
  include LocksetAnalysis.MakeMust (Arg)
  let name () = "mutex"

  module D = Arg.D (* help type checker using explicit constraint *)
  let should_join x y = D.equal x y

  (** Global data is collected using dirty side-effecting. *)
  module G = P.G
  module V = VarinfoV

  let rec conv_offset_inv = function
    | `NoOffset -> `NoOffset
    | `Field (f, o) -> `Field (f, conv_offset_inv o)
    | `Index (i, o) ->
      let i_exp =
        match ValueDomain.IndexDomain.to_int i with
        | Some i -> Const (CInt (i, Cilfacade.ptrdiff_ikind (), Some (Z.to_string i)))
        | None -> MyCFG.unknown_exp
      in
      `Index (i_exp, conv_offset_inv o)

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
    | Queries.MustLockset ->
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
      Mutexes.mem MutexEventsAnalysis.verifier_atomic held_locks
    | Queries.IterSysVars (Global g, f) ->
      f (Obj.repr g)
    | _ -> Queries.Result.top q

  module A =
  struct
    include D
    let name () = "lock"
    let may_race ls1 ls2 =
      (* not mutually exclusive *)
      not @@ D.exists (fun ((m1, w1) as l1) ->
          if w1 then
            (* write lock is exclusive with write lock or read lock *)
            D.mem l1 ls2 || D.mem (m1, false) ls2
          else
            (* read lock is exclusive with just write lock *)
            D.mem (m1, true) ls2
        ) ls1
    let should_print ls = not (is_empty ls)
  end

  let access ctx (a: Queries.access) =
    ctx.local

  let event ctx e octx =
    match e with
    | Events.Access {exp; lvals; kind; reach} when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* threadflag query in post-threadspawn ctx *)
      let ctx = octx in (* must use original (pre-assign, etc) ctx queries *)
      (* TODO: simplify *)
      let old_access var_opt offs_opt (kind: AccessKind.t) =
        (*privatization*)
        match var_opt with
        | Some v ->
          if not (Lockset.is_bot ctx.local) then
            let ls = Lockset.filter snd ctx.local in
            let write = match kind with
              | Write | Free -> true
              | Read -> false
              | Spawn -> false (* TODO: nonsense? *)
            in
            let el = P.effect_fun ~write ls in
            ctx.sideg v el
        | None -> M.info ~category:Unsound "Write to unknown address: privatization is unsound."
      in
      let module LS = Queries.LS in
      let part_access ctx (vo:varinfo option) (oo: offset option) (kind: AccessKind.t) =
        old_access vo oo kind
      in
      let add_access vo oo =
        part_access ctx vo oo kind
      in
      let add_access_struct ci =
        part_access ctx None None kind
      in
      let has_escaped g = ctx.ask (Queries.MayEscape g) in
      let on_lvals ls includes_uk =
        let ls = LS.filter (fun (g,_) -> g.vglob || has_escaped g) ls in
        let f (var, offs) =
          let coffs = Lval.CilLval.to_ciloffs offs in
          if CilType.Varinfo.equal var dummyFunDec.svar then
            add_access None (Some coffs)
          else
            add_access (Some var) (Some coffs)
        in
        LS.iter f ls
      in
      begin match lvals with
        | ls when not (LS.is_top ls) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
          (* the case where the points-to set is non top and does not contain unknown values *)
          on_lvals ls false
        | ls when not (LS.is_top ls) ->
          (* the case where the points-to set is non top and contains unknown values *)
          let includes_uk = ref false in
          (* now we need to access all fields that might be pointed to: is this correct? *)
          begin match ctx.ask (ReachableUkTypes exp) with
            | ts when Queries.TS.is_top ts ->
              includes_uk := true
            | ts ->
              if Queries.TS.is_empty ts = false then
                includes_uk := true;
              let f = function
                | TComp (ci, _) ->
                  add_access_struct ci
                | _ -> ()
              in
              Queries.TS.iter f ts
          end;
          on_lvals ls !includes_uk
        | _ ->
          add_access None None
      end;
      ctx.local (* TODO: don't return octx.local! *)
    | _ ->
      event ctx e octx (* delegate to must lockset analysis *)
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
  let effect_fun ?(write=false) ls =
    let locks = Lockset.export_locks ls in
    (locks, if write then locks else Mutexes.top ())
  let check_fun ?(write=false) ls =
    let locks = Lockset.export_locks ls in
    if write then (Mutexes.bot (), locks) else (locks, Mutexes.bot ())
end

module Spec = MakeSpec (WriteBased)

let _ =
  MCP.register_analysis ~dep:["mutexEvents"; "access"] (module Spec : MCPSpec)
