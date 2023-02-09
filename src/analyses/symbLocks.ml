(** Symbolic lock-sets for use in per-element patterns.

    See Section 5 and 6 in https://dl.acm.org/doi/10.1145/2970276.2970337 for more details. *)

module LF = LibraryFunctions
module LP = SymbLocksDomain.LockingPattern
module Exp = SymbLocksDomain.Exp
module ILock = SymbLocksDomain.ILock
module VarEq = VarEq.Spec

module PS = SetDomain.ToppedSet (LP) (struct let topname = "All" end)

open Prelude.Ana
open Analyses

(* Note: This is currently more conservative than varEq --- but
   it should suffice for tests. *)
module Spec =
struct
  include Analyses.DefaultSpec

  exception Top

  module D = LockDomain.Symbolic
  module C = LockDomain.Symbolic

  let name () = "symb_locks"

  let startstate v = D.top ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let branch ctx exp tv = ctx.local
  let body   ctx f = ctx.local

  let invalidate_exp ask exp st =
    D.filter (fun e -> not (VarEq.may_change ask exp e)) st

  let invalidate_lval ask lv st =
    invalidate_exp ask (mkAddrOf lv) st

  let assign ctx lval rval = invalidate_lval (Analyses.ask_of_ctx ctx) lval ctx.local

  let return ctx exp fundec =
    List.fold_right D.remove_var (fundec.sformals@fundec.slocals) ctx.local

  let enter ctx lval f args = [(ctx.local,ctx.local)]
  let combine ctx ?(longjmpthrough = false) lval fexp f args fc st2 = st2

  let get_locks e st =
    let add_perel x xs =
      match LP.from_exps e x with
      | Some x -> PS.add x xs
      | None -> xs
    in
    D.fold add_perel st (PS.empty ())

  let get_all_locks (ask: Queries.ask) e st : PS.t =
    let exps =
      match ask.f (Queries.EqualSet e) with
      | a when not (Queries.ES.is_bot a) -> Queries.ES.add e a
      | _ -> Queries.ES.singleton e
    in
    if M.tracing then M.tracel "symb_locks" "get_all_locks exps %a = %a\n" d_plainexp e Queries.ES.pretty exps;
    if M.tracing then M.tracel "symb_locks" "get_all_locks st = %a\n" D.pretty st;
    let add_locks x xs = PS.union (get_locks x st) xs in
    let r = Queries.ES.fold add_locks exps (PS.empty ()) in
    if M.tracing then M.tracel "symb_locks" "get_all_locks %a = %a\n" d_plainexp e PS.pretty r;
    r

  let same_unknown_index (ask: Queries.ask) exp slocks =
    let uk_index_equal = Queries.must_be_equal ask in
    let lock_index ei ee x xs =
      match Exp.one_unknown_array_index x with
      | Some (true, i, e) when uk_index_equal ei i ->
        PS.add (zero, ee, e) xs
      | _ -> xs
    in
    match Exp.one_unknown_array_index exp with
    | Some (_, i, e) -> D.fold (lock_index i e) slocks (PS.empty ())
    | _ -> PS.empty ()

  let special ctx lval f arglist =
    let desc = LF.find f in
    match desc.special arglist, f.vname with
    | Lock { lock; _ }, _ ->
      D.add (Analyses.ask_of_ctx ctx) lock ctx.local
    | Unlock lock, _ ->
      D.remove (Analyses.ask_of_ctx ctx) lock ctx.local
    | _, _ ->
      let st =
        match lval with
        | Some lv -> invalidate_lval (Analyses.ask_of_ctx ctx) lv ctx.local
        | None -> ctx.local
      in
      let write_args =
        LibraryDesc.Accesses.find_kind desc.accs Write arglist
      in
      (* TODO: why doesn't invalidate_exp involve any reachable for deep write? *)
      List.fold_left (fun st e -> invalidate_exp (Analyses.ask_of_ctx ctx) e st) st write_args


  module A =
  struct
    module E = struct
      include Printable.Either (CilType.Offset) (ILock)

      let pretty () = function
        | `Left o -> Pretty.dprintf "p-lock:%a" (d_offset (text "*")) o
        | `Right addr -> Pretty.dprintf "i-lock:%a" ILock.pretty addr

      include Printable.SimplePretty (
        struct
          type nonrec t = t
          let pretty = pretty
        end
        )
    end
    include SetDomain.Make (E)

    let name () = "symblock"
    let may_race lp lp2 = disjoint lp lp2
    let should_print lp = not (is_empty lp)
  end

  let add_per_element_access ctx e rw =
    (* Per-element returns a triple of exps, first are the "element" pointers,
       in the second and third positions are the respectively access and mutex.
       Access and mutex expressions have exactly the given "elements" as "prefixes".

       To know if a access-mutex pair matches our per-element pattern we listify
       the offset (adding dereferencing to our special offset type). Then we take
       the longest common prefix till a dereference and check if the rest is "concrete".
    *)
    let one_perelem (e,a,l) xs =
      (* ignore (printf "one_perelem (%a,%a,%a)\n" Exp.pretty e Exp.pretty a Exp.pretty l); *)
      if M.tracing then M.tracel "symb_locks" "one_perelem (%a,%a,%a)\n" Exp.pretty e Exp.pretty a Exp.pretty l;
      match Exp.fold_offs (Exp.replace_base (dummyFunDec.svar,`NoOffset) e l) with
      | Some (v, o) ->
        (* ignore (printf "adding lock %s\n" l); *)
        A.add (`Left o) xs
      | None -> xs
    in
    (* Array lockstep also returns a triple of exps. Second and third elements in
       triples are access and mutex exps. Common index is replaced with *.
       First element is unused.

       To find if this pattern matches, we try to separate the base variable and
       the index from both -- access exp and mutex exp. We check if indexes match
       and the rest is concrete. Then replace the common index with *. *)
    let one_lockstep (_,a,m) xs =
      match m with
      | AddrOf (Var v,o) ->
        let lock = ILock.from_var_offset (v, o) in
        A.add (`Right lock) xs
      | _ ->
        Messages.info ~category:Unsound "Internal error: found a strange lockstep pattern.";
        xs
    in
    let do_perel e xs =
      match get_all_locks (Analyses.ask_of_ctx ctx) e ctx.local with
      | a
        when not (PS.is_top a || PS.is_empty a)
        -> PS.fold one_perelem a xs
      | _ -> xs
    in
    let do_lockstep e xs =
      match same_unknown_index (Analyses.ask_of_ctx ctx) e ctx.local with
      | a
        when not (PS.is_top a || PS.is_empty a)
        -> PS.fold one_lockstep a xs
      | _ -> xs
    in
    let matching_exps =
      Queries.ES.meet
        (match ctx.ask (Queries.EqualSet e) with
         | es when not (Queries.ES.is_top es || Queries.ES.is_empty es)
           -> Queries.ES.add e es
         | _ -> Queries.ES.singleton e)
        (match ctx.ask (Queries.Regions e) with
         | ls when not (Queries.LS.is_top ls || Queries.LS.is_empty ls)
           -> let add_exp x xs =
                try Queries.ES.add (Lval.CilLval.to_exp x) xs
                with Lattice.BotValue -> xs
           in begin
             try Queries.LS.fold add_exp ls (Queries.ES.singleton e)
             with Lattice.TopValue -> Queries.ES.top () end
         | _ -> Queries.ES.singleton e)
    in
    Queries.ES.fold do_lockstep matching_exps
      (Queries.ES.fold do_perel matching_exps (A.empty ()))

  let access ctx (a: Queries.access) =
    match a with
    | Point ->
      A.empty ()
    | Memory {exp = e; _} ->
      add_per_element_access ctx e false
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
