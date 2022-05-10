(** Incremental terminating top down solver that optionally only keeps values at widening points and restores other values afterwards. *)
(* Incremental: see paper 'Incremental Abstract Interpretation' https://link.springer.com/chapter/10.1007/978-3-030-41103-9_5 *)
(* TD3: see paper 'Three Improvements to the Top-Down Solver' https://dl.acm.org/doi/10.1145/3236950.3236967
 * Option solvers.td3.* (default) ? true : false (solver in paper):
 * - term (true) ? use phases for widen+narrow (TDside) : use box (TDwarrow)
 * - space (false) ? only keep values at widening points (TDspace + side) in rho : keep all values in rho
 * - space_cache (true) ? local cache l for eval calls in each solve (TDcombined) : no cache
 * - space_restore (true) ? eval each rhs and store all in rho : do not restore missing values
 * For simpler (but unmaintained) versions without the incremental parts see the paper or topDown{,_space_cache_term}.ml.
 *)

open Prelude
open Analyses
open Constraints
open Messages
open CompareCIL
open Cil

module WP =
  functor (Arg: IncrSolverArg) ->
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  struct
    module Post = PostSolver.MakeList (PostSolver.ListArgFromStdArg (S) (HM) (Arg))

    include Generic.SolverStats (S) (HM)
    module VS = Set.Make (S.Var)

    type solver_data = {
      mutable st: (S.Var.t * S.Dom.t) list; (* needed to destabilize start functions if their start state changed because of some changed global initializer *)
      mutable infl: VS.t HM.t;
      mutable sides: VS.t HM.t;
      mutable rho: S.Dom.t HM.t;
      mutable wpoint: unit HM.t;
      mutable stable: unit HM.t
    }

    type marshal = solver_data

    let create_empty_data () = {
      st = [];
      infl = HM.create 10;
      sides = HM.create 10;
      rho = HM.create 10;
      wpoint = HM.create 10;
      stable = HM.create 10
    }

    let clear_data data =
      HM.clear data.infl;
      HM.clear data.stable

    let print_data data str =
      if GobConfig.get_bool "dbg.verbose" then
        Printf.printf "%s:\n|rho|=%d\n|stable|=%d\n|infl|=%d\n|wpoint|=%d\n"
          str (HM.length data.rho) (HM.length data.stable) (HM.length data.infl) (HM.length data.wpoint)

    let verify_data data =
      if GobConfig.get_bool "solvers.td3.verify" then (
        (* every variable in (pruned) rho should be stable *)
        HM.iter (fun x _ ->
            if not (HM.mem data.stable x) then (
              ignore (Pretty.printf "unstable in rho: %a\n" S.Var.pretty_trace x);
              assert false
            )
          ) data.rho
        (* vice versa doesn't currently hold, because stable is not pruned *)
      )

    let exists_key f hm = HM.fold (fun k _ a -> a || f k) hm false

    module P =
    struct
      type t = S.Var.t * S.Var.t [@@deriving eq, hash]
    end

    module HPM = Hashtbl.Make (P)

    type phase = Widen | Narrow

    let solve box st vs data =
      let term  = GobConfig.get_bool "solvers.td3.term" in
      let side_widen = GobConfig.get_string "solvers.td3.side_widen" in
      let space = GobConfig.get_bool "solvers.td3.space" in
      let cache = GobConfig.get_bool "solvers.td3.space_cache" in
      let called = HM.create 10 in

      let infl = data.infl in
      let sides = data.sides in
      let rho = data.rho in
      let wpoint = data.wpoint in
      let stable = data.stable in

      let () = print_solver_stats := fun () ->
        Printf.printf "|rho|=%d\n|called|=%d\n|stable|=%d\n|infl|=%d\n|wpoint|=%d\n"
          (HM.length rho) (HM.length called) (HM.length stable) (HM.length infl) (HM.length wpoint);
        print_context_stats rho
      in

      if GobConfig.get_bool "incremental.load" then (
        print_data data "Loaded data for incremental analysis";
        verify_data data
      );

      let cache_sizes = ref [] in

      let add_infl y x =
        if tracing then trace "sol2" "add_infl %a %a\n" S.Var.pretty_trace y S.Var.pretty_trace x;
        HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty))
      in
      let add_sides y x = HM.replace sides y (VS.add x (try HM.find sides y with Not_found -> VS.empty)) in
      let rec destabilize x =
        if tracing then trace "sol2" "destabilize %a\n" S.Var.pretty_trace x;
        let w = HM.find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        VS.iter (fun y ->
            HM.remove stable y;
            if not (HM.mem called y) then destabilize y
          ) w
      and destabilize_vs x = (* TODO remove? Only used for side_widen cycle. *)
        if tracing then trace "sol2" "destabilize_vs %a\n" S.Var.pretty_trace x;
        let w = HM.find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        VS.fold (fun y b ->
            let was_stable = HM.mem stable y in
            HM.remove stable y;
            HM.mem called y || destabilize_vs y || b || was_stable && List.mem y vs
          ) w false
      and solve ?reuse_eq x phase =
        if tracing then trace "sol2" "solve %a, called: %b, stable: %b\n" S.Var.pretty_trace x (HM.mem called x) (HM.mem stable x);
        init x;
        assert (S.system x <> None);
        if not (HM.mem called x || HM.mem stable x) then (
          HM.replace stable x ();
          HM.replace called x ();
          let wp = HM.mem wpoint x in
          let old = HM.find rho x in
          let l = HM.create 10 in
          let tmp =
            match reuse_eq with
            | Some d -> d
            | None -> eq x (eval l x) (side ~x)
          in
          let new_eq = tmp in
          (* let tmp = if GobConfig.get_bool "ana.opt.hashcons" then S.Dom.join (S.Dom.bot ()) tmp else tmp in (* Call hashcons via dummy join so that the tag of the rhs value is up to date. Otherwise we might get the same value as old, but still with a different tag (because no lattice operation was called after a change), and since Printable.HConsed.equal just looks at the tag, we would unnecessarily destabilize below. Seems like this does not happen. *) *)
          if tracing then trace "sol" "Var: %a\n" S.Var.pretty_trace x ;
          if tracing then trace "sol" "Contrib:%a\n" S.Dom.pretty tmp;
          HM.remove called x;
          let tmp =
            if not wp then tmp
            else
              if term then
                match phase with Widen -> S.Dom.widen old (S.Dom.join old tmp) | Narrow -> S.Dom.narrow old tmp
              else
                box x old tmp
          in
          if tracing then trace "sol" "Old value:%a\n" S.Dom.pretty old;
          if tracing then trace "sol" "New Value:%a\n" S.Dom.pretty tmp;
          if tracing then trace "cache" "cache size %d for %a\n" (HM.length l) S.Var.pretty_trace x;
          cache_sizes := HM.length l :: !cache_sizes;
          if not (Stats.time "S.Dom.equal" (fun () -> S.Dom.equal old tmp) ()) then (
            update_var_event x old tmp;
            HM.replace rho x tmp;
            destabilize x;
            (solve[@tailcall]) x phase;
          ) else if not (HM.mem stable x) then (
            if tracing then trace "sol2" "solve still unstable %a\n" S.Var.pretty_trace x;
            (solve[@tailcall]) x Widen;
          ) else if term && phase = Widen && HM.mem wpoint x then ( (* TODO: or use wp? *)
            if tracing then trace "sol2" "solve switching to narrow %a\n" S.Var.pretty_trace x;
            HM.remove stable x;
            (solve[@tailcall]) ~reuse_eq:new_eq x Narrow;
          ) else if not space && (not term || phase = Narrow) then ( (* this makes e.g. nested loops precise, ex. tests/regression/34-localization/01-nested.c - if we do not remove wpoint, the inner loop head will stay a wpoint and widen the outer loop variable. *)
            if tracing then trace "sol2" "solve removing wpoint %a\n" S.Var.pretty_trace x;
            HM.remove wpoint x;
          )
        )
      and eq x get set =
        if tracing then trace "sol2" "eq %a\n" S.Var.pretty_trace x;
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set
      and simple_solve l x y =
        if tracing then trace "sol2" "simple_solve %a (rhs: %b)\n" S.Var.pretty_trace y (S.system y <> None);
        if S.system y = None then (init y; HM.replace stable y (); HM.find rho y) else
        if HM.mem rho y || not space then (solve y Widen; HM.find rho y) else
        if HM.mem called y then (init y; HM.remove l y; HM.find rho y) else
        (* if HM.mem called y then (init y; let y' = HM.find_default l y (S.Dom.bot ()) in HM.replace rho y y'; HM.remove l y; y') else *)
        if cache && HM.mem l y then HM.find l y
        else (
          HM.replace called y ();
          let tmp = eq y (eval l x) (side ~x) in
          HM.remove called y;
          if HM.mem rho y then (HM.remove l y; solve y Widen; HM.find rho y)
          else (if cache then HM.replace l y tmp; tmp)
        )
      and eval l x y =
        if tracing then trace "sol2" "eval %a ## %a\n" S.Var.pretty_trace x S.Var.pretty_trace y;
        get_var_event y;
        if HM.mem called y then HM.replace wpoint y ();
        let tmp = simple_solve l x y in
        if HM.mem rho y then add_infl y x;
        tmp
      and side ?x y d = (* side from x to y; only to variables y w/o rhs; x only used for trace *)
        if tracing then trace "sol2" "side to %a (wpx: %b) from %a ## value: %a\n" S.Var.pretty_trace y (HM.mem wpoint y) (Pretty.docOpt (S.Var.pretty_trace ())) x S.Dom.pretty d;
        if S.system y <> None then (
          ignore @@ Pretty.printf "side-effect to unknown w/ rhs: %a, contrib: %a\n" S.Var.pretty_trace y S.Dom.pretty d;
        );
        assert (S.system y = None);
        init y;
        (match x with None -> () | Some x -> if side_widen = "unstable_self" then add_infl x y);
        let op =
          if HM.mem wpoint y then fun a b ->
            if M.tracing then M.traceli "sol2" "side widen %a %a\n" S.Dom.pretty a S.Dom.pretty b;
            let r = S.Dom.widen a (S.Dom.join a b) in
            if M.tracing then M.traceu "sol2" "-> %a\n" S.Dom.pretty r;
            r
          else S.Dom.join
        in
        let old = HM.find rho y in
        let tmp = op old d in
        HM.replace stable y ();
        if not (S.Dom.leq tmp old) then (
          (* if there already was a `side x y d` that changed rho[y] and now again, we make y a wpoint *)
          let old_sides = HM.find_default sides y VS.empty in
          let sided = match x with
            | Some x ->
              let sided = VS.mem x old_sides in
              if not sided then add_sides y x;
              sided
            | None -> false
          in
          (* HM.replace rho y ((if HM.mem wpoint y then S.Dom.widen old else identity) (S.Dom.join old d)); *)
          HM.replace rho y tmp;
          if side_widen <> "cycle" then destabilize y;
          (* make y a widening point if ... This will only matter for the next side _ y.  *)
          let wpoint_if e = if e then HM.replace wpoint y () in
          match side_widen with
          | "always" -> (* Any side-effect after the first one will be widened which will unnecessarily lose precision. *)
            wpoint_if true
          | "never" -> (* On side-effect cycles, this should terminate via the outer `solver` loop. TODO check. *)
            wpoint_if false
          | "sides" -> (* x caused more than one update to y. >=3 partial context calls will be precise since sides come from different x. TODO this has 8 instead of 5 phases of `solver` for side_cycle.c *)
            wpoint_if sided
          | "sides-pp" ->
            (match x with
            | Some x ->
              let n = S.Var.node x in
              let sided = VS.exists (fun v -> Node.equal (S.Var.node v) n) old_sides in
              wpoint_if sided
            | None -> ())
          | "cycle" -> (* destabilized a called or start var. Problem: two partial context calls will be precise, but third call will widen the state. *)
            (* if this side destabilized some of the initial unknowns vs, there may be a side-cycle between vs and we should make y a wpoint *)
            let destabilized_vs = destabilize_vs y in
            wpoint_if destabilized_vs
          (* TODO: The following two don't check if a vs got destabilized which may be a problem. *)
          | "unstable_self" -> (* TODO test/remove. Side to y destabilized itself via some infl-cycle. The above add_infl is only required for this option. Check for which examples this is problematic! *)
            wpoint_if @@ not (HM.mem stable y)
          | "unstable_called" -> (* TODO test/remove. Widen if any called var (not just y) is no longer stable. Expensive! *)
            wpoint_if @@ exists_key (neg (HM.mem stable)) called (* this is very expensive since it folds over called! see https://github.com/goblint/analyzer/issues/265#issuecomment-880748636 *)
          | x -> failwith ("Unknown value '" ^ x ^ "' for option solvers.td3.side_widen!")
        )
      and init x =
        if tracing then trace "sol2" "init %a\n" S.Var.pretty_trace x;
        if not (HM.mem rho x) then (
          new_var_event x;
          HM.replace rho x (S.Dom.bot ())
        )
      in

      let set_start (x,d) =
        if tracing then trace "sol2" "set_start %a ## %a\n" S.Var.pretty_trace x S.Dom.pretty d;
        init x;
        HM.replace rho x d;
        HM.replace stable x ();
        (* solve x Widen *)
      in

      start_event ();

      if GobConfig.get_bool "incremental.load" then (
        let c = S.increment.changes in
        List.(Printf.printf "change_info = { unchanged = %d; changed = %d; added = %d; removed = %d }\n" (length c.unchanged) (length c.changed) (length c.added) (length c.removed));

        let filter_map f l =
          List.fold_left (fun acc el -> match f el with Some x -> x::acc | _ -> acc) [] l
        in
        let changed_funs = filter_map (fun c -> match c.old, c.diff with GFun (f,l), None -> Some f | _ -> None) S.increment.changes.changed in
        let part_changed_funs = filter_map (fun c -> match c.old, c.diff with GFun (f,l), Some nd -> Some (f,nd.primObsoleteNodes,nd.unchangedNodes) | _ -> None) S.increment.changes.changed in
        let prim_old_nodes_ids = Set.of_list (List.concat_map (fun (_,pn,_) -> List.map Node.show_id pn) part_changed_funs) in
        let removed_funs = filter_map (fun g -> match g with GFun (f,l) -> Some f | _ -> None) S.increment.changes.removed in
        (* TODO: don't use string-based nodes, make obsolete of type Node.t BatSet.t *)
        let obsolete_ret = Set.union (Set.of_list (List.map (fun f -> Node.show_id (Function f)) changed_funs))
                                     (Set.of_list (List.map (fun (f,_,_) -> Node.show_id (Function f)) part_changed_funs)) in
        let obsolete_entry = Set.of_list (List.map (fun f -> Node.show_id (FunctionEntry f)) changed_funs) in

        List.iter (fun a -> print_endline ("Completely changed function: " ^ a.svar.vname)) changed_funs;
        List.iter (fun (f,_,_) -> print_endline ("Partially changed function: " ^ (f.svar.vname))) part_changed_funs;

        let old_ret = Hashtbl.create 103 in
        if GobConfig.get_bool "incremental.reluctant.on" then (
          (* save entries of changed functions in rho for the comparison whether the result has changed after a function specific solve *)
          HM.iter (fun k v -> if Set.mem (S.Var.var_id k) obsolete_ret then ( (* TODO: don't use string-based nodes *)
              let old_rho = HM.find rho k in
              let old_infl = HM.find_default infl k VS.empty in
              Hashtbl.replace old_ret k (old_rho, old_infl))) rho;
        ) else (
          (* If reluctant destabilization is turned off we need to destabilize all nodes in completely changed functions
             and the primary obsolete nodes of partly changed functions *)
          print_endline "Destabilizing changed functions and primary old nodes ...";
          HM.iter (fun k _ -> if Set.mem (S.Var.var_id k) obsolete_entry || Set.mem (S.Var.var_id k) prim_old_nodes_ids then destabilize k) stable;
        );

        (* We remove all unknowns for program points in changed or removed functions from rho, stable, infl and wpoint *)
        (* TODO: don't use string-based nodes, make marked_for_deletion of type unit (Hashtbl.Make (Node)).t *)
        let add_nodes_of_fun (functions: fundec list) (nodes) withEntry =
          let add_stmts (f: fundec) =
            List.iter (fun s -> Hashtbl.replace nodes (Node.show_id (Statement s)) ()) (f.sallstmts)
          in
          List.iter (fun f -> if withEntry then Hashtbl.replace nodes (Node.show_id (FunctionEntry f)) (); Hashtbl.replace nodes (Node.show_id (Function f)) (); add_stmts f; Hashtbl.replace nodes (string_of_int (CfgTools.get_pseudo_return_id f)) ()) functions;
        in

        let marked_for_deletion = Hashtbl.create 103 in
        add_nodes_of_fun changed_funs marked_for_deletion (not (GobConfig.get_bool "incremental.reluctant.on"));
        add_nodes_of_fun removed_funs marked_for_deletion true;
        (* it is necessary to remove all unknowns for changed pseudo-returns because they have static ids *)
        let add_pseudo_return f un =
          let pid = CfgTools.get_pseudo_return_id f in
          let is_pseudo_return n = match n with MyCFG.Statement s -> s.sid = pid | _ -> false in
          if not (List.exists (fun x -> is_pseudo_return @@ fst @@ x) un)
          then Hashtbl.replace marked_for_deletion (string_of_int pid) () in
        List.iter (fun (f,_,un) -> Hashtbl.replace marked_for_deletion (Node.show_id (Function f)) (); add_pseudo_return f un) part_changed_funs;

        print_endline "Removing data for changed and removed functions...";
        let delete_marked s = HM.filteri_inplace (fun k _ -> not (Hashtbl.mem  marked_for_deletion (S.Var.var_id k))) s in (* TODO: don't use string-based nodes *)
        delete_marked rho;
        delete_marked infl;
        delete_marked wpoint;
        delete_marked stable;


        print_data data "Data after clean-up";

        (* Call side on all globals and functions in the start variables to make sure that changes in the initializers are propagated.
         * This also destabilizes start functions if their start state changes because of globals that are neither in the start variables nor in the contexts *)
        List.iter (fun (v,d) -> side v d) st;

        if GobConfig.get_bool "incremental.reluctant.on" then (
          (* solve on the return node of changed functions. Only destabilize the function's return node if the analysis result changed *)
          print_endline "Separately solving changed functions...";
          let op = if GobConfig.get_string "incremental.reluctant.compare" = "leq" then S.Dom.leq else S.Dom.equal in
          Hashtbl.iter (
            fun x (old_rho, old_infl) ->
              ignore @@ Pretty.printf "test for %a\n" Node.pretty_trace (S.Var.node x);
              solve x Widen;
              if not (op (HM.find rho x) old_rho) then (
                print_endline "Destabilization required...";
                HM.replace infl x old_infl;
                destabilize x;
                HM.replace stable x ()
              )
          ) old_ret;

          print_endline "Final solve..."
        )
      ) else (
        List.iter set_start st;
      );
      List.iter init vs;
      (* If we have multiple start variables vs, we might solve v1, then while solving v2 we side some global which v1 depends on with a new value. Then v1 is no longer stable and we have to solve it again. *)
      let i = ref 0 in
      let rec solver () = (* as while loop in paper *)
        incr i;
        let unstable_vs = List.filter (neg (HM.mem stable)) vs in
        if unstable_vs <> [] then (
          if GobConfig.get_bool "dbg.verbose" then (
            if !i = 1 then print_newline ();
            Printf.printf "Unstable solver start vars in %d. phase:\n" !i;
            List.iter (fun v -> ignore @@ Pretty.printf "\t%a\n" S.Var.pretty_trace v) unstable_vs;
            print_newline ();
            flush_all ();
          );
          List.iter (fun x -> solve x Widen) unstable_vs;
          solver ();
        )
      in
      solver ();
      (* Before we solved all unstable vars in rho with a rhs in a loop. This is unneeded overhead since it also solved unreachable vars (reachability only removes those from rho further down). *)
      (* After termination, only those variables are stable which are
       * - reachable from any of the queried variables vs, or
       * - effected by side-effects and have no constraints on their own (this should be the case for all of our analyses). *)

      (* verifies values at widening points and adds values for variables in-between *)
      let visited = HM.create 10 in
      let check_side x y d =
        HM.replace visited y ();
        let mem = HM.mem rho y in
        let d' = try HM.find rho y with Not_found -> S.Dom.bot () in
        if not (S.Dom.leq d d') then ignore @@ Pretty.printf "TDFP Fixpoint not reached in restore step at side-effected variable (mem: %b) %a from %a: %a not leq %a\n" mem S.Var.pretty_trace y S.Var.pretty_trace x S.Dom.pretty d S.Dom.pretty d'
      in
      let rec eq check x =
        HM.replace visited x ();
        match S.system x with
        | None -> if HM.mem rho x then HM.find rho x else (ignore @@ Pretty.printf "TDFP Found variable %a w/o rhs and w/o value in rho\n" S.Var.pretty_trace x; S.Dom.bot ())
        | Some f -> f (get ~check) (check_side x)
      and get ?(check=false) x =
        if HM.mem visited x then (
          HM.find rho x
        ) else if HM.mem rho x then ( (* `vs` are in `rho`, so to restore others we need to skip to `eq`. *)
          let d1 = HM.find rho x in
          let d2 = eq check x in (* just to reach unrestored variables *)
          if check then (
            if not (HM.mem stable x) && S.system x <> None then ignore @@ Pretty.printf "TDFP Found an unknown in rho that should be stable: %a\n" S.Var.pretty_trace x;
            if not (S.Dom.leq d2 d1) then
              ignore @@ Pretty.printf "TDFP Fixpoint not reached in restore step at %a\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\nCalculating one more step changes: %a\n@]" S.Var.pretty_trace x S.Dom.pretty d1 S.Dom.pretty d2 S.Dom.pretty_diff (d1,d2);
          );
          d1
        ) else (
          let d = eq check x in
          HM.replace rho x d;
          d
        )
      in
      (* restore values for non-widening-points *)
      if space && GobConfig.get_bool "solvers.td3.space_restore" then (
        if GobConfig.get_bool "dbg.verbose" then
          print_endline ("Restoring missing values.");
        let restore () =
          let get x =
            let d = get ~check:true x in
            if tracing then trace "sol2" "restored var %a ## %a\n" S.Var.pretty_trace x S.Dom.pretty d
          in
          List.iter get vs;
          HM.filteri_inplace (fun x _ -> HM.mem visited x) rho
        in
        Stats.time "restore" restore ();
        if GobConfig.get_bool "dbg.verbose" then ignore @@ Pretty.printf "Solved %d vars. Total of %d vars after restore.\n" !Goblintutil.vars (HM.length rho);
        let avg xs = if List.is_empty !cache_sizes then 0.0 else float_of_int (BatList.sum xs) /. float_of_int (List.length xs) in
        if tracing then trace "cache" "#caches: %d, max: %d, avg: %.2f\n" (List.length !cache_sizes) (List.max !cache_sizes) (avg !cache_sizes);
      );

      stop_event ();
      print_data data "Data after solve completed";

      if GobConfig.get_bool "dbg.print_wpoints" then (
        Printf.printf "\nWidening points:\n";
        HM.iter (fun k () -> ignore @@ Pretty.printf "%a\n" S.Var.pretty_trace k) wpoint;
        print_newline ();
      );

      Post.post st vs rho; (* TODO: add side_infl postsolver *)

      verify_data data;
      {st; infl; sides; rho; wpoint; stable}

    let solve box st vs =
      let reuse_stable = GobConfig.get_bool "incremental.stable" in
      let reuse_wpoint = GobConfig.get_bool "incremental.wpoint" in
      if GobConfig.get_bool "incremental.load" then (
        let loaded, data = match S.increment.old_data with
          | Some d -> true, Obj.obj d.solver_data
          | _ -> false, create_empty_data ()
        in
        (* This hack is for fixing hashconsing.
         * If hashcons is enabled now, then it also was for the loaded values (otherwise it would crash). If it is off, we don't need to do anything.
         * HashconsLifter uses BatHashcons.hashcons on Lattice operations like join, so we call join (with bot) to make sure that the old values will populate the empty hashcons table via side-effects and at the same time get new tags that are conform with its state.
         * The tags are used for `equals` and `compare` to avoid structural comparisons. TODO could this be replaced by `==` (if values are shared by hashcons they should be physically equal)?
         * We have to replace all tags since they are not derived from the value (like hash) but are incremented starting with 1, i.e. dependent on the order in which lattice operations for different values are called, which will very likely be different for an incremental run.
         * If we didn't do this, during solve, a rhs might give the same value as from the old rho but it wouldn't be detected as equal since the tags would be different.
         * In the worst case, every rhs would yield the same value, but we would destabilize for every var in rho until we replaced all values (just with new tags).
         * The other problem is that we would likely use more memory since values from old rho would not be shared with the same values in the hashcons table. So we would keep old values in memory until they are replace in rho and eventually garbage collected.
         *)
        (* Another problem are the tags for the context part of a S.Var.t.
         * This will cause problems when old and new vars interact or when new S.Dom values are used as context:
         * - reachability is a problem since it marks vars reachable with a new tag, which will remove vars with the same context but old tag from rho.
         * - If we destabilized a node with a call, we will also destabilize all vars of the called function. However, if we end up with the same state at the caller node, without hashcons we would only need to go over all vars in the function once to restabilize them since we have
         *   the old values, whereas with hashcons, we would get a context with a different tag, could not find the old value for that var, and have to recompute all vars in the function (without access to old values).
         *)
        if loaded && S.increment.server then (
          data.rho <- HM.copy data.rho;
          data.stable <- HM.copy data.stable;
          data.wpoint <- HM.copy data.wpoint;
          data.infl <- HM.copy data.infl;
          (* data.st is immutable, no need to copy *)
        )
        else if loaded && GobConfig.get_bool "ana.opt.hashcons" then (
          let rho' = HM.create (HM.length data.rho) in
          HM.iter (fun k v ->
            (* call hashcons on contexts and abstract values; results in new tags *)
            let k' = S.Var.relift k in
            let v' = S.Dom.relift v in
            HM.replace rho' k' v';
          ) data.rho;
          data.rho <- rho';
          let stable' = HM.create (HM.length data.stable) in
          HM.iter (fun k v ->
            HM.replace stable' (S.Var.relift k) v
          ) data.stable;
          data.stable <- stable';
          let wpoint' = HM.create (HM.length data.wpoint) in
          HM.iter (fun k v ->
            HM.replace wpoint' (S.Var.relift k) v
          ) data.wpoint;
          data.wpoint <- wpoint';
          let infl' = HM.create (HM.length data.infl) in
          HM.iter (fun k v ->
            HM.replace infl' (S.Var.relift k) (VS.map S.Var.relift v)
          ) data.infl;
          data.infl <- infl';
          data.st <- List.map (fun (k, v) -> S.Var.relift k, S.Dom.relift v) data.st;
        );
        if not reuse_stable then (
          print_endline "Destabilizing everything!";
          data.stable <- HM.create 10;
          data.infl <- HM.create 10
        );
        if not reuse_wpoint then data.wpoint <- HM.create 10;
        let result = solve box st vs data in
        result.rho, result
      )
      else (
        let data = create_empty_data () in
        let result = solve box st vs data in
        result.rho, result
      )
  end

let _ =
  Selector.add_solver ("td3", (module WP : GenericEqBoxIncrSolver));
