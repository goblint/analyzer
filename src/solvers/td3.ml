(** Incremental terminating top down solver that optionally only keeps values at widening points and restores other values afterwards. *)
(* Incremental: see paper 'Incremental Abstract Interpretation' https://link.springer.com/chapter/10.1007/978-3-030-41103-9_5 *)
(* TD3: see paper 'Three Improvements to the Top-Down Solver' https://dl.acm.org/doi/10.1145/3236950.3236967
 * Option exp.solver.td3.* (default) ? true : false (solver in paper):
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
open CompareAST
open Cil

let result_file_name = "td3.data"

let incremental_mode = ref "off"

module WP =
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct
    include Generic.SolverStats (S) (HM)
    module VS = Set.Make (S.Var)

    type solver_data = {
      mutable st: (S.Var.t * S.Dom.t) list; (* needed to destabilize start functions if their start state changed because of some changed global initializer *)
      mutable infl: VS.t HM.t;
      mutable rho: S.Dom.t HM.t;
      mutable wpoint: unit HM.t;
      mutable stable: unit HM.t
    }

    let create_empty_data () = {
      st = [];
      infl = HM.create 10;
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

    let exists_key f hm = HM.fold (fun k _ a -> a || f k) hm false

    module P =
    struct
      type t = S.Var.t * S.Var.t [@@deriving eq]
      let hash  (x1,x2)         = (S.Var.hash x1 * 13) + S.Var.hash x2
    end

    module HPM = Hashtbl.Make (P)

    type phase = Widen | Narrow

    let solve box st vs data =
      let term  = GobConfig.get_bool "exp.solver.td3.term" in
      let side_widen = GobConfig.get_string "exp.solver.td3.side_widen" in
      let space = GobConfig.get_bool "exp.solver.td3.space" in
      let cache = GobConfig.get_bool "exp.solver.td3.space_cache" in
      let called = HM.create 10 in

      let infl = data.infl in
      let rho = data.rho in
      let wpoint = data.wpoint in
      let stable = data.stable in

      let () = print_solver_stats := fun () ->
        Printf.printf "|rho|=%d\n|called|=%d\n|stable|=%d\n|infl|=%d\n|wpoint|=%d\n"
          (HM.length rho) (HM.length called) (HM.length stable) (HM.length infl) (HM.length wpoint);
        print_context_stats rho
      in

      if !incremental_mode = "incremental" then print_data data "Loaded data for incremental analysis";

      let cache_sizes = ref [] in

      let add_infl y x =
        if tracing then trace "sol2" "add_infl %a %a\n" S.Var.pretty_trace y S.Var.pretty_trace x;
        HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty))
      in
      let rec destabilize x =
        if tracing then trace "sol2" "destabilize %a\n" S.Var.pretty_trace x;
        let w = HM.find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        VS.fold (fun y b ->
            let was_stable = HM.mem stable y in
            HM.remove stable y;
            if HM.mem called y then b
            else destabilize y || was_stable && List.mem y vs || b
          ) w false
      and solve x phase =
        if tracing then trace "sol2" "solve %a, called: %b, stable: %b\n" S.Var.pretty_trace x (HM.mem called x) (HM.mem stable x);
        init x;
        assert (S.system x <> None);
        if not (HM.mem called x || HM.mem stable x) then (
          HM.replace stable x ();
          HM.replace called x ();
          let wp = HM.mem wpoint x in
          let old = HM.find rho x in
          let l = HM.create 10 in
          let tmp = eq x (eval l x) (side x) in
          (* let tmp = if GobConfig.get_bool "ana.opt.hashcons" then S.Dom.join (S.Dom.bot ()) tmp else tmp in (* Call hashcons via dummy join so that the tag of the rhs value is up to date. Otherwise we might get the same value as old, but still with a different tag (because no lattice operation was called after a change), and since Printable.HConsed.equal just looks at the tag, we would uneccessarily destabilize below. Seems like this does not happen. *) *)
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
          if tracing then trace "cache" "cache size %d for %a\n" (HM.length l) S.Var.pretty_trace x;
          cache_sizes := HM.length l :: !cache_sizes;
          if not (Stats.time "S.Dom.equal" (fun () -> S.Dom.equal old tmp) ()) then (
            update_var_event x old tmp;
            if tracing then trace "sol" "New Value:%a\n\n" S.Dom.pretty tmp;
            HM.replace rho x tmp;
            let _ = destabilize x in
            (solve[@tailcall]) x phase;
          ) else if not (HM.mem stable x) then (
            (solve[@tailcall]) x Widen;
          ) else if term && phase = Widen then (
            HM.remove stable x;
            (solve[@tailcall]) x Narrow;
          ) else if not space && (not term || phase = Narrow) then (* this makes e.g. nested loops precise, ex. tests/regression/34-localization/01-nested.c - if we do not remove wpoint, the inner loop head will stay a wpoint and widen the outer loop variable. *)
            HM.remove wpoint x;
        )
      and eq x get set =
        if tracing then trace "sol2" "eq %a\n" S.Var.pretty_trace x;
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set
      and simple_solve l x y =
        if tracing then trace "sol2" "simple_solve %a (rhs: %b)\n" S.Var.pretty_trace y (S.system y <> None);
        if S.system y = None then (init y; HM.find rho y) else
        if HM.mem rho y || not space then (solve y Widen; HM.find rho y) else
        if HM.mem called y then (init y; HM.remove l y; HM.find rho y) else
        (* if HM.mem called y then (init y; let y' = HM.find_default l y (S.Dom.bot ()) in HM.replace rho y y'; HM.remove l y; y') else *)
        if cache && HM.mem l y then HM.find l y
        else (
          HM.replace called y ();
          let tmp = eq y (eval l x) (side x) in
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
      and side x y d = (* side from x to y; only to variables y w/o rhs; x only used for trace *)
        if tracing then trace "sol2" "side to %a (wpx: %b) from %a ## value: %a\n" S.Var.pretty_trace y (HM.mem wpoint y) S.Var.pretty_trace x S.Dom.pretty d;
        if S.system y <> None then (
          ignore @@ Pretty.printf "side-effect to unknown w/ rhs: %a, contrib: %a\n" S.Var.pretty_trace y S.Dom.pretty d;
        );
        assert (S.system y = None);
        init y;
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
          (* HM.replace rho y ((if HM.mem wpoint y then S.Dom.widen old else identity) (S.Dom.join old d)); *)
          HM.replace rho y tmp;
          (* if this side destabilized some of the initial unknowns vs, there may be a side-cycle between vs and we should make y a wpoint *)
          let destabilized_vs = destabilize y in
          (* make y a widening point if ... *)
          let widen_if e = if destabilized_vs || e then HM.replace wpoint y () in
          match side_widen with
          | "always" -> (* any side-effect after the first one will be widened which will unnecessarily lose precision *)
            widen_if true
          | "cycle_self" -> (* widen if the side-effect to y destabilized itself via some infl-cycle *)
            widen_if @@ not (HM.mem stable y)
          | "cycle" -> (* widen if any called var (not just y) is no longer stable *)
            widen_if @@ exists_key (neg (HM.mem stable)) called
          | "never" | _ -> widen_if false (* on side-effect cycles, this should terminate via the outer `solver` loop *)
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

      if !incremental_mode = "incremental" then (
        let c = S.increment.changes in
        List.(Printf.printf "change_info = { unchanged = %d; changed = %d; added = %d; removed = %d }\n" (length c.unchanged) (length c.changed) (length c.added) (length c.removed));
        (* If a global changes because of some assignment inside a function, we reanalyze,
         * but if it changes because of a different global initializer, then
         *   if not exp.earlyglobs: the contexts of start functions will change, we don't find the value in rho and reanalyze;
         *   if exp.earlyglobs: the contexts will be the same since they don't contain the global, but the start state will be different!
         *)
        print_endline "Destabilizing start functions if their start state changed...";
        (* ignore @@ Pretty.printf "st: %d, data.st: %d\n" (List.length st) (List.length data.st); *)
        List.iter (fun (v,d) ->
          match List.assoc_opt v data.st with
          | Some d' ->
              if S.Dom.equal d d' then
                (* ignore @@ Pretty.printf "Function %a has the same state %a\n" S.Var.pretty_trace v S.Dom.pretty d *)
                ()
              else (
                ignore @@ Pretty.printf "Function %a has changed start state: %a\n" S.Var.pretty_trace v S.Dom.pretty_diff (d, d');
                ignore @@ destabilize v
              )
          | None -> ignore @@ Pretty.printf "New start function %a not found in old list!\n" S.Var.pretty_trace v
        ) st;

        print_endline "Destabilizing changed functions...";

        (* We need to destabilize all nodes in changed functions *)
        let filter_map f l =
          List.fold_left (fun acc el -> match f el with Some x -> x::acc | _ -> acc) [] l
        in
        let obsolete_funs = filter_map (fun c -> match c.old with GFun (f,l) -> Some f | _ -> None) S.increment.changes.changed in
        let removed_funs = filter_map (fun g -> match g with GFun (f,l) -> Some f | _ -> None) S.increment.changes.removed in
        let obsolete = Set.union (Set.of_list (List.map (fun a -> "ret" ^ (string_of_int a.Cil.svar.vid))  obsolete_funs))
                                 (Set.of_list (List.map (fun a -> "fun" ^ (string_of_int a.Cil.svar.vid))  obsolete_funs)) in

        List.iter (fun a -> print_endline ("Obsolete function: " ^ a.svar.vname)) obsolete_funs;

        (* Actually destabilize all nodes contained in changed functions. TODO only destabilize fun_... nodes *)
        HM.iter (fun k v -> if Set.mem (S.Var.var_id k) obsolete then ignore @@ destabilize k) stable;

        (* We remove all unknowns for program points in changed or removed functions from rho, stable, infl and wpoint *)
        let add_nodes_of_fun (functions: fundec list) (nodes)=
          let add_stmts (f: fundec) =
            List.iter (fun s -> Hashtbl.replace nodes (string_of_int s.sid) ()) (f.sallstmts)
          in
          List.iter (fun f -> Hashtbl.replace nodes ("fun"^(string_of_int f.svar.vid)) (); Hashtbl.replace nodes ("ret"^(string_of_int f.svar.vid)) (); add_stmts f) functions;
        in

        let marked_for_deletion = Hashtbl.create 103 in
        add_nodes_of_fun obsolete_funs marked_for_deletion;
        add_nodes_of_fun removed_funs marked_for_deletion;

        print_endline "Removing data for changed and removed functions...";
        let delete_marked s = HM.iter (fun k v -> if Hashtbl.mem  marked_for_deletion (S.Var.var_id k) then HM.remove s k ) s in
        delete_marked rho;
        delete_marked infl;
        delete_marked wpoint;
        delete_marked stable;

        print_data data "Data after clean-up"
      );

      List.iter set_start st;
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
      if space && GobConfig.get_bool "exp.solver.td3.space_restore" then (
        if GobConfig.get_bool "dbg.verbose" then
          print_endline ("Restoring missing values.");
        let restore () =
          let get x =
            let d = get ~check:true x in
            if tracing then trace "sol2" "restored var %a ## %a\n" S.Var.pretty_trace x S.Dom.pretty d
          in
          List.iter get vs;
          HM.iter (fun x v -> if not (HM.mem visited x) then HM.remove rho x) rho
        in
        Stats.time "restore" restore ();
        if GobConfig.get_bool "dbg.verbose" then ignore @@ Pretty.printf "Solved %d vars. Total of %d vars after restore.\n" !Goblintutil.vars (HM.length rho);
        let avg xs = if List.is_empty !cache_sizes then 0.0 else float_of_int (BatList.sum xs) /. float_of_int (List.length xs) in
        if tracing then trace "cache" "#caches: %d, max: %d, avg: %.2f\n" (List.length !cache_sizes) (List.max !cache_sizes) (avg !cache_sizes);
      );

      let reachability xs =
        let reachable = HM.create (HM.length rho) in
        let rec one_var x =
          if not (HM.mem reachable x) then (
            HM.replace reachable x ();
            match S.system x with
            | None -> ()
            | Some x -> one_constraint x
          )
        and one_constraint f =
          ignore (f (fun x -> one_var x; try HM.find rho x with Not_found -> ignore @@ Pretty.printf "reachability: one_constraint: could not find variable %a\n" S.Var.pretty_trace x; S.Dom.bot ()) (fun x _ -> one_var x))
        in
        List.iter one_var xs;
        HM.iter (fun x v -> if not (HM.mem reachable x) then HM.remove rho x) rho;
      in
      reachability vs;

      stop_event ();
      print_data data "Data after solve completed";

      if GobConfig.get_bool "dbg.print_wpoints" then (
        Printf.printf "\nWidening points:\n";
        HM.iter (fun k () -> ignore @@ Pretty.printf "%a\n" S.Var.pretty_trace k) wpoint;
        print_newline ();
      );

      {st; infl; rho; wpoint; stable}

    let solve box st vs =
      incremental_mode := GobConfig.get_string "exp.incremental.mode";
      let reuse_stable = GobConfig.get_bool "exp.incremental.stable" in
      let reuse_wpoint = GobConfig.get_bool "exp.incremental.wpoint" in
      if !incremental_mode <> "off" then (
        let file_in = Filename.concat S.increment.analyzed_commit_dir result_file_name in
        let loaded, data =  if Sys.file_exists file_in && !incremental_mode <> "complete"
          then true, Serialize.unmarshal file_in
          else false, create_empty_data ()
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
        if loaded && GobConfig.get_bool "ana.opt.hashcons" then (
          HM.iter (fun k v ->
            HM.remove data.rho k; (* remove old values *)
            (* call hashcons on contexts and abstract values; results in new tags *)
            let k' = S.Var.relift k in
            let v' = S.Dom.join (S.Dom.bot ()) v in
            HM.replace data.rho k' v';
          ) data.rho;
          HM.iter (fun k v ->
            HM.remove data.stable k;
            HM.replace data.stable (S.Var.relift k) v
          ) data.stable;
          HM.iter (fun k v ->
            HM.remove data.wpoint k;
            HM.replace data.wpoint (S.Var.relift k) v
          ) data.wpoint;
          HM.iter (fun k v ->
            HM.remove data.infl k;
            HM.replace data.infl (S.Var.relift k) (VS.map S.Var.relift v)
          ) data.infl;
          data.st <- List.map (fun (k, v) -> S.Var.relift k, S.Dom.join (S.Dom.bot ()) v) data.st;
        );
        if not reuse_stable then (
          print_endline "Destabilizing everything!";
          data.stable <- HM.create 10;
          data.infl <- HM.create 10
        );
        if not reuse_wpoint then data.wpoint <- HM.create 10;
        let result = solve box st vs data in
        let path = Goblintutil.create_dir S.increment.current_commit_dir in
        if Sys.file_exists path then (
          let file_out = Filename.concat S.increment.current_commit_dir result_file_name in
          print_endline @@ "Saving solver result to " ^ file_out;
          Serialize.marshal result file_out;
        );
        clear_data result;

        (* Compare current rho to old rho *)
        if Sys.file_exists file_in && !incremental_mode <> "complete" then (
          let old_rho = (Serialize.unmarshal file_in: solver_data).rho in
          let eq r s =
            let leq r s = HM.fold (fun k v acc -> acc && (try S.Dom.leq v (HM.find s k) with Not_found -> false)) r true
          in leq r s && leq s r in
          print_endline @@ "Rho " ^ (if eq result.rho old_rho then "did not change" else "changed") ^ " compared to previous analysis.";
        );

        result.rho
      )
      else (
        let data = create_empty_data () in
        let result = solve box st vs data in
        clear_data result;
        result.rho
      )
  end

let _ =
  let module WP = GlobSolverFromIneqSolver (SLR.JoinContr (WP)) in
  Selector.add_solver ("td3", (module WP : GenericGlobSolver));
