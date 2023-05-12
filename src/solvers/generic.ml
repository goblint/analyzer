open Batteries
open GobConfig
open Analyses

module LoadRunSolver: GenericEqSolver =
  functor (S: EqConstrSys) (VH: Hashtbl.S with type key = S.v) ->
  struct
    let solve xs vs =
      (* copied from Control.solve_and_postprocess *)
      let solver_file = "solver.marshalled" in
      let load_run = Fpath.v (get_string "load_run") in
      let solver = Fpath.(load_run / solver_file) in
      if get_bool "dbg.verbose" then
        (* Do NOT replace with Printf because of GobView: https://github.com/goblint/gobview/issues/10 *)
        print_endline ("Loading the solver result of a saved run from " ^ (Fpath.to_string solver));
      let vh: S.d VH.t = Serialize.unmarshal solver in
      if get_bool "ana.opt.hashcons" then (
        let vh' = VH.create (VH.length vh) in
        VH.iter (fun x d ->
            let x' = S.Var.relift x in
            let d' = S.Dom.relift d in
            VH.replace vh' x' d'
          ) vh;
        vh'
      )
      else
        vh
  end

module LoadRunIncrSolver: GenericEqIncrSolver =
  Constraints.EqIncrSolverFromEqSolver (LoadRunSolver)

module SolverStats (S:EqConstrSys) (HM:Hashtbl.S with type key = S.v) =
struct
  open S
  open Messages

  let stack_d = ref 0
  let full_trace = false
  let start_c = 0
  let max_c   : int ref = ref (-1)
  let max_var : Var.t option ref = ref None

  let histo = HM.create 1024
  let increase (v:Var.t) =
    let set v c =
      if not full_trace && (c > start_c && c > !max_c && (Option.is_none !max_var || not (Var.equal (Option.get !max_var) v))) then begin
        if tracing then trace "sol" "Switched tracing to %a\n" Var.pretty_trace v;
        max_c := c;
        max_var := Some v
      end
    in
    try let c = HM.find histo v in
      set v (c+1);
      HM.replace histo v (c+1)
    with Not_found -> begin
        set v 1;
        HM.add histo v 1
      end

  let start_event () = ()
  let stop_event () = ()

  let new_var_event x =
    incr Goblintutil.vars;
    if tracing then trace "sol" "New %a\n" Var.pretty_trace x

  let get_var_event x =
    if full_trace then trace "sol" "Querying %a\n" Var.pretty_trace x

  let eval_rhs_event x =
    if full_trace then trace "sol" "(Re-)evaluating %a\n" Var.pretty_trace x;
    incr Goblintutil.evals;
    if (get_bool "dbg.solver-progress") then (incr stack_d; print_int !stack_d; flush stdout)

  let update_var_event x o n =
    if tracing then increase x;
    if full_trace || ((not (Dom.is_bot o)) && Option.is_some !max_var && Var.equal (Option.get !max_var) x) then begin
      if tracing then tracei "sol_max" "(%d) Update to %a.\n" !max_c Var.pretty_trace x;
      if tracing then traceu "sol_max" "%a\n\n" Dom.pretty_diff (n, o)
    end

  (* solvers can assign this to print solver specific statistics using their data structures *)
  let print_solver_stats = ref (fun () -> ())

  (* this can be used in print_solver_stats *)
  let ncontexts = ref 0
  let print_context_stats rho =
    let histo = Hashtbl.create 13 in (* histogram: node id -> number of contexts *)
    let str k = S.Var.pretty_trace () k |> Pretty.sprint ~width:max_int in (* use string as key since k may have cycles which lead to exception *)
    let is_fun k = match S.Var.node k with FunctionEntry _ -> true | _ -> false in (* only count function entries since other nodes in function will have leq number of contexts *)
    HM.iter (fun k _ -> if is_fun k then Hashtbl.modify_def 0 (str k) ((+)1) histo) rho;
    (* let max_k, n = Hashtbl.fold (fun k v (k',v') -> if v > v' then k,v else k',v') histo (Obj.magic (), 0) in *)
    (* ignore @@ Pretty.printf "max #contexts: %d for %s\n" n max_k; *)
    ncontexts := Hashtbl.fold (fun _ -> (+)) histo 0;
    let topn = 5 in
    Printf.printf "Found %d contexts for %d functions. Top %d functions:\n" !ncontexts (Hashtbl.length histo) topn;
    Hashtbl.to_list histo
    |> List.sort (fun (_,n1) (_,n2) -> compare n2 n1)
    |> List.take topn
    |> List.iter @@ fun (k,n) -> ignore @@ Pretty.printf "%d\tcontexts for %s\n" n k

  let stats_csv =
    let save_run_str = GobConfig.get_string "save_run" in
    if save_run_str <> "" then (
      let save_run = Fpath.v save_run_str in
      GobSys.mkdir_or_exists save_run;
      Fpath.(to_string (save_run / "solver_stats.csv")) |> open_out |> Option.some
    ) else None
  let write_csv xs oc = output_string oc @@ String.concat ",\t" xs ^ "\n"

  (* print generic and specific stats *)
  let print_stats _ =
    print_newline ();
    (* print_endline "# Generic solver stats"; *)
    Printf.printf "runtime: %s\n" (GobSys.string_of_time ());
    Printf.printf "vars: %d, evals: %d\n" !Goblintutil.vars !Goblintutil.evals;
    Option.may (fun v -> ignore @@ Pretty.printf "max updates: %d for var %a\n" !max_c Var.pretty_trace v) !max_var;
    print_newline ();
    (* print_endline "# Solver specific stats"; *)
    !print_solver_stats ();
    print_newline ();
    (* Timing.print (M.get_out "timing" Legacy.stdout) "Timings:\n"; *)
    (* Gc.print_stat stdout; (* too verbose, slow and words instead of MB *) *)
    let gc = Goblintutil.print_gc_quick_stat Legacy.stdout in
    print_newline ();
    Option.may (write_csv [GobSys.string_of_time (); string_of_int !Goblintutil.vars; string_of_int !Goblintutil.evals; string_of_int !ncontexts; string_of_int gc.Gc.top_heap_words]) stats_csv;
    (* print_string "Do you want to continue? [Y/n]"; *)
    flush stdout
    (* if read_line () = "n" then raise Break *)

  let () =
    let write_header = write_csv ["runtime"; "vars"; "evals"; "contexts"; "max_heap"] (* TODO @ !solver_stats_headers *) in
    Option.may write_header stats_csv;
    (* call print_stats on dbg.solver-signal *)
    Sys.set_signal (GobSys.signal_of_string (get_string "dbg.solver-signal")) (Signal_handle print_stats);
    (* call print_stats every dbg.solver-stats-interval *)
    Sys.set_signal Sys.sigvtalrm (Signal_handle print_stats);
    (* https://ocaml.org/api/Unix.html#TYPEinterval_timer ITIMER_VIRTUAL is user time; sends sigvtalarm; ITIMER_PROF/sigprof is already used in Timeout.Unix.timeout *)
    let ssi = get_int "dbg.solver-stats-interval" in
    if ssi > 0 then
      let it = float_of_int ssi in
      ignore Unix.(setitimer ITIMER_VIRTUAL { it_interval = it; it_value = it });
end

(** use this if your [box] is [join] --- the simple solver *)
module DirtyBoxSolver : GenericEqSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hashtbl.S with type key = S.v) ->
  struct
    open SolverBox.Warrow (S.Dom)
    include SolverStats (S) (H)

    let h_find_default h x d =
      try H.find h x
      with Not_found -> d

    let solve xs vs =
      (* the stabile "set" *)
      let stbl = H.create 1024 in
      (* the influence map *)
      let infl = H.create 1024 in
      (* the solution map *)
      let sol  = H.create 1024 in

      (* solve the variable [x] *)
      let rec solve_one x =
        (* solve [x] only if it is not stable *)
        if not (H.mem stbl x) then begin
          (* initialize [sol] for [x], if [x] is [sol] then we have "seen" it *)
          if not (H.mem sol x) then (new_var_event x; H.add sol x (S.Dom.bot ()));
          (* mark [x] stable *)
          H.replace stbl x ();
          (* set the new value for [x] *)
          eval_rhs_event x;
          Option.may (fun f -> set x (f (eval x) set)) (S.system x)
        end

      (* return the value for [y] and mark its influence on [x] *)
      and eval x y =
        (* solve variable [y] *)
        get_var_event y;
        solve_one y;
        (* add that [x] will be influenced by [y] *)
        H.replace infl y (x :: h_find_default infl y []);
        (* return the value for [y] *)
        H.find sol y

      and set x d =
        (* solve variable [y] if it has not been seen before *)
        if not (H.mem sol x) then solve_one x;
        (* do nothing if we have stabilized [x] *)
        let oldd = H.find sol x in
        let newd = box oldd d in
        update_var_event x oldd newd;
        if not (S.Dom.equal oldd newd) then begin
          (* set the new value for [x] *)
          H.replace sol x newd;
          (* mark dependencies unstable *)
          let deps = h_find_default infl x [] in
          List.iter (H.remove stbl) deps;
          (* remove old influences of [x] -- they will be re-generated if still needed *)
          H.remove infl x;
          (* solve all dependencies *)
          solve_all deps
        end

      (* solve all elements of the list *)
      and solve_all xs =
        List.iter solve_one xs
      in

      (* solve interesting variables and then return the produced table *)
      start_event ();
      List.iter (fun (k,v) -> set k v) xs;
      solve_all vs;
      stop_event ();
      sol
  end

(* use this if you do widenings & narrowings (but no narrowings for globals) --- maybe outdated *)
module SoundBoxSolverImpl =
  functor (S:EqConstrSys) ->
  functor (H:Hashtbl.S with type key = S.v) ->
  struct
    open SolverBox.Warrow (S.Dom)
    include SolverStats (S) (H)

    let h_find_default h x d =
      try H.find h x
      with Not_found -> d

    let solveWithStart (ht,hts) xs vs =
      (* the stabile "set" *)
      let stbl = H.create 1024 in
      (* the influence map *)
      let infl = H.create 1024 in
      (* the solution map  *)
      let sol  = ht (*H.create 1024*) in
      (* the side-effected solution map  *)
      let sols = hts(*H.create 1024*) in
      (* the called set *)
      let called = H.create 1024 in

      (* solve the variable [x] *)
      let rec solve_one x =
        (* solve [x] only if it is not stable *)
        if not (H.mem stbl x) then begin
          (* initialize [sol] for [x], if [x] is [sol] then we have "seen" it *)
          if not (H.mem sol x) then (new_var_event x; H.add sol x (S.Dom.bot ()));
          (* mark [x] stable *)
          H.replace stbl x ();
          (* mark [x] called *)
          H.replace called x ();
          (* set the new value for [x] *)
          eval_rhs_event x;
          let set_x d = if H.mem called x then set x d else () in
          Option.may (fun f -> set_x (f (eval x) side)) (S.system x);
          (* remove [x] from called *)
          H.remove called x
        end

      (* return the value for [y] and mark its influence on [x] *)
      and eval x y =
        (* solve variable [y] *)
        get_var_event y;
        solve_one y;
        (* add that [x] will be influenced by [y] *)
        H.replace infl y (x :: h_find_default infl y []);
        (* return the value for [y] *)
        H.find sol y

      (* this is the function we give to [S.system] *)
      and side x d =
        (* accumulate all side-effects in [sols] *)
        let nd = S.Dom.join d (h_find_default sols x (S.Dom.bot ())) in
        H.replace sols x nd;
        (* do the normal writing operation with the accumulated value *)
        set x nd

      and set x d =
        (* solve variable [y] if it has not been seen before *)
        if not (H.mem sol x) then solve_one x;
        (* do nothing if we have stabilized [x] *)
        let oldd = H.find sol x in
        (* compute the new value *)
        let newd = box oldd (S.Dom.join d (h_find_default sols x (S.Dom.bot ()))) in
        if not (S.Dom.equal oldd newd) then begin
          update_var_event x oldd newd;
          (* set the new value for [x] *)
          H.replace sol x newd;
          (* mark dependencies unstable *)
          let deps = h_find_default infl x [] in
          List.iter (H.remove stbl) deps;
          (* remove old influences of [x] -- they will be re-generated if still needed *)
          H.remove infl x;
          H.replace infl x [x];
          if full_trace
          then Messages.trace "sol" "Need to review %d deps.\n" (List.length deps); (* nosemgrep: trace-not-in-tracing *)
          (* solve all dependencies *)
          solve_all deps
        end

      (* solve all elements of the list *)
      and solve_all xs =
        List.iter solve_one xs
      in

      (* solve interesting variables and then return the produced table *)
      start_event ();
      List.iter (fun (k,v) -> side k v) xs;
      solve_all vs; stop_event ();
      sol, sols

    (** the solve function *)
    let solve xs ys = solveWithStart (H.create 1024, H.create 1024) xs ys |> fst
  end

module SoundBoxSolver : GenericEqSolver = SoundBoxSolverImpl



(* use this if you do widenings & narrowings for globals --- outdated *)
module PreciseSideEffectBoxSolver : GenericEqSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hashtbl.S with type key = S.v) ->
  struct
    open SolverBox.Warrow (S.Dom)
    include SolverStats (S) (H)

    let h_find_default h x d =
      try H.find h x
      with Not_found -> d

    module VM = Map.Make (S.Var)
    module VS = Set.Make (S.Var)

    let solve xs vs =
      (* the stabile "set" *)
      let stbl  = H.create 1024 in
      (* the influence map *)
      let infl  = H.create 1024 in
      (* the solution map  *)
      let sol   = H.create 1024 in
      (* the side-effected solution map  *)
      let sols  = H.create 1024 in
      (* the side-effected solution map  *)
      let sdeps = H.create 1024 in
      (* the side-effected solution map  *)
      let called = H.create 1024 in

      (* solve the variable [x] *)
      let rec solve_one x =
        (* solve [x] only if it is not stable *)
        if not (H.mem stbl x) then begin
          (* initialize [sol] for [x], if [x] is [sol] then we have "seen" it *)
          if not (H.mem sol x) then (new_var_event x; H.add sol x (S.Dom.bot ()));
          (* mark [x] stable *)
          H.replace stbl x ();
          (* mark [x] called *)
          H.replace called x ();
          (* remove side-effected values *)
          H.remove sols x;
          (* set the new value for [x] *)
          eval_rhs_event x;
          Option.may (fun f -> set x (f (eval x) (side x))) (S.system x);
          (* remove [x] from called *)
          H.remove called x
        end

      (* return the value for [y] and mark its influence on [x] *)
      and eval x y =
        (* solve variable [y] *)
        get_var_event y;
        solve_one y;
        (* add that [x] will be influenced by [y] *)
        H.replace infl y (x :: h_find_default infl y []);
        (* return the value for [y] *)
        H.find sol y

      (* this is the function we give to [S.system] *)
      and side y x d =
        (* mark that [y] has a side-effect to [x] *)
        H.replace sdeps x (VS.add y (h_find_default sdeps x VS.empty));
        (* save the value in [sols] *)
        let om = h_find_default sols y VM.empty in
        let nm = VM.modify_def (S.Dom.bot ()) x (S.Dom.join d) om in
        H.replace sols y nm;
        (* do the normal writing operation with the accumulated value *)
        set x d

      and set x d =
        (* solve variable [y] if it has not been seen before *)
        if not (H.mem sol x) then solve_one x;
        (* do nothing if we have stabilized [x] *)
        let oldd = H.find sol x in
        (* accumulate all side-effects in [sols] *)
        let find_join_sides z d =
          try S.Dom.join d (VM.find x (H.find sols z))
          with Not_found -> d
        in
        let newd = box oldd (VS.fold find_join_sides (h_find_default sdeps x VS.empty) d) in
        update_var_event x oldd newd;
        if not (S.Dom.equal oldd newd) then begin
          (* set the new value for [x] *)
          H.replace sol x newd;
          (* mark dependencies unstable *)
          let deps = h_find_default infl x [] in
          List.iter (H.remove stbl) deps;
          (* remove old influences of [x] -- they will be re-generated if still needed *)
          H.remove infl x;
          (* solve all dependencies *)
          solve_all deps
        end

      (* solve all elements of the list *)
      and solve_all xs =
        List.iter solve_one xs
      in

      (* solve interesting variables and then return the produced table *)
      start_event ();
      List.iter (fun (k,v) -> side k k v) xs;
      solve_all vs;
      stop_event ();
      sol
  end
