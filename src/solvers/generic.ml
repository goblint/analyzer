open Prelude
open GobConfig
open Analyses

(** Convert a an [IneqConstrSys] into an equation system by joining all right-hand sides. *)
module SimpleSysConverter (S:IneqConstrSys)
  : sig include EqConstrSys val conv : S.v -> S.v end
  with type v = S.v
   and type d = S.d
   and module Var = S.Var
   and module Dom = S.Dom
=
struct
  type v = S.v
  type d = S.d

  module Var = S.Var
  module Dom = S.Dom

  let increment = S.increment

  let box = S.box

  let conv x = x

  let system x =
    match S.system x with
    | [] -> None
    | r::rs -> Some (fun get set -> List.fold_left (fun d r' -> Dom.join d (r' get set)) (r get set) rs)
end

(* move this to some other place! *)
module ExtendInt (B:Analyses.VarType) : Analyses.VarType with type t = B.t * int =
struct
  type t = B.t * int
  let relift x = x
  let compare ((u1,u2):t) (v1,v2) =
    match Stdlib.compare u2 v2 with (* cannot derive, compares snd first for efficiency *)
    | 0 -> B.compare u1 v1
    | n -> n
  let equal ((u1,u2):t) (v1,v2) = u2=v2 && B.equal u1 v1 (* cannot derive, compares snd first for efficiency *)
  let category (u,_) = B.category u
  let hash (u,v) = B.hash u + 131233 * v
  let pretty_trace () (u,v:t) =
    Pretty.dprintf "(%a,%d)" B.pretty_trace u v

  let var_id (c,_) = B.var_id c
  let printXml f (c,_) = B.printXml f c
  let file_name (c,_) = B.file_name c
  let line_nr (c,_) = B.line_nr c
  let node (c,_) = B.node c
end


(** Convert a an [IneqConstrSys] into an equation system. *)
module NormalSysConverter (S:IneqConstrSys)
  : sig include EqConstrSys val conv : S.v -> (S.v * int) end
  with type v = S.v * int
   and type d = S.d
   and module Var = ExtendInt (S.Var)
   and module Dom = S.Dom
=
struct
  type v = S.v * int
  type d = S.d

  module Var = ExtendInt (S.Var)
  module Dom = S.Dom

  let increment = S.increment

  let box (x,n) = S.box x

  let conv x = (x,-1)

  let system (x,n) : ((v -> d) -> (v -> d -> unit) -> d) option =
    let fold_left1 f xs =
      match xs with
      | [] -> failwith "You promised!!!"
      | x::xs -> List.fold_left f x xs
    in
    match S.system x with
    | []           -> None
    | [f] when n=0 -> Some (fun get set -> f (get % conv) (set % conv))
    | xs when n=(-1) ->
      let compute get set =
        fold_left1 Dom.join (List.mapi (fun n _ -> get (x,n)) xs)
      in
      Some compute
    | xs ->
      try Some (fun get set -> List.at xs n (get % conv) (set % conv))
      with Invalid_argument _ -> None
end



module SolverInteractiveWGlob
    (S:Analyses.GlobConstrSys)
    (LH:Hash.H with type key=S.LVar.t)
    (GH:Hash.H with type key=S.GVar.t) =
struct
  open S
  open Printf
  open Cil

  let enabled = ref false
  let step    = ref 1
  let stopped = ref false

  let interact_init () =
    enabled := get_bool "interact.enabled";
    stopped := get_bool "interact.paused"

  let _ =
    let open Sys in
    set_signal sigtstp (Signal_handle (fun i -> stopped := true));
    set_signal sigusr1 (Signal_handle (fun i -> stopped := false));
    set_signal sigusr2 (Signal_handle (fun i -> step    := 1))

  let loc_start f =
    fprintf f "<?xml version=\"1.0\" ?>\n<?xml-stylesheet type=\"text/xsl\" href=\"../node.xsl\"?>\n<loc>"

  let glob_start f =
    fprintf f "<?xml version=\"1.0\" ?>\n<?xml-stylesheet type=\"text/xsl\" href=\"../globals.xsl\"?>\n<globs>"

  let loc_end f =
    fprintf f "</loc>\n"

  let glob_end f =
    fprintf f "</globs>\n"

  let write_one_call v d f =
    fprintf f "%a%a</call>\n" LVar.printXml v D.printXml d

  let write_one_glob v d f =
    fprintf f "<glob><key>%a</key>\n%a</glob>\n" GVar.printXml v G.printXml d

  let mkdirs =
    List.fold_left (fun p d -> Goblintutil.create_dir (p^"/"^d)) "."

  let warning_id = ref 1
  let writeXmlWarnings () =
    let one_text f (m,l) =
      fprintf f "\n<text file=\"%s\" line=\"%d\">%s</text>" l.file l.line m
    in
    let one_w f = function
      | `text (m,l)  -> one_text f (m,l)
      | `group (n,e) ->
        fprintf f "<group name=\"%s\">%a</group>\n" n (List.print ~first:"" ~last:"" ~sep:"" one_text) e
    in
    let one_w x f = fprintf f "\n<warning>%a</warning>" one_w x in
    let res_dir = mkdirs [get_string "interact.out" ^ "/warn"] in
    let write_warning x =
      let full_name = res_dir ^ "/warn" ^ string_of_int !warning_id ^ ".xml" in
      incr warning_id;
      File.with_file_out ~mode:[`create;`excl;`text] full_name (one_w x)
    in
    List.iter write_warning !Messages.warning_table

  module SSH = Hashtbl.Make (struct include String let hash (x:string) = Hashtbl.hash x end)
  let funs = SSH.create 100
  module NH = Hashtbl.Make (Node)
  let liveness = NH.create 100
  let updated_l = NH.create 100
  let updated_g = GH.create 100

  let write_files lh gh =
    let res_dir = mkdirs [get_string "interact.out"; "nodes"] in
    let created_files = Hashtbl.create 100 in
    let one_var v d =
      let fname = LVar.var_id v in
      let full_name = res_dir ^ "/" ^ fname ^ ".xml" in
      if not (Sys.file_exists full_name) then begin
        File.with_file_out ~mode:[`excl;`create;`text] full_name loc_start;
        Hashtbl.add created_files full_name ()
      end;
      File.with_file_out ~mode:[`append;`excl;`text] full_name (write_one_call v d)
    in
    LH.iter one_var lh;
    let full_gname = res_dir ^ "/globals.xml" in
    File.with_file_out ~mode:[`excl;`create;`text] full_gname glob_start;
    let one_glob v d =
      File.with_file_out ~mode:[`append;`excl;`text] full_gname (write_one_glob v d)
    in
    GH.iter one_glob gh;
    File.with_file_out ~mode:[`append;`excl;`text] full_gname glob_end;
    let close_vars f _ =
      File.with_file_out ~mode:[`append;`excl;`text] f loc_end
    in
    Hashtbl.iter close_vars created_files

  let write_updates () =
    let dir = get_string "interact.out" in
    let full_name = dir ^ "/updates.xml" in
    let write_updates f =
      fprintf f "<?xml version=\"1.0\" ?>\n<?xml-stylesheet type=\"text/xsl\" href=\"updates.xsl\"?>\n<updates>\n";
      NH.iter (fun v () -> fprintf f "%a</call>\n" Var.printXml v) updated_l;
      GH.iter (fun v () -> fprintf f "<global>\n%a</global>\n" GVar.printXml v) updated_g;
      let g n _ = fprintf f "<warning warn=\"warn%d\" />\n" (n + !warning_id) in
      List.iteri g !Messages.warning_table;
      (* List.iter write_warning !Messages.warning_table *)
      fprintf f "</updates>\n";
    in
    File.with_file_out ~mode:[`excl;`create;`text] full_name write_updates

  let write_index () =
    let dir = get_string "interact.out" in
    let full_name = dir ^ "/index.xml" in
    let write_index f =
      let print_funs f fs =
        Set.iter (fun n -> fprintf f "<function name=\"%s\"></function>\n" n) fs
      in
      fprintf f "<?xml version=\"1.0\" ?>\n<?xml-stylesheet type=\"text/xsl\" href=\"report.xsl\"?>\n<report>\n";
      SSH.iter (fun file funs -> fprintf f "<file name=\"%s\">%a</file>" file print_funs funs) funs;
      fprintf f "</report>\n";
    in
    File.with_file_out ~mode:[`excl;`create;`text] full_name write_index

  let delete_old_results () =
    let dir = get_string "interact.out" in
    if Sys.file_exists dir && Sys.is_directory dir then
      try Goblintutil.rm_rf dir with _ -> ()

  let write_all hl hg =
    delete_old_results ();
    write_files hl hg;
    write_index ();
    if !stopped then
      write_updates ();
    writeXmlWarnings (); (* must be after write_update! *)
    !MyCFG.write_cfgs (NH.mem liveness);
    NH.clear updated_l;
    GH.clear updated_g

  let update_var_event_local hl hg x o n =
    if !enabled && not (D.is_bot n) then begin
      let node = LVar.node x in
      let file = (MyCFG.getFun node).svar in
      NH.replace updated_l node ();
      NH.replace liveness node ();
      SSH.replace funs file.vdecl.file (Set.add file.vname (SSH.find_default funs file.vdecl.file Set.empty));
      if !stopped then begin
        write_all hl hg;
        decr step;
        while !stopped && !step <=0 do Unix.sleep 1 done
      end
    end

  let update_var_event_global hl hg x o n =
    GH.replace updated_g x ()

  let done_event hl hg =
    if !enabled then
      write_all hl hg
end

module SolverStats (S:EqConstrSys) (HM:Hash.H with type key = S.v) =
struct
  open S
  open Messages

  module GU = Goblintutil

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
    Goblintutil.vars := !Goblintutil.vars + 1;
    if tracing then trace "sol" "New %a\n" Var.pretty_trace x

  let get_var_event x =
    if full_trace then trace "sol" "Querying %a\n" Var.pretty_trace x

  let eval_rhs_event x =
    if full_trace then trace "sol" "(Re-)evaluating %a\n" Var.pretty_trace x;
    if Config.tracking then M.track "eval";
    Goblintutil.evals := !Goblintutil.evals + 1;
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
    HM.iter (fun k _ -> if is_fun k then Hashtbl.modify_def 1 (str k) ((+)1) histo) rho;
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
    let save_run = GobConfig.get_string "save_run" in
    if save_run <> "" then (
      ignore @@ Goblintutil.create_dir save_run;
      save_run ^ Filename.dir_sep ^ "solver_stats.csv" |> open_out |> Option.some
    ) else None
  let write_csv xs oc = output_string oc @@ String.concat ",\t" xs ^ "\n"

  (* print generic and specific stats *)
  let print_stats _ =
    print_newline ();
    (* print_endline "# Generic solver stats"; *)
    Printf.printf "runtime: %s\n" (string_of_time ());
    Printf.printf "vars: %d, evals: %d\n" !Goblintutil.vars !Goblintutil.evals;
    Option.may (fun v -> ignore @@ Pretty.printf "max updates: %d for var %a on line %d\n" !max_c Var.pretty_trace v (Var.line_nr v)) !max_var;
    print_newline ();
    (* print_endline "# Solver specific stats"; *)
    !print_solver_stats ();
    print_newline ();
    (* Stats.print (M.get_out "timing" Legacy.stdout) "Timings:\n"; *)
    (* Gc.print_stat stdout; (* too verbose, slow and words instead of MB *) *)
    let gc = Goblintutil.print_gc_quick_stat Legacy.stdout in
    print_newline ();
    Option.may (write_csv [string_of_time (); string_of_int !Goblintutil.vars; string_of_int !Goblintutil.evals; string_of_int !ncontexts; string_of_int gc.Gc.top_heap_words]) stats_csv;
    (* print_string "Do you want to continue? [Y/n]"; *)
    flush stdout
    (* if read_line () = "n" then raise Break *)

  let () =
    let write_header = write_csv ["runtime"; "vars"; "evals"; "contexts"; "max_heap"] (* TODO @ !solver_stats_headers *) in
    Option.may write_header stats_csv;
    (* call print_stats on dbg.solver-signal *)
    Sys.set_signal (Goblintutil.signal_of_string (get_string "dbg.solver-signal")) (Signal_handle print_stats);
    (* call print_stats every dbg.solver-stats-interval *)
    Sys.set_signal Sys.sigvtalrm (Signal_handle print_stats);
    (* https://ocaml.org/api/Unix.html#TYPEinterval_timer ITIMER_VIRTUAL is user time; sends sigvtalarm; ITIMER_PROF/sigprof is already used in Timeout.Unix.timeout *)
    let ssi = get_int "dbg.solver-stats-interval" in
    if ssi > 0 then
      let it = float_of_int ssi in
      ignore Unix.(setitimer ITIMER_VIRTUAL { it_interval = it; it_value = it });
end

(** use this if your [box] is [join] --- the simple solver *)
module DirtyBoxSolver : GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
  struct
    include SolverStats (S) (H)

    let h_find_default h x d =
      try H.find h x
      with Not_found -> d

    let solve box xs vs =
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
        let newd = box x oldd d in
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
  functor (H:Hash.H with type key = S.v) ->
  struct
    include SolverStats (S) (H)

    let h_find_default h x d =
      try H.find h x
      with Not_found -> d

    let solveWithStart box (ht,hts) xs vs =
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
        let newd = box x oldd (S.Dom.join d (h_find_default sols x (S.Dom.bot ()))) in
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
          then Messages.trace "sol" "Need to review %d deps.\n" (List.length deps);
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
    let solve box xs ys = solveWithStart box (H.create 1024, H.create 1024) xs ys |> fst
  end

module SoundBoxSolver : GenericEqBoxSolver = SoundBoxSolverImpl



(* use this if you do widenings & narrowings for globals --- outdated *)
module PreciseSideEffectBoxSolver : GenericEqBoxSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hash.H with type key = S.v) ->
  struct
    include SolverStats (S) (H)

    let h_find_default h x d =
      try H.find h x
      with Not_found -> d

    module VM = Map.Make (S.Var)
    module VS = Set.Make (S.Var)

    let solve box xs vs =
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
        let newd = box x oldd (VS.fold find_join_sides (h_find_default sdeps x VS.empty) d) in
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
