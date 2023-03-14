(** An analyzer that takes the CFG from [MyCFG], a solver from [Selector], constraints from [Constraints] (using the specification from [MCP]) *)

open Prelude
open GoblintCil
open MyCFG
open Analyses
open GobConfig
open Constraints

module type S2S = functor (X : Spec) -> Spec

(* spec is lazy, so HConsed table in Hashcons lifters is preserved between analyses in server mode *)
let spec_module: (module Spec) Lazy.t = lazy (
  GobConfig.building_spec := true;
  let arg_enabled = get_bool "ana.sv-comp.enabled" || get_bool "exp.arg" in
  let open Batteries in
  (* apply functor F on module X if opt is true *)
  let lift opt (module F : S2S) (module X : Spec) = (module (val if opt then (module F (X)) else (module X) : Spec) : Spec) in
  let module S1 = (val
            (module MCP.MCP2 : Spec)
            |> lift true (module WidenContextLifterSide) (* option checked in functor *)
            (* hashcons before witness to reduce duplicates, because witness re-uses contexts in domain and requires tag for PathSensitive3 *)
            |> lift (get_bool "ana.opt.hashcons" || arg_enabled) (module HashconsContextLifter)
            |> lift arg_enabled (module HashconsLifter)
            |> lift arg_enabled (module WitnessConstraints.PathSensitive3)
            |> lift (not arg_enabled) (module PathSensitive2)
            |> lift (get_bool "ana.dead-code.branches") (module DeadBranchLifter)
            |> lift true (module DeadCodeLifter)
            |> lift (get_bool "dbg.slice.on") (module LevelSliceLifter)
            |> lift (get_int "dbg.limit.widen" > 0) (module LimitLifter)
            |> lift (get_bool "ana.opt.equal" && not (get_bool "ana.opt.hashcons")) (module OptEqual)
            |> lift (get_bool "ana.opt.hashcons") (module HashconsLifter)
            (* Widening tokens must be outside of hashcons, because widening token domain ignores token sets for identity, so hashcons doesn't allow adding tokens.
               Also must be outside of deadcode, because deadcode splits (like mutex lock event) don't pass on tokens. *)
            |> lift (get_bool "ana.widen.tokens") (module WideningTokens.Lifter)
          ) in
  GobConfig.building_spec := false;
  ControlSpecC.control_spec_c := (module S1.C);
  (module S1)
)

(** gets Spec for current options *)
let get_spec (): (module Spec) =
  Lazy.force spec_module

let current_node_state_json : (Node.t -> Yojson.Safe.t option) ref = ref (fun _ -> None)

(** Given a [Cfg], a [Spec], and an [Inc], computes the solution to [MCP.Path] *)
module AnalyzeCFG (Cfg:CfgBidir) (Spec:Spec) (Inc:Increment) =
struct

  module SpecSys: SpecSys with module Spec = Spec =
  struct
    (* Must be created in module, because cannot be wrapped in a module later. *)
    module Spec = Spec

    (* The Equation system *)
    module EQSys = FromSpec (Spec) (Cfg) (Inc)

    (* Hashtbl for locals *)
    module LHT   = BatHashtbl.Make (EQSys.LVar)
    (* Hashtbl for globals *)
    module GHT   = BatHashtbl.Make (EQSys.GVar)
  end

  open SpecSys

  (* The solver *)
  module PostSolverArg =
  struct
    let should_prune = true
    let should_verify = get_bool "verify"
    let should_warn = get_string "warn_at" <> "never"
    let should_save_run =
      (* copied from solve_and_postprocess *)
      let gobview = get_bool "gobview" in
      let save_run = let o = get_string "save_run" in if o = "" then (if gobview then "run" else "") else o in
      save_run <> ""
  end
  module Slvr  = (GlobSolverFromEqSolver (Selector.Make (PostSolverArg))) (EQSys) (LHT) (GHT)
  (* The comparator *)
  module CompareGlobSys = Constraints.CompareGlobSys (SpecSys)

  (* Triple of the function, context, and the local value. *)
  module RT = Analyses.ResultType2 (Spec)
  (* Set of triples [RT] *)
  module LT = SetDomain.HeadlessSet (RT)
  (* Analysis result structure---a hashtable from program points to [LT] *)
  module Result = Analyses.Result (LT) (struct let result_name = "analysis" end)

  module Query = ResultQuery.Query (SpecSys)

  (* print out information about dead code *)
  let print_dead_code (xs:Result.t) uncalled_fn_loc =
    let module NH = Hashtbl.Make (Node) in
    let live_nodes : unit NH.t = NH.create 10 in
    let count = ref 0 in (* Is only populated if "ana.dead-code.lines" or "ana.dead-code.branches" is true *)
    let module StringMap = BatMap.Make (String) in
    let live_lines = ref StringMap.empty in
    let dead_lines = ref StringMap.empty in
    let add_one n v =
      match n with
      | Statement s when Cilfacade.(StmtH.mem pseudo_return_to_fun s) ->
        (* Exclude pseudo returns from dead lines counting. No user code at "}". *)
        ()
      | _ ->
        (* Not using Node.location here to have updated locations in incremental analysis.
           See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
        let l = UpdateCil.getLoc n in
        let f = Node.find_fundec n in
        let add_fun  = BatISet.add l.line in
        let add_file = StringMap.modify_def BatISet.empty f.svar.vname add_fun in
        let is_dead = LT.for_all (fun (_,x,f) -> Spec.D.is_bot x) v in
        if is_dead then (
          dead_lines := StringMap.modify_def StringMap.empty l.file add_file !dead_lines
        ) else (
          live_lines := StringMap.modify_def StringMap.empty l.file add_file !live_lines;
          NH.add live_nodes n ()
        );
    in
    Result.iter add_one xs;
    let live_count = StringMap.fold (fun _ file_lines acc ->
        StringMap.fold (fun _ fun_lines acc ->
            acc + ISet.cardinal fun_lines
          ) file_lines acc
      ) !live_lines 0
    in
    let live file fn =
      try StringMap.find fn (StringMap.find file !live_lines)
      with Not_found -> BatISet.empty
    in
    dead_lines := StringMap.mapi (fun fi -> StringMap.mapi (fun fu ded -> BatISet.diff ded (live fi fu))) !dead_lines;
    dead_lines := StringMap.map (StringMap.filter (fun _ x -> not (BatISet.is_empty x))) !dead_lines;
    dead_lines := StringMap.filter (fun _ x -> not (StringMap.is_empty x)) !dead_lines;
    let warn_func file f xs =
      let warn_range b e =
        count := !count + (e - b + 1); (* for total count below *)
        let doc =
          if b = e then
            Pretty.dprintf "on line %d" b
          else
            Pretty.dprintf "on lines %d..%d" b e
        in
        let loc: Cil.location = {
          file;
          line = b;
          column = -1; (* not shown *)
          byte = 0; (* wrong, but not shown *)
          endLine = e;
          endColumn = -1; (* not shown *)
          endByte = 0; (* wrong, but not shown *)
          synthetic = false;
        }
        in
        (doc, Some (Messages.Location.CilLocation loc)) (* CilLocation is fine because always printed from scratch *)
      in
      let msgs =
        BatISet.fold_range (fun b e acc ->
            warn_range b e :: acc
          ) xs []
      in
      let msgs = List.rev msgs in (* lines in ascending order *)
      M.msg_group Warning ~category:Deadcode "Function '%s' has dead code" f msgs
    in
    let warn_file f = StringMap.iter (warn_func f) in
    if get_bool "ana.dead-code.lines" then (
      StringMap.iter warn_file !dead_lines; (* populates count by side-effect *)
      let severity: M.Severity.t = if StringMap.is_empty !dead_lines then Info else Warning in
      let dead_total = !count + uncalled_fn_loc in
      let total = live_count + dead_total in (* We can only give total LoC if we counted dead code *)
      M.msg_group severity ~category:Deadcode "Logical lines of code (LLoC) summary" [
        (Pretty.dprintf "live: %d" live_count, None);
        (Pretty.dprintf "dead: %d%s" dead_total (if uncalled_fn_loc > 0 then Printf.sprintf " (%d in uncalled functions)" uncalled_fn_loc else ""), None);
        (Pretty.dprintf "total lines: %d" total, None);
      ]
    );
    NH.mem live_nodes

  (* convert result that can be out-put *)
  let solver2source_result h : Result.t =
    (* processed result *)
    let res = Result.create 113 in

    (* Adding the state at each system variable to the final result *)
    let add_local_var (n,es) state =
      (* Not using Node.location here to have updated locations in incremental analysis.
          See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
      let loc = UpdateCil.getLoc n in
      if loc <> locUnknown then try
          let fundec = Node.find_fundec n in
          if Result.mem res n then
            (* If this source location has been added before, we look it up
              * and add another node to it information to it. *)
            let prev = Result.find res n in
            Result.replace res n (LT.add (es,state,fundec) prev)
          else
            Result.add res n (LT.singleton (es,state,fundec))
        (* If the function is not defined, and yet has been included to the
          * analysis result, we generate a warning. *)
        with Not_found ->
          Messages.debug ~category:Analyzer ~loc:(CilLocation loc) "Calculated state for undefined function: unexpected node %a" Node.pretty_trace n
    in
    LHT.iter add_local_var h;
    res

  (** The main function to preform the selected analyses. *)
  let analyze (file: file) (startfuns, exitfuns, otherfuns: Analyses.fundecs) =
    let module FileCfg: FileCfg =
    struct
      let file = file
      module Cfg = Cfg
    end
    in

    Goblintutil.should_warn := false; (* reset for server mode *)

    (* exctract global xml from result *)
    let make_global_fast_xml f g =
      let open Printf in
      let print_globals k v =
        fprintf f "\n<glob><key>%s</key>%a</glob>" (XmlUtil.escape (EQSys.GVar.show k)) EQSys.G.printXml v;
      in
      GHT.iter print_globals g
    in

    (* add extern variables to local state *)
    let do_extern_inits ctx (file : file) : Spec.D.t =
      let module VS = Set.Make (Basetype.Variables) in
      let add_glob s = function
          GVar (v,_,_) -> VS.add v s
        | _            -> s
      in
      let vars = foldGlobals file add_glob VS.empty in
      let set_bad v st =
        Spec.assign {ctx with local = st} (var v) MyCFG.unknown_exp
      in
      let is_std = function
        | {vname = ("__tzname" | "__daylight" | "__timezone"); _} (* unix time.h *)
        | {vname = ("tzname" | "daylight" | "timezone"); _} (* unix time.h *)
        | {vname = "getdate_err"; _} (* unix time.h, but somehow always in MacOS even without include *)
        | {vname = ("stdin" | "stdout" | "stderr"); _} (* standard stdio.h *)
        | {vname = ("optarg" | "optind" | "opterr" | "optopt" ); _} (* unix unistd.h *)
        | {vname = ("__environ"); _} -> (* Linux Standard Base Core Specification *)
          true
        | _ -> false
      in
      let add_externs s = function
        | GVarDecl (v,_) when not (VS.mem v vars || isFunctionType v.vtype) && not (get_bool "exp.hide-std-globals" && is_std v) -> set_bad v s
        | _ -> s
      in
      foldGlobals file add_externs (Spec.startstate MyCFG.dummy_func.svar)
    in

    (* Simulate globals before analysis. *)
    (* TODO: make extern/global inits part of constraint system so all of this would be unnecessary. *)
    let gh = GHT.create 13 in
    let getg v = GHT.find_default gh v (EQSys.G.bot ()) in
    let sideg v d =
      if M.tracing then M.trace "global_inits" "sideg %a = %a\n" EQSys.GVar.pretty v EQSys.G.pretty d;
      GHT.replace gh v (EQSys.G.join (getg v) d)
    in
    (* Old-style global function for context.
     * This indirectly prevents global initializers from depending on each others' global side effects, which would require proper solving. *)
    let getg v = EQSys.G.bot () in

    (* analyze cil's global-inits function to get a starting state *)
    let do_global_inits (file: file) : Spec.D.t * fundec list =
      let ctx =
        { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
        ; emit   = (fun _ -> failwith "Cannot \"emit\" in global initializer context.")
        ; node    = MyCFG.dummy_node
        ; prev_node = MyCFG.dummy_node
        ; control_context = (fun () -> ctx_failwith "Global initializers have no context.")
        ; context = (fun () -> ctx_failwith "Global initializers have no context.")
        ; edge    = MyCFG.Skip
        ; local   = Spec.D.top ()
        ; global  = (fun g -> EQSys.G.spec (getg (EQSys.GVar.spec g)))
        ; spawn   = (fun _ -> failwith "Global initializers should never spawn threads. What is going on?")
        ; split   = (fun _ -> failwith "Global initializers trying to split paths.")
        ; sideg   = (fun g d -> sideg (EQSys.GVar.spec g) (EQSys.G.create_spec d))
        }
      in
      let edges = CfgTools.getGlobalInits file in
      if (get_bool "dbg.verbose") then Logs.debug "Executing %d assigns." (List.length edges);
      let funs = ref [] in
      (*let count = ref 0 in*)
      let transfer_func (st : Spec.D.t) (loc, edge) : Spec.D.t =
        if M.tracing then M.trace "con" "Initializer %a\n" CilType.Location.pretty loc;
        (*incr count;
          if (get_bool "dbg.verbose")&& (!count mod 1000 = 0)  then Printf.printf "%d %!" !count;    *)
        Tracing.current_loc := loc;
        match edge with
        | MyCFG.Entry func        ->
          if M.tracing then M.trace "global_inits" "Entry %a\n" d_lval (var func.svar);
          Spec.body {ctx with local = st} func
        | MyCFG.Assign (lval,exp) ->
          if M.tracing then M.trace "global_inits" "Assign %a = %a\n" d_lval lval d_exp exp;
          (match lval, exp with
            | (Var v,o), (AddrOf (Var f,NoOffset))
              when v.vstorage <> Static && isFunctionType f.vtype ->
              (try funs := Cilfacade.find_varinfo_fundec f :: !funs with Not_found -> ())
            | _ -> ()
          );
          let res = Spec.assign {ctx with local = st} lval exp in
          (* Needed for privatizations (e.g. None) that do not side immediately *)
          let res' = Spec.sync {ctx with local = res} `Normal in
          if M.tracing then M.trace "global_inits" "\t\t -> state:%a\n" Spec.D.pretty res;
          res'
        | _                       -> failwith "Unsupported global initializer edge"
      in
      let with_externs = do_extern_inits ctx file in
      (*if (get_bool "dbg.verbose") then Printf.printf "Number of init. edges : %d\nWorking:" (List.length edges);    *)
      let old_loc = !Tracing.current_loc in
      let result : Spec.D.t = List.fold_left transfer_func with_externs edges in
      Tracing.current_loc := old_loc;
      if M.tracing then M.trace "global_inits" "startstate: %a\n" Spec.D.pretty result;
      result, !funs
    in

    let print_globals glob =
      let out = M.get_out (Spec.name ()) !GU.out in
      let print_one v st =
        ignore (Pretty.fprintf out "%a -> %a\n" EQSys.GVar.pretty_trace v EQSys.G.pretty st)
      in
      GHT.iter print_one glob
    in

    (* real beginning of the [analyze] function *)
    if get_bool "ana.sv-comp.enabled" then
      Witness.init (module FileCfg); (* TODO: move this out of analyze_loop *)

    GU.global_initialization := true;
    GU.earlyglobs := get_bool "exp.earlyglobs";
    let marshal: Spec.marshal option =
      if get_string "load_run" <> "" then
        Some (Serialize.unmarshal Fpath.(v (get_string "load_run") / "spec_marshal"))
      else if Serialize.results_exist () && get_bool "incremental.load" then
        Some (Serialize.Cache.(get_data AnalysisData))
      else
        None
    in

    (* Some happen in init, so enable this temporarily (if required by option). *)
    Goblintutil.should_warn := PostSolverArg.should_warn;
    Spec.init marshal;
    Access.init file;
    Goblintutil.should_warn := false;

    let test_domain (module D: Lattice.S): unit =
      let module DP = DomainProperties.All (D) in
      Logs.debug "domain testing...: %s" (D.name ());
      let errcode = QCheck_base_runner.run_tests DP.tests in
      if (errcode <> 0) then
        failwith "domain tests failed"
    in
    let _ =
      if (get_bool "dbg.test.domain") then (
        Logs.debug "domain testing analysis...: %s" (Spec.name ());
        test_domain (module Spec.D);
        test_domain (module Spec.G);
      )
    in

    let startstate, more_funs =
      if (get_bool "dbg.verbose") then Logs.debug "Initializing %d globals." (CfgTools.numGlobals file);
      Timing.wrap "global_inits" do_global_inits file
    in

    let otherfuns = if get_bool "kernel" then otherfuns @ more_funs else otherfuns in

    let enter_with st fd =
      let st = st fd.svar in
      let ctx =
        { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
        ; emit   = (fun _ -> failwith "Cannot \"emit\" in enter_with context.")
        ; node    = MyCFG.dummy_node
        ; prev_node = MyCFG.dummy_node
        ; control_context = (fun () -> ctx_failwith "enter_func has no context.")
        ; context = (fun () -> ctx_failwith "enter_func has no context.")
        ; edge    = MyCFG.Skip
        ; local   = st
        ; global  = (fun g -> EQSys.G.spec (getg (EQSys.GVar.spec g)))
        ; spawn   = (fun _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
        ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
        ; sideg   = (fun g d -> sideg (EQSys.GVar.spec g) (EQSys.G.create_spec d))
        }
      in
      let args = List.map (fun x -> MyCFG.unknown_exp) fd.sformals in
      let ents = Spec.enter ctx None fd args in
      List.map (fun (_,s) -> fd, s) ents
    in

    (try MyCFG.dummy_func.svar.vdecl <- (List.hd otherfuns).svar.vdecl with Failure _ -> ());

    let startvars =
      if startfuns = []
      then [[MyCFG.dummy_func, startstate]]
      else
        let morph f = Spec.morphstate f startstate in
        List.map (enter_with morph) startfuns
    in

    let exitvars = List.map (enter_with Spec.exitstate) exitfuns in
    let otherstate st v =
      let ctx =
        { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
        ; emit   = (fun _ -> failwith "Cannot \"emit\" in otherstate context.")
        ; node    = MyCFG.dummy_node
        ; prev_node = MyCFG.dummy_node
        ; control_context = (fun () -> ctx_failwith "enter_func has no context.")
        ; context = (fun () -> ctx_failwith "enter_func has no context.")
        ; edge    = MyCFG.Skip
        ; local   = st
        ; global  = (fun g -> EQSys.G.spec (getg (EQSys.GVar.spec g)))
        ; spawn   = (fun _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
        ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
        ; sideg   = (fun g d -> sideg (EQSys.GVar.spec g) (EQSys.G.create_spec d))
        }
      in
      (* TODO: don't hd *)
      List.hd (Spec.threadenter ctx None v [])
      (* TODO: do threadspawn to mainfuns? *)
    in
    let prestartstate = Spec.startstate MyCFG.dummy_func.svar in (* like in do_extern_inits *)
    let othervars = List.map (enter_with (otherstate prestartstate)) otherfuns in
    let startvars = List.concat (startvars @ exitvars @ othervars) in
    if startvars = [] then
      failwith "BUG: Empty set of start variables; may happen if enter_func of any analysis returns an empty list.";

    GU.global_initialization := false;

    let startvars' =
      if get_bool "exp.forward" then
        List.map (fun (n,e) -> (MyCFG.FunctionEntry n, Spec.context n e)) startvars
      else
        List.map (fun (n,e) -> (MyCFG.Function n, Spec.context n e)) startvars
    in

    let entrystates = List.map (fun (n,e) -> (MyCFG.FunctionEntry n, Spec.context n e), e) startvars in
    let entrystates_global = GHT.to_list gh in

    let uncalled_dead = ref 0 in

    let solve_and_postprocess () =
      (* handle save_run/load_run *)
      let solver_file = "solver.marshalled" in
      let load_run = get_string "load_run" in
      let compare_runs = get_string_list "compare_runs" in
      let gobview = get_bool "gobview" in
      let save_run_str = let o = get_string "save_run" in if o = "" then (if gobview then "run" else "") else o in

      let lh, gh = if load_run <> "" then (
          let module S2' = (GlobSolverFromEqSolver (Generic.LoadRunIncrSolver (PostSolverArg))) (EQSys) (LHT) (GHT) in
          let (r2, _) = S2'.solve entrystates entrystates_global startvars' None in (* TODO: has incremental data? *)
          r2
        ) else if compare_runs <> [] then (
          match compare_runs with
          | d1::d2::[] -> (* the directories of the runs *)
            if d1 = d2 then Logs.warn "Beware that you are comparing a run with itself! There should be no differences.";
            (* instead of rewriting Compare for EqConstrSys, just transform unmarshaled EqConstrSys solutions to GlobConstrSys soltuions *)
            let module Splitter = GlobConstrSolFromEqConstrSol (EQSys) (LHT) (GHT) in
            let module S2 = Splitter.S2 in
            let module VH = Splitter.VH in
            let (r1, r1'), (r2, r2') = Tuple2.mapn (fun d ->
                let vh = Serialize.unmarshal Fpath.(v d / solver_file) in

                let vh' = VH.create (VH.length vh) in
                VH.iter (fun k v ->
                    VH.replace vh' (S2.Var.relift k) (S2.Dom.relift v)
                  ) vh;

                (Splitter.split_solution vh', vh')
              ) (d1, d2)
            in

            if get_bool "dbg.compare_runs.globsys" then
              CompareGlobSys.compare (d1, d2) r1 r2;

            let module CompareEqSys = Constraints.CompareEqSys (S2) (VH) in
            if get_bool "dbg.compare_runs.eqsys" then
              CompareEqSys.compare (d1, d2) r1' r2';

            let module CompareGlobal = Constraints.CompareGlobal (EQSys.GVar) (EQSys.G) (GHT) in
            if get_bool "dbg.compare_runs.global" then
              CompareGlobal.compare (d1, d2) (snd r1) (snd r2);

            let module CompareNode = Constraints.CompareNode (Spec.C) (EQSys.D) (LHT) in
            if get_bool "dbg.compare_runs.node" then
              CompareNode.compare (d1, d2) (fst r1) (fst r2);

            r1 (* return the result of the first run for further options -- maybe better to exit early since compare_runs is its own mode. Only excluded verify below since it's on by default. *)
          | _ -> failwith "Currently only two runs can be compared!";
        ) else (
          let solver_data =
            match Inc.increment with
            | Some {solver_data; server; _} ->
              if server then
                Some (Slvr.copy_marshal solver_data) (* Copy, so that we can abort and reuse old data unmodified. *)
              else if GobConfig.get_bool "ana.opt.hashcons" then
                Some (Slvr.relift_marshal solver_data)
              else
                Some solver_data
            | None -> None
          in
          if get_bool "dbg.verbose" then
            Logs.info "%s" ("Solving the constraint system with " ^ get_string "solver" ^ ". Solver statistics are shown every " ^ string_of_int (get_int "dbg.solver-stats-interval") ^ "s or by signal " ^ get_string "dbg.solver-signal" ^ ".");
          Goblintutil.should_warn := get_string "warn_at" = "early" || gobview;
          let (lh, gh), solver_data = Timing.wrap "solving" (Slvr.solve entrystates entrystates_global startvars') solver_data in
          if GobConfig.get_bool "incremental.save" then
            Serialize.Cache.(update_data SolverData solver_data);
          if save_run_str <> "" then (
            let save_run = Fpath.v save_run_str in
            let analyses = Fpath.(save_run / "analyses.marshalled") in
            let config = Fpath.(save_run / "config.json") in
            let meta = Fpath.(save_run / "meta.json") in
            let solver_stats = Fpath.(save_run / "solver_stats.csv") in (* see Generic.SolverStats... *)
            let cil = Fpath.(save_run / "cil.marshalled") in
            let warnings = Fpath.(save_run / "warnings.marshalled") in
            let stats = Fpath.(save_run / "stats.marshalled") in
            if get_bool "dbg.verbose" then (
              Logs.Format.info "Saving the current configuration to %a, meta-data about this run to %a, and solver statistics to %a" Fpath.pp config Fpath.pp meta Fpath.pp solver_stats;
            );
            GobSys.mkdir_or_exists save_run;
            GobConfig.write_file config;
            let module Meta = struct
                type t = { command : string; version: string; timestamp : float; localtime : string } [@@deriving to_yojson]
                let json = to_yojson { command = GU.command_line; version = Version.goblint; timestamp = Unix.time (); localtime = localtime () }
              end
            in
            (* Yojson.Safe.to_file meta Meta.json; *)
            Yojson.Safe.pretty_to_channel (Stdlib.open_out (Fpath.to_string meta)) Meta.json; (* the above is compact, this is pretty-printed *)
            if gobview then (
              if get_bool "dbg.verbose" then (
                Logs.Format.info "Saving the analysis table to %a, the CIL state to %a, the warning table to %a, and the runtime stats to %a" Fpath.pp analyses Fpath.pp cil Fpath.pp warnings Fpath.pp stats;
              );
              Serialize.marshal MCPRegistry.registered_name analyses;
              Serialize.marshal (file, Cabs2cil.environment) cil;
              Serialize.marshal !Messages.Table.messages_list warnings;
            );
            Goblintutil.(self_signal (signal_of_string (get_string "dbg.solver-signal"))); (* write solver_stats after solving (otherwise no rows if faster than dbg.solver-stats-interval). TODO better way to write solver_stats without terminal output? *)
          );
          lh, gh
        )
      in

      if get_string "comparesolver" <> "" then (
        let compare_with (module S2 : GenericEqIncrSolver) =
          let module PostSolverArg2 =
          struct
            include PostSolverArg
            let should_warn = false (* we already warn from main solver *)
            let should_save_run = false (* we already save main solver *)
          end
          in
          let module S2' = (GlobSolverFromEqSolver (S2 (PostSolverArg2))) (EQSys) (LHT) (GHT) in
          let (r2, _) = S2'.solve entrystates entrystates_global startvars' None in (* TODO: has incremental data? *)
          CompareGlobSys.compare (get_string "solver", get_string "comparesolver") (lh,gh) (r2)
        in
        compare_with (Selector.choose_solver (get_string "comparesolver"))
      );

      (* Most warnings happen before during postsolver, but some happen later (e.g. in finalize), so enable this for the rest (if required by option). *)
      Goblintutil.should_warn := PostSolverArg.should_warn;

      let insrt k _ s = match k with
        | (MyCFG.Function fn,_) -> if not (get_bool "exp.forward") then Set.Int.add fn.svar.vid s else s
        | (MyCFG.FunctionEntry fn,_) -> if (get_bool "exp.forward") then Set.Int.add fn.svar.vid s else s
        | _ -> s
      in
      (* set of ids of called functions *)
      let calledFuns = LHT.fold insrt lh Set.Int.empty in
      let is_bad_uncalled fn loc =
        not (Set.Int.mem fn.vid calledFuns) &&
        not (Str.last_chars loc.file 2 = ".h") &&
        not (LibraryFunctions.is_safe_uncalled fn.vname) &&
        not (Cil.hasAttribute "goblint_stub" fn.vattr)
      in
      let print_and_calculate_uncalled = function
        | GFun (fn, loc) when is_bad_uncalled fn.svar loc->
            let cnt = Cilfacade.countLoc fn in
            uncalled_dead := !uncalled_dead + cnt;
            if get_bool "ana.dead-code.functions" then
              M.warn ~loc:(CilLocation loc) ~category:Deadcode "Function '%a' is uncalled: %d LLoC" CilType.Fundec.pretty fn cnt  (* CilLocation is fine because always printed from scratch *)
        | _ -> ()
      in
      List.iter print_and_calculate_uncalled file.globals;

      (* check for dead code at the last state: *)
      let main_sol = try LHT.find lh (List.hd startvars') with Not_found -> Spec.D.bot () in
      if get_bool "dbg.debug" && Spec.D.is_bot main_sol then
        M.warn_noloc ~category:Deadcode "Function 'main' does not return";

      if get_bool "dump_globs" then
        print_globals gh;

      (* run activated transformations with the analysis result *)
      let active_transformations = get_string_list "trans.activated" in
      (if active_transformations <> [] then
        (* Transformations work using Cil visitors which use the location, so we join all contexts per location. *)
        let joined =
          let open Batteries in let open Enum in
          let e = LHT.enum lh |> map (Tuple2.map1 (Node.location % fst)) in (* drop context from key and get location from node *)
          let h = Hashtbl.create (if fast_count e then count e else 123) in
          iter (fun (k,v) ->
            (* join values for the same location *)
            let v' = try Spec.D.join (Hashtbl.find h k) v with Not_found -> v in
            Hashtbl.replace h k v') e;
          h
        in
        let ask ~node loc = (fun (type a) (q: a Queries.t) ->
            let local = Hashtbl.find_option joined loc in
            match local with
            | None -> Queries.Result.bot q
            | Some local ->
              Query.ask_local_node gh node local q
          )
        in
        let ask ?(node=MyCFG.dummy_node) loc = { Queries.f = fun (type a) (q: a Queries.t) -> ask ~node loc q } in
        List.iter (fun name -> Transform.run name ask file) active_transformations
      );

      lh, gh
    in

    (* Use "normal" constraint solving *)
    let timeout_reached () =
      M.error "Timeout reached!";
      (* let module S = Generic.SolverStats (EQSys) (LHT) in *)
      (* Can't call Generic.SolverStats...print_stats :(
         print_stats is triggered by dbg.solver-signal, so we send that signal to ourself in maingoblint before re-raising Timeout.
         The alternative would be to catch the below Timeout, print_stats and re-raise in each solver (or include it in some functor above them). *)
      raise GU.Timeout
    in
    let timeout = get_string "dbg.timeout" |> Goblintutil.seconds_of_duration_string in
    let lh, gh = Goblintutil.timeout solve_and_postprocess () (float_of_int timeout) timeout_reached in
    let module SpecSysSol: SpecSysSol with module SpecSys = SpecSys =
    struct
      module SpecSys = SpecSys
      let lh = lh
      let gh = gh
    end
    in
    let module R: ResultQuery.SpecSysSol2 with module SpecSys = SpecSys = ResultQuery.Make (FileCfg) (SpecSysSol) in

    let local_xml = solver2source_result lh in
    current_node_state_json := (fun node -> Option.map LT.to_yojson (Result.find_option local_xml node));

    let liveness =
      if get_bool "ana.dead-code.lines" || get_bool "ana.dead-code.branches" then
        print_dead_code local_xml !uncalled_dead
      else
        fun _ -> true (* TODO: warn about conflicting options *)
    in

    if get_bool "exp.cfgdot" then
      CfgTools.dead_code_cfg (module FileCfg) liveness;

    let warn_global g v =
      (* ignore (Pretty.printf "warn_global %a %a\n" EQSys.GVar.pretty_trace g EQSys.G.pretty v); *)
      match g with
      | `Left g -> (* Spec global *)
        R.ask_global (WarnGlobal (Obj.repr g))
      | `Right _ -> (* contexts global *)
        ()
    in
    Timing.wrap "warn_global" (GHT.iter warn_global) gh;

    if get_bool "exp.arg" then (
      let module ArgTool = ArgTools.Make (R) in
      let module Arg = (val ArgTool.create entrystates) in
      if get_bool "exp.argdot" then (
        let module ArgDot = ArgTools.Dot (Arg) in
        let oc = Stdlib.open_out "arg.dot" in
        Fun.protect (fun () ->
            let ppf = Format.formatter_of_out_channel oc in
            ArgDot.dot ppf;
            Format.pp_print_flush ppf ()
          ) ~finally:(fun () ->
            Stdlib.close_out oc
          )
      );
      ArgTools.current_arg := Some (module Arg);
    );

    (* Before SV-COMP, so result can depend on YAML witness validation. *)
    if get_string "witness.yaml.validate" <> "" then (
      let module YWitness = YamlWitness.Validator (R) in
      YWitness.validate ()
    );

    if get_bool "ana.sv-comp.enabled" then (
      (* SV-COMP and witness generation *)
      let module WResult = Witness.Result (R) in
      WResult.write entrystates
    );

    if get_bool "witness.yaml.enabled" then (
      let module YWitness = YamlWitness.Make (R) in
      YWitness.write ()
    );

    let marshal = Spec.finalize () in
    (* copied from solve_and_postprocess *)
    let gobview = get_bool "gobview" in
    let save_run = let o = get_string "save_run" in if o = "" then (if gobview then "run" else "") else o in
    if save_run <> "" then (
      Serialize.marshal marshal Fpath.(v save_run / "spec_marshal")
    );
    if get_bool "incremental.save" then (
      Serialize.Cache.(update_data AnalysisData marshal);
      if not (get_bool "server.enabled") then
        Serialize.Cache.store_data ()
    );
    if get_bool "dbg.verbose" && get_string "result" <> "none" then Logs.info "Generating output: %s" (get_string "result");
    Timing.wrap "result output" (Result.output (lazy local_xml) gh make_global_fast_xml) file
end

(* This function was originally a part of the [AnalyzeCFG] module, but
   now that [AnalyzeCFG] takes [Spec] as a functor parameter,
   [analyze_loop] cannot reside in it anymore since each invocation of
   [get_spec] in the loop might/should return a different module, and we
   cannot swap the functor parameter from inside [AnalyzeCFG]. *)
let rec analyze_loop (module CFG : CfgBidir) file fs change_info =
  try
    let (module Spec) = get_spec () in
    let module A = AnalyzeCFG (CFG) (Spec) (struct let increment = change_info end) in
    GobConfig.with_immutable_conf (fun () -> A.analyze file fs)
  with Refinement.RestartAnalysis ->
    (* Tail-recursively restart the analysis again, when requested.
        All solving starts from scratch.
        Whoever raised the exception should've modified some global state
        to do a more precise analysis next time. *)
    (* TODO: do some more incremental refinement and reuse parts of solution *)
    analyze_loop (module CFG) file fs change_info

let compute_cfg file =
  let cfgF, cfgB = CfgTools.getCFG file in
  (module struct let prev = cfgB let next = cfgF end : CfgBidir)

(** The main function to perform the selected analyses. *)
let analyze change_info (file: file) fs =
  if (get_bool "dbg.verbose") then Logs.debug "Generating the control flow graph.";
  let (module CFG) = compute_cfg file in
  MyCFG.current_cfg := (module CFG);
  analyze_loop (module CFG) file fs change_info
