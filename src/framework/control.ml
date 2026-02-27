(** Main internal functionality: analysis of the program by abstract interpretation via constraint solving. *)

(** An analyzer that takes the CFG from [MyCFG], a solver from [Selector], constraints from [Constraints] (using the specification from [MCP]) *)

open Batteries
open GoblintCil
open MyCFG
open Analyses
open BackwAnalyses
open Goblint_constraint.ConstrSys
open Goblint_constraint.Translators
open Goblint_constraint.SolverTypes
open GobConfig
open Constraints
open SpecLifters
open BidirConstrains

module type S2S = Spec2Spec

(* spec is lazy, so HConsed table in Hashcons lifters is preserved between analyses in server mode *)
let spec_module: (module Spec) Lazy.t = lazy (
  GobConfig.building_spec := true;
  let arg_enabled = get_bool "exp.arg.enabled" in
  let termination_enabled = List.mem "termination" (get_string_list "ana.activated") in (* check if loop termination analysis is enabled*)
  (* apply functor F on module X if opt is true *)
  let lift opt (module F : S2S) (module X : Spec) = (module (val if opt then (module F (X)) else (module X) : Spec) : Spec) in
  let module S1 =
    (val
      (module MCP.MCP2 : Spec)
      |> lift (get_int "ana.context.gas_value" >= 0) (ContextGasLifter.get_gas_lifter ())
      |> lift true (module WidenContextLifterSide) (* option checked in functor *)
      |> lift (get_int "ana.widen.delay.local" > 0) (module WideningDelay.DLifter)
      (* hashcons before witness to reduce duplicates, because witness re-uses contexts in domain and requires tag for PathSensitive3 *)
      |> lift (get_bool "ana.opt.hashcons" || arg_enabled) (module HashconsContextLifter)
      |> lift (get_bool "ana.opt.hashcached") (module HashCachedContextLifter)
      |> lift arg_enabled (module HashconsLifter)
      |> lift arg_enabled (module ArgConstraints.PathSensitive3)
      |> lift (not arg_enabled) (module PathSensitive2)
      |> lift (get_bool "ana.dead-code.branches") (module DeadBranchLifter)
      |> lift true (module DeadCodeLifter)
      |> lift (get_bool "dbg.slice.on") (module LevelSliceLifter)
      |> lift (get_bool "ana.opt.equal" && not (get_bool "ana.opt.hashcons")) (module OptEqual)
      |> lift (get_bool "ana.opt.hashcons") (module HashconsLifter)
      (* Widening tokens must be outside of hashcons, because widening token domain ignores token sets for identity, so hashcons doesn't allow adding tokens.
         Also must be outside of deadcode, because deadcode splits (like mutex lock event) don't pass on tokens. *)
      |> lift (get_bool "ana.widen.tokens") (module WideningTokenLifter.Lifter)
      |> lift true (module LongjmpLifter.Lifter)
      |> lift termination_enabled (module RecursionTermLifter.Lifter) (* Always activate the recursion termination analysis, when the loop termination analysis is activated*)
      |> lift (get_int "ana.widen.delay.global" > 0) (module WideningDelay.GLifter)
    )
  in
  GobConfig.building_spec := false;
  ControlSpecC.control_spec_c := (module S1.C);
  (module S1)
)

(** gets Spec for current options *)
let get_spec (): (module Spec) =
  Lazy.force spec_module

let current_node_state_json : (Node.t -> Yojson.Safe.t option) ref = ref (fun _ -> None)

let current_varquery_global_state_json: (Goblint_constraint.VarQuery.t option -> Yojson.Safe.t) ref = ref (fun _ -> `Null)

(** Given a [Cfg], a [Spec], and an [Inc], computes the solution to [MCP.Path] *)
module AnalyzeCFG (Cfg:CfgBidirSkip) (Spec:Spec) (Inc:Increment) =
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
  module Slvr  = (GlobSolverFromEqSolver (Goblint_solver.Selector.Make (PostSolverArg))) (EQSys) (LHT) (GHT)
  (* The comparator *)
  module CompareGlobSys = CompareConstraints.CompareGlobSys (SpecSys)

  (* Triple of the function, context, and the local value. *)
  module RT = AnalysisResult.ResultType2 (Spec)
  (* Set of triples [RT] *)
  module LT = SetDomain.HeadlessSet (RT)
  (* Analysis result structure---a hashtable from program points to [LT] *)
  module Result = AnalysisResult.Result (LT) (struct let result_name = "analysis" end)
  module ResultOutput = AnalysisResultOutput.Make (Result)

  module Query = ResultQuery.Query (SpecSys)

  (* print out information about dead code *)
  let print_dead_code (xs:Result.t) uncalled_fn_loc =
    let module NH = Hashtbl.Make (Node) in
    let live_nodes : unit NH.t = NH.create 10 in
    let count = ref 0 in (* Is only populated if "ana.dead-code.lines" or "ana.dead-code.branches" is true *)
    let module StringMap = BatMap.Make (String) in
    let live_lines = ref StringMap.empty in
    let dead_lines = ref StringMap.empty in
    let module FunSet = Hashtbl.Make (CilType.Fundec) in
    let live_funs: unit FunSet.t = FunSet.create 13 in
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
        FunSet.replace live_funs f ();
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
    if List.mem "termination" @@ get_string_list "ana.activated" then (
      (* check if we have upjumping gotos *)
      let open Cilfacade in
      let warn_for_upjumps fundec gotos =
        if FunSet.mem live_funs fundec then (
          (* set nortermiantion flag *)
          AnalysisState.svcomp_may_not_terminate := true;
          (* iterate through locations to produce warnings *)
          LocSet.iter (fun l _ ->
              M.warn ~loc:(M.Location.CilLocation l) ~category:Termination "The program might not terminate! (Upjumping Goto)"
            ) gotos
        )
      in
      FunLocH.iter warn_for_upjumps funs_with_upjumping_gotos
    );
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
      M.msg_group Warning ~category:Deadcode "Function '%s' has dead code" f msgs (* TODO: function location for group *)
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

    AnalysisState.should_warn := false; (* reset for server mode *)

    (* exctract global xml from result *)
    let make_global_fast_xml f g =
      let open Printf in
      let print_globals k v =
        fprintf f "\n<glob><key>%s</key>%a</glob>" (XmlUtil.escape (EQSys.GVar.show k)) EQSys.G.printXml v;
      in
      GHT.iter print_globals g
    in

    (* add extern variables to local state *)
    let do_extern_inits man (file : file) : Spec.D.t =
      let module VS = Set.Make (Basetype.Variables) in
      let add_glob s = function
          GVar (v,_,_) -> VS.add v s
        | _            -> s
      in
      let vars = foldGlobals file add_glob VS.empty in
      let set_bad v st =
        Spec.assign {man with local = st} (var v) MyCFG.unknown_exp
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
      if M.tracing then M.trace "global_inits" "sideg %a = %a" EQSys.GVar.pretty v EQSys.G.pretty d;
      GHT.replace gh v (EQSys.G.join (getg v) d)
    in
    (* Old-style global function for context.
     * This indirectly prevents global initializers from depending on each others' global side effects, which would require proper solving. *)
    let getg v = EQSys.G.bot () in

    (* analyze cil's global-inits function to get a starting state *)
    let do_global_inits (file: file) : Spec.D.t * fundec list =
      let man =
        { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
        ; emit   = (fun _ -> failwith "Cannot \"emit\" in global initializer context.")
        ; node    = MyCFG.dummy_node
        ; prev_node = MyCFG.dummy_node
        ; control_context = (fun () -> man_failwith "Global initializers have no context.")
        ; context = (fun () -> man_failwith "Global initializers have no context.")
        ; edge    = MyCFG.Skip
        ; local   = Spec.D.top ()
        ; global  = (fun g -> EQSys.G.spec (getg (EQSys.GVar.spec g)))
        ; spawn   = (fun ?(multiple=false) _ -> failwith "Global initializers should never spawn threads. What is going on?")
        ; split   = (fun _ -> failwith "Global initializers trying to split paths.")
        ; sideg   = (fun g d -> sideg (EQSys.GVar.spec g) (EQSys.G.create_spec d))
        }
      in
      let edges = CfgTools.getGlobalInits file in
      Logs.debug "Executing %d assigns." (List.length edges);
      let funs = ref [] in
      (*let count = ref 0 in*)
      let transfer_func (st : Spec.D.t) (loc, edge) : Spec.D.t =
        if M.tracing then M.trace "con" "Initializer %a" CilType.Location.pretty loc;
        (*incr count;
          if (get_bool "dbg.verbose")&& (!count mod 1000 = 0)  then Printf.printf "%d %!" !count;    *)
        match edge with
        | MyCFG.Entry func        ->
          if M.tracing then M.trace "global_inits" "Entry %a" d_lval (var func.svar);
          Spec.body {man with local = st} func
        | MyCFG.Assign (lval,exp) ->
          if M.tracing then M.trace "global_inits" "Assign %a = %a" d_lval lval d_exp exp;
          begin match lval, exp with
            | (Var v,o), (AddrOf (Var f,NoOffset))
              when v.vstorage <> Static && isFunctionType f.vtype ->
              (try funs := Cilfacade.find_varinfo_fundec f :: !funs with Not_found -> ())
            | _ -> ()
          end;
          let res = Spec.assign {man with local = st} lval exp in
          (* Needed for privatizations (e.g. None) that do not side immediately *)
          let res' = Spec.sync {man with local = res} `Normal in
          if M.tracing then M.trace "global_inits" "\t\t -> state:%a" Spec.D.pretty res;
          res'
        | _                       -> failwith "Unsupported global initializer edge"
      in
      let transfer_func st (loc, edge) =
        let old_loc = !Goblint_tracing.current_loc in
        Goblint_tracing.current_loc := loc;
        (* TODO: next_loc? *)
        Goblint_backtrace.protect ~mark:(fun () -> Constraints.TfLocation loc) ~finally:(fun () ->
            Goblint_tracing.current_loc := old_loc;
          ) (fun () ->
            transfer_func st (loc, edge)
          )
      in
      let with_externs = do_extern_inits man file in
      (*if (get_bool "dbg.verbose") then Printf.printf "Number of init. edges : %d\nWorking:" (List.length edges);    *)
      let result : Spec.D.t = List.fold_left transfer_func with_externs edges in
      if M.tracing then M.trace "global_inits" "startstate: %a" Spec.D.pretty result;
      result, !funs
    in

    let print_globals glob =
      let out = M.get_out (Spec.name ()) !M.out in
      let print_one v st =
        ignore (Pretty.fprintf out "%a -> %a\n" EQSys.GVar.pretty_trace v EQSys.G.pretty st)
      in
      GHT.iter print_one glob
    in

    (* real beginning of the [analyze] function *)
    if get_bool "ana.sv-comp.enabled" then
      Witness.init (module FileCfg); (* TODO: move this out of analyze_loop *)
    YamlWitness.init ();

    AnalysisState.global_initialization := true;
    GobConfig.earlyglobs := get_bool "exp.earlyglobs";
    let marshal: Spec.marshal option =
      if get_string "load_run" <> "" then
        Some (Serialize.unmarshal Fpath.(v (get_string "load_run") / "spec_marshal"))
      else if Serialize.results_exist () && get_bool "incremental.load" then
        Some (Serialize.Cache.(get_data AnalysisData))
      else
        None
    in

    (* Some happen in init, so enable this temporarily (if required by option). *)
    AnalysisState.should_warn := PostSolverArg.should_warn;
    Spec.init marshal;
    Access.init file;
    AnalysisState.should_warn := false;

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
      Logs.debug "Initializing %d globals." (CfgTools.numGlobals file);
      Timing.wrap "global_inits" do_global_inits file
    in

    let otherfuns = if get_bool "kernel" then otherfuns @ more_funs else otherfuns in

    let enter_with st fd =
      let st = st fd.svar in
      let man =
        { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
        ; emit   = (fun _ -> failwith "Cannot \"emit\" in enter_with context.")
        ; node    = MyCFG.dummy_node
        ; prev_node = MyCFG.dummy_node
        ; control_context = (fun () -> man_failwith "enter_with has no control_context.")
        ; context = Spec.startcontext
        ; edge    = MyCFG.Skip
        ; local   = st
        ; global  = (fun g -> EQSys.G.spec (getg (EQSys.GVar.spec g)))
        ; spawn   = (fun ?(multiple=false) _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
        ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
        ; sideg   = (fun g d -> sideg (EQSys.GVar.spec g) (EQSys.G.create_spec d))
        }
      in
      let args = List.map (fun x -> MyCFG.unknown_exp) fd.sformals in
      let ents = Spec.enter man None fd args in
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
      let man =
        { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
        ; emit   = (fun _ -> failwith "Cannot \"emit\" in otherstate context.")
        ; node    = MyCFG.dummy_node
        ; prev_node = MyCFG.dummy_node
        ; control_context = (fun () -> man_failwith "enter_func has no context.")
        ; context = (fun () -> man_failwith "enter_func has no context.")
        ; edge    = MyCFG.Skip
        ; local   = st
        ; global  = (fun g -> EQSys.G.spec (getg (EQSys.GVar.spec g)))
        ; spawn   = (fun ?(multiple=false) _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
        ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
        ; sideg   = (fun g d -> sideg (EQSys.GVar.spec g) (EQSys.G.create_spec d))
        }
      in
      (* TODO: don't hd *)
      List.hd (Spec.threadenter man ~multiple:false None v [])
      (* TODO: do threadspawn to mainfuns? *)
    in
    let prestartstate = Spec.startstate MyCFG.dummy_func.svar in (* like in do_extern_inits *)
    let othervars = List.map (enter_with (otherstate prestartstate)) otherfuns in
    let startvars = List.concat (startvars @ exitvars @ othervars) in
    if startvars = [] then
      failwith "BUG: Empty set of start variables; may happen if enter_func of any analysis returns an empty list.";

    AnalysisState.global_initialization := false;

    let man e =
      { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
      ; emit   = (fun _ -> failwith "Cannot \"emit\" in enter_with context.")
      ; node    = MyCFG.dummy_node
      ; prev_node = MyCFG.dummy_node
      ; control_context = (fun () -> man_failwith "enter_with has no control_context.")
      ; context = Spec.startcontext
      ; edge    = MyCFG.Skip
      ; local   = e
      ; global  = (fun g -> EQSys.G.spec (getg (EQSys.GVar.spec g)))
      ; spawn   = (fun ?(multiple=false) _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
      ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
      ; sideg   = (fun g d -> sideg (EQSys.GVar.spec g) (EQSys.G.create_spec d))
      }
    in
    let startvars' =
      if get_bool "exp.forward" then
        List.map (fun (n,e) -> (MyCFG.FunctionEntry n, Spec.context (man e) n e)) startvars
      else
        List.map (fun (n,e) -> (MyCFG.Function n, Spec.context (man e) n e)) startvars
    in

    let entrystates = List.map (fun (n,e) -> (MyCFG.FunctionEntry n, Spec.context (man e) n e), e) startvars in
    let entrystates_global = GHT.to_list gh in

    let uncalled_dead = ref 0 in

    let solve_and_postprocess () =
      let lh, gh =
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
        Logs.debug "%s" ("Solving the constraint system with " ^ get_string "solver" ^ ". Solver statistics are shown every " ^ string_of_int (get_int "dbg.solver-stats-interval") ^ "s or by signal " ^ get_string "dbg.solver-signal" ^ ".");
        AnalysisState.should_warn := get_string "warn_at" = "early";

        let log_analysis_inputs () =
          Logs.debug "=== Analysis Inputs ===";

          (* Log entrystates *)
          Logs.debug "--- Entry States (count: %d) ---" (List.length entrystates);
          List.iteri (fun i ((node, ctx), state) ->
              Logs.debug "EntryState %d:" (i + 1);
              Logs.debug "  Node: %a" Node.pretty_trace node;
              Logs.debug "  Context: %a" Spec.C.pretty ctx;
              Logs.debug "  State: %a" Spec.D.pretty state;
            ) entrystates;

          (* Log entrystates_global *)
          Logs.debug "--- Global Entry States (count: %d) ---" (List.length entrystates_global);
          List.iteri (fun i (gvar, gstate) ->
              Logs.debug "GlobalEntryState %d:" (i + 1);
              Logs.debug "  GVar: %a" EQSys.GVar.pretty gvar;
              Logs.debug "  GState: %a" EQSys.G.pretty gstate;
            ) entrystates_global;

          (* Log startvars' *)
          Logs.debug "--- Start Variables (count: %d) ---" (List.length startvars');
          List.iteri (fun i (node, ctx) ->
              Logs.debug "StartVar %d:" (i + 1);
              Logs.debug "  Node: %a" Node.pretty_trace node;
              Logs.debug "  Context: %a" Spec.C.pretty ctx;
            ) startvars';

          (* Log startvars (without apostrophe) *)
          Logs.debug "--- Start Variables (no apostrophe) (count: %d) ---" (List.length startvars);
          List.iteri (fun i (node, state) ->
              Logs.debug "StartVar (no apostrophe) %d:" (i + 1);
              Logs.debug "  Node: %a" CilType.Fundec.pretty node;
              Logs.debug "  State: (of type EQSys.D.t) %a" Spec.D.pretty state;
            ) startvars;

          Logs.debug "=== End Analysis Inputs ==="
        in
        log_analysis_inputs ();


        let (lh, gh), solver_data = Timing.wrap "solving" (Slvr.solve entrystates entrystates_global startvars') solver_data in
        if GobConfig.get_bool "incremental.save" then
          Serialize.Cache.(update_data SolverData solver_data);
        lh, gh

      in

      (* Most warnings happen before during postsolver, but some happen later (e.g. in finalize), so enable this for the rest (if required by option). *)
      AnalysisState.should_warn := PostSolverArg.should_warn;

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
      if Spec.D.is_bot main_sol then
        M.warn_noloc ~category:Deadcode "Function 'main' does not return";

      if get_bool "dump_globs" then
        print_globals gh;

      (* run activated transformations with the analysis result *)
      let active_transformations = get_string_list "trans.activated" in
      if active_transformations <> [] then (

        (* Most transformations use the locations of statements, since they run using Cil visitors.
           Join abstract values once per location and once per node. *)
        let joined_by_loc, joined_by_node =
          let open Enum in
          let node_values = LHT.enum lh |> map (Tuple2.map1 fst) in (* drop context from key *) (* nosemgrep: batenum-enum *)
          let hashtbl_size = if fast_count node_values then count node_values else 123 in
          let by_loc, by_node = Hashtbl.create hashtbl_size, NodeH.create hashtbl_size in
          iter (fun (node, v) ->
              let loc = match node with
                | Statement s -> Cil.get_stmtLoc s.skind (* nosemgrep: cilfacade *) (* Must use CIL's because syntactic search is in CIL. *)
                | FunctionEntry _ | Function _ -> Node.location node
              in
              (* join values once for the same location and once for the same node *)
              let join = Option.some % function None -> v | Some v' -> Spec.D.join v v' in
              Hashtbl.modify_opt loc join by_loc;
              NodeH.modify_opt node join by_node;
            ) node_values;
          by_loc, by_node
        in

        let ask ?(node = MyCFG.dummy_node) loc =
          let f (type a) (q : a Queries.t) : a =
            match Hashtbl.find_option joined_by_loc loc with
            | None -> Queries.Result.bot q
            | Some local -> Query.ask_local_node gh node local q
          in
          ({ f } : Queries.ask)
        in

        (* A node is dead when its abstract value is bottom in all contexts;
           it holds that: bottom in all contexts iff. bottom in the join of all contexts.
           Therefore, we just answer whether the (stored) join is bottom. *)
        let must_be_dead node =
          NodeH.find_option joined_by_node node
          (* nodes that didn't make it into the result are definitely dead (hence for_all) *)
          |> GobOption.for_all Spec.D.is_bot
        in

        let must_be_uncalled fd = not @@ BatSet.Int.mem fd.svar.vid calledFuns in

        let skipped_statements from_node edge to_node =
          try
            Cfg.skippedByEdge from_node edge to_node
          with Not_found ->
            []
        in

        Transform.run_transformations file active_transformations
          { ask ; must_be_dead ; must_be_uncalled ;
            cfg_forward = Cfg.next ; cfg_backward = Cfg.prev ; skipped_statements };
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
      raise Timeout.Timeout
    in
    let timeout = get_string "dbg.timeout" |> TimeUtil.seconds_of_duration_string in
    let lh, gh = Timeout.wrap solve_and_postprocess () (float_of_int timeout) timeout_reached in
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

    current_varquery_global_state_json := (fun vq_opt ->
        let iter_vars f = match vq_opt with
          | None -> GHT.iter (fun v _ -> f v) gh
          | Some vq ->
            EQSys.iter_vars
              (fun x -> try LHT.find lh x with Not_found -> EQSys.D.bot ())
              (fun x -> try GHT.find gh x with Not_found -> EQSys.G.bot ())
              vq
              (fun _ -> ())
              f
        in
        (* TODO: optimize this once server has a way to properly convert vid -> varinfo *)
        let vars = GHT.create 113 in
        iter_vars (fun x ->
            GHT.replace vars x ()
          );
        let assoc = GHT.fold (fun x g acc ->
            if GHT.mem vars x then
              (EQSys.GVar.show x, EQSys.G.to_yojson g) :: acc
            else
              acc
          ) gh []
        in
        `Assoc assoc
      );

    let liveness =
      if get_bool "ana.dead-code.lines" || get_bool "ana.dead-code.branches" then
        print_dead_code local_xml !uncalled_dead
      else
        fun _ -> true (* TODO: warn about conflicting options *)
    in

    if get_bool "exp.cfgdot" then
      CfgTools.dead_code_cfg ~path:(Fpath.v "cfgs") (module FileCfg) liveness;

    let warn_global g v =
      (* Logs.debug "warn_global %a %a" EQSys.GVar.pretty_trace g EQSys.G.pretty v; *)
      match g with
      | `Left g -> (* Spec global *)
        R.ask_global (WarnGlobal (Obj.repr g))
      | `Right _ -> (* contexts global *)
        ()
    in
    Timing.wrap "warn_global" (GHT.iter warn_global) gh;

    if get_bool "exp.arg.enabled" then (
      let module ArgTool = ArgTools.Make (R) in
      let module Arg = (val ArgTool.create entrystates) in
      let arg_dot_path = get_string "exp.arg.dot.path" in
      if arg_dot_path <> "" then (
        let module NoLabelNodeStyle =
        struct
          type node = Arg.Node.t
          let extra_node_styles node =
            match GobConfig.get_string "exp.arg.dot.node-label" with
            | "node" -> []
            | "empty" -> ["label=\"_\""] (* can't have empty string because graph-easy will default to node ID then... *)
            | _ -> assert false
        end
        in
        let module ArgDot = ArgTools.Dot (Arg) (NoLabelNodeStyle) in
        Out_channel.with_open_text arg_dot_path (fun oc ->
            let ppf = Stdlib.Format.formatter_of_out_channel oc in
            ArgDot.dot ppf;
            Format.pp_print_flush ppf ()
          )
      );
      ArgTools.current_arg := Some (module Arg);
    );

    (* Before SV-COMP, so result can depend on YAML witness validation. *)
    let yaml_validate_result =
      if get_string "witness.yaml.validate" <> "" then (
        let module YWitness = YamlWitness.Validator (R) in
        Some (YWitness.validate ())
      )
      else
        None
    in

    let svcomp_result =
      if get_bool "ana.sv-comp.enabled" then (
        (* SV-COMP and witness generation *)
        let module WResult = Witness.Result (R) in
        Some (WResult.write yaml_validate_result entrystates)
      )
      else
        None
    in

    if get_bool "witness.yaml.enabled" then (
      let module YWitness = YamlWitness.Make (R) in
      YWitness.write ~svcomp_result
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
    if get_string "result" <> "none" then Logs.debug "Generating output: %s" (get_string "result");

    Messages.finalize ();
    Timing.wrap "result output" (ResultOutput.output (lazy local_xml) liveness gh make_global_fast_xml) (module FileCfg)
end

(** Given a [Cfg], a [Spec_forw], [Spec_back], and an unused [Inc], computes the solution] *)
module AnalyzeCFG_bidir (Cfg:CfgBidirSkip) (Spec_forw:Spec) (BackwSpecSpec : BackwAnalyses.BackwSpecSpec) (Inc:Increment) =
struct
  module Spec_backw = BackwSpecSpec (Spec_forw)
  (* The Equation system *)
  module EQSys = BidirConstrains.BidirFromSpec (Spec_forw) (Spec_backw) (Cfg) (Inc)

  (* Hashtbl for locals *)
  module LHT   = BatHashtbl.Make (EQSys.LVar)
  (* Hashtbl for globals *)
  module GHT   = BatHashtbl.Make (EQSys.GVar)

  (* The solver *)
  module PostSolverArg =
  struct
    let should_prune = true
    let should_verify = true (*get_bool "verify"*)
    let should_warn = get_string "warn_at" <> "never"
    let should_save_run =
      (* copied from solve_and_postprocess *)
      let gobview = get_bool "gobview" in
      let save_run = let o = get_string "save_run" in if o = "" then (if gobview then "run" else "") else o in
      save_run <> ""
  end
  module Slvr  = (GlobSolverFromEqSolver (Goblint_solver.Selector.Make (PostSolverArg))) (EQSys) (LHT) (GHT)

  (* Forward result module *)
  (* Triple of the function, context, and the local value. It uses Spec and therefore has the wrong types.*)
  module type ResultBundle = sig
    module Spec : Spec
    module RT : module type of AnalysisResult.ResultType2 (Spec)
    module LT : module type of SetDomain.HeadlessSet (RT)
    module Result : module type of AnalysisResult.Result (LT) (struct let result_name = "" end)
    module ResultOutput : module type of AnalysisResultOutput.Make (Result)
  end

  module ResBundle_forw : ResultBundle with module Spec = Spec_forw = 
  struct
    module Spec = Spec_forw
    module RT = AnalysisResult.ResultType2 (Spec_forw)
    module LT = SetDomain.HeadlessSet (RT)
    module Result = AnalysisResult.Result (LT) (struct let result_name = "analysis_forw" end)
    module ResultOutput = AnalysisResultOutput.Make (Result)
  end

  (** this function converts the LHT to two Results of type forwards and backwards *)
  let solver2source_result h  = 
    let res_forw = ResBundle_forw.Result.create 113 in
    (* let res_backw = ResBundle_backw.Result.create 113 in *)

    (* Adding the state at each system variable to the final result *)
    let add_local_var_forw (n,es) state  =
      (* Not using Node.location here to have updated locations in incremental analysis.
          See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
      let state = match state with
        | `Lifted1 s -> s
        | `Bot -> Spec_forw.D.bot ()
        | `Top -> Spec_forw.D.top ()
        | `Lifted2 _ -> failwith "Unexpected backward state in forward result"
      in

      let loc = UpdateCil.getLoc n in
      if loc <> locUnknown then try
          let fundec = Node.find_fundec n in
          if ResBundle_forw.Result.mem res_forw n then
            (* If this source location has been added before, we look it up
              * and add another node to it information to it. *)
            let prev = ResBundle_forw.Result.find res_forw n in
            ResBundle_forw.Result.replace res_forw n (ResBundle_forw.LT.add (es,state,fundec) prev)
          else
            ResBundle_forw.Result.add res_forw n (ResBundle_forw.LT.singleton (es,state,fundec))
        (* If the function is not defined, and yet has been included to the
          * analysis result, we generate a warning. *)
        with Not_found ->
          Messages.debug ~category:Analyzer ~loc:(CilLocation loc) "Calculated state for undefined function: unexpected node %a" Node.pretty_trace n
    in

    LHT.iter (fun key -> 
        match key with  
        | `L_forw (n,es) -> add_local_var_forw (n,es)
        | `L_backw (n,es) -> (fun _ -> ())  (* add_local_var_backw (n, es))*) ) h;

    res_forw(*, res_backw*)


  (** [analyze file startfuns exitfuns otherfuns] is the main function to preform the selected analyses.*)
  let analyze (file: file) (startfuns, exitfuns, otherfuns: Analyses.fundecs) =
    let module FileCfg: FileCfg =
    struct
      let file = file
      module Cfg = Cfg
    end
    in

    let module GV_forw = GVarF (Spec_forw.V) in
    let module GV_backw = GVarF (Spec_backw.V) in

    let module G_forw = GVarG (Spec_forw.G) (Spec_forw.C) in
    let module G_backw = GVarG (Spec_backw.G) (Spec_backw.C) in


    let log_analysis_setup () =
      let log_fun_list name funs =
        let fun_names = List.map (fun f -> f.svar.vname) funs in
        Logs.debug "%s functions: %s" name (String.concat ", " fun_names)
      in
      Logs.debug "================= Analysis Setup ================";
      log_fun_list "Start" startfuns;
      log_fun_list "Exit" exitfuns;
      log_fun_list "Other" otherfuns;
      Logs.debug "=================================================";
    in
    log_analysis_setup ();

    AnalysisState.should_warn := false; 

    (* initialize hastable for globals*)
    let gh = GHT.create 13 in
    let getg v = GHT.find_default gh v (EQSys.G.bot ()) in
    let sideg v d = GHT.replace gh v (EQSys.G.join (getg v) d)
    in

    (* the intit globals should not depend on each other*)
    let getg v = EQSys.G.bot () in

    (** this function calculates and returns  [startvars'_forw] and [entrystates_forw] *)
    let do_forward_inits () : (node * Spec_forw.C.t) list * ((node * Spec_forw.C.t) * Spec_forw.D.t) list = 

      (* wrapping functions accessing and modifying global variables *)
      let sideg_forw v d = sideg (`Forw (v)) ((`Lifted1 d)) in
      let getg_forw v =
        match EQSys.G.spec (getg (`Forw v)) with
        | `Lifted1 g -> G_forw.create_spec g
        | `Bot ->  failwith "Unexpected global state" (*G_forw.bot (); *)
        | `Top ->  failwith "Unexpected global state" (*G_forw.top ()*)
        | `Lifted2 _ -> failwith "Unexpected backward global state"
      in

      let do_extern_inits_forw man (file: file) : Spec_forw.D.t =
        let module VS = Set.Make (Basetype.Variables) in
        let add_glob s = function
          | GVar (v,_,_) -> VS.add v s
          | _            -> s
        in
        let vars = foldGlobals file add_glob VS.empty in
        let set_bad v st =
          Spec_forw.assign {man with local = st} (var v) MyCFG.unknown_exp
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
        Logs.debug "startstate of Spec_forw: %a" Spec_forw.D.pretty (Spec_forw.startstate MyCFG.dummy_func.svar);
        foldGlobals file add_externs (Spec_forw.startstate MyCFG.dummy_func.svar)
      in

      (** this function uses cil's global-inits function to get a starting state *)
      let do_global_inits_forw (file: file) : Spec_forw.D.t * fundec list =
        let man =
          { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
          ; emit   = (fun _ -> failwith "Cannot \"emit\" in global initializer context.")
          ; node    = MyCFG.dummy_node
          ; prev_node = MyCFG.dummy_node
          ; control_context = (fun () -> man_failwith "Global initializers have no context.")
          ; context = (fun () -> man_failwith "Global initializers have no context.")
          ; edge    = MyCFG.Skip
          ; local   = Spec_forw.D.top ()
          ; global  = (fun g -> G_forw.spec (getg (GV_forw.spec g)))
          ; spawn   = (fun ?(multiple=false) _ -> failwith "Global initializers should never spawn threads. What is going on?")
          ; split   = (fun _ -> failwith "Global initializers trying to split paths.")
          ; sideg   = (fun g d -> sideg_forw (GV_forw.spec g) (G_forw.create_spec d))
          }
        in

        let edges = CfgTools.getGlobalInits file in
        Logs.debug "Executing %d assigns." (List.length edges);
        let funs = ref [] in

        let transfer_func (st : Spec_forw.D.t) (loc, edge) : Spec_forw.D.t =
          match edge with
          | MyCFG.Entry func        -> Spec_forw.body {man with local = st} func
          | MyCFG.Assign (lval,exp) ->
            begin match lval, exp with
              | (Var v,o), (AddrOf (Var f,NoOffset))
                when v.vstorage <> Static && isFunctionType f.vtype ->
                (try funs := Cilfacade.find_varinfo_fundec f :: !funs with Not_found -> ())
              | _ -> ()
            end;
            let res = Spec_forw.assign {man with local = st} lval exp in
            (* Needed for privatizations (e.g. None) that do not side immediately *)
            let res' = Spec_forw.sync {man with local = res} `Normal in
            res'
          | _                       -> failwith "Unsupported global initializer edge"
        in

        let with_externs = do_extern_inits_forw man file in
        Logs.debug "witch_externs: %a" Spec_forw.D.pretty with_externs;
        let result : Spec_forw.D.t = List.fold_left transfer_func with_externs edges in
        result, !funs
      in

      let startstate, _ = do_global_inits_forw file in

      (** calculate startvars *)
      let calculate_startvars_forw ()  =

        let enter_with st fd =
          let st = st fd.svar in
          let man =
            { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
            ; emit   = (fun _ -> failwith "Cannot \"emit\" in enter_with context.")
            ; node    = MyCFG.dummy_node
            ; prev_node = MyCFG.dummy_node
            ; control_context = (fun () -> man_failwith "enter_with has no control_context.")
            ; context = Spec_forw.startcontext
            ; edge    = MyCFG.Skip
            ; local   = st
            ; global  = (fun g -> G_forw.spec (getg_forw (GV_forw.spec g)))
            ; spawn   = (fun ?(multiple=false) _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
            ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
            ; sideg   = (fun g d -> sideg_forw (GV_forw.spec g) (G_forw.create_spec (d)))
            }
          in
          let args = List.map (fun x -> MyCFG.unknown_exp) fd.sformals in
          let ents = Spec_forw.enter man None fd args in
          List.map (fun (_,s) -> fd, s) ents
        in

        (try MyCFG.dummy_func.svar.vdecl <- (List.hd otherfuns).svar.vdecl with Failure _ -> ());

        let startvars =
          if startfuns = []
          then [[MyCFG.dummy_func, startstate]]
          else
            let morph f = Spec_forw.morphstate f startstate in
            List.map (enter_with morph) startfuns
        in

        let exitvars = List.map (enter_with Spec_forw.exitstate) exitfuns in
        let otherstate st v =
          let man =
            { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
            ; emit   = (fun _ -> failwith "Cannot \"emit\" in otherstate context.")
            ; node    = MyCFG.dummy_node
            ; prev_node = MyCFG.dummy_node
            ; control_context = (fun () -> man_failwith "enter_func has no context.")
            ; context = (fun () -> man_failwith "enter_func has no context.")
            ; edge    = MyCFG.Skip
            ; local   = st
            ; global  = (fun g -> G_forw.spec (getg_forw (GV_forw.spec g)))
            ; spawn   = (fun ?(multiple=false) _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
            ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
            ; sideg   = (fun g d -> sideg_forw  (GV_forw.spec g) (G_forw.create_spec (d)))
            }
          in
          (* TODO: don't hd *)
          List.hd (Spec_forw.threadenter man ~multiple:false None v [])
          (* TODO: do threadspawn to mainfuns? *)
        in
        let prestartstate = Spec_forw.startstate MyCFG.dummy_func.svar in (* like in do_extern_inits *)
        let othervars = List.map (enter_with (otherstate prestartstate)) otherfuns in
        let startvars = List.concat (startvars @ exitvars @ othervars) in
        if startvars = [] then
          failwith "BUG: Empty set of start variables; may happen if enter_func of any analysis returns an empty list.";

        AnalysisState.global_initialization := false;

        let man e =
          { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
          ; emit   = (fun _ -> failwith "Cannot \"emit\" in enter_with context.")
          ; node    = MyCFG.dummy_node
          ; prev_node = MyCFG.dummy_node
          ; control_context = (fun () -> man_failwith "enter_with has no control_context.")
          ; context = Spec_forw.startcontext
          ; edge    = MyCFG.Skip
          ; local   = e
          ; global  = (fun g -> G_forw.spec (getg_forw (GV_forw.spec g)))
          ; spawn   = (fun ?(multiple=false) _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
          ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
          ; sideg   = (fun g d -> sideg_forw (GV_forw.spec g) (G_forw.create_spec d))
          }
        in
        let startvars' = List.map (fun (n,e) -> (MyCFG.Function n, Spec_forw.context (man e) n e)) startvars in
        let entrystates = List.map (fun (n,e) -> (MyCFG.FunctionEntry n, Spec_forw.context (man e) n e), e) startvars in

        startvars', entrystates
      in

      calculate_startvars_forw ()
    in

    (** this function calculates and returns  [startvars'_backw] and [entrystates_backw] *)
    let do_backward_inits () : (node * Spec_backw.C.t) list * ((node * Spec_forw.C.t) * Spec_backw.D.t) list = 

      let sideg_backw v d = sideg (`Backw v) (EQSys.G.create_spec (`Lifted2 d)) in
      let getg_backw v =
        match EQSys.G.spec (getg (`Backw v)) with
        | `Lifted1 _ -> failwith "Unexpected backward global state"
        | `Bot -> G_backw.bot ()
        | `Top -> G_backw.top ()
        | `Lifted2 g -> G_backw.create_spec g
      in

      let do_extern_inits_backw man man_forw (file: file) : Spec_backw.D.t =
        let module VS = Set.Make (Basetype.Variables) in
        let add_glob s = function
          | GVar (v,_,_) -> VS.add v s
          | _            -> s
        in
        let vars = foldGlobals file add_glob VS.empty in
        let set_bad v st =
          Spec_backw.assign {man with local = st} man_forw (var v) MyCFG.unknown_exp
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
        foldGlobals file add_externs (Spec_backw.startstate MyCFG.dummy_func.svar)
      in

      (** This function analyses cil's global-inits function to get a starting state *)
      let do_global_inits_backw (file: file) : Spec_backw.D.t * fundec list =

        let man =
          { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
          ; emit   = (fun _ -> failwith "Cannot \"emit\" in global initializer context.")
          ; node    = MyCFG.dummy_node
          ; prev_node = MyCFG.dummy_node
          ; control_context = (fun () -> man_failwith "Global initializers have no context.")
          ; context = (fun () -> man_failwith "Global initializers have no context.")
          ; edge    = MyCFG.Skip
          ; local   = Spec_backw.D.top ()
          ; global  = (fun _ -> Spec_backw.G.bot ())
          ; spawn   = (fun ?(multiple=false) _ -> failwith "Global initializers should never spawn threads. What is going on?")
          ; split   = (fun _ -> failwith "Global initializers trying to split paths.")
          ; sideg   = (fun g d -> sideg_backw (GV_backw.spec g) d)
          }
        in

        let man_forw =
          { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
          ; emit   = (fun _ -> failwith "Cannot \"emit\" in global initializer context.")
          ; node    = MyCFG.dummy_node
          ; prev_node = MyCFG.dummy_node
          ; control_context = (fun () -> man_failwith "Global initializers have no context.")
          ; context = (fun () -> man_failwith "Global initializers have no context.")
          ; edge    = MyCFG.Skip
          ; local   = Spec_forw.D.top () (*TODO: SOULD I GET THE VALUE FROM THE FORWARD INITIALIZATION?*)
          ; global  = (fun _ -> Spec_forw.G.bot ())
          ; spawn   = (fun ?(multiple=false) _ -> failwith "Global initializers should never spawn threads. What is going on?")
          ; split   = (fun _ -> failwith "Global initializers trying to split paths.")
          ; sideg   = (fun _ _ -> failwith "forw_man in the backwards initialization should not be used to sideeffect globals.")
          }
        in

        let edges = CfgTools.getGlobalInits file in
        Logs.debug "Executing %d assigns." (List.length edges);
        let funs = ref [] in

        let transfer_func (st : Spec_backw.D.t) (loc, edge) : Spec_backw.D.t =
          match edge with
          | MyCFG.Entry func        -> Spec_backw.body {man with local = st} man_forw func
          | MyCFG.Assign (lval,exp) ->
            begin match lval, exp with
              | (Var v,o), (AddrOf (Var f,NoOffset))
                when v.vstorage <> Static && isFunctionType f.vtype ->
                (try funs := Cilfacade.find_varinfo_fundec f :: !funs with Not_found -> ())
              | _ -> ()
            end;
            let res = Spec_backw.assign {man with local = st} man_forw lval exp in
            (* Needed for privatizations (e.g. None) that do not side immediately *)
            let res' = Spec_backw.sync {man with local = res} man_forw `Normal in
            res'
          | _                       -> failwith "Unsupported global initializer edge"
        in

        let with_externs = do_extern_inits_backw man man_forw file in
        let result : Spec_backw.D.t = List.fold_left transfer_func with_externs edges in
        result, !funs
      in

      let startstate, _ = do_global_inits_backw file in

      (** calculate startvars *)
      let calculate_startvars_backw ()  =

        let enter_with st fd =
          let st = st fd.svar in
          let man =
            { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
            ; emit   = (fun _ -> failwith "Cannot \"emit\" in enter_with context.")
            ; node    = MyCFG.dummy_node
            ; prev_node = MyCFG.dummy_node
            ; control_context = (fun () -> man_failwith "enter_with has no control_context.")
            ; context = Spec_forw.startcontext
            ; edge    = MyCFG.Skip
            ; local   = st
            ; global  = (fun g -> G_backw.spec (getg_backw (GV_backw.spec g)))
            ; spawn   = (fun ?(multiple=false) _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
            ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
            ; sideg   = (fun g d -> sideg_backw (GV_backw.spec g) d)
            }
          in
          let man_forw =
            { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
            ; emit   = (fun _ -> failwith "Cannot \"emit\" in global initializer context.")
            ; node    = man.node
            ; prev_node = MyCFG.dummy_node (* SHOULD I USE DUMMY NODES HERE IN GENERAL? I PROBABLY SHOULÖD*)
            ; control_context = (fun () -> man_failwith "Global initializers have no context.")
            ; context = man.context
            ; edge    = MyCFG.Skip
            ; local   = Spec_forw.D.top () (*TODO: SOULD I GET THE VALUE FROM THE FORWARD INITIALIZATION?*)
            ; global  = (fun _ -> Spec_forw.G.bot ()) (*TODO: SHOULD I ALLOW TO ASK FOR GLOBALS?*)
            ; spawn   = (fun ?(multiple=false) _ -> failwith "Global initializers should never spawn threads. What is going on?")
            ; split   = (fun _ -> failwith "Global initializers trying to split paths.")
            ; sideg   = (fun _ _ -> failwith "forw_man in the backwards initialization should not be used to sideeffect globals.")
            }
          in

          let args = List.map (fun x -> MyCFG.unknown_exp) fd.sformals in
          let ents = Spec_backw.enter man man_forw None fd args in
          List.map (fun (_,s) -> fd, s) ents
        in

        (try MyCFG.dummy_func.svar.vdecl <- (List.hd otherfuns).svar.vdecl with Failure _ -> ());

        let startvars =
          if startfuns = []
          then [[MyCFG.dummy_func, startstate]]
          else
            let morph f = Spec_backw.morphstate f startstate in
            List.map (enter_with morph) startfuns
        in

        let exitvars = List.map (enter_with Spec_backw.exitstate) exitfuns in
        let otherstate st v =
          let man =
            { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
            ; emit   = (fun _ -> failwith "Cannot \"emit\" in otherstate context.")
            ; node    = MyCFG.dummy_node
            ; prev_node = MyCFG.dummy_node
            ; control_context = (fun () -> man_failwith "enter_func has no context.")
            ; context = (fun () -> man_failwith "enter_func has no context.")
            ; edge    = MyCFG.Skip
            ; local   = st
            ; global  = (fun g -> G_backw.spec (getg_backw (GV_backw.spec g)))
            ; spawn   = (fun ?(multiple=false) _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
            ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
            ; sideg   = (fun g d -> sideg_backw (GV_backw.spec g) d)
            }
          in

          let man_forw =
            { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
            ; emit   = (fun _ -> failwith "Cannot \"emit\" in otherstate context.")
            ; node    = man.node
            ; prev_node = MyCFG.dummy_node
            ; control_context = (fun () -> man_failwith "enter_func has no context.")
            ; context = (fun () -> man_failwith "enter_func has no context.")
            ; edge    = MyCFG.Skip
            ; local   = Spec_forw.D.top () (*TODO: SOULD I GET THE VALUE FROM THE FORWARD INITIALIZATION?*)
            ; global  = (fun _ -> Spec_forw.G.bot ()) (*TODO: SHOULD I ALLOW TO ASK FOR GLOBALS?*)
            ; spawn   = (fun ?(multiple=false) _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
            ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
            ; sideg   = (fun _ _ -> failwith "forw_man in the backwards initialization should not be used to sideeffect globals.")
            }
          in
          (* TODO: don't hd *)
          List.hd (Spec_backw.threadenter man man_forw ~multiple:false None v [])
          (* TODO: do threadspawn to mainfuns? *)
        in
        let prestartstate = Spec_backw.startstate MyCFG.dummy_func.svar in (* like in do_extern_inits *)
        let othervars = List.map (enter_with (otherstate prestartstate)) otherfuns in
        let startvars = List.concat (startvars @ exitvars @ othervars) in
        if startvars = [] then
          failwith "BUG: Empty set of start variables; may happen if enter_func of any analysis returns an empty list.";

        AnalysisState.global_initialization := false;

        (*
        let man e =
          { ask     = (fun (type a) (q: a Queries.t) -> Queries.Result.top q)
          ; emit   = (fun _ -> failwith "Cannot \"emit\" in enter_with context.")
          ; node    = MyCFG.dummy_node
          ; prev_node = MyCFG.dummy_node
          ; control_context = (fun () -> man_failwith "enter_with has no control_context.")
          ; context = Spec_forw.startcontext
          ; edge    = MyCFG.Skip
          ; local   = e
          ; global  = (fun g -> G_backw.spec (getg_backw (GV_backw.spec g)))
          ; spawn   = (fun ?(multiple=false) _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
          ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
          ; sideg   = (fun g d -> sideg_backw (GV_backw.spec g) d)
          }
        in
        let startvars' = List.map (fun (n,e) -> (MyCFG.FunctionEntry n, Spec_backw.context (man e) n e)) startvars in
        let entrystates = List.map (fun (n,e) -> (MyCFG.Function n, Spec_backw.context (man e) n e), e) startvars in *)

        (* Using dummy contexts which will be replaced by the contextx of the forward functions*)
        let startvars' = List.map (fun (n,e) -> (MyCFG.FunctionEntry n, Spec_forw.startcontext ())) startvars in
        let entrystates = List.map (fun (n,e) -> (MyCFG.Function n, Spec_forw.startcontext ()), e) startvars in

        startvars', entrystates
      in

      calculate_startvars_backw ()
    in

    (** calculates and combines the solver input calculation from the forwards and backwards part of the constraint system. Returns [startvars'] and [entrystate] and [entrystates_global].*)
    let calculate_solver_input () =
      (* Spec_forw (MCP) initialization *)
      AnalysisState.should_warn := PostSolverArg.should_warn;
      Spec_forw.init None;
      Access.init file;
      AnalysisState.should_warn := false;


      let entrystates_global = GHT.to_list gh in
      let startvars'_forw, entrystates_forw = do_forward_inits () in
      let startvars'_backw, entrystates_backw = do_backward_inits () in

      (* Let's assume there is onyl one entrystate and startvar each. In what examples is this not the case?*)
      let forward_context = match startvars'_forw with
        | (_, ctx) :: _ -> ctx
        | [] -> failwith "No startvars from forward analysis"
      in 
      let startvars'_backw = List.map (fun (n, _) -> (n, forward_context)) startvars'_backw in
      let entrystates_backw = List.map (fun ((n, _), d) -> ((n, forward_context), d)) entrystates_backw in

      (* Lifting and combining the startvars and entrystates from forwards and backwards analysis*)
      let startvars' = List. append (List.map (fun v -> `L_forw v) startvars'_forw) (List.map (fun v -> `L_backw v) startvars'_backw) in
      let entrystates = List.append (List.map (fun (v, d) -> (`L_forw v, `Lifted1 d)) entrystates_forw) (List.map (fun (v, d) -> (`L_backw v, `Lifted2 d)) entrystates_backw) in

      startvars', entrystates, entrystates_global
    in

    (** solves constraint system*)
    let solve () = 
      let solver_data = None in
      let startvars', entrystates, entrystates_global = calculate_solver_input () in

      let log_analysis_inputs () =
        Logs.debug "=== Analysis Inputs ===";

        (* Log entrystates *)
        Logs.debug "--- Entry States (count: %d) ---" (List.length entrystates);
        List.iteri (fun i (v, state) ->
            Logs.debug "EntryState %d:" (i + 1);
            Logs.debug "  Var: %a" EQSys.LVar.pretty_trace v;
            (* (match v with
               | `L_forw (node, ctx)
               | `L_backw (node, ctx) ->
               Logs.debug "  Node: %a" Node.pretty_trace node;
               Logs.debug "  Context: %a" Spec_forw.C.pretty ctx
               ); *)
            (match state with 
             | `Lifted1 d ->
               Logs.debug "  State: %a" Spec_forw.D.pretty d
             | `Lifted2 d ->
               Logs.debug "  State: %a" Spec_backw.D.pretty d
             | `Top ->
               Logs.debug "  State kind: Top";
             | `Bot ->
               Logs.debug "  State kind: Bot"
            );
          ) entrystates;

        (* Log entrystates_global *)
        Logs.debug "--- Global Entry States (count: %d) ---" (List.length entrystates_global);
        List.iteri (fun i (gvar, gstate) ->
            Logs.debug "GlobalEntryState %d:" (i + 1);
            Logs.debug "  GVar: %a" EQSys.GVar.pretty_trace gvar;
            Logs.debug "  GState: %a" EQSys.G.pretty gstate;
          ) entrystates_global;

        (* Log startvars' *)
        Logs.debug "--- Start Variables (count: %d) ---" (List.length startvars');
        List.iteri (fun i v ->
            Logs.debug "StartVar %d:" (i + 1);
            Logs.debug "  Var: %a" EQSys.LVar.pretty_trace v;
            (* (match v with
               | `L_forw (node, ctx)
               | `L_backw (node, ctx) ->
               Logs.debug "  Node: %a" Node.pretty_trace node;
               Logs.debug "  Context: %a" Spec_forw.C.pretty ctx
               ) *)
          ) startvars';

        Logs.debug "=== End Analysis Inputs ==="
      in
      log_analysis_inputs ();

      let (lh, gh), solver_data = Slvr.solve entrystates entrystates_global startvars' solver_data in

      let log_lh_contents lh = 
        let print_forw_entries : bool = false in
        let print_backw_entries : bool = true in

        Logs.debug "=== LHT Contents ===";
        Logs.debug "LHT size: %d" (LHT.length lh);
        let count = ref 0 in

        Logs.debug "--- Full entry details ---";
        LHT.iter (fun v state ->
            incr count;
            Logs.debug "Entry %d:" !count;
            if (match v with `L_forw _ -> print_forw_entries | `L_backw _ -> print_backw_entries)
            then (
              Logs.debug "  Var: %a" EQSys.LVar.pretty_trace v;
              Logs.debug "  Context: %a" Spec_forw.C.pretty (match v with
                  | `L_forw (_, ctx)
                  | `L_backw (_, ctx) -> ctx);
              (match state with 
               | `Lifted1 d -> 
                 (try
                    Logs.debug "  State:%a" Spec_forw.D.pretty d
                  with e ->
                    Logs.debug "  State: ERROR - %s" (Printexc.to_string e))
               | `Lifted2 d ->
                 (try
                    Logs.debug "  State: %a" Spec_backw.D.pretty d
                  with e ->
                    Logs.debug "  State: ERROR - %s" (Printexc.to_string e)
                 );
               | `Top ->
                 Logs.debug "  State kind: Top";
               | `Bot ->
                 Logs.debug "  State kind: Bot"
              );
            ) else (
              Logs.debug "  (Entry skipped in log)"
            )
          )
          lh;
        Logs.debug "Total entries in LHT: %d" !count;
        Logs.debug "=== End LHT Contents ===";
      in
      log_lh_contents lh;

      let joined_by_loc_backw, joined_by_node_backw =
        let open Enum in
        let node_values = LHT.enum lh in
        let node_backw_values =  filter_map (
            fun (key, d) -> 
              match key with 
              | `L_forw (_,_)  ->  None 
              | `L_backw (node, context) -> 
                (match d with 
                 | `Lifted2 d -> Some (node, d)
                 | _ -> None)
          ) node_values 
        in
        let hashtbl_size = if fast_count node_values then count node_values else 123 in
        let by_loc, by_node = Hashtbl.create hashtbl_size, NodeH.create hashtbl_size in
        iter (fun (node, v) ->
            let loc = match node with
              | Statement s -> Cil.get_stmtLoc s.skind (* nosemgrep: cilfacade *) (* Must use CIL's because syntactic search is in CIL. *)
              | FunctionEntry _ | Function _ -> Node.location node
            in
            (* join values once for the same location and once for the same node *)
            let join = Option.some % function None -> v | Some v' -> Spec_backw.D.join v v' in
            Hashtbl.modify_opt loc join by_loc;
            NodeH.modify_opt node join by_node;
          ) node_backw_values; 
        by_loc, by_node
      in

      (* NodeH.iter (fun node d -> 
         match node with 
         | Statement s -> (
          match s. with 
          | _ -> ()
         )
         | _ -> ()
         ) joined_by_node_backw; *)

      let make_global_fast_xml f g =
        let open Printf in
        let print_globals k v =
          fprintf f "\n<glob><key>%s</key>%a</glob>" (XmlUtil.escape (EQSys.GVar.show k)) EQSys.G.printXml v;
        in
        GHT.iter print_globals g
      in

      let liveness _ = true in

      let local_xml_forw = solver2source_result lh in

      ResBundle_forw.ResultOutput.output (lazy local_xml_forw) liveness gh make_global_fast_xml (module FileCfg); 
      (* ResBundle_backw.ResultOutput.output (lazy local_xml_backw) liveness gh make_global_fast_xml (module FileCfg) *)

      (*This is disgusting, but I have more imprtant things to do right now*)
      let output_wp_results_to_xml lh =
        (* iterate through all nodes and update corresponding .xml in result/nodes *)
        LHT.iter (fun v state ->
            match v with
            | `L_forw _ -> ()
            | `L_backw (node, c) -> (
                let state = match state with
                  | `Lifted2 d -> d
                  | _ -> failwith "Expected backward state"
                in
                try
                  let node_id_str = Node.show_id node in

                  let xml_path = Filename.concat "./result/nodes" (node_id_str ^ ".xml") in
                  if Sys.file_exists xml_path then (
                    (* Read existing XML *)
                    let ic = Stdlib.open_in xml_path in
                    let content = Stdlib.really_input_string ic (Stdlib.in_channel_length ic) in
                    Stdlib.close_in ic;

                    (* Create WP analysis data *)
                    let wp_res = Pretty.sprint 100 (Spec_backw.D.pretty () state) in
                    let wp_data =
                      "\n<wp_path>\n<analysis name=\"wp_test\">\n<value>\n<data>" ^ wp_res ^" \n</data>\n</value>\n</analysis>\n</wp_path>\n"
                    in

                    (* Insert before </path>*)
                    let close_pattern = "</call>" in
                    let updated_content =
                      try
                        let insert_pos = Str.search_backward (Str.regexp_string close_pattern) content (String.length content) in
                        let before = String.sub content 0 insert_pos in
                        let after = String.sub content insert_pos (String.length content - insert_pos) in
                        before ^ wp_data ^ after
                      with Not_found ->
                        content ^ wp_data
                    in

                    (* Write back *)
                    let oc = Stdlib.open_out xml_path in
                    Stdlib.output_string oc updated_content;
                    Stdlib.close_out oc;
                    Logs.debug "Updated XML file for node %s" node_id_str
                  )
                with _ -> ()  (* Skip errors silently *)
              )
          ) lh
      in

      output_wp_results_to_xml lh;
    in

    solve();
end

(** This function was originally a part of the [AnalyzeCFG] module, but
    now that [AnalyzeCFG] takes [Spec] as a functor parameter,
    [analyze_loop] cannot reside in it anymore since each invocation of
    [get_spec] in the loop might/should return a different module, and we
    cannot swap the functor parameter from inside [AnalyzeCFG]. *)
let rec analyze_loop (module CFG : CfgBidirSkip) file fs change_info =
  try

    let (module Spec) = get_spec () in

    if (GobConfig.get_bool "ana.wp_run") then (
      let module LivenesSpec = Wp_test.BackwSpec in
      let module A = AnalyzeCFG_bidir (CFG) (Spec) (LivenesSpec) (struct let increment = change_info end) in
      GobConfig.with_immutable_conf (fun () -> A.analyze file fs) 
    ) else (
      let module A = AnalyzeCFG (CFG) (Spec) (struct let increment = change_info end) in
      GobConfig.with_immutable_conf (fun () -> A.analyze file fs)
    )

  with Refinement.RestartAnalysis ->
    (* Tail-recursively restart the analysis again, when requested.
        All solving starts from scratch.
        Whoever raised the exception should've modified some global state
        to do a more precise analysis next time. *)
    (* TODO: do some more incremental refinement and reuse parts of solution *)
    analyze_loop (module CFG) file fs change_info

(** The main function to perform the selected analyses. *)
let analyze change_info (file: file) fs =
  Logs.debug "Generating the control flow graph.";
  let (module CFG) = CfgTools.compute_cfg file in
  MyCFG.current_cfg := (module CFG);
  analyze_loop (module CFG) file fs change_info
