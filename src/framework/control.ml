(** An analyzer that takes the CFG from [MyCFG], a solver from [Selector], constraints from [Constraints] (using the specification from [MCP]) *)

open Prelude
open Cil
open MyCFG
open Analyses
open GobConfig
open Constraints

module type S2S = functor (X : Spec) -> Spec
(* gets Spec for current options *)
let get_spec () : (module SpecHC) =
  let open Batteries in
  (* apply functor F on module X if opt is true *)
  let lift opt (module F : S2S) (module X : Spec) = (module (val if opt then (module F (X)) else (module X) : Spec) : Spec) in
  let module S1 = (val
            (module MCP.MCP2 : Spec)
            |> lift (get_bool "exp.widen-context" && get_bool "exp.full-context") (module WidenContextLifter)
            |> lift (get_bool "exp.widen-context" && neg get_bool "exp.full-context") (module WidenContextLifterSide)
            |> lift (get_bool "ana.opt.hashcons") (module HashconsContextLifter)
            (* hashcons contexts before witness to reduce duplicates, because witness re-uses contexts in domain *)
            (* |> lift (get_bool "ana.sv-comp") (module WitnessConstraints.WitnessLifter) *)
            |> lift (get_bool "ana.sv-comp") (module WitnessConstraints.PathSensitive3)
            |> lift (not (get_bool "ana.sv-comp")) (module PathSensitive2)
            |> lift true (module DeadCodeLifter)
            |> lift (get_bool "dbg.slice.on") (module LevelSliceLifter)
            |> lift (get_int "dbg.limit.widen" > 0) (module LimitLifter)
            |> lift (get_bool "ana.opt.equal" && not (get_bool "ana.opt.hashcons")) (module OptEqual)
          ) in
  (module (val if get_bool "ana.opt.hashcons" then (module HashconsLifter (S1)) else (module NoHashconsLifter (S1)) : SpecHC))

(** Given a [Cfg], computes the solution to [MCP.Path] *)
module AnalyzeCFG (Cfg:CfgBidir) =
struct

    (** The main function to preform the selected analyses. *)
    let analyze (file: file) (startfuns, exitfuns, otherfuns: Analyses.fundecs)  (module Spec : SpecHC) (increment: increment_data) (rerun: bool ref) =

    let module Inc = struct let increment = increment end in

    (* The Equation system *)
    let module EQSys = FromSpec (Spec) (Cfg) (Inc) in

    (* Hashtbl for locals *)
    let module LHT   = BatHashtbl.Make (EQSys.LVar) in
    (* Hashtbl for globals *)
    let module GHT   = BatHashtbl.Make (EQSys.GVar) in

    (* The solver *)
    let module Slvr  = Selector.Make (EQSys) (LHT) (GHT) in
    (* The verifyer *)
    let module Vrfyr = Verify2 (EQSys) (LHT) (GHT) in
    (* The comparator *)
    let module Comp = Compare (Spec) (EQSys) (LHT) (GHT) in

    (* Triple of the function, context, and the local value. *)
    let module RT = Analyses.ResultType2 (Spec) in
    (* Set of triples [RT] *)
    let module LT = SetDomain.HeadlessSet (RT) in
    (* Analysis result structure---a hashtable from program points to [LT] *)
    let module Result = Analyses.Result (LT) (struct let result_name = "analysis" end) in

    (* print out information about dead code *)
    let print_dead_code (xs:Result.t) =
      let dead_locations : unit Deadcode.Locmap.t = Deadcode.Locmap.create 10 in
      let module NH = Hashtbl.Make (MyCFG.Node) in
      let live_nodes : unit NH.t = NH.create 10 in
      let count = ref 0 in
      let module StringMap = BatMap.Make (String) in
      let open BatPrintf in
      let live_lines = ref StringMap.empty in
      let dead_lines = ref StringMap.empty in
      let add_one (l,n,f) v =
        let add_fun  = BatISet.add l.line in
        let add_file = StringMap.modify_def BatISet.empty f.svar.vname add_fun in
        let is_dead = LT.for_all (fun (_,x,f) -> Spec.D.is_bot x) v in
        if is_dead then begin
          dead_lines := StringMap.modify_def StringMap.empty l.file add_file !dead_lines;
          Deadcode.Locmap.add dead_locations l ();
        end else begin
          live_lines := StringMap.modify_def StringMap.empty l.file add_file !live_lines;
          NH.add live_nodes n ()
        end;
      in
      Result.iter add_one xs;
      let live file fn =
        try StringMap.find fn (StringMap.find file !live_lines)
        with Not_found -> BatISet.empty
      in
      dead_lines := StringMap.mapi (fun fi -> StringMap.mapi (fun fu ded -> BatISet.diff ded (live fi fu))) !dead_lines;
      dead_lines := StringMap.map (StringMap.filter (fun _ x -> not (BatISet.is_empty x))) !dead_lines;
      dead_lines := StringMap.filter (fun _ x -> not (StringMap.is_empty x)) !dead_lines;
      let print_func f xs =
        let one_range b e first =
          count := !count + (e - b + 1);
          if not first then printf ", ";
          begin if b=e then
              printf "%d" b
            else
              printf "%d..%d" b e
          end; false
        in
        printf "  function '%s' has dead code on lines: " f;
        ignore (BatISet.fold_range one_range xs true);
        printf "\n"
      in
      let print_file f =
        printf "File '%s':\n" f;
        StringMap.iter print_func
      in
      if get_bool "dbg.print_dead_code" then begin
        if StringMap.is_empty !dead_lines
        then printf "No dead code found!\n"
        else begin
          StringMap.iter print_file !dead_lines;
          printf "Found dead code on %d line%s!\n" !count (if !count>1 then "s" else "")
        end
      end;
      let str = function true -> "then" | false -> "else" in
      let report tv loc dead =
        if Deadcode.Locmap.mem dead_locations loc then
          match dead, Deadcode.Locmap.find_option Deadcode.dead_branches_cond loc with
          | true, Some exp -> ignore (Pretty.printf "Dead code: the %s branch over expression '%a' is dead! (%a)\n" (str tv) d_exp exp d_loc loc)
          | true, None     -> ignore (Pretty.printf "Dead code: an %s branch is dead! (%a)\n" (str tv) d_loc loc)
          | _ -> ()
      in
      if get_bool "dbg.print_dead_code" then begin
        Deadcode.Locmap.iter (report true)  Deadcode.dead_branches_then;
        Deadcode.Locmap.iter (report false) Deadcode.dead_branches_else;
        Deadcode.Locmap.clear Deadcode.dead_branches_then;
        Deadcode.Locmap.clear Deadcode.dead_branches_else
      end;
      NH.mem live_nodes
    in

    (* convert result that can be out-put *)
    let solver2source_result h : Result.t =
      (* processed result *)
      let res = Result.create 113 in

      (* Adding the state at each system variable to the final result *)
      let add_local_var (n,es) state =
        let loc = Tracing.getLoc n in
        if loc <> locUnknown then try
            let (_,_, fundec) as p = loc, n, MyCFG.getFun n in
            if Result.mem res p then
              (* If this source location has been added before, we look it up
               * and add another node to it information to it. *)
              let prev = Result.find res p in
              Result.replace res p (LT.add (es,state,fundec) prev)
            else
              Result.add res p (LT.singleton (es,state,fundec))
          (* If the function is not defined, and yet has been included to the
           * analysis result, we generate a warning. *)
          with Not_found ->
            Messages.warn ("Calculated state for undefined function: unexpected node "^Ana.sprint MyCFG.pretty_node n)
      in
      LHT.iter add_local_var h;
      res
    in

    (* exctract global xml from result *)
    let make_global_xml g =
      let one_glob k v =
        let k = Xml.PCData k.vname in
        let varname = Xml.Element ("td",[],[k]) in
        let varvalue = Xml.Element ("td",[],[Spec.G.toXML v]) in
        Xml.Element ("tr",[],[varname; varvalue])
      in
      let head =
        Xml.Element ("tr",[],[Xml.Element ("th",[],[Xml.PCData "var"])
                             ;Xml.Element ("th",[],[Xml.PCData "value"])])
      in
      let collect_globals k v b = one_glob k v :: b in
      Xml.Element ("table", [], head :: GHT.fold collect_globals g [])
    in
    (* exctract global xml from result *)
    let make_global_fast_xml f g =
      let open Printf in
      let print_globals k v =
        fprintf f "\n<glob><key>%s</key>%a</glob>" (Goblintutil.escape (Basetype.Variables.short 800 k)) Spec.G.printXml v;
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
      let add_externs s = function
        | GVarDecl (v,_) when not (VS.mem v vars || isFunctionType v.vtype) -> set_bad v s
        | _ -> s
      in
      foldGlobals file add_externs (Spec.startstate MyCFG.dummy_func.svar)
    in

    (* analyze cil's global-inits function to get a starting state *)
    let do_global_inits (file: file) : Spec.D.t * fundec list =
      let ctx =
        { ask     = (fun _ -> Queries.Result.top ())
        ; node    = MyCFG.dummy_node
        ; prev_node = MyCFG.dummy_node
        ; control_context = Obj.repr (fun () -> ctx_failwith "Global initializers have no context.")
        ; context = (fun () -> ctx_failwith "Global initializers have no context.")
        ; edge    = MyCFG.Skip
        ; local   = Spec.D.top ()
        ; global  = (fun _ -> Spec.G.bot ())
        ; presub  = []
        ; postsub = []
        ; spawn   = (fun _ -> failwith "Global initializers should never spawn threads. What is going on?")
        ; split   = (fun _ -> failwith "Global initializers trying to split paths.")
        ; sideg   = (fun _ -> failwith "Global initializers trying to side-effect globals.")
        ; assign  = (fun ?name _ -> failwith "Global initializers trying to assign.")
        }
      in
      let edges = MyCFG.getGlobalInits file in
      if (get_bool "dbg.verbose") then print_endline ("Executing "^string_of_int (List.length edges)^" assigns.");
      let funs = ref [] in
      (*let count = ref 0 in*)
      let transfer_func (st : Spec.D.t) (edge, loc) : Spec.D.t =
        try
          if M.tracing then M.trace "con" "Initializer %a\n" d_loc loc;
          (*incr count;
            if (get_bool "dbg.verbose")&& (!count mod 1000 = 0)  then Printf.printf "%d %!" !count;    *)
          Tracing.current_loc := loc;
          match edge with
          | MyCFG.Entry func        ->
            if M.tracing then M.trace "global_inits" "Entry %a\n" d_lval (var func.svar);
            Spec.body {ctx with local = st} func
          | MyCFG.Assign (lval,exp) ->
            if M.tracing then M.trace "global_inits" "Assign %a = %a\n" d_lval lval d_exp exp;
            begin match lval, exp with
              | (Var v,o), (AddrOf (Var f,NoOffset))
                when v.vstorage <> Static && isFunctionType f.vtype ->
                begin try funs := Cilfacade.getdec f :: !funs with Not_found -> () end
              | _ -> ()
            end;
            Spec.assign {ctx with local = st} lval exp
          | _                       -> raise (Failure "This iz impossible!")
        with Failure x -> M.warn x; st
      in
      let with_externs = do_extern_inits ctx file in
      (*if (get_bool "dbg.verbose") then Printf.printf "Number of init. edges : %d\nWorking:" (List.length edges);    *)
      let result : Spec.D.t = List.fold_left transfer_func with_externs edges in
      result, !funs
    in

    let print_globals glob =
      let out = M.get_out (Spec.name ()) !GU.out in
      let print_one v st =
        ignore (Pretty.fprintf out "%a -> %a\n" EQSys.GVar.pretty_trace v Spec.G.pretty st)
      in
      GHT.iter print_one glob
    in

    (* real beginning of the [analyze] function *)
    let _ = GU.global_initialization := true in
    let _ = GU.earlyglobs := false in
    Spec.init ();
    Access.init file;

    let startstate, more_funs =
      if (get_bool "dbg.verbose") then print_endline ("Initializing "^string_of_int (MyCFG.numGlobals file)^" globals.");
      do_global_inits file
    in

    let otherfuns = if get_bool "kernel" then otherfuns @ more_funs else otherfuns in

    let enter_with st fd =
      let st = st fd.svar in
      let ctx =
        { ask     = (fun _ -> Queries.Result.top ())
        ; node    = MyCFG.dummy_node
        ; prev_node = MyCFG.dummy_node
        ; control_context = Obj.repr (fun () -> ctx_failwith "enter_func has no context.")
        ; context = (fun () -> ctx_failwith "enter_func has no context.")
        ; edge    = MyCFG.Skip
        ; local   = st
        ; global  = (fun _ -> Spec.G.bot ())
        ; presub  = []
        ; postsub = []
        ; spawn   = (fun _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.")
        ; split   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
        ; sideg   = (fun _ -> failwith "Bug3: Using enter_func for toplevel functions with 'otherstate'.")
        ; assign  = (fun ?name _ -> failwith "Bug4: Using enter_func for toplevel functions with 'otherstate'.")
        }
      in
      let args = List.map (fun x -> MyCFG.unknown_exp) fd.sformals in
      let ents = Spec.enter ctx None fd.svar args in
      List.map (fun (_,s) -> fd.svar, s) ents
    in

    let _ = try MyCFG.dummy_func.svar.vdecl <- (List.hd otherfuns).svar.vdecl with Failure _ -> () in

    let startvars =
      if startfuns = []
      then [[MyCFG.dummy_func.svar, startstate]]
      else
        let morph f = Spec.morphstate f startstate in
        List.map (enter_with morph) startfuns
    in

    let exitvars = List.map (enter_with Spec.exitstate) exitfuns in
    let othervars = List.map (enter_with Spec.otherstate) otherfuns in
    let startvars = List.concat (startvars @ exitvars @ othervars) in

    let _ =
      if startvars = []
      then failwith "BUG: Empty set of start variables; may happen if \
                     enter_func of any analysis returns an empty list."
    in
    let _ = GU.earlyglobs := get_bool "exp.earlyglobs" in
    let _ = GU.global_initialization := false in

    let startvars' =
      if get_bool "exp.forward" then
        List.map (fun (n,e) -> (MyCFG.FunctionEntry n, Spec.context e)) startvars
      else
        List.map (fun (n,e) -> (MyCFG.Function n, Spec.context e)) startvars
    in

    let entrystates =
      List.map (fun (n,e) -> (MyCFG.FunctionEntry n, Spec.context e), e) startvars in


    let module Task =
    struct
      let file = file
      let specification = Svcomp.unreach_call_specification

      module Cfg = Cfg
    end
    in

    let local_xml = ref (Result.create 0) in
    let global_xml = ref (GHT.create 0) in
    let lh_ref = ref (LHT.create 0) in
    let do_analyze_using_solver () =
      if get_bool "dbg.earlywarn" then Goblintutil.may_narrow := false;
      let lh, gh = Stats.time "solving" (Slvr.solve entrystates []) startvars' in
      lh_ref := lh;

      if not (get_string "comparesolver"="") then begin
        let compare_with (module S2 :  GenericGlobSolver) =
          let module S2' = S2 (EQSys) (LHT) (GHT) in
          let r2 = S2'.solve entrystates [] startvars' in
          Comp.compare (lh,gh) (r2)
        in
        compare_with (Slvr.choose_solver (get_string "comparesolver"))
      end;

      if not (get_bool "noverify") then begin
        if (get_bool "dbg.verbose") then print_endline "Verifying the result.";
        Goblintutil.may_narrow := false;
        Vrfyr.verify lh gh;
      end;

      if get_bool "ana.sv-comp" then begin
        (* prune already here so local_xml and thus HTML are also pruned *)
        let module Reach = Reachability (EQSys) (LHT) (GHT) in
        Reach.prune !lh_ref !global_xml startvars'
      end;

      local_xml := solver2source_result lh;
      global_xml := gh;

      if get_bool "dbg.uncalled" then
        begin
          let out = M.get_out "uncalled" Legacy.stdout in
          let insrt k _ s = match k with
            | (MyCFG.Function fn,_) -> if not (get_bool "exp.forward") then Set.Int.add fn.vid s else s
            | (MyCFG.FunctionEntry fn,_) -> if (get_bool "exp.forward") then Set.Int.add fn.vid s else s
            | _ -> s
          in
          (* set of ids of called functions *)
          let calledFuns = LHT.fold insrt lh Set.Int.empty in
          let is_bad_uncalled fn loc =
            not (Set.Int.mem fn.vid calledFuns) &&
            not (Str.last_chars loc.file 2 = ".h") &&
            not (LibraryFunctions.is_safe_uncalled fn.vname)
          in
          let print_uncalled = function
            | GFun (fn, loc) when is_bad_uncalled fn.svar loc->
              begin
                let msg = "Function \"" ^ fn.svar.vname ^ "\" will never be called." in
                ignore (Pretty.fprintf out "%s (%a)\n" msg Basetype.ProgLines.pretty loc)
              end
            | _ -> ()
          in
          List.iter print_uncalled file.globals
        end;

      (* check for dead code at the last state: *)
      let main_sol = try LHT.find lh (List.hd startvars') with Not_found -> Spec.D.bot () in
      (if (get_bool "dbg.debug") && Spec.D.is_bot main_sol then
         Printf.printf "NB! Execution does not reach the end of Main.\n");

      if get_bool "dump_globs" then
        print_globals gh;

      (* run activated transformations with the analysis result *)
      let ask loc =
        let open Batteries in let open Enum in
        (* first join all contexts *)
        let joined =
          LHT.enum lh |> map (Tuple2.map1 fst) (* drop context from key *)
          |> group fst (* group by key=node *)
          |> map (reduce (fun (k,a) (_,b) -> k, Spec.D.join a b))
          (* also, in cil visitors we only have the location, so we use that as the key *)
          |> map (Tuple2.map1 MyCFG.getLoc)
          |> Hashtbl.of_enum
        in
        (* build a ctx for using the query system *)
        let rec ctx =
          { ask    = query
          ; node   = MyCFG.dummy_node (* TODO maybe ask should take a node (which could be used here) instead of a location *)
          ; prev_node = MyCFG.dummy_node
          ; control_context = Obj.repr (fun () -> ctx_failwith "No context in query context.")
          ; context = (fun () -> ctx_failwith "No context in query context.")
          ; edge    = MyCFG.Skip
          ; local  = Hashtbl.find joined loc
          ; global = GHT.find gh
          ; presub = []
          ; postsub= []
          ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in query context.")
          ; split  = (fun d e tv -> failwith "Cannot \"split\" in query context.")
          ; sideg  = (fun v g    -> failwith "Cannot \"split\" in query context.")
          ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in query context.")
          }
        and query x = Spec.query ctx x in
        Spec.query ctx
      in
      get_list "trans.activated" |> List.map Json.string
      |> List.iter (fun name -> Transform.run name ask file)
      (* Transform.PartialEval.transform ask file *)
    in

    MyCFG.write_cfgs := MyCFG.dead_code_cfg file (module Cfg:CfgBidir);

    (* Use "normal" constraint solving *)
    if (get_bool "dbg.verbose") then
      print_endline ("Solving the constraint system with " ^ get_string "solver" ^ ".");
    Goblintutil.timeout do_analyze_using_solver () (float_of_int (get_int "dbg.timeout"))
      (fun () -> Messages.waitWhat "Timeout reached!");

    let liveness = ref (fun _ -> true) in
    if (get_bool "dbg.print_dead_code" || get_bool "ana.sv-comp") then
      liveness := print_dead_code !local_xml;

    if get_bool "ana.sv-comp" then begin
      let svcomp_unreach_call =
        let dead_verifier_error (l, n, f) v acc =
          match n with
          (* FunctionEntry isn't used for extern __VERIFIER_error... *)
          | FunctionEntry f when Svcomp.is_error_function f ->
            let is_dead = not (!liveness n) in
            acc && is_dead
          | _ -> acc
        in
        Result.fold dead_verifier_error !local_xml true
      in
      Printf.printf "SV-COMP (unreach-call): %B\n" svcomp_unreach_call;

      let get: node * Spec.C.t -> Spec.D.t =
        fun nc -> LHT.find_default !lh_ref nc (Spec.D.bot ())
      in
      let ask_local (lvar:EQSys.LVar.t) local =
        (* build a ctx for using the query system *)
        let rec ctx =
          { ask    = query
          ; node   = fst lvar
          ; prev_node = MyCFG.dummy_node
          ; control_context = Obj.repr (fun () -> snd lvar)
          ; context = (fun () -> snd lvar)
          ; edge    = MyCFG.Skip
          ; local  = local
          ; global = GHT.find !global_xml
          ; presub = []
          ; postsub= []
          ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in witness context.")
          ; split  = (fun d e tv -> failwith "Cannot \"split\" in witness context.")
          ; sideg  = (fun v g    -> failwith "Cannot \"split\" in witness context.")
          ; assign = (fun ?name _ -> failwith "Cannot \"assign\" in witness context.")
          }
        and query x = Spec.query ctx x in
        Spec.query ctx
      in
      let ask_indices lvar =
        let local = get lvar in
        let indices = ref [] in
        ignore (ask_local lvar local (Queries.IterVars (fun i ->
            indices := i :: !indices
          )));
        !indices
      in

      let module Node =
      struct
        type t = MyCFG.node * Spec.C.t * int

        let equal (n1, c1, i1) (n2, c2, i2) =
          EQSys.LVar.equal (n1, c1) (n2, c2) && i1 = i2

        let hash (n, c, i) = 31 * EQSys.LVar.hash (n, c) + i

        let cfgnode (n, c, i) = n

        let to_string (n, c, i) =
          (* copied from NodeCtxStackGraphMlWriter *)
          let c_tag = Spec.C.tag c in
          let i_str = string_of_int i in
          match n with
          | Statement stmt  -> Printf.sprintf "s%d(%d)[%s]" stmt.sid c_tag i_str
          | Function f      -> Printf.sprintf "ret%d%s(%d)[%s]" f.vid f.vname c_tag i_str
          | FunctionEntry f -> Printf.sprintf "fun%d%s(%d)[%s]" f.vid f.vname c_tag i_str

        (* TODO: less hacky way (without ask_indices) to move node *)
        let is_live (n, c, i) = not (Spec.D.is_bot (get (n, c)))
        let move_opt (n, c, i) to_n =
          match ask_indices (to_n, c) with
          | [] -> None
          | [to_i] ->
            let to_node = (to_n, c, to_i) in
            Option.filter is_live (Some to_node)
          | _ :: _ :: _ ->
            failwith "Node.move_opt: ambiguous moved index"
        let equal_node_context (n1, c1, i1) (n2, c2, i2) =
          EQSys.LVar.equal (n1, c1) (n2, c2)
      end
      in

      let module NHT = BatHashtbl.Make (Node) in

      let (witness_prev_map, witness_prev, witness_next) =
        let prev = NHT.create 100 in
        let next = NHT.create 100 in
        LHT.iter (fun lvar local ->
            ignore (ask_local lvar local (Queries.IterPrevVars (fun i (prev_node, prev_c_obj, j) edge ->
                let lvar' = (fst lvar, snd lvar, i) in
                let prev_lvar: NHT.key = (prev_node, Obj.obj prev_c_obj, j) in
                NHT.modify_def [] lvar' (fun prevs -> (edge, prev_lvar) :: prevs) prev;
                NHT.modify_def [] prev_lvar (fun nexts -> (edge, lvar') :: nexts) next
              )))
          ) !lh_ref;

        (prev,
         (fun n ->
            NHT.find_default prev n []), (* main entry is not in prev at all *)
         (fun n ->
            NHT.find_default next n [])) (* main return is not in next at all *)
      in
      let witness_main =
        let lvar = WitnessUtil.find_main_entry entrystates in
        let main_indices = ask_indices lvar in
        (* TODO: get rid of this hack for getting index of entry state *)
        assert (List.length main_indices = 1);
        let main_index = List.hd main_indices in
        (fst lvar, snd lvar, main_index)
      in

      let module Arg =
      struct
        module Node = Node
        module Edge = MyARG.InlineEdge
        let main_entry = witness_main
        let next = witness_next
      end
      in
      let module Arg =
      struct
        open MyARG
        module ArgIntra = UnCilTernaryIntra (UnCilLogicIntra (CfgIntra (Cfg)))
        include Intra (ArgIntra) (Arg)
      end
      in

      let find_invariant (n, c, i) = Spec.D.invariant {i; varinfo=None} (get (n, c)) in

      let witness_path = get_string "exp.witness_path" in
      if svcomp_unreach_call then begin
        let module TaskResult =
        struct
          module Arg = Arg
          let result = true
          let invariant = find_invariant
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        Witness.write_file witness_path (module Task) (module TaskResult)
      end else begin
        let is_violation = function
          | FunctionEntry f, _, _ when Svcomp.is_error_function f -> true
          | _, _, _ -> false
        in
        (* redefine is_violation to shift violations back by one, so enterFunction __VERIFIER_error is never used *)
        let is_violation n =
          Arg.next n
          |> List.exists (fun (_, to_n) -> is_violation to_n)
        in
        let violations =
          NHT.fold (fun lvar _ acc ->
              if is_violation lvar then
                lvar :: acc
              else
                acc
            ) witness_prev_map []
        in
        let module ViolationArg =
        struct
          include Arg

          let prev = witness_prev
          let violations = violations
        end
        in
        let write_violation_witness = ref true in
        if get_bool "ana.wp" then begin
          match Violation.find_path (module ViolationArg) with
          | Feasible (module PathArg) ->
            (* TODO: add assumptions *)
            let module TaskResult =
            struct
              module Arg = PathArg
              let result = false
              let invariant _ = Invariant.none
              let is_violation = is_violation
              let is_sink _ = false
            end
            in
            Witness.write_file witness_path (module Task) (module TaskResult);
            write_violation_witness := false
          | Infeasible ->
            rerun := true
          | Unknown -> ()
        end;
        if !write_violation_witness then begin
          (* TODO: exclude sinks before find_path? *)
          let is_sink = Violation.find_sinks (module ViolationArg) in
          let module TaskResult =
          struct
            module Arg = Arg
            let result = false
            let invariant _ = Invariant.none
            let is_violation = is_violation
            let is_sink = is_sink
          end
          in
          Witness.write_file witness_path (module Task) (module TaskResult)
        end
      end
    end;

    if (get_bool "exp.cfgdot") then
      MyCFG.dead_code_cfg file (module Cfg:CfgBidir) !liveness;

    Spec.finalize ();

    if (get_bool "dbg.verbose") then print_endline "Generating output.";
    Result.output (lazy !local_xml) !global_xml make_global_xml make_global_fast_xml file


  let analyze file fs change_info rerun =
    analyze file fs (get_spec ()) change_info rerun

  let rec analyze_loop file fs change_info =
    let rerun = ref false in
    analyze file fs change_info rerun;
    if !rerun then
      analyze_loop file fs change_info
end

(** The main function to perform the selected analyses. *)
let analyze change_info (file: file) fs =
  if (get_bool "dbg.verbose") then print_endline "Generating the control flow graph.";
  let cfgF, cfgB = MyCFG.getCFG file in
  let cfgB' = function
    | MyCFG.Statement s as n -> ([get_stmtLoc s.skind,MyCFG.SelfLoop], n) :: cfgB n
    | n -> cfgB n
  in
  let cfgB = if (get_bool "ana.osek.intrpts") then cfgB' else cfgB in
  let module CFG = struct let prev = cfgB let next = cfgF end in
  let module A = AnalyzeCFG (CFG) in
  A.analyze_loop file fs change_info
