(** An analyzer that takes the CFG from [MyCFG], a solver from [Selector], constraints from [Constraints] (using the specification from [MCP]) *)

open Prelude
open Cil
open Arinc_cfg
open Analyses_arinc
open GobConfig
open Constraints_arinc

module type S2S = functor (X : ArincSpec) -> ArincSpec
(* gets Spec for current options *)
let get_spec () : (module ArincSpecHC) =
  let open Batteries in
  (* apply functor F on module X if opt is true *)
  let lift opt (module F : S2S) (module X : ArincSpec) = (module (val if opt then (module F (X)) else (module X) : ArincSpec) : ArincSpec) in
  let module S1 = (val
            (module MCP_arinc.MCP2 : ArincSpec)
            |> lift (get_bool "exp.widen-context" && get_bool "exp.full-context") (module WidenContextLifter)
            |> lift (get_bool "exp.widen-context" && neg get_bool "exp.full-context") (module WidenContextLifterSide)
            |> lift (get_bool "ana.opt.hashcons") (module HashconsContextLifter)
            (* hashcons contexts before witness to reduce duplicates, because witness re-uses contexts in domain *)
            |> lift true (module PathSensitive2)
            |> lift true (module DeadCodeLifter)
            |> lift (get_bool "dbg.slice.on") (module LevelSliceLifter)
            |> lift (get_int "dbg.limit.widen" > 0) (module LimitLifter)
            |> lift (get_bool "ana.opt.equal" && not (get_bool "ana.opt.hashcons")) (module OptEqual)
          ) in
  (module (val if get_bool "ana.opt.hashcons" then (module HashconsLifter (S1)) else (module NoHashconsLifter (S1)) : ArincSpecHC))

(** Given a [Cfg], computes the solution to [MCP.Path] *)
module AnalyzeCFG (Cfg:CfgBidir) =
struct

    (** The main function to preform the selected analyses. *)
    let analyze (module Spec : ArincSpecHC) (increment: increment_data) =

    let module Inc = struct let increment = empty_increment_data () end in

    (* The Equation system *)
    let module EQSys = FromSpec (Spec) (Cfg) (Inc) in

    (* Hashtbl for locals *)
    let module LHT   = BatHashtbl.Make (EQSys.LVar) in
    (* Hashtbl for globals *)
    let module GHT   = BatHashtbl.Make (EQSys.GVar) in

    (* The solver *)
    let module Slvr  = Selector_arinc.Make (EQSys) (LHT) (GHT) in
    (* The verifyer *)
    let module Vrfyr = Verify2 (EQSys) (LHT) (GHT) in
    (* The comparator *)
    let module Comp = Compare (Spec) (EQSys) (LHT) (GHT) in

    (* Triple of the function, context, and the local value. *)
    let module RT = Analyses_arinc.ResultType2 (Spec) in
    (* Set of triples [RT] *)
    let module LT = SetDomain.HeadlessSet (RT) in
    (* Analysis result structure---a hashtable from program points to [LT] *)
    let module Result = Analyses_arinc.Result (LT) (struct let result_name = "analysis" end) in

    (* convert result that can be out-put *)
    let solver2source_result h : Result.t =
      (* processed result *)
      let res = Result.create 113 in

      (* Adding the state at each system variable to the final result *)
      let add_local_var ((n:arinc_node),es) state =
        try
            if Result.mem res n then
              (* If this source location has been added before, we look it up
               * and add another node to it information to it. *)
              let prev = Result.find res n in
              Result.replace res n (LT.add (es,state,MyCFG.dummy_func) prev)
            else
              Result.add res n (LT.singleton (es,state,MyCFG.dummy_func))
          (* If the function is not defined, and yet has been included to the
           * analysis result, we generate a warning. *)
        with Not_found ->
            Messages.warn ("Calculated state for undefined function: unexpected node ")
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


    (* analyze cil's global-inits function to get a starting state *)
    let do_global_inits : Spec.D.t =
      Spec.startstate MyCFG.dummy_func.svar
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

    let startstate =
      if (get_bool "dbg.verbose") then print_endline ("Initializing globals.");
      do_global_inits
    in

    let enter_with st fd =
      let st = st fd.svar in
      let ctx =
        { ask     = (fun _ -> Queries.Result.top ())
        ; node    = Arinc_cfg.PC [-1; -1]
        ; control_context = Obj.repr (fun () -> failwith "enter_func has no context.")
        ; context = (fun () -> failwith "enter_func has no context.")
        ; edge    = (-1, Arinc_cfg.NOP)
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

    let (_, s) = List.hd (enter_with (Spec.startstate) MyCFG.dummy_func) in
    let startvars = [[Arinc_cfg.PC [0;0], s]] in
    let exitvars = [] in (* This would only be used for cleanup functions and the like *)

    (* let othervars = List.map (enter_with Spec.otherstate) otherfuns in *)
    let startvars = List.concat (startvars @ exitvars) in

    let _ =
      if startvars = []
      then failwith "BUG: Empty set of start variables; may happen if \
                     enter_func of any analysis returns an empty list."
    in
    let _ = GU.earlyglobs := get_bool "exp.earlyglobs" in
    let _ = GU.global_initialization := false in

    let startvars' =
      if get_bool "exp.forward" then
        List.map (fun (n,e) -> (Arinc_cfg.PC [0;0], Spec.context e)) startvars
      else
        List.map (fun (n,e) -> (Arinc_cfg.PC [11;5], Spec.context e)) startvars
    in

    let entrystates =
      List.map (fun (n,e) -> (n, Spec.context e), e) startvars in


    let local_xml = ref (Result.create 0) in
    let global_xml = ref (GHT.create 0) in
    let lh_ref = ref (LHT.create 0) in
    let do_analyze_using_solver () =
      if get_bool "dbg.earlywarn" then Goblintutil.should_warn := false;
      let lh, gh = Stats.time "solving" (Slvr.solve entrystates []) startvars' in
      lh_ref := lh;


      if get_bool "verify" then begin
        if (get_bool "dbg.verbose") then print_endline "Verifying the result.";
        Goblintutil.should_warn := false;
        Vrfyr.verify lh gh;
      end;

      local_xml := solver2source_result lh;
      global_xml := gh;

      if get_bool "dump_globs" then
        print_globals gh;

    in

    (* Use "normal" constraint solving *)
    if (get_bool "dbg.verbose") then
      print_endline ("Solving the constraint system with " ^ get_string "solver" ^ ".");
    Goblintutil.timeout do_analyze_using_solver () (float_of_int (get_int "dbg.timeout"))
      (fun () -> Messages.waitWhat "Timeout reached!");

    Spec.finalize ();

    if (get_bool "dbg.verbose") then print_endline "Generating output.";
    Result.output (lazy !local_xml) !global_xml make_global_xml make_global_fast_xml


  let analyze change_info =
    analyze (get_spec ()) change_info
end

(** The main function to perform the selected analyses. *)
let analyze change_info =
  if (get_bool "dbg.verbose") then print_endline "Generating the control flow graph.";
  let cfgF, cfgB = Arinc_cfg.our_arinc_cfg in
  let module CFG = struct let prev = cfgB let next = cfgF end in
  let module A = AnalyzeCFG (CFG) in
  A.analyze change_info
