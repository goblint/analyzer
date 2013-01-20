open Cil
open MyCFG
open Pretty
open Analyses
open GobConfig
open Constraints
open Batteries_uni


module BaseSpec : Spec with module Glob.Var = Basetype.Variables = MCP.Path
module Spec = DeadCodeLifter (HashconsLifter (Spec2OfSpec (BaseSpec)))

module SystemFromCFG (Cfg:CfgBackward) =
struct

  module EQSys = FromSpec (Spec) (Cfg)
  module LHT   = BatHashtbl.Make (EQSys.LVar)
  module GHT   = BatHashtbl.Make (EQSys.GVar)
  module Slvr  = EffectWCon.Make2 (EQSys) (LHT) (GHT)
  
  module RT = Analyses.ResultType2 (Spec)
  module LT = SetDomain.HeadlessSet (RT)
  module RC = struct let result_name = "Analysis" end
  module Result = Analyses.Result (LT) (RC)
      
  
  (** convert result that can be out-put *)
  let solver2source_result h : Result.t =
    (* processed result *)
    let res = Result.create 113 in
      
    (* Adding the state at each system variable to the final result *)
    let add_local_var (n,es) state =
      let loc = MyCFG.getLoc n in
      if loc <> Cil.locUnknown then try 
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
      with Not_found -> Messages.warn ("Undefined function has escaped.")
    in
      LHT.iter add_local_var h;
      res
      
  (** exctract global xml from result *)
  let make_global_xml g =
    let one_glob k v = 
      let k = Xml.PCData k.Cil.vname in
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
  
end

(** add extern variables to local state *)
let do_extern_inits ctx (file : Cil.file) : Spec.D.t =
  let module VS = Set.Make (Basetype.Variables) in    
  let add_glob s = function
      GVar (v,_,_) -> VS.add v s
    | _            -> s
  in
  let vars = Cil.foldGlobals file add_glob VS.empty in
  let set_bad v st =
    Spec.assign {ctx with local2 = st} (var v) MyCFG.unknown_exp 
  in
  let add_externs s = function
    | GVarDecl (v,_) when not (VS.mem v vars || isFunctionType v.vtype) -> set_bad v s
    | _ -> s
  in    
  Cil.foldGlobals file add_externs (Spec.startstate ())

(** analyze cil's global-inits function to get a starting state *)
let do_global_inits (file: Cil.file) : Spec.D.t * Cil.fundec list = 
  let ctx = 
    { ask2     = (fun _ -> Queries.Result.top ())
    ; local2   = Spec.D.top ()
    ; global2  = (fun _ -> Spec.G.bot ())
    ; presub2  = []
    ; postsub2 = []
    ; spawn2   = (fun _ -> failwith "Global initializers should never spawn threads. What is going on?") 
    ; split2   = (fun _ -> failwith "Global initializers trying to split paths.")
    ; sideg2   = (fun _ -> failwith "Global initializers trying to side-effect globals.")
    } 
  in  
  let edges = MyCFG.getGlobalInits file in
  let funs = ref [] in
  let transfer_func (st : Spec.D.t) (edge, loc) : Spec.D.t = 
    try
      if M.tracing then M.trace "con" "Initializer %a\n" d_loc loc;
      Tracing.current_loc := loc;
      match edge with
        | MyCFG.Entry func        -> Spec.body {ctx with local2 = st} func
        | MyCFG.Assign (lval,exp) -> 
            begin match lval, exp with
              | (Var v,o), (Cil.AddrOf (Cil.Var f,Cil.NoOffset)) 
                when v.Cil.vstorage <> Static && isFunctionType f.vtype -> 
                begin try funs := Cilfacade.getdec f :: !funs with Not_found -> () end 
              | _ -> ()
            end;
            Spec.assign {ctx with local2 = st} lval exp
        | _                       -> raise (Failure "This iz impossible!") 
    with Failure x -> M.warn x; st
  in
  let with_externs = do_extern_inits ctx file in
  let result : Spec.D.t = List.fold_left transfer_func with_externs edges in
    result, !funs

let analyze (file: Cil.file) (startfuns, exitfuns, otherfuns: Analyses.fundecs) = 
  let early = (get_bool "exp.earlyglobs") in
  let _ = GU.global_initialization := true in
  let _ = set_bool "exp.earlyglobs" false in
  Spec.init ();
    
  if (get_bool "dbg.verbose") then print_endline "Generating constraints."; 
  let cfg = MyCFG.getCFG file true in
  let module CFG = struct let prev = cfg end in
  let module SystemDeclarations = SystemFromCFG (CFG) in 
  let open SystemDeclarations in
  
	let startstate, more_funs = 
    if (get_bool "dbg.verbose") then print_endline "Initializing globals.";
    Stats.time "initializers" do_global_inits file 
  in
    
  let otherfuns = if get_bool "kernel" then otherfuns @ more_funs else otherfuns in
  
  let enter_with st fd =
    let ctx = 
      { ask2     = (fun _ -> Queries.Result.top ())
      ; local2   = st
      ; global2  = (fun _ -> Spec.G.bot ())
      ; presub2  = []
      ; postsub2 = []
      ; spawn2   = (fun _ -> failwith "Bug1: Using enter_func for toplevel functions with 'otherstate'.") 
      ; split2   = (fun _ -> failwith "Bug2: Using enter_func for toplevel functions with 'otherstate'.")
      ; sideg2   = (fun _ -> failwith "Bug3: Using enter_func for toplevel functions with 'otherstate'.")
      } 
    in
    let args = List.map (fun x -> MyCFG.unknown_exp) fd.Cil.sformals in
    let ents = Spec.enter ctx None fd.Cil.svar args in
      List.map (fun (_,s) -> fd.Cil.svar, s) ents  
  in
  
  let _ = try MyCFG.dummy_func.Cil.svar.Cil.vdecl <- (List.hd otherfuns).Cil.svar.Cil.vdecl with Failure _ -> () in
  
  let startvars = 
    if startfuns = [] 
    then [[MyCFG.dummy_func.Cil.svar, startstate]]
    else List.map (enter_with startstate) startfuns 
  in
   
  let exitvars = List.map (enter_with (Spec.exitstate ())) exitfuns in
  let othervars = List.map (enter_with (Spec.otherstate ())) otherfuns in
  let startvars = List.concat (startvars @ exitvars @ othervars) in
  
  let _ = 
  if startvars = [] 
    then failwith "BUG: Empty set of start variables; may happen if\
       enter_func of any analysis returns an empty list." 
  in
  let _ = set_bool "exp.earlyglobs" early in
  let _ = GU.global_initialization := false in
  
  (*let context_fn f = if get_bool "exp.full-context" then fun x->x else Spec.context in*)
  
  let startvars' = 
    List.map (fun (n,e) -> (MyCFG.Function n, Spec.context e)) startvars in
  
  let entrystates = 
    List.map (fun (n,e) -> (MyCFG.FunctionEntry n, Spec.context e), e) startvars in
  
    
  let local_xml = ref (Result.create 0) in
  let global_xml = ref (Xml.PCData "not-ready" ) in
  let do_analyze () = 
    let lh, gh = Slvr.solve entrystates [] startvars' in
    local_xml := solver2source_result lh;
    global_xml := make_global_xml gh;
    (* check for dead code at the last state: *)
    let main_sol = LHT.find lh (List.hd startvars') in
    (if (get_bool "dbg.debug") && Spec.D.is_bot main_sol then
      Printf.printf "NB! Execution does not reach the end of Main.\n");
  in
  
  if (get_bool "dbg.verbose") then print_endline "Solving the constraint system.";
  Goblintutil.timeout do_analyze () (float_of_int (get_int "dbg.timeout"))
    (fun () -> Messages.waitWhat "Timeout reached!");
  
  Spec.finalize ();
    
  if (get_bool "dbg.verbose") then print_endline "Generating output.";
  Result.output (lazy !local_xml) (lazy (!global_xml :: [])) file;
  
  
  
  
    
