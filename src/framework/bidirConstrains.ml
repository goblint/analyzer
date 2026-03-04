open Batteries
open GoblintCil
open MyCFG
open Analyses
open BackwAnalyses
open Goblint_constraint.ConstrSys
open GobConfig

module type Increment =
sig
  val increment: increment_data option
end

module GVarF2 (V_forw: SpecSysVar) (V_backw : SpecSysVar) : 
sig
  module GV_forw : module type of GVarF (V_forw)
  module GV_backw : module type of GVarF (V_backw)  
  include VarType with type t = [ `Forw of GV_forw.t | `Backw of GV_backw.t ]
  include SpecSysVar with type t := t
  val spec : [ `Forw of V_forw.t | `Backw of V_backw.t ] -> [ `Forw of [`Left of V_forw.t] | `Backw of [`Left of V_backw.t] ]
  val contexts : [ `Forw of V_forw.t | `Backw of V_backw.t ] -> [`Forw of [`Right of V_forw.t] | `Backw of  [`Right of V_backw.t]]
end
=
struct
  module GV_forw = GVarF (V_forw)
  module GV_backw = GVarF (V_backw)  
  type t = [ `Forw of GV_forw.t | `Backw of GV_backw.t ] [@@deriving eq, ord, hash]
  let name () = "BidirFromSpec"

  let tag _ = failwith "Std: no tag"

  let relift = function
    | `Forw x -> `Forw (GV_forw.relift x)
    | `Backw x -> `Backw (GV_backw.relift x)

  let pretty_trace () = function
    | `Forw a -> GoblintCil.Pretty.dprintf "G_forw:%a" GV_forw.pretty_trace a
    | `Backw a -> GoblintCil.Pretty.dprintf "G_backw:%a" GV_backw.pretty_trace a

  let printXml f = function
    | `Forw a -> GV_forw.printXml f a
    | `Backw a -> GV_backw.printXml f a

  let node = function
    | `Forw a -> GV_forw.node a
    | `Backw a -> GV_backw.node a

  let is_write_only = function
    | `Forw a -> GV_forw.is_write_only a
    | `Backw a -> GV_backw.is_write_only a

  let show = function
    | `Forw a -> GV_forw.show a
    | `Backw a -> GV_backw.show a

  let pretty () = function
    | `Forw a -> GV_forw.pretty () a
    | `Backw a -> GV_backw.pretty () a
  let to_yojson = function
    | `Forw a -> GV_forw.to_yojson a
    | `Backw a -> GV_backw.to_yojson a

  let spec : [ `Forw of V_forw.t | `Backw of V_backw.t ] -> [ `Forw of [`Left of V_forw.t] | `Backw of [`Left of V_backw.t] ] = function
    | `Forw v -> `Forw (GV_forw.spec v )
    | `Backw v ->  `Backw (GV_backw.spec v )

  let contexts : [ `Forw of V_forw.t | `Backw of V_backw.t ] -> [`Forw of [`Right of V_forw.t] | `Backw of  [`Right of V_backw.t]] = function
    | `Forw v -> `Forw (GV_forw.contexts v)
    | `Backw v ->  `Backw (GV_backw.contexts v)

  let var_id = show

  let arbitrary () =
    failwith "no arbitrary"
end 

module BidirFromSpec (S_forw:Spec) (S_backw:BackwSpec with type D_forw.t = S_forw.D.t and type G_forw.t = S_forw.G.t and type C.t = S_forw.C.t and type V_forw.t = S_forw.V.t) (Cfg:CfgBidir) (I:Increment)
  : sig
    module LVar : Goblint_constraint.ConstrSys.VarType with type t = [ `L_forw of VarF(S_forw.C).t | `L_backw of VarF(S_forw.C).t ]
    module GVar : (module type of GVarF2(S_forw.V)(S_backw.V))
    include DemandGlobConstrSys with module LVar := LVar
                                 and module GVar := GVar
                                 and module D = Lattice.Lift2(S_forw.D)(S_backw.D)
                                 and module G = GVarG (Lattice.Lift2(S_forw.G)(S_backw.G)) (S_forw.C)
  end 
= 
struct
  module LV = VarF (S_forw.C)
  module LVar =
  struct
    type t = [ `L_forw of LV.t | `L_backw of LV.t ] [@@deriving eq, ord, hash]

    let relift = function
      | `L_forw x -> `L_forw (LV.relift x)
      | `L_backw x -> `L_backw (LV.relift x)

    let pretty_trace () = function
      | `L_forw a -> GoblintCil.Pretty.dprintf "L_forw:%a" LV.pretty_trace a
      | `L_backw a -> GoblintCil.Pretty.dprintf "L_backw:%a" LV.pretty_trace a

    let printXml f = function
      | `L_forw a -> LV.printXml f a
      | `L_backw a -> LV.printXml f a

    let var_id = function
      | `L_forw a -> LV.var_id a
      | `L_backw a -> LV.var_id a

    let node = function
      | `L_forw a -> LV.node a
      | `L_backw a -> LV.node a

    let is_write_only = function
      | `L_forw a -> LV.is_write_only a
      | `L_backw a -> LV.is_write_only a
  end

  module D = Lattice.Lift2(S_forw.D)(S_backw.D)
  module GVar = GVarF2(S_forw.V)(S_backw.V)

  module G_forw = GVarG (S_forw.G) (S_forw.C)
  module G_backw = GVarG (S_backw.G) (S_forw.C)
  module G  = GVarG (Lattice.Lift2(S_forw.G)(S_backw.G)) (S_forw.C)

  module Forward = Constraints.FromSpec(S_forw)(Cfg)(I)

  (* Lowering functions for local values.*)
  let to_forw_d (d: D.t) : S_forw.D.t =
    match d with
    | `Lifted1 d -> d
    | `Bot -> S_forw.D.bot ()
    | `Top -> S_forw.D.top ()
    | `Lifted2 _ -> failwith "bidirConstrains: forward local has backward value"

  let to_backw_d (d: D.t) : S_backw.D.t =
    match d with
    | `Lifted2 d -> d
    | `Bot -> S_backw.D.bot ()
    | `Top -> S_backw.D.top ()
    | `Lifted1 _ -> failwith "bidirConstrains: backward local has forward value"

  (* Lowering and lifting functions to deal with different global values. This is convoluted -- but tbh, it is not that much worse than the G module in the existing forwards analysis. 
   * The conversion between the CSets is quite disgusting though. *) 

  let to_forw_g (g: G.t) : Forward.G.t =
    match g with
    | `Lifted1 (`Lifted1 g) -> `Lifted1 g
    | `Lifted1 `Bot -> `Bot
    | `Lifted1 `Top -> `Top
    | `Lifted1 (`Lifted2 _) -> failwith "bidirConstrains: forward global got backward value"
    | `Lifted2 c -> `Lifted2 (G_forw.CSet.of_list (G.CSet.elements c))
    | `Bot -> `Bot
    | `Top -> `Top

  let to_backw_g (g: G.t) : G_backw.t =
    match g with
    | `Lifted1 (`Lifted2 g) -> `Lifted1 g
    | `Lifted1 `Bot -> `Bot
    | `Lifted1 `Top -> `Top
    | `Lifted1 (`Lifted1 _) -> failwith "bidirConstrains: backward global got forward value"
    | `Lifted2 c -> `Lifted2 (G_backw.CSet.of_list (G.CSet.elements c))
    | `Bot -> `Bot
    | `Top -> `Top

  let of_forw_g (g: Forward.G.t) : G.t =
    match g with
    | `Lifted1 g -> `Lifted1 (`Lifted1 g)
    | `Lifted2 c -> `Lifted2 (G.CSet.of_list (G_forw.CSet.elements c))
    | `Bot -> `Bot
    | `Top -> `Top

  let of_backw_g (g: G_backw.t) : G.t =
    match g with
    | `Lifted1 g -> `Lifted1 (`Lifted2 g)
    | `Lifted2 c -> `Lifted2 (G.CSet.of_list (G_backw.CSet.elements c))
    | `Bot -> `Bot
    | `Top -> `Top


  (* actually relevant (transfer) functions *)
  let sync_backw man man_forw =
    match man.prev_node, Cfg.next man.prev_node with
    | _, _ :: _ :: _ -> (* Join in CFG. *)
      S_backw.sync man man_forw `Join
    (* | FunctionEntry f, _ -> (* Function entry, also needs sync because partial contexts joined by solver, see 00-sanity/35-join-contexts. *)
       S_backw.sync man man_forw (`JoinCall f) *)
    | Function f, _ -> (* Function entry, also needs sync because partial contexts joined by solver, see 00-sanity/35-join-contexts. *)
      S_backw.sync man man_forw (`JoinCall f)
    | _, _ -> S_backw.sync man man_forw `Normal

  let side_context_backw sideg f c =
    if !AnalysisState.postsolving then 
      sideg (GVar.GV_backw.contexts f) (G_backw.create_contexts (G_backw.CSet.singleton c))


  let create_basic_man_forw var edge prev_node pval getl getl_forw sidel demandl getg getg_forw sideg : (S_forw.D.t, S_forw.G.t, S_forw.C.t, S_forw.V.t) man =
    (* let r = ref [] in *)
    let node = fst var in
    let context : (unit -> S_forw.C.t) = snd var |> Obj.obj in

    let rec man_forw = 
      { ask     = (fun (type a) (q: a Queries.t) -> S_forw.query man_forw q)
      ; emit    = (fun _ -> failwith "emit outside MCP")
      ; node    = fst var
      ; prev_node = MyCFG.dummy_node (*I do not have *)
      ; control_context = (fun () -> failwith "control context not implemented (yet) for forward manager.")
      ; context = context
      ; edge    = edge
      ; local   =  getl_forw (node, context())
      ; global  = (fun g -> G_forw.spec (getg_forw (GVar.GV_forw.spec g)))
      ; spawn   = (fun ?multiple _ _ _ -> failwith "spawn should not be called from forward manager")
      ; split   = (fun _ _ -> failwith "split? what does this do?")
      ; sideg   = (fun _ _ -> failwith "sideg should not be called from forward manager")
      }
    in 
    man_forw

  let common_man_backw (var:node*Obj.t) edge prev_node pval getl getl_forw (sidel : node * S_forw.C.t -> S_backw.D.t -> unit) demandl getg getg_forw sideg : (S_backw.D.t, S_backw.G.t, S_backw.C.t, S_backw.V.t) man * S_backw.D.t list ref * (lval option * varinfo * exp list * S_backw.D.t * bool) list ref =
    let r = ref [] in
    let spawns = ref [] in

    let man_forw = create_basic_man_forw var edge prev_node pval getl getl_forw sidel demandl getg getg_forw sideg in      

    (* Logs.debug "Created forward manager for node %a, now creating backward manager" Node.pretty (fst var); *)
    (* now watch this ... *)
    let rec man =
      { ask     = (fun (type a) (q: a Queries.t) -> S_backw.query man man_forw q)
      ; emit    = (fun _ -> failwith "emit outside MCP")
      ; node    = fst var
      ; prev_node = prev_node
      ; control_context = snd var |> Obj.obj
      ; context = snd var |> Obj.obj
      ; edge    = edge
      ; local   = pval
      ; global  = (fun g -> G_backw.spec (getg (GVar.GV_backw.spec g)))
      ; spawn   = spawn
      ; split   = (fun (d:S_backw.D.t) es -> assert (List.is_empty es); r := d::!r)
      ; sideg   = (fun g d -> sideg (GVar.GV_backw.spec g) (G_backw.create_spec d))
      }
    and spawn ?(multiple=false) lval f args =
      (* TODO: adjust man node/edge? *)
      (* TODO: don't repeat for all paths that spawn same *)

      (* TODO: This needs to be changed for backwards!! Context is created using S_backw.context*)
      let ds = S_backw.threadenter ~multiple man man_forw lval f args in
      List.iter (fun (d : S_backw.D.t) ->
          spawns := (lval, f, args, d, multiple) :: !spawns;
          match Cilfacade.find_varinfo_fundec f with
          | fd ->
            let c = S_forw.context man_forw fd (man_forw.local) in
            (* sidel (FunctionEntry fd, c) d;
               demandl (Function fd, c) *)
            sidel (Function fd, c) d;
            demandl (FunctionEntry fd, c)
          | exception Not_found ->
            (* unknown function *)
            M.error ~category:Imprecise ~tags:[Category Unsound] "Created a thread from unknown function %s" f.vname;
            (* actual implementation (e.g. invalidation) is done by threadenter *)
            (* must still sync for side effects, e.g., old sync-based none privatization soundness in 02-base/51-spawn-special *)
            let rec sync_man =
              { man with
                ask = (fun (type a) (q: a Queries.t) -> (S_backw.query sync_man man_forw q));
                local = d;
                (* prev_node = Function dummyFunDec; *)
                prev_node = FunctionEntry dummyFunDec;
              }
            in
            (* TODO: more accurate man? *)
            ignore (sync_backw sync_man man_forw)
        ) ds
    in
    (* ... nice, right! *)
    let pval = sync_backw man man_forw in
    { man with local = pval }, r, spawns

  let rec bigsqcup_backw = function
    | []    -> S_backw.D.bot ()
    | [x]   -> x
    | x::xs -> S_backw.D.join x (bigsqcup_backw xs)

  let thread_spawns_backws man man_forw d spawns =
    if List.is_empty spawns then
      d
    else
      let rec man' =
        { man with
          ask = (fun (type a) (q: a Queries.t) -> S_backw.query man' man_forw q)
        ; local = d
        }
      in
      (* TODO: don't forget path dependencies *)
      let one_spawn (lval, f, args, fd, multiple) =
        let rec fman =
          { man with
            ask = (fun (type a) (q: a Queries.t) -> S_backw.query fman man_forw q)
          ; local = fd
          }
        in
        S_backw.threadspawn man' man_forw ~multiple lval f args fman
      in
      bigsqcup_backw (List.map one_spawn spawns)

  let common_join_backw man man_forw d splits spawns =
    thread_spawns_backws man man_forw (bigsqcup_backw (d :: splits)) spawns

  let common_joins_backw man man_forw ds splits spawns = common_join_backw man man_forw (bigsqcup_backw ds) splits spawns

  let tf_assign_backw var edge prev_node lv e getl getl_forw sidel demandl getg getg_forw sideg d =
    let man, r, spawns = common_man_backw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let man_forw = create_basic_man_forw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let d = S_backw.assign man man_forw lv e in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join_backw man man_forw d !r !spawns

  let tf_vdecl_backw var edge prev_node v getl getl_forw sidel demandl getg getg_forw sideg d =
    let man, r, spawns = common_man_backw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let man_forw = create_basic_man_forw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let d = S_backw.vdecl man man_forw v in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join_backw man man_forw d !r !spawns

  let normal_return_backw r fd man man_forw sideg =
    let spawning_return = S_backw.return man man_forw r fd in
    let nval = S_backw.sync { man with local = spawning_return } man_forw `Return in
    nval

  let toplevel_kernel_return_backw r fd man man_forw sideg =
    let st = if fd.svar.vname = MyCFG.dummy_func.svar.vname then man.local else S_backw.return man man_forw r fd in
    let spawning_return = S_backw.return {man with local = st} man_forw None MyCFG.dummy_func in
    let nval = S_backw.sync { man with local = spawning_return } man_forw `Return in
    nval

  let tf_ret_backw var edge prev_node ret fd getl getl_forw sidel demandl getg getg_forw sideg d =
    let man, r, spawns = common_man_backw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let man_forw = create_basic_man_forw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let d = (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
      if (CilType.Fundec.equal fd MyCFG.dummy_func ||
          List.mem fd.svar.vname (get_string_list "mainfun")) &&
         get_bool "kernel"
      then toplevel_kernel_return_backw ret fd man man_forw sideg
      else normal_return_backw ret fd man man_forw sideg
    in
    common_join_backw man man_forw d !r !spawns

  let tf_entry_backw var edge prev_node fd getl getl_forw sidel demandl getg getg_forw sideg d =
    (* Side effect function context here instead of at sidel to FunctionEntry,
       because otherwise context for main functions (entrystates) will be missing or pruned during postsolving. *)
    let c: unit -> S_forw.C.t = snd var |> Obj.obj in
    side_context_backw sideg fd (c ());
    let man, r, spawns = common_man_backw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let man_forw = create_basic_man_forw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let d = S_backw.body man man_forw fd in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join_backw man man_forw d !r !spawns

  let tf_test_backw var edge prev_node e tv getl getl_forw sidel demandl getg getg_forw sideg d =
    let man, r, spawns = common_man_backw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let man_forw = create_basic_man_forw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let d = S_backw.branch man man_forw e tv in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join_backw man man_forw d !r !spawns

  (*TODO: THIS HAS TO BE BACKWARDS*) (*forward context not implemented yet*)
  let tf_normal_call_backw man man_forw lv e (f:fundec) args getl (getl_forw : node * S_forw.C.t -> S_forw.D.t) sidel demandl getg getg_forw sideg =
    let combine (cd, fc, fd) =
      if M.tracing then M.traceli "combine" "local: %a" S_backw.D.pretty cd;
      if M.tracing then M.trace "combine" "function: %a" S_backw.D.pretty fd;

      (* Logs.debug "combine: local: %a" S_backw.D.pretty cd;
         Logs.debug "combine: function: %a" S_backw.D.pretty fd; *)

      let rec cd_man =
        { man with
          ask = (fun (type a) (q: a Queries.t) -> S_backw.query cd_man man_forw q);
          local = cd;
        }
      in
      let fd_man =
        (* Inner scope to prevent unsynced fd_man from being used. *)
        (* Extra sync in case function has multiple returns.
           Each `Return sync is done before joining, so joined value may be unsound.
           Since sync is normally done before tf (in common_man), simulate it here for fd. *)
        (* TODO: don't do this extra sync here *)
        let rec sync_man =
          { man with
            ask = (fun (type a) (q: a Queries.t) -> S_backw.query sync_man man_forw q);
            local = fd;
            (*prev_node = Function f*)
            prev_node = FunctionEntry f;
          }
        in
        (* TODO: more accurate man? *)
        let synced = sync_backw sync_man man_forw in
        let rec fd_man =
          { sync_man with
            ask = (fun (type a) (q: a Queries.t) -> S_backw.query fd_man man_forw q);
            local = synced;
          }
        in
        fd_man
      in
      let r = List.fold_left (fun acc fd1 ->
          let rec fd1_man =
            { fd_man with
              ask = (fun (type a) (q: a Queries.t) -> S_backw.query fd1_man man_forw q);
              local = fd1;
            }
          in
          let combine_enved = S_backw.combine_env cd_man man_forw lv e f args fc fd1_man.local (Analyses.ask_of_man fd1_man) in
          let rec combine_assign_man =
            { cd_man with
              ask = (fun (type a) (q: a Queries.t) -> S_backw.query combine_assign_man man_forw q);
              local = combine_enved;
            }
          in
          S_backw.D.join acc (S_backw.combine_assign combine_assign_man man_forw lv e f args fc fd1_man.local (Analyses.ask_of_man fd1_man))
        ) (S_backw.D.bot ()) (S_backw.paths_as_set fd_man man_forw)
      in
      if M.tracing then M.traceu "combine" "combined local: %a" S_backw.D.pretty r;
      (* Logs.debug "combined local: %a" S_backw.D.pretty r; *)
      r
    in
    let paths = S_backw.enter man man_forw lv f args in

    (* getl_forw should query the corresopoding unknown from the forward analysis *)
    (* context = S_forw.context (S_forw.enter (getl_forw [this_node_, this_context])) *)

    let man_forw =
      { ask     = (fun (type a) (q: a Queries.t) -> failwith "manager for calculating context does not support queries")
      ; emit    = (fun _ -> failwith "emit outside MCP")
      ; node    = man.node
      ; prev_node = MyCFG.dummy_node
      ; control_context = man.control_context
      ; context = man.context
      ; edge    = man.edge
      ; local   = (getl_forw (man.node, man.context ()))
      ; global  = (fun g -> G_forw.spec (getg_forw (GVar.GV_forw.spec g)))
      ; spawn   = (fun ?multiple _ _ _ -> failwith "manager for calculating context does not support spawn")
      ; split   = (fun _ _ -> failwith "manager for calculating context does not support split") 
      ; sideg   = (fun _ _ -> failwith "manager for calculating context does not support sideg")
      } in

    let paths_forw = 
      Logs.debug "forward manager info at call to %a" Node.pretty man_forw.node;
      S_forw.enter man_forw lv f args in

    let paths = List.combine paths paths_forw in

    (* filter paths were the forward analysis found out they are unreachable*)
    let paths = List.filter (fun ((c,v),(_,b)) -> not (S_forw.D.is_bot b)) paths in 

    (* this list now uses forward contexts*)
    let paths = List.map (fun ((c,v),(_,b)) -> (c, S_forw.context man_forw f b, v)) paths in

    (* The two lines below is what I should use. *)
    (* List.iter (fun (c,fc,v) -> if not (S_backw.D.is_bot v) then sidel (Function f, fc) v) paths;  *)
    (* let paths = List.map (fun (c,fc,v) -> (c, fc, if S_backw.D.is_bot v then v else getl (FunctionEntry f, fc))) paths in *)

    (* A problem with my liveness analysis is that D.empty = D.bot, but I still need to evaluate a function since variables might become live inside. This is not optimal and the liveness analysis should be changed.*)
    List.iter (fun (c,fc,v) -> sidel (Function f, fc) v) paths;
    let paths = List.map (fun (c,fc,v) -> (c, fc, getl (FunctionEntry f, fc))) paths in


    (* Don't filter bot paths, otherwise LongjmpLifter is not called. *)
    (* let paths = List.filter (fun (c,fc,v) -> not (D.is_bot v)) paths in *)
    let paths = List.map (Tuple3.map2 Option.some) paths in
    if M.tracing then M.traceli "combine" "combining";
    (* Logs.debug  "combining"; *)
    let paths = List.map combine paths in
    let r = List.fold_left S_backw.D.join (S_backw.D.bot ()) paths in
    if M.tracing then M.traceu "combine" "combined: %a" S_backw.D.pretty r;
    (* Logs.debug "combined: %a" S_backw.D.pretty r; *)
    r

  let rec tf_proc_backw var edge prev_node lv e args getl (getl_forw: node * S_forw.C.t -> S_forw.D.t) sidel demandl getg getg_forw sideg d =
    let tf_special_call man man_forw f =
      let once once_control init_routine =
        (* Executes leave event for new local state d if it is not bottom *)
        let leave_once d =
          if not (S_backw.D.is_bot d) then
            let rec man' =
              { man with
                ask = (fun (type a) (q: a Queries.t) -> S_backw.query man' man_forw q);
                local = d;
              }
            in
            S_backw.event man' man_forw (Events.LeaveOnce { once_control }) man'
          else
            S_backw.D.bot ()
        in
        let first_call =
          let d' = S_backw.event man man_forw (Events.EnterOnce { once_control;  ran = false }) man in
          tf_proc_backw var edge prev_node None init_routine [] getl getl_forw sidel demandl getg getg_forw sideg d'
        in
        let later_call = S_backw.event man man_forw (Events.EnterOnce { once_control;  ran = true }) man in
        S_backw.D.join (leave_once first_call) (leave_once later_call)
      in
      let is_once = LibraryFunctions.find ~nowarn:true f in
      (* If the prototpye for a library function is wrong, this will throw an exception. Such exceptions are usually unrelated to pthread_once, it is just that the call to `is_once.special` raises here *)
      match is_once.special args with
      | Once { once_control; init_routine } -> once once_control init_routine
      | _  -> S_backw.special man man_forw lv f args
    in
    let man, r, spawns = common_man_backw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let man_forw = create_basic_man_forw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let functions =
      match e with
      | Lval (Var v, NoOffset) ->
        (* Handle statically known function call directly.
           Allows deactivating base. *)
        [v]
      | _ ->
        (*constructing fake forwards manager s.t. the inforamtion for the pointer information can be retireved*)
        (* let r = ref [] in
           let rec man_forw =
           { ask     = (fun (type a) (q: a Queries.t) -> S_forw.query man_forw q)
           ; emit    = (fun _ -> failwith "emit outside MCP")
           ; node    = man.node
           ; prev_node = man.prev_node (* this is problematic, as this is backwards *)
           ; control_context = man.control_context
           ; context = man.context
           ; edge    = man.edge
           ; local   = (getl_forw (man.node, man.context ())) (* accessing forward inforkation*)
           ; global  = (fun _ -> failwith "whoops, query for resolving function pointer depends on globals")
           ; spawn   = (fun ?multiple _ _ _ -> failwith "manager for resolving function pointer does not support spawn")
           ; split   = (fun (d:S_forw.D.t) es -> assert (List.is_empty es); r := d::!r) (*what is this?*)
           ; sideg   = (fun _ _ -> failwith "manager for resolving function pointer does not support sideg")
           } in *)
        let () = Logs.debug "manager info at call to function pointer %a" Node.pretty man_forw.node in
        (* Depends on base for query. *)
        let ad = man_forw.ask (Queries.EvalFunvar e) in
        let res = Queries.AD.to_var_may ad in (* TODO: don't convert, handle UnknownPtr below *) 
        (*PROBLEM: Pointer. Brauche Ergebnisse der anderen Analysen*)
        (Logs.debug "(!) resolved function pointer to %d functions" (List.length res);
         (match res with 
          | x::xs -> 
            List.iter (fun vi -> Logs.debug "      possible function: %s" vi.vname) res;
          | _ -> ();
         ));
        res
    in
    let one_function f =
      match Cil.unrollType f.vtype with
      | TFun (_, params, var_arg, _)  ->
        let arg_length = List.length args in
        let p_length = Option.map_default List.length 0 params in
        (* Check whether number of arguments fits. *)
        (* If params is None, the function or its parameters are not declared, so we still analyze the unknown function call. *)
        if Option.is_none params || p_length = arg_length || (var_arg && arg_length >= p_length) then
          let d =
            (match Cilfacade.find_varinfo_fundec f with
             | fd when LibraryFunctions.use_special f.vname ->
               M.info ~category:Analyzer "Using special for defined function %s" f.vname;
               tf_special_call man man_forw f
             | fd ->
               tf_normal_call_backw man man_forw lv e fd args getl getl_forw sidel demandl getg getg_forw sideg
             | exception Not_found ->
               tf_special_call man man_forw f)
          in
          Some d
        else begin
          let geq = if var_arg then ">=" else "" in
          M.warn ~category:Unsound ~tags:[Category Call; CWE 685] "Potential call to function %a with wrong number of arguments (expected: %s%d, actual: %d). This call will be ignored." CilType.Varinfo.pretty f geq p_length arg_length;
          None
        end
      | _ ->
        M.warn ~category:Call "Something that is not a function (%a) is called." CilType.Varinfo.pretty f;
        None
    in
    let funs = List.filter_map one_function functions in
    if [] = funs && not (S_backw.D.is_bot man.local) then begin
      M.msg_final Warning ~category:Unsound ~tags:[Category Call] "No suitable function to call";
      M.warn ~category:Unsound ~tags:[Category Call] "No suitable function to be called at call site. Continuing with state before call.";
      d (* because LevelSliceLifter *)
    end else
      common_joins_backw man man_forw funs !r !spawns

  let tf_asm_backw var edge prev_node getl getl_forw sidel demandl getg getg_forw sideg d =
    let man, r, spawns = common_man_backw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let man_forw = create_basic_man_forw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let d = S_backw.asm man man_forw in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join_backw man man_forw d !r !spawns

  let tf_skip_backw var edge prev_node getl getl_forw sidel demandl getg getg_forw sideg d =
    let man, r, spawns = common_man_backw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let man_forw = create_basic_man_forw var edge prev_node d getl getl_forw sidel demandl getg getg_forw sideg in
    let d = S_backw.skip man man_forw in (* Force transfer function to be evaluated before dereferencing in common_join argument. *)
    common_join_backw man man_forw d !r !spawns

  let tf_backw var getl getl_forw sidel demandl getg getg_forw sideg prev_node edge d =
    begin match edge with
      | Assign (lv,rv) -> tf_assign_backw var edge prev_node lv rv
      | VDecl (v)      -> tf_vdecl_backw var edge prev_node v
      | Proc (r,f,ars) -> tf_proc_backw var edge prev_node r f ars
      | Entry f        -> tf_entry_backw var edge prev_node f
      | Ret (r,fd)     -> tf_ret_backw var edge prev_node r fd
      | Test (p,b)     -> tf_test_backw var edge prev_node p b
      | ASM (_, _, _)  -> tf_asm_backw var edge prev_node (* TODO: use ASM fields for something? *)
      | Skip           -> tf_skip_backw var edge prev_node
    end getl getl_forw sidel demandl getg getg_forw sideg d

  (* TODO: Don't call it prev_node when it is actually the next node. *)
  let tf_backw var getl getl_forw sidel demandl getg getg_forw sideg prev_node (_,edge) d (f,t) =
    (* let old_loc  = !Goblint_tracing.current_loc in
       let old_loc2 = !Goblint_tracing.next_loc in
       Goblint_tracing.current_loc := f;
       Goblint_tracing.next_loc := t;
       Goblint_backtrace.protect ~mark:(fun () -> TfLocation f) ~finally:(fun () ->
        Goblint_tracing.current_loc := old_loc;
        Goblint_tracing.next_loc := old_loc2
       ) (fun () ->
        let d       = tf_backw var getl sidel demandl getg sideg prev_node edge d in
        d
       ) *)
    tf_backw var getl getl_forw sidel demandl getg getg_forw sideg prev_node edge d

  let tf_backw (v,c) (edges, u) getl getl_forw sidel demandl getg getg_forw sideg =
    let pval = getl (u,c) in
    let _, locs = List.fold_right (fun (f,e) (t,xs) -> f, (f,t)::xs) edges (Node.location v,[]) in
    List.fold_left2 (|>) pval (List.map (tf_backw (v,Obj.repr (fun () -> c)) getl getl_forw sidel demandl getg getg_forw sideg u) edges) locs

  let tf_backw (v,c) (e,u) getl getl_forw sidel demandl getg getg_forw sideg =
    let old_node = !current_node in
    let old_fd = Option.map Node.find_fundec old_node |? Cil.dummyFunDec in
    let new_fd = Node.find_fundec v in
    if not (CilType.Fundec.equal old_fd new_fd) then
      Timing.Program.enter new_fd.svar.vname;
    let old_context = !M.current_context in
    current_node := Some u;
    M.current_context := Some (Obj.magic c); (* magic is fine because Spec is top-level Control Spec *)
    Fun.protect ~finally:(fun () ->
        current_node := old_node;
        M.current_context := old_context;
        if not (CilType.Fundec.equal old_fd new_fd) then
          Timing.Program.exit new_fd.svar.vname
      ) (fun () ->
        let d       = tf_backw (v,c) (e,u) getl getl_forw sidel demandl getg getg_forw sideg in
        d
      )

  let system_backw (v,c) =

    match v with
    | Function _ -> None
    | _ ->
      let tf_backw getl sidel demandl getg sideg =
        let getl_backw d =  getl (`L_backw d) |> to_backw_d in
        let getl_forw d =  getl (`L_forw d) |> to_forw_d in
        let getg_backw v = getg (`Backw v) |> to_backw_g in
        let getg_forw v = getg (`Forw v) |> to_forw_g in
        let tf' eu = tf_backw (v,c) eu getl_backw getl_forw sidel demandl getg_backw getg_forw sideg in
        let xs = List.map tf' (Cfg.next v) in
        List.fold_left S_backw.D.join (S_backw.D.bot ()) xs
      in
      Some tf_backw

  let system var =
    match var with
    | `L_forw v ->
      Forward.system v
      |> Option.map (fun tf getl sidel demandl getg sideg ->
          let getl' v = getl (`L_forw v) |> to_forw_d in
          let sidel' v d = sidel (`L_forw v) (`Lifted1 d) in
          let demandl' v = demandl (`L_forw v) in
          let getg' v = getg (`Forw v) |> to_forw_g in
          let sideg' v d = sideg (`Forw v) (of_forw_g d) in
          tf getl' sidel' demandl' getg' sideg' |> (fun d -> `Lifted1 d)
        )
    | `L_backw v ->
      system_backw v
      |> Option.map (fun tf getl sidel demandl getg sideg ->
          let sidel' v d = sidel (`L_backw v) (`Lifted2 d) in
          let demandl' v = demandl (`L_backw v) in
          let sideg' v d = sideg (`Backw v) (of_backw_g d) in
          tf getl sidel' demandl' getg sideg' |> (fun d -> `Lifted2 d)
        )

  let iter_vars getl getg vq fl fg =
    failwith "iter_vars not implemented for bidirectional constraint system."

  let sys_change getl getg =
    failwith "sys_change not implemented for bidirectional constraint system."

  let postmortem_backw v =
    failwith "postmortem not implemented for backward analysis"
  (* match v with
     | Function fd, c -> [(FunctionEntry fd, c)]
     | _ -> [] *)

  let postmortem = function
    | `L_forw v -> List.map (fun v -> `L_forw v) (Forward.postmortem v)
    | `L_backw v -> List.map (fun v -> `L_backw (v)) (postmortem_backw v)
end