open GoblintCil
open Analyses


module Lifter (S: Spec)
  : Spec with module D = S.D
          and module C = S.C
=
(* two global invariants:
   - S.V -> S.G
     Needed to store the previously built global invariants
   - fundec * S.C -> (Set (fundec * S.C))
     The second global invariant maps from the callee fundec and context to a set of caller fundecs and contexts.
     This structure therefore stores the context-sensitive call graph.
     For example:
      let the function f in context c call function g in context c'.
      In the global invariant structure it would be stored like this: (g,c') -> {(f, c)}
*)

struct
  include S

  (* contains all the callee fundecs and contexts *)
  module V = GVarFC(S.V)(S.C)

  (* Tuple containing the fundec and context of a caller *)
  module Call = Printable.Prod (CilType.Fundec) (S.C)

  (* Set containing multiple caller tuples *)
  module CallerSet = SetDomain.Make (Call)

  module G =
  struct
    include Lattice.Lift2 (G) (CallerSet)

    let spec = function
      | `Bot -> G.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "RecursionTermLifter.spec"

    let callers = function
      | `Bot -> CallerSet.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "RecursionTermLifter.callGraph"

    let create_spec spec = `Lifted1 spec
    let create_singleton_caller caller = `Lifted2 (CallerSet.singleton caller)

    let printXml f = function
      | `Lifted1 x -> G.printXml f x
      | `Lifted2 x -> BatPrintf.fprintf f "<analysis name=\"recTerm-context\">%a</analysis>" CallerSet.printXml x
      | x -> BatPrintf.fprintf f "<analysis name=\"recTerm\">%a</analysis>" printXml x

  end

  let name () = "RecursionTermLifter (" ^ S.name () ^ ")"

  let conv (ctx: (_, G.t, _, V.t) ctx): (_, S.G.t, _, S.V.t) ctx =
    { ctx with
      global = (fun v -> G.spec (ctx.global (V.spec v)));
      sideg = (fun v g -> ctx.sideg (V.spec v) (G.create_spec g));
    }

  let cycleDetection ctx call =
    let module LH = Hashtbl.Make (Printable.Prod (CilType.Fundec) (S.C)) in
    let module LS = Set.Make (Printable.Prod (CilType.Fundec) (S.C)) in
    (* find all cycles/SCCs *)
    let global_visited_calls = LH.create 100 in

    (* DFS *)
    let rec iter_call (path_visited_calls: LS.t) ((fundec, _) as call) =
      if LS.mem call path_visited_calls then (
        AnalysisState.svcomp_may_not_terminate := true; (*set the indicator for a non-terminating program for the sv comp*)
        (*Cycle found*)
        let loc = M.Location.CilLocation fundec.svar.vdecl in
        M.warn ~loc ~category:Termination "The program might not terminate! (Fundec %a is contained in a call graph cycle)" CilType.Fundec.pretty fundec) (* output a warning for non-termination*)
      else if not (LH.mem global_visited_calls call) then begin
        LH.replace global_visited_calls call ();
        let new_path_visited_calls = LS.add call path_visited_calls in
        let gvar = V.call call in
        let callers = G.callers (ctx.global gvar) in
        CallerSet.iter (fun to_call ->
            iter_call new_path_visited_calls to_call
          ) callers;
      end
    in
    iter_call LS.empty call

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal v ->
      (* check result of loop analysis *)
      if not (ctx.ask Queries.MustTermAllLoops) then
        AnalysisState.svcomp_may_not_terminate := true;
      let v: V.t = Obj.obj v in
      begin match v with
        | `Left v' ->
          S.query (conv ctx) (WarnGlobal (Obj.repr v'))
        | `Right call -> cycleDetection ctx call (* Note: to make it more efficient, one could only execute the cycle detection in case the loop analysis returns true, because otherwise the program will probably not terminate anyway*)
      end
    | InvariantGlobal v ->
      let v: V.t = Obj.obj v in
      begin match v with
        | `Left v ->
          S.query (conv ctx) (InvariantGlobal (Obj.repr v))
        | `Right v ->
          Queries.Result.top q
      end
    | YamlEntryGlobal (v, task) ->
      let v: V.t = Obj.obj v in
      begin match v with
        | `Left v ->
          S.query (conv ctx) (YamlEntryGlobal (Obj.repr v, task))
        | `Right v ->
          Queries.Result.top q
      end
    | _ -> S.query (conv ctx) q

  let branch ctx = S.branch (conv ctx)
  let assign ctx = S.assign (conv ctx)
  let vdecl ctx = S.vdecl (conv ctx)


  let record_call sideg callee caller =
    sideg (V.call callee) (G.create_singleton_caller caller)

  let enter ctx  = S.enter (conv ctx)
  let context ctx = S.context (conv ctx)
  let paths_as_set ctx = S.paths_as_set (conv ctx)
  let body ctx = S.body (conv ctx)
  let return ctx = S.return (conv ctx)
  let combine_env ctx r fe f args fc es f_ask =
    if !AnalysisState.postsolving then (
      let c_r: S.C.t = ctx.context () in (* Caller context *)
      let nodeF = ctx.node in
      let fd_r : fundec = Node.find_fundec nodeF in (* Caller fundec *)
      let caller: (fundec * S.C.t) = (fd_r, c_r) in
      let c_e: S.C.t = Option.get fc in (* Callee context *)
      let fd_e : fundec = f in (* Callee fundec *)
      let callee = (fd_e, c_e) in
      record_call ctx.sideg callee caller
    );
    S.combine_env (conv ctx) r fe f args fc es f_ask

  let combine_assign ctx = S.combine_assign (conv ctx)
  let special ctx = S.special (conv ctx)
  let threadenter ctx = S.threadenter (conv ctx)
  let threadspawn ctx ~multiple lv f args fctx = S.threadspawn (conv ctx) ~multiple lv f args (conv fctx)
  let sync ctx = S.sync (conv ctx)
  let skip ctx = S.skip (conv ctx)
  let asm ctx = S.asm (conv ctx)
  let event ctx e octx = S.event (conv ctx) e (conv octx)
end
