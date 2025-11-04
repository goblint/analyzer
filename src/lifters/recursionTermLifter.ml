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

  let conv (man: (_, G.t, _, V.t) man): (_, S.G.t, _, S.V.t) man =
    { man with
      global = (fun v -> G.spec (man.global (V.spec v)));
      sideg = (fun v g -> man.sideg (V.spec v) (G.create_spec g));
    }

  let cycleDetection man call =
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
        let callers = G.callers (man.global gvar) in
        CallerSet.iter (fun to_call ->
            iter_call new_path_visited_calls to_call
          ) callers;
      end
    in
    iter_call LS.empty call

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal v ->
      (* check result of loop analysis *)
      if not (man.ask Queries.MustTermAllLoops) then
        AnalysisState.svcomp_may_not_terminate := true;
      let v: V.t = Obj.obj v in
      begin match v with
        | `Left v' ->
          S.query (conv man) (WarnGlobal (Obj.repr v'))
        | `Right call -> cycleDetection man call (* Note: to make it more efficient, one could only execute the cycle detection in case the loop analysis returns true, because otherwise the program will probably not terminate anyway*)
      end
    | InvariantGlobal v ->
      let v: V.t = Obj.obj v in
      begin match v with
        | `Left v ->
          S.query (conv man) (InvariantGlobal (Obj.repr v))
        | `Right v ->
          Queries.Result.top q
      end
    | YamlEntryGlobal (v, task) ->
      let v: V.t = Obj.obj v in
      begin match v with
        | `Left v ->
          S.query (conv man) (YamlEntryGlobal (Obj.repr v, task))
        | `Right v ->
          Queries.Result.top q
      end
    | _ -> S.query (conv man) q

  let branch man = S.branch (conv man)
  let assign man = S.assign (conv man)
  let vdecl man = S.vdecl (conv man)


  let record_call sideg callee caller =
    sideg (V.call callee) (G.create_singleton_caller caller)

  let enter man  = S.enter (conv man)
  let context man = S.context (conv man)
  let paths_as_set man = S.paths_as_set (conv man)
  let body man = S.body (conv man)
  let return man = S.return (conv man)
  let combine_env man r fe f args fc es f_ask =
    if !AnalysisState.postsolving then (
      let c_r: S.C.t = man.context () in (* Caller context *)
      let nodeF = man.node in
      let fd_r : fundec = Node.find_fundec nodeF in (* Caller fundec *)
      let caller: (fundec * S.C.t) = (fd_r, c_r) in
      let c_e: S.C.t = Option.get fc in (* Callee context *)
      let fd_e : fundec = f in (* Callee fundec *)
      let callee = (fd_e, c_e) in
      record_call man.sideg callee caller
    );
    S.combine_env (conv man) r fe f args fc es f_ask

  let combine_assign man = S.combine_assign (conv man)
  let special man = S.special (conv man)
  let threadenter man = S.threadenter (conv man)
  let threadspawn man ~multiple lv f args fman = S.threadspawn (conv man) ~multiple lv f args (conv fman)
  let sync man = S.sync (conv man)
  let skip man = S.skip (conv man)
  let asm man = S.asm (conv man)
  let event man e oman = S.event (conv man) e (conv oman)
end
