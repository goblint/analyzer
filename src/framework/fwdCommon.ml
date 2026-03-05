open Goblint_constraint.ConstrSys


let gas_default = ref (10,3)

(** Manage warrowing

      Widening will be delayed 'delay' times in each phase
      There will be at most 'gas' narrowing phases.
*)
module Warrow (L : Lattice.S) = struct
  (** (value, delay, gas, narrowing_flag) 
        Narrowing flag denotes if the last update lead
        to a narrowing. This is required to maintain delay/gas values.
  *)
  type state = L.t * int * int * bool

  type contribution = {
    value: L.t;
    delay: int;
    gas: int;
    last_was_narrow: bool;
  }

  let default () =
    let (delay,gas) = !gas_default in { value = L.bot (); delay; gas; last_was_narrow=false }

  let warrowc contribution new_value =
    let (delay0, _) = !gas_default in

    let narrow () =
      if contribution.last_was_narrow then
        { contribution with value = L.narrow contribution.value new_value }
      else (
        if contribution.gas > 0 then  
          { contribution with
            value = L.narrow contribution.value new_value;
            gas = contribution.gas - 1;
            last_was_narrow = true;
          }
        else contribution 
      )
    in

    let widen () =
      if contribution.last_was_narrow then
        { contribution with
          value = L.join contribution.value new_value;
          last_was_narrow = false;
          delay = delay0
        }
      else if contribution.delay <= 0 then
        { contribution with value = L.widen contribution.value (L.join contribution.value new_value) }
      else
        { contribution with
          value = L.join contribution.value new_value;
          delay = contribution.delay - 1
        }
    in

    let current_value = contribution.value in
    if L.equal new_value current_value then contribution
    else if L.leq new_value current_value then narrow ()
    else widen ()


  (* Legacy warrow with tuples instead of contribution types *)
  (* remove once all solvers are migrated *)
  let warrow (current, delay, gas, narrow_flag) newval =
    let (delay0, _) = !gas_default in
    if L.equal current newval then (current, delay, gas, narrow_flag)

    else if L.leq newval current then (
      if narrow_flag then (L.narrow current newval, delay, gas, true)
      else if gas <= 0 then (current, delay, gas, false)
      else (L.narrow current newval, delay, gas - 1, true)
    )

    else (
      if narrow_flag then (L.join current newval, delay0, gas, false)
      else if delay <= 0 then (L.widen current (L.join current newval), 0, gas, false)
      else (L.join current newval, delay - 1, gas, false)
    )
end

module SolverLocals (Sys: FwdGlobConstrSys) (LM: Hashtbl.S with type key=Sys.LVar.t) = struct

  module System = Sys
  module D = Sys.D
  module LWarrow = Warrow(D)
  module LM = LM

  type lt = System.LVar.t

  type contribution = LWarrow.contribution

  type t = {
    mutable loc_value : D.t; 
    loc_init : D.t; 
    mutable called: bool;
    mutable aborted: bool;
    loc_from : contribution LM.t}

  let loc: t LM.t = LM.create 100

  let get x =
    let add_default () = 
      let default = {loc_value = D.bot (); loc_init = D.bot (); 
                     called = false; aborted = false;
                     loc_from = LM.create 10} in
      LM.add loc x default; 
      default in
    BatOption.default_delayed add_default (LM.find_opt loc x)

  let init (x, d) =
    LM.add loc x {
      loc_value = d;
      loc_init = d;
      called = false;
      aborted = false;
      loc_from = LM.create 10
    }

  let construct_value data =
    LM.fold (fun _ (c : contribution) a -> D.join a c.value) data.loc_from data.loc_init

  let get_contribution contributor data =
    let add_default () = (
      let contribution = LWarrow.default () in
      LM.add data.loc_from contributor contribution;
      contribution
    ) in
    BatOption.default_delayed add_default (LM.find_opt data.loc_from contributor)

  let warrow = LWarrow.warrowc

end

module SolverGlobals (Sys: FwdGlobConstrSys) (LS: Set.S with type elt = Sys.LVar.t) (LM: Hashtbl.S with type key = Sys.LVar.t) (GM: Hashtbl.S with type key = Sys.GVar.t) (OM: Hashtbl.S with type key = Node0.t) = struct

  module Sys = Sys

  module G = Sys.G
  module GWarrow = Warrow(G)
  module LS = LS
  module LM = LM
  module GM = GM
  module OM = OM

  type gt = G.t

  type contribution = {
    value: G.t;
    delay: int;
    gas: int;
    last_was_narrow: bool;
    set: LS.t;
  }

  let warrow (old_contribution : contribution) d set: contribution =
    (* This is a weird wrapper to account for the set field, hopefully it  *)
    (* can be refactored properly *)
    let warrow_contribution: GWarrow.contribution = {
      value = old_contribution.value;
      delay = old_contribution.delay;
      gas = old_contribution.gas;
      last_was_narrow = old_contribution.last_was_narrow
    } in
    let c = GWarrow.warrowc warrow_contribution d in
    {
      value = c.value;
      delay = c.delay;
      gas = c.gas;
      last_was_narrow = c.last_was_narrow;
      set = set
    }

  let default_contribution () =
    let (delay,gas) = !gas_default in 
    { value = G.bot (); delay; gas; last_was_narrow=false; set=LS.empty}

  (** Values for globals

      value: The value of the global, as calculated from init and from. Since
      this calculation is costly, we save the result

      init: Initial value of this global. We will never narrow below this value.

      last: ?

      from: A map of contributions from each origin with the corresponding warrowing data.
  *)
  type t = {value : G.t; init : G.t;  infl : LS.t ; last: G.t LM.t; 
            from : contribution OM.t}

  let glob: t GM.t = GM.create 100

  let get g =
    let make_default () = 
      let rglob = {value = G.bot (); init = G.bot (); infl = LS.empty; last = LM.create 10; from = OM.create 10} in
      GM.add glob g rglob;
      rglob in
    BatOption.default_delayed make_default (GM.find_opt glob g)

  (** Initialize a global with the value d *)
  let init (g, d) =
    GM.add glob g {
      value = d;
      init = d;
      infl = LS.empty;
      last = LM.create 10;
      from = OM.create 10
    }

  let add_infl glob_data g x = GM.replace glob g { glob_data with infl = LS.add x glob_data.infl }

  let replace = GM.replace glob

  let construct_value data = OM.fold (fun _ (c : contribution) a -> G.join a c.value) data.from data.init

  let get_contribution orig data =
    let add_default () =
      let contribution = default_contribution () in
      OM.add data.from orig contribution;
      contribution in
    BatOption.default_delayed add_default (OM.find_opt data.from orig)

  let get_last_contrib set last = 
    LS.fold (fun x d -> G.join d (LM.find last x)) set (G.bot()) 

end

module type SolverLocalsSig = sig
  module System : FwdGlobConstrSys
  module LM : Hashtbl.S with type key = System.LVar.t

  type contribution 

  type t = {
    mutable loc_value : System.D.t; 
    loc_init : System.D.t; 
    mutable called: bool;
    mutable aborted: bool;
    loc_from : contribution LM.t}

  val get : System.LVar.t -> t
end

module type SolverGlobalsSig = sig
  module Sys : FwdGlobConstrSys
  module G : Lattice.S with type t = Sys.G.t
  module LM : Hashtbl.S with type key = Sys.LVar.t
  module LS : Set.S with type elt = Sys.LVar.t
  module GM : Hashtbl.S with type key = Sys.GVar.t
  module OM : Hashtbl.S

  type contribution

  type t = {value : G.t; init : G.t;  infl : LS.t ; last: G.t LM.t; 
            from : contribution OM.t}

  val get : Sys.GVar.t -> t
  val get_contribution : OM.key -> t -> contribution
end

module Checker (System: FwdGlobConstrSys)
    (Lcl: SolverLocalsSig with module System=System)
    (Gbl: SolverGlobalsSig with module Sys=System )
= struct

  module D = System.D
  module G = System.G

  module LM = Lcl.LM
  module GM = Gbl.GM
  module LS = Gbl.LS

  let work = ref (([] : System.LVar.t list), LS.empty)

  let add_work x = let (l,s) = !work in
    if LS.mem x s then ()
    else work := (x::l, LS.add x s)

  let rem_work () = let (l,s) = !work in
    match l with
    | [] -> None
    | x::xs ->
      let s = LS.remove x s in
      let _ = work := (xs,s) in
      Some x

  let check localinit globalinit xs =

    let sigma_out = LM.create 100 in
    let tau_out   = GM.create 100 in

    (* let get_local x = try (LM.find Lcl.loc x).loc_value with _ -> D.bot () in *)
    let get_local x = try (Lcl.get x).loc_value with _ -> D.bot () in

    let check_local x d = if D.leq d (D.bot ()) then ()
      else let {loc_value:D.t;loc_init;called;aborted;loc_from}: Lcl.t = Lcl.get x in
        if D.leq d loc_value then
          if LM.mem sigma_out x then ()
          else (
            LM.add sigma_out x loc_value;
            add_work x
          )
        else (
          Logs.error "Fixpoint not reached for local %a" System.LVar.pretty_trace x;
          AnalysisState.verified := Some false;
          if LM.mem sigma_out x then ()
          else (
            LM.add sigma_out x loc_value;
            add_work x
          )
        ) in

    let get_global g = try (Gbl.get g).value with _ -> G.bot () in

    let check_global x g d =
      if G.leq d (G.bot ()) then
        ()
      else if System.GVar.is_write_only g then
        GM.replace tau_out g (G.join (GM.find_opt tau_out g |> BatOption.default (G.bot ())) d)
      else
        let {value;infl;_}: Gbl.t = Gbl.get g in
        if G.leq d value then
          if GM.mem tau_out g then ()
          else (
            GM.add tau_out g value;
            LS.iter add_work infl
          )
        else (
          Logs.error "Fixpoint not reached for global %a\n Side from %a is %a \n Solver Computed %a\n Diff is %a" System.GVar.pretty_trace g System.LVar.pretty_trace x G.pretty d G.pretty value G.pretty_diff (d,value);
          AnalysisState.verified := Some false;
          if GM.mem tau_out g then ()
          else (
            GM.add tau_out g value;
            LS.iter add_work infl
          )
        ) in

    let rec doit () =
      match rem_work () with
      | None -> (LM.to_seq sigma_out, GM.to_seq tau_out)
      | Some x -> (match System.system x with
          | None -> doit ()
          | Some f -> (
              f (get_local x)
                get_local check_local
                get_global (check_global x);
              doit ()
            )
        ) in

    List.iter (fun (x,_) -> let value = get_local x in LM.add sigma_out x value) localinit;
    List.iter (fun (g, _) -> let value = get_global g in GM.add tau_out g value) globalinit;
    List.iter add_work xs;
    doit ()

  let check localinit globalinit xs =
    let check_local (x,d) =
      if D.leq d (Lcl.get x).loc_value then ()
      else (
        Logs.error "initialization not subsumed for local %a" System.LVar.pretty_trace x;
        AnalysisState.verified := Some false)
    in
    let check_global (g,d) =
      if G.leq d (Gbl.get g).value then ()
      else (
        Logs.error "initialization not subsumed for global %a" System.GVar.pretty_trace g;
        AnalysisState.verified := Some false;
      ) in

    let _ = List.iter check_local localinit in
    let _ = List.iter check_global globalinit in

    check localinit globalinit xs
end

module SolverStats (Sys: FwdGlobConstrSys) = struct
  let rhs_event_count = ref 0

  let eval_rhs_event x = rhs_event_count := !rhs_event_count + 1

  let solver_start_event () =
    let starttime_ms = int_of_float (Unix.gettimeofday () *. 1000.) in
    Logs.info "Solver start: %d" starttime_ms

  let solver_end_event () =
    let endtime_ms = int_of_float (Unix.gettimeofday () *. 1000.) in
    Logs.info "Solver end: %d" endtime_ms;
    Logs.info "RHS: %d" !rhs_event_count
end
