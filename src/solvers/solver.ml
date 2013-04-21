open Batteries 
(** Types & utilities for solvers. *)

module GU = Goblintutil
module Types
  (Var: Analyses.VarType) 
  (VDom: Lattice.S) 
  (G: Glob.S) =
struct
  module VMap = Hash.Make(Var)  
  module GMap = Hash.Make(G.Var)
  type variable    = Var.t
  type global      = G.Var.t
  type var_domain  = VDom.t
  type glob_domain = G.Val.t
  type var_assign  = variable -> var_domain
  type glob_assign = global -> glob_domain
  type glob_diff   = (global * glob_domain) list
  type effect_fun  = [`G of (global * glob_domain) | `L of (variable * var_domain)] -> unit
  type calls       = variable list (* spawned calls from thread creation *)
  type rhs         = var_assign * glob_assign -> effect_fun -> var_domain * calls
  type lhs         = variable
  type constrain   = lhs * rhs  (* constraint is an OCaml keyword *)
  type system      = lhs -> rhs list (* a set of constraints for each variable *)
  type solution    = var_assign * glob_assign
  type solution'   = var_domain VMap.t * glob_domain GMap.t
  
  let garbage_collect (system:system) (vars:variable list) (sigma, theta: solution') : solution' =
    let nsigma = VMap.create (VMap.length sigma) (VDom.bot ()) in
    let ntheta = GMap.create (GMap.length theta) (G.Val.bot ()) in
    let visited = VMap.create 111 false in
    let apply_diff = function
      | `G (var, value) -> GMap.replace ntheta var (G.Val.join value (GMap.find ntheta var))
      | `L (var, value) -> VMap.replace nsigma var (VDom.join value (VMap.find nsigma var))      
    in
    let rec visit_lhs var =
      if VMap.find visited var then () else begin
        VMap.replace visited var true;
        List.iter (visit_rhs var) (system var)
      end
    and visit_rhs var rhs = 
      let v, c = rhs (get_local, GMap.find ntheta) apply_diff in
      VMap.replace nsigma var (VDom.join v (VMap.find nsigma var));
      List.iter visit_lhs c
    and get_local var =
      visit_lhs var;
      VMap.find nsigma var
    in
    List.iter visit_lhs vars;
    (nsigma,ntheta)

  let verify () (system: system) (sigma,theta: solution') =
    Goblintutil.in_verifying_stage := true;
    let correct = ref true in
    let complain_l (v: variable) lhs rhs = 
      correct := false; 
      ignore (Pretty.printf "Fixpoint not reached at %a (%s:%d)\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\nCalculating one more step changes: %a\n@]" 
                Var.pretty_trace v (Var.file_name v) (Var.line_nr v) VDom.pretty lhs VDom.pretty rhs VDom.pretty_diff (rhs,lhs))
    in
    let complain_g v (g: global) lhs rhs = 
      correct := false; 
      ignore (Pretty.printf "Unsatisfied constraint for global %a at variable %a\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\n@]" 
                G.Var.pretty_trace g Var.pretty_trace v G.Val.pretty lhs G.Val.pretty rhs)
    in
    (* For each variable v which has been assigned value d', would like to check
     * that d' satisfied all constraints. *)
    let verify_var v d' = 
      let verify_constraint rhs =
        let sigma' x = VMap.find sigma x in
        let theta' x = GMap.find theta x in
        (* First check that each (global) delta is included in the (global)
         * invariant. *)
        let check_glob = function
          | `L (l,lv) ->
            let lv' = VMap.find sigma l in 
              if not (VDom.leq lv lv') then 
                complain_l l lv' lv  
          | `G (g,gv) -> 
            let gv' = GMap.find theta g in 
              if not (G.Val.leq gv gv') then 
                complain_g v g gv' gv  in
        let (d,s) = rhs (sigma',theta') check_glob in
        (* Then we check that the local state satisfies this constraint. *)
          if not (VDom.leq d d') then
            complain_l v d' d;
          if (not @@ List.for_all (VMap.mem sigma) s) then begin
            correct := false;
            let ex = List.hd @@ List.filter (not % VMap.mem sigma) s in 
            ignore (Pretty.printf "Constraint spawns a thread that is not in the solution!\n  @[Variable:\n%a\n@]" 
                      Var.pretty_trace ex)
          end
      in
      let rhs = system v in
        List.iter verify_constraint rhs
    in
      VMap.iter verify_var sigma;
      Goblintutil.in_verifying_stage := false
end

(* SharirPnueli algo *)

type proc = Cil.varinfo (* CIL.something *)

type nodeKind = 
  [ `ProcCall  
  | `ExitOfProc of proc
  | `Other               ]

module type NodeType =
sig
  include Analyses.VarType 
  val kind : t -> nodeKind
end

module Prod (O1:NodeType) (O2:Lattice.S) =
struct
  type t = O1.t * O2.t
  let compare (u1,u2) (v1,v2) =
    match O1.compare u1 v1 with
      | 0 -> O2.compare u2 v2
      | n -> n
  let equal (u1,u2) (v1,v2) = O1.equal u1 v1 && O2.equal u2 v2
  let category (u,_) = O1.category u
  let hash (u,v) = O1.hash u lxor O2.hash v
  let pretty_trace () (u,v) =
    Pretty.dprintf "(%a,%a)" O1.pretty_trace u O2.pretty v
    
  let line_nr (n,_) = O1.line_nr n 
  let file_name (n,_) = O1.file_name n 
  let description (n,_) = O1.description n 
  let context () (_,c) = O1.context () c 
end

  
