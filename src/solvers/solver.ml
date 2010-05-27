
module Types
  (Var: Analyses.VarType) 
  (VDom: Lattice.S) 
  (G: Global.S) = 
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
  type diff        = [`G of global * glob_domain | `L of variable * var_domain] list
  type calls       = variable list (* spawned calls from thread creation *)
  type rhs         = var_assign * glob_assign -> var_domain * diff * calls
  type lhs         = variable
  type constrain   = lhs * rhs  (* constraint is an OCaml keyword *)
  type system      = lhs -> rhs list (* a set of constraints for each variable *)
  type solution    = var_assign * glob_assign
  type solution'   = var_domain VMap.t * glob_domain GMap.t

  let verify () (system: system) (sigma,theta: solution') =
    let correct = ref true in
    let complain_l (v: variable) lhs rhs = 
      correct := false; 
      ignore (Pretty.printf "Unsatisfied constraint at %a\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\nReason: %a\n@]" 
                Var.pretty_trace v VDom.pretty lhs VDom.pretty rhs VDom.why_not_leq (rhs,lhs))
    in
    let complain_g (g: global) lhs rhs = 
      correct := false; 
      ignore (Pretty.printf "Unsatisfied constraint for global %a\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\n@]" 
                G.Var.pretty_trace g G.Val.pretty lhs G.Val.pretty rhs)
    in
    (* For each variable v which has been assigned value d', would like to check
     * that d' satisfied all constraints. *)
    let verify_var v d' = 
      let verify_constraint rhs =
        let sigma' x = VMap.find sigma x in
        let theta' x = GMap.find theta x in
        let (d,gs,s) = rhs (sigma',theta') in
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
                complain_g g gv' gv  in
        let _ = List.iter check_glob gs in
        (* Then we check that the local state satisfies this constraint. *)
          if not (VDom.leq d d') then
            complain_l v d' d
      in
      let rhs = system v in
        List.iter verify_constraint rhs 
    in
      VMap.iter verify_var sigma
end

