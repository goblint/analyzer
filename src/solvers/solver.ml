
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
  type calls       = variable list (* spawned calls from thread creation *)
  type rhs         = var_assign * glob_assign -> var_domain * glob_diff * calls
  type lhs         = variable
  type constrain   = lhs * rhs  (* constraint is an OCaml keyword *)
  type system      = lhs -> rhs list (* a set of constraints for each variable *)
  type solution    = var_assign * glob_assign
  type solution'   = var_domain VMap.t * glob_domain GMap.t

  let verify () (system: system) (sigma,theta: solution') =
    let correct = ref true in
    let complain_l (v: variable) = 
      correct := false; 
      ignore (Pretty.printf "BAD: %a\n" Var.pretty_trace v)
    in
    let complain_g (g: global) = 
      correct := false; 
      ignore (Pretty.printf "BAD: %a\n" G.Var.pretty_trace g)
    in
    let verify_var v d' = 
      let verify_constraint rhs =
        let sigma' x = VMap.find sigma x in
        let theta' x = GMap.find theta x in
        let (d,g,s) = rhs (sigma',theta') in
        let check_glob (g,gv) = 
          let gv' = GMap.find theta g in 
            if not (G.Val.leq gv gv') then 
              complain_g g in
        let _ = List.iter check_glob g in
          if not (VDom.leq d d') then
            complain_l v
      in
      let rhs = system v in
        List.iter verify_constraint rhs 
    in
      VMap.iter verify_var sigma
end

