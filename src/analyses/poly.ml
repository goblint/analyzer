
open Batteries
open Cil
open Pretty
open Analyses
open Apron

open ApronDomain

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "poly"

  module D = D
  module G = Lattice.Unit
  module C = D
  
  let val_of x = x
  let context x = x
  
  let otherstate _ = D.top ()       
  let exitstate  _ = D.top ()      
  let startstate _ = D.top ()       
  
  let enter ctx r f args = 
    let f = Cilfacade.getdec f in
    let newnames = List.map (fun x -> x.vname) f.sformals @
                   List.map (fun x -> x.vname^"'") f.sformals in
    let newd = D.add_vars ctx.local newnames in
    List.iter2 (fun v e -> D.assign_var_with newd    (v.vname^"'") e) f.sformals args;
    D.forget_all_with newd (List.map (fun x -> x.vname) f.sformals);
    List.iter  (fun v   -> D.assign_var_eq_with newd v.vname (v.vname^"'")) f.sformals;
    D.remove_all_but_with newd newnames;
    [ctx.local, newd]  
  
  
  let combine ctx r fe f args d = 
    let f = Cilfacade.getdec f in
    match r with
      | Some (Var v, NoOffset) ->
        let nd = D.forget_all ctx.local [v.vname] in
        let nd' = D.add_vars d (List.map (fun x -> Var.to_string x) (D.get_vars ctx.local)) in
        List.iter2 (fun v e -> D.substitute_var_with nd' (v.vname^"'") e) f.sformals args;
        D.remove_all_with nd' (List.map (fun x -> x.vname^"'") f.sformals);
        D.forget_all_with nd' [v.vname];
        D.substitute_var_eq_with nd' "#ret" v.vname;
        D.remove_all_with nd' ["#ret"];
        A.unify Man.mgr nd nd'
      | _ -> D.top ()
      
  let special ctx r f args = D.top () 
  
  let branch ctx e b = D.assert_inv ctx.local e b 
    
  let return ctx e f = 
    match e with 
      | Some e -> 
          let nd = D.add_vars ctx.local ["#ret"] in
          D.assign_var_with nd "#ret" e;
          let vars = List.map (fun x -> x.vname) (f.slocals @ f.sformals) in
          D.remove_all_with nd vars;
          nd
      | None -> D.top ()
  
  let body ctx f = 
    let vars = List.map (fun x -> x.vname) f.slocals in
    D.add_vars ctx.local vars
    
  let assign ctx (lv:lval) e = 
    match lv with
      | Var v, NoOffset -> D.assign_var ctx.local v.vname e
      | _ -> D.top ()
end

let _ = 
  MCP.register_analysis (module Spec : Spec)
