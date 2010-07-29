open Cil
open Pretty

module Var = Basetype.Variables
module ArgSet = SetDomain.ToppedSet (Var) (struct let topname = "all args" end) 

module Danger = 
struct
  include MapDomain.MapBot_LiftTop (Var) (ArgSet) 
  
  let remove_formals f st =
    let remove_arg st v =
      remove v st
    in
    if is_top st
    then st
    else List.fold_left remove_arg st f.Cil.sformals
  
  let add_formals f st =
    let add_arg st v =
      add v (ArgSet.singleton v) st
    in
    if is_top st
    then st
    else List.fold_left add_arg st f.Cil.sformals
  
  
  let used_args st = 
    let rec used_args = function 
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | Const _ 
      | AlignOfE _ -> ArgSet.bot () 
      | UnOp  (_,e,_)     -> used_args e      
      | BinOp (_,e1,e2,_) -> ArgSet.join (used_args e1) (used_args e2)  
      | AddrOf  (Mem e,o) 
      | StartOf (Mem e,o) 
      | Lval    (Mem e,o) -> used_args e
      | CastE (_,e)           -> used_args e 
      | Lval    (Var v2,o) 
      | AddrOf  (Var v2,o) 
      | StartOf (Var v2,o) -> 
          try find v2 st
          with Not_found _ -> ArgSet.bot ()
    in
    used_args

  let warn_bad_reachables ask args st =
    let warn_exp e = 
      let warn_one_lv v =
        try 
          let args = find v st in
          Messages.report ("Calling argument "^v.vname^" may point contain pointers from "^ArgSet.short 80 args^".")
        with Not_found _ -> ()
      in
      ArgSet.iter warn_one_lv (used_args st e);
      match ask (Queries.ReachableFrom e) with
        | `LvalSet s when not (Queries.LS.is_top s) ->
            Queries.LS.iter (fun (v,_) -> warn_one_lv v) s
        | _ -> 
            Messages.report ("Argument '"^(sprint 80 (d_exp () e))^"' is unknown and may point to global data.")
    in
    List.iter warn_exp args
    
 
  let assign_argmap ask lval exp st =
    let assign_to_lval args =
      match lval with 
        | Var v , ofs -> add v args st
        | Mem e , ofs -> 
      match ask (Queries.MayPointTo e) with
        | `Bot -> bot ()
        | `LvalSet s when not (Queries.LS.is_top s) ->
            let add_lv (v,_) st = 
              add v args st
            in
            Queries.LS.fold add_lv s st
        | _ ->  
            Messages.warn ("Need to know where "^(sprint 80 (d_exp () exp))^" may point.");
            st
    in
    match used_args st exp with
      | s when ArgSet.is_top s ->
          Messages.warn ("Expression "^(sprint 80 (d_exp () exp))^" too complicated.");
          st
      | s when ArgSet.is_bot s -> st
      | s -> assign_to_lval s
          
end