open Cil
open Pretty

module Var = Basetype.Variables
module ArgSet = SetDomain.ToppedSet (Var) (struct let topname = "all args" end) 

module Danger = 
struct
  include MapDomain.MapBot_LiftTop (Var) (ArgSet) 
  
  let remove_formals f st =
    let f k s st = 
      let p y = List.exists (fun x -> x.vid = y.vid) f.Cil.sformals in
      if p k
      then st
      else 
        let ns = ArgSet.filter (fun x -> not (p x)) s in
        if ArgSet.is_bot ns
        then st
        else add k ns st
    in
    fold f st (bot ())
  
  let add_formals f st =
    let add_arg st v =
      if isIntegralType v.vtype
      then st
      else add v (ArgSet.singleton v) st
    in
    List.fold_left add_arg st f.Cil.sformals
  
  
  let used_args st = 
    let rec used_args_idx = function
      | NoOffset -> ArgSet.bot ()
      | Field (_,o) -> used_args_idx o
      | Index (e,o) -> ArgSet.join (used_args_idx o) (used_args e)
    and used_args = function 
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
      | Lval    (Mem e,o) -> ArgSet.join (used_args_idx o) (used_args e)
      | CastE (_,e)           -> used_args e 
      | Lval    (Var v2,o) 
      | AddrOf  (Var v2,o) 
      | StartOf (Var v2,o) -> 
          ArgSet.join (find v2 st) (used_args_idx o)
    in
    used_args

  let used_ptrs ask = 
    let pt e = 
      match ask (Queries.MayPointTo e) with
          | `LvalSet s when not (Queries.LS.is_top s) ->
              Queries.LS.fold (fun (v,_) st -> ArgSet.add v st) s (ArgSet.empty ())
          | _ -> ArgSet.bot ()
    in
    let rec used_ptrs_idx = function
      | NoOffset -> ArgSet.bot ()
      | Field (_,o) -> used_ptrs_idx o
      | Index (e,o) -> ArgSet.join (used_ptrs_idx o) (used_ptrs e)
    and used_ptrs = function 
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | Const _ 
      | AlignOfE _ -> ArgSet.bot () 
      | UnOp  (_,e,_)     -> used_ptrs e      
      | BinOp (_,e1,e2,_) -> ArgSet.join (used_ptrs e1) (used_ptrs e2)  
      | AddrOf  (Mem e,o) 
      | StartOf (Mem e,o) 
      | Lval    (Mem e,o) -> ArgSet.join (ArgSet.join (pt e) (used_ptrs_idx o)) (used_ptrs e)
      | CastE (_,e) -> used_ptrs e
      | Lval    (Var v2,o) 
      | AddrOf  (Var v2,o) 
      | StartOf (Var v2,o) -> 
          ArgSet.bot ()
    in
    used_ptrs
    
  let warn_bad_reachables ask args fromFun st =
  
    let warn_exp e = 
      let query = if fromFun then Queries.ReachableFrom e else Queries.MayPointTo e in
      let warn_one_lv = function
        | v when (not fromFun) && v.vname = "this" -> ()
        | v ->
          let args = find v st in
          if not (ArgSet.is_bot args)    
          then Messages.report ("Calling argument "^v.vname^" may point contain pointers from "^ArgSet.short 80 args^".")
      in
      if isPointerType (typeOf (stripCasts e)) then begin 
        ArgSet.iter warn_one_lv (used_args st e) ;
        ArgSet.iter warn_one_lv (used_ptrs ask e) ;
        match ask query with
          | `LvalSet s when not (Queries.LS.is_top s) ->
              Queries.LS.iter (fun (v,_) -> warn_one_lv v) s
          | _ -> 
              Messages.report ("Argument '"^(sprint 80 (d_exp () e))^"' is unknown and may point to global data.")
  (*             () (* -- it is true but here we assume nothing important has escaped and then warn on escapes *) *)
      end
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