module A = Analyses
module M = Messages
module GU = Goblintutil
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module AD = ValueDomain.AD
(*module BS = Base.Spec*)
module LF = LibraryFunctions
open Cil
open Pretty


module Spec : Analyses.Spec with type Glob.Val.t = unit =
struct
  exception Top

  module Dom = PartitionDomain.SetSet (Lval.CilLval)
  module Glob = Global.Make (Lattice.Unit)

  let name = "Partition"

  let init () = ()
  let finalize () = ()
  let startstate = Dom.top ()
  let otherstate = Dom.top ()
  let es_to_string f es = f.svar.vname
      
  let reset_diff x = x
  let get_diff   x = []
  let should_join x y = true

  let branch a exp tv glob st     = st
  let return a exp fundec glob st = Dom.top ()
  let body a f glob st            = Dom.top ()

  let assign ask (lval:lval) (rval:exp) (glob:Glob.Var.t -> Glob.Val.t) (st:Dom.t) : Dom.t  = 
    let l = ask (Queries.MayPointTo (Cil.mkAddrOf lval)) in   
    match l, rval with
      | `LvalSet l, Lval rlval when Queries.LS.cardinal l == 1 -> begin
          let r  = ask (Queries.MayPointTo (Cil.mkAddrOf rlval)) in
          let v1 = Queries.LS.choose l in
          let st = Dom.remove v1 st in
          match r with 
            | `LvalSet r when Queries.LS.cardinal r == 1 -> 
                let v2 = Queries.LS.choose r in
                Dom.add_eq (v1,v2) st
            | _ -> st
          end
      | `LvalSet l, _ when not (Queries.LS.is_top l) ->
          Queries.LS.fold Dom.remove l st 
      | _ -> Dom.top ()

  let enter_func a lval f args glob st = [(st,Dom.top ())]
  let leave_func a lval f args glob st1 st2 = Dom.top ()
  let special_fn a lval f args glob st = [Dom.top ()]
  let fork       a lval f args glob st = []
  
  let eval_funvar a exp glob st = []

  (* query stuff *)
  
  let exp_equal ask e1 e2 (g:Glob.Var.t -> Glob.Val.t) s =
    match e1, e2 with
      | Lval  llval, Lval rlval -> begin
        let v1 = ask (Queries.MayPointTo (Cil.mkAddrOf llval)) in
        let v2 = ask (Queries.MayPointTo (Cil.mkAddrOf rlval)) in
        match v1, v2 with
          | `LvalSet v1, `LvalSet v2 when 
                 Queries.LS.cardinal v1 == 1 
              && Queries.LS.cardinal v2 == 1 -> begin
            let v1 = Queries.LS.choose v1 in
            let v2 = Queries.LS.choose v2 in          
            match Dom.find_class v1 s with
              | Some ss when Dom.S.mem v2 ss -> true
              | _ -> false
            end 
           | _ -> false end
      | _ -> false
      
  let query a g s x = 
    match x with 
      | Queries.ExpEq (e1,e2) when exp_equal a e1 e2 g s -> `Int (Queries.ID.of_bool true)
      | _ -> `Top

end

module Analysis = Multithread.Forward(Spec)
